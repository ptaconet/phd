library(ggplot2)
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(purrr)
library(patchwork)
library(plotly)
library(sf)
library(googlesheets4)
library(furrr)
library(stringr)
library(patchwork)
library(lubridate)
library(ranger)
library(sp)
library(gstat)
library(raster)
library(mlr)
library(tuneRanger)
library(mlr)
library(GSIF)
library(gstat)
library(spdplyr)
require(ncf)
require(landscapemetrics)
require(correlation)
library(glmmTMB)
library(GGally)
library(tidyverse)
library(ggplot2)
library(car)
library(DHARMa)
library(buildmer)
library(mltools)
library(CAST)
library(caret)

### connect to the database
path_to_db <- "data/react_db/react_db.gpkg" 
react_gpkg <- DBI::dbConnect(RSQLite::SQLite(),dbname = path_to_db) 


### open the tables
## dates and positions of the entomological missions (1 row = 1 point de capture)
entomo_csh_metadata_l1 <- dbReadTable(react_gpkg, 'entomo_csh_metadata_l1') %>% filter(!(nummission %in% c("11","12","13","15")))

## table containing the response variables (ie variables to model)
#trmetrics_entomo_postedecapture <- dbReadTable(react_gpkg, 'trmetrics_entomo_postedecapture') %>% dplyr::select(-fid) %>% left_join(entomo_csh_metadata_l1 %>% dplyr::select(idpointdecapture, codevillage, pointdecapture, codepays, nummission, period_interv)) %>% filter(!is.na(codevillage))

## table of exhaustive definitions of the explanatory variables
googlesheets4::sheets_deauth()
prediction_vars <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1dIeSOa2WinXvOQGLmIjA0gFdsHnb6zMMsDME-G5pyMc/edit?usp=sharing", sheet = "var_explication", col_types="c")


### source home-made functions 
source("r_scripts/data_analysis_tests/functions_script_data_analysis.R")


###### input parameters
# response_var <- "ma_funestus_ss"  # "ma_gambiae_ss"
# code_pays <- "BF"            # "BF"
# mod <- "abundance" # abundance, presence        si abondance + GLMM : glmm negatif binomial zero tronqué ; si présence + GLMM : glmm binomial ; si abondance + RF : RF regression ; si abondance + RF : RF classification 
# model_type <- "glmm"  # glmm, rf       si RF : VCM en dummy variables
# intervention <- "pre_intervention"  # pre_intervention, post_intervention, all
# predictive_type <- "roi_villages" #  "roi_villages" (within the ROI, for the sampled villages)  or "roi_notvillages" (within the ROI, for the not sampled villages) or  "notroi" (outside the roi)




fun_workflow_model <- function(response_var, 
                               code_pays, 
                               mod, 
                               model_type, 
                               intervention, 
                               predictive_type, 
                               timevars_selection = "1day", 
                               lag_time_window = c(0,60),
                               buffer_sizes = c(2000)){
  
cat("Executing workflow for parameters : ", response_var, code_pays, mod, model_type, intervention, predictive_type,"\n")
  
###### load the data

# load spatiotemporal data
env_spatiotemporal <- load_spatiotemporal_data(vars = c("RFD1F","TMIN1","TMAX1"), 
                         buffers = buffer_sizes,
                         lag_time_window = lag_time_window,
                         summarize_days_to_week = FALSE,
                         code_pays = code_pays,
                         entomo_csh_metadata_l1 = entomo_csh_metadata_l1)

# load spatial data 
if(predictive_type == "notroi"){
  landcover_layers_to_keep <- c(11,12)
} else {
  if(code_pays == "BF"){
    landcover_layers_to_keep <- c(3,4,5)
  } else if (code_pays == "CI"){
    landcover_layers_to_keep <- c(8,9,10)
  }
}

landcover_metrics_to_keep <- NULL
landcover_metrics_to_keep <- "pland"

env_spatial_all <- load_spatial_data(code_pays, landcover_layers_to_keep, mod, landcover_metrics_to_keep, buffer_sizes)
env_landcover <- env_spatial_all[[1]]
env_spatial <- env_spatial_all[[2]]
th_env_nightcatch_postedecapture <- env_spatial_all[[3]]
th_env_nightcatch <- env_spatial_all[[4]]
th_env_static <- env_spatial_all[[5]]
rm(env_spatial_all)

# load coordinates
spatial_coordinates <- load_csh_sp_coord()
mean_coords_points_4326 <- spatial_coordinates[[1]]
mean_coords_points_32630 <- spatial_coordinates[[2]]
rm(spatial_coordinates)

# load human beahviour use data
hum_behav <- load_hmnbehav_data(code_pays,entomo_csh_metadata_l1)

# load response variable
if(mod %in% c("presence","abundance")){
  th_trmetrics_entomo_postedecapture <- dbReadTable(react_gpkg, 'trmetrics_entomo_postedecapture') %>% 
    dplyr::select(-fid) %>% 
    left_join(entomo_csh_metadata_l1 %>% dplyr::select(idpointdecapture, codevillage, pointdecapture, codepays, nummission, period_interv)) %>% 
    filter(!is.na(codevillage)) %>%
    filter(codepays == code_pays) %>%
    mutate(heuredecapture = NA)

  th_trmetrics_entomo_postedecapture$resp_var <- th_trmetrics_entomo_postedecapture[,response_var]
} else if(mod %in% c("physiological_resistance_kdrw","physiological_resistance_kdre","exophagy","late_prec_aggressiveness")){
  
  if(response_var == "ma_funestus_ss"){
    response_var <- "An.funestus_ss"
  } else if(response_var == "ma_coluzzi"){
    response_var <- "An.coluzzii"
  } else if(response_var == "ma_gambiae_ss"){
    response_var <- "An.gambiae_ss"
  }
  
  th_trmetrics_entomo_postedecapture <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
    dplyr::select(-fid) %>% 
    filter(codepays == code_pays, pcr_espece == response_var) %>%
    mutate(nummission = as.character(nummission))
    
  if(mod=="late_prec_aggressiveness"){
    
    th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
      mutate(resp_var = ifelse(heuredecapture >= 6 & heuredecapture <= 20,1,0))

  } else if (mod=="exophagy"){
    
    th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
      mutate(resp_var = ifelse(postedecapture == "e",1,0))
    
  } else if (mod=="physiological_resistance_kdrw"){
    th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
      mutate(resp_var = ifelse(kdrw == "RR",1,0)) %>%
      mutate(resp_var = ifelse(kdrw == "RS",0.5,resp_var))
  } else if (mod=="physiological_resistance_kdre"){
    th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
      mutate(resp_var = ifelse(kdre == "RR",1,0)) %>%
      mutate(resp_var = ifelse(kdre == "RS",0.5,resp_var))
  }
  
}

if(intervention == "pre_intervention"){
  th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% filter(period_interv == "pre_intervention")
} else if(intervention == "post_intervention"){
  th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% filter(period_interv == "post_intervention")
}

if(mod == "abundance" ){
  th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% filter(resp_var > 0 )
} else if (mod == "presence" ){
  th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% mutate(resp_var = ifelse(resp_var == 0,0,1 ))
}

if(model_type == "rf"){
  th_env_static <- th_env_static %>% mutate(v = 1) %>% pivot_wider(names_from = VCM,  values_from = v, values_fill = list(v = 0), names_prefix = "VCM_")
}

mean_date_mission <- entomo_csh_metadata_l1 %>% mutate(date_capture = as.Date(date_capture)) %>% dplyr::group_by(codepays,nummission) %>% dplyr::summarise(mean_date_mission=mean(date_capture)) %>% as_tibble() %>% filter(codepays==code_pays) %>% dplyr::select(-codepays) 


######## join response variable with explanatory variables

th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% 
  left_join(mean_date_mission) %>%
  left_join(mean_coords_points_4326) %>%
  left_join(mean_coords_points_32630) %>%
  mutate(int_ext = substr( idpostedecapture,nchar(idpostedecapture),nchar(idpostedecapture))) %>%
  dplyr::select(idpostedecapture,idpointdecapture,heuredecapture,int_ext,pointdecapture,codevillage,codepays,nummission,mean_date_mission,X_4326,Y_4326,X_32630,Y_32630,resp_var) %>%
  left_join(env_spatiotemporal) %>%
  left_join(env_spatial) %>%
  left_join(th_env_nightcatch_postedecapture) %>%
  left_join(th_env_nightcatch) %>%
  left_join(th_env_static) %>%
  left_join(env_landcover) %>%
  left_join(hum_behav) %>%
  mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))
th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture[, colSums(is.na(th_trmetrics_entomo_postedecapture)) != nrow(th_trmetrics_entomo_postedecapture)]

######## feature forward selection of temporal variables

  df_temporal <- NULL
  
  time_vars <- c("RFD1F","TMIN1","TMAX1")

  df_temporal_corr <- data.frame(var = character(), col_to_keep = character(), correlation = numeric())
  ccm_plots <- list()
  ts_plots <- list()
### select the first temporal variable
for(i in 1:length(time_vars)){

  cat("Calculating CCM for variable ",time_vars[i],"\n")
  expl_vars_to_test =  colnames(th_trmetrics_entomo_postedecapture[which(grepl(time_vars[i],colnames(th_trmetrics_entomo_postedecapture)))])
    
  if(timevars_selection == "7day"){
    if(mod %in% c("presence","abundance")){
    expl_vars_to_test <- c(paste0(time_vars[i],"_2000_0_7"),
                           paste0(time_vars[i],"_2000_7_14"),
                           paste0(time_vars[i],"_2000_14_21"),
                           paste0(time_vars[i],"_2000_21_28"),
                           paste0(time_vars[i],"_2000_28_35"),
                           paste0(time_vars[i],"_2000_35_42"),
                           paste0(time_vars[i],"_2000_0_14"),
                           paste0(time_vars[i],"_2000_0_21"),
                           paste0(time_vars[i],"_2000_0_28"),
                           paste0(time_vars[i],"_2000_0_35"),
                           paste0(time_vars[i],"_2000_0_42"),
                           paste0(time_vars[i],"_2000_7_21"),
                           paste0(time_vars[i],"_2000_7_28"),
                           paste0(time_vars[i],"_2000_7_35"),
                           paste0(time_vars[i],"_2000_7_42"),
                           paste0(time_vars[i],"_2000_14_28"),
                           paste0(time_vars[i],"_2000_14_35"),
                           paste0(time_vars[i],"_2000_14_42"),
                           paste0(time_vars[i],"_2000_21_35"),
                           paste0(time_vars[i],"_2000_21_42"),
                           paste0(time_vars[i],"_2000_28_42")
                           )
    } else {
      expl_vars_to_test <- c(paste0(time_vars[i],"_2000_0_31"),
                             paste0(time_vars[i],"_2000_31_60"),
                             paste0(time_vars[i],"_2000_0_60")
      )
    }
  }
  
  corr <- fun_feature_forward_selection(
    df = th_trmetrics_entomo_postedecapture,
    stat_method = "spearman",
    spearman_factor = "codevillage",
    mod = mod,
    type = "univariate_selection",
    expl_vars_to_keep = NULL,
    expl_vars_to_test = expl_vars_to_test)

  
  corr$time_lag_1 <- as.numeric(sub('.*\\_', '', corr$name))
  corr$time_lag_2 <- as.numeric(stringr::str_match( corr$name, '([^_]+)(?:_[^_]+){1}$')[,2])
  if(timevars_selection == "7day"){
    corr <- corr %>%
      mutate(time_lag_1 = time_lag_1/7) %>%
      mutate(time_lag_2 = time_lag_2/7)
  }
  corr$diff_lag <- corr$time_lag_1 - corr$time_lag_2
  corr <- arrange(corr, time_lag_1, time_lag_2)

  corr$correlation[which(corr$pval > 0.05)] <- NA

  # plot the CCM
  time_var_name <- data.frame(code = time_vars[i]) %>% left_join( prediction_vars %>% dplyr::select(code,long_name))
  ccm_plots[[i]] <- fun_ccm_plot(corr, "max", "Spearman\ncorrelation",time_var_name$long_name, filter_lags_ndays = 0)

  # plot the timeseries
  #ts_plots[[i]] <- fun_spatiotemparal_plots(th_trmetrics_entomo_postedecapture, mod, time_vars[i], code_pays)
    
  # keep best time lag
  # if(grepl("1", time_vars[i])){
  #   corr <- corr %>% filter(diff_lag >= 4)
  # }

  corr <- corr %>% filter(!is.na(correlation), pval <= 0.05, abs_corr >= 0.2)

  if(nrow(corr) > 0){
    col_to_keep <- corr$name[which.max(abs(corr$abs_corr))]
    correlation <- corr$correlation[which.max(abs(corr$abs_corr))]
  } else {
    col_to_keep <- NA
    correlation <- NA
  }

  th_df_temporal <- data.frame(var = time_vars[i], col_to_keep = col_to_keep, correlation = correlation)

  df_temporal_corr <- rbind(df_temporal_corr, th_df_temporal) %>%
    dplyr::filter(!is.na(correlation))
  
}

  if(nrow(df_temporal_corr) > 0){
  # add labels
  df_temporal_corr <- df_temporal_corr %>%
    rename(name = col_to_keep) %>%
    mutate(name1 = gsub("_"," ",name)) %>%
    mutate(var = word(name1,1)) %>%
    mutate(buffer = word(name1,2)) %>%
    mutate(lag1 = word(name1,3)) %>%
    mutate(lag2 = word(name1,4)) %>%
    mutate(var = ifelse(var == "RFD1", "RFD1F", var)) %>%
    mutate(var = ifelse(var == "TMAX7", "TMAX1", var)) %>%
    mutate(var = ifelse(var == "TMIN7", "TMIN1", var)) %>%
    mutate(var = ifelse(var == "TAMP7", "TAMP1", var)) %>%
    mutate(var = ifelse(var == "SMO7", "SMO1", var)) %>%
    left_join(prediction_vars %>% dplyr::select(code,short_name), by = c("var"="code")) %>%
    dplyr::rename(lab = short_name) %>%
    mutate(lab = paste0(lab," - ",buffer," m buffer"," \n mean b/w ",as.numeric(lag1)/7," and ",as.numeric(lag2)/7," week(s) before the catch")) %>%
    mutate(lab = ifelse(grepl("RFD1",name),gsub("mean","sum",lab),lab)) %>%
    dplyr::select(-c(name1,buffer,lag1,lag2,var)) %>%
    mutate(type = "Climate")

  df_temporal <- df_temporal_corr
  
  }

if(nrow(df_temporal_corr) > 1){
#best_time_var <- as.character(df_temporal_corr$name[which.max(abs(df_temporal_corr$correlation))])

cols_to_keep_best_time_var <- as.character(df_temporal_corr$name[which.max(abs(df_temporal_corr$correlation))])
# cols_to_keep_best_time_var <- as.character(df_temporal_corr$name)
# index_cols_to_keep_best_time_var <- grep("RFD1F",cols_to_keep_best_time_var)
# cols_to_keep_best_time_var <- cols_to_keep_best_time_var[index_cols_to_keep_best_time_var]

### now forward selection to select the next 2 temporal variables
# 2nd time var
# time_vars <- setdiff(time_vars, best_time_var)
ffs_temp_it2 <- fun_ffs_tempvar(df = th_trmetrics_entomo_postedecapture, model_type, mod, time_vars, cols_to_keep_best_time_var, timevars_selection)
# best_time_var <- as.character(ffs_temp_it2$var[which.min(ffs_temp_it2$res)])
cols_to_keep_best_time_var <- c(cols_to_keep_best_time_var, as.character(ffs_temp_it2$name[which.min(ffs_temp_it2$res)]))

# 3d time var
#time_vars <- setdiff(time_vars, best_time_var)
ffs_temp_it3 <- fun_ffs_tempvar(df = th_trmetrics_entomo_postedecapture, model_type, mod, time_vars, cols_to_keep_best_time_var, timevars_selection)
#best_time_var <- as.character(ffs_temp_it3$var[which.min(ffs_temp_it3$res)])
cols_to_keep_best_time_var <- c(cols_to_keep_best_time_var, as.character(ffs_temp_it3$name[which.min(ffs_temp_it3$res)]))
#time_vars <- setdiff(time_vars, best_time_var)


df_temporal <- data.frame(name = cols_to_keep_best_time_var)
df_temporal$correlation <- NA
df_temporal$type <- "climate-related"

# add labels
df_temporal <- df_temporal %>%
  mutate(name1 = gsub("_"," ",name)) %>%
  mutate(var = word(name1,1)) %>%
  mutate(buffer = word(name1,2)) %>%
  mutate(lag1 = word(name1,3)) %>%
  mutate(lag2 = word(name1,4)) %>%
  mutate(var = ifelse(var == "RFD1", "RFD1F", var)) %>%
  mutate(var = ifelse(var == "TMAX7", "TMAX1", var)) %>%
  mutate(var = ifelse(var == "TMIN7", "TMIN1", var)) %>%
  mutate(var = ifelse(var == "TAMP7", "TAMP1", var)) %>%
  mutate(var = ifelse(var == "SMO7", "SMO1", var)) %>%
  left_join(prediction_vars %>% dplyr::select(code,short_name), by = c("var"="code")) %>%
  dplyr::rename(lab = short_name) %>%
  mutate(lab = paste0(lab," - ",buffer," m buffer"," \n mean b/w ",as.numeric(lag1)/7," and ",as.numeric(lag2)/7," week(s) before the catch")) %>%
  mutate(lab = ifelse(grepl("RFD1",name),gsub("mean","sum",lab),lab)) %>%
  dplyr::select(-c(name1,buffer,lag1,lag2,var))

for (i in 1:nrow(df_temporal)){
  df_temporal$correlation[i] <- correlation::correlation(data.frame(th_trmetrics_entomo_postedecapture$resp_var,th_trmetrics_entomo_postedecapture[,as.character(df_temporal$name[i])],th_trmetrics_entomo_postedecapture$codevillage), method = "spearman", multilevel = TRUE)$r
}

  }
  
######## selection of spatial variables
corr_lsm <- NULL
corr_env_spatial <- NULL
corr_env_nightcatch <- NULL
corr_env_nightcatch_postedecapture <- NULL
corr_env_static <- NULL
corr_env_hum_behav <- NULL

## filter landcover variables with spearman coefficient 
lco_metadata <- dbReadTable(react_gpkg, 'lco_metadata') # table containing pixel value and label for each land cover map
metrics_defs <- landscapemetrics::list_lsm() # list of landscape metrics

expl_vars_to_test =  colnames(th_trmetrics_entomo_postedecapture[which(grepl("lsm",colnames(th_trmetrics_entomo_postedecapture)))])

if(model_type=="glmm"){
  if(code_pays=="BF"){
    expl_vars_to_test =  colnames(th_trmetrics_entomo_postedecapture[which(grepl("lsm_c_pland_2000_3",colnames(th_trmetrics_entomo_postedecapture)))])
  } else if(code_pays=="CI"){
    expl_vars_to_test =  colnames(th_trmetrics_entomo_postedecapture[which(grepl("lsm_c_pland_2000_8",colnames(th_trmetrics_entomo_postedecapture)))])
  }
}

expl_vars_to_test_lcid <- as.numeric(word(gsub("_"," ",expl_vars_to_test),5))
expl_vars_to_test_buffer <- as.numeric(word(gsub("_"," ",expl_vars_to_test),4))


corr_lsm1 <- fun_feature_forward_selection(
  df = th_trmetrics_entomo_postedecapture,
  stat_method = "spearman", 
  mod = mod, 
  type = "univariate_selection", 
  expl_vars_to_test = expl_vars_to_test)

corr_lsm1 <- corr_lsm1 %>% filter(abs_corr >= 0.2, pval <= 0.2)

var_to_remove <- NULL

if(nrow(corr_lsm1) > 1){
  
  m <- th_trmetrics_entomo_postedecapture[,corr_lsm1$name] %>%
    cor(.,method = "pearson", use = "na.or.complete")
  index <- which(abs(m) > .7 & abs(m) < 1,arr.ind = T) 
  p <- cbind.data.frame(stock1 = rownames(m)[index[,1]], stock2 = colnames(m)[index[,2]])
  
  p <- p %>%
    mutate(name1 = gsub("_"," ",stock1)) %>%
    mutate(function_name1 = paste(word(name1,1),word(name1,2),word(name1,3),sep="_")) %>%
    mutate(buffer1 = as.numeric(word(name1,4))) %>%
    mutate(layer_id = as.numeric(word(name1,5))) %>%
    mutate(pixval = as.numeric(word(name1,6))) %>%
    left_join(corr_lsm1 %>% dplyr::select(name,correlation), by = c("stock1"="name")) %>%
    left_join(lco_metadata %>% dplyr::select(pixval,pixlabel,layer_id,priority2)) %>%
    dplyr::rename(priority = priority2) %>%
    dplyr::rename(layer_id1 = layer_id, pixval1 = pixval, pixlabel1 = pixlabel, priority1 = priority, correlation1 =  correlation) %>%
    mutate(name2 = gsub("_"," ",stock2)) %>%
    mutate(function_name2 = paste(word(name2,1),word(name2,2),word(name2,3),sep="_")) %>%
    mutate(buffer2 = as.numeric(word(name2,4))) %>%
    mutate(layer_id = as.numeric(word(name2,5))) %>%
    mutate(pixval = as.numeric(word(name2,6))) %>%
    left_join(corr_lsm1 %>% dplyr::select(name,correlation), by = c("stock2"="name")) %>%
    left_join(lco_metadata %>% dplyr::select(pixval,pixlabel,layer_id,priority2)) %>%
    dplyr::rename(priority = priority2) %>%
    dplyr::rename(layer_id2 = layer_id, pixval2 = pixval, pixlabel2 = pixlabel, priority2 = priority, correlation2 =  correlation) %>%
    mutate(priority1 = ifelse(is.na(priority1),100,priority1)) %>%
    mutate(priority2 = ifelse(is.na(priority2),100,priority2)) 
  
  if(nrow(p)>0){
    
    for(i in 1:nrow(p)){
      if(p$priority1[i] < p$priority2[i]){
        var_to_remove <- c(var_to_remove,p$stock2[i])
      } else if (p$priority1[i] > p$priority2[i]) {
        var_to_remove <- c(var_to_remove,p$stock1[i])
      } else if (p$priority1[i] == p$priority2[i]) {  # case the lc class and the lc layer are the same
        if(p$correlation1[i] < p$correlation2[i]){
          var_to_remove <- c(var_to_remove,p$stock1[i])
        } else {
          var_to_remove <- c(var_to_remove,p$stock2[i])
        }
      }
    }
    
    var_to_remove <- unique(var_to_remove)
  }
}

corr_lsm <- corr_lsm1 %>% 
  dplyr::filter(!(name %in% var_to_remove)) %>%
  mutate(name = gsub("_"," ",name)) %>%
  mutate(function_name = paste(word(name,1),word(name,2),word(name,3),sep="_")) %>%
  mutate(buffer = as.numeric(word(name,4))) %>%
  mutate(layer_id = as.numeric(word(name,5))) %>%
  mutate(pixval = as.numeric(word(name,6)))

cor_df2 <- corr_lsm %>%
  group_by(layer_id,pixval) %>%
  summarise(abs_corr=max(abs_corr)) %>%
  as_tibble() %>%
  mutate(to_keep = TRUE)

corr_lsm <- corr_lsm %>%
  left_join(cor_df2) %>%
  filter(to_keep==TRUE)

cor_df_prd <- corr_lsm %>%
  filter(is.na(pixval)) %>%
  filter(abs_corr==max(abs_corr))

corr_lsm <- corr_lsm %>%
    filter(!is.na(pixval)) %>%
    bind_rows(cor_df_prd) %>%
    mutate(name = gsub(" ","_", name)) %>%
    left_join(lco_metadata) %>%
    dplyr::select(name,correlation,pixlabel,buffer) %>%
    rename(lab = pixlabel) %>%
    mutate(lab = gsub("_"," ",lab)) %>%
  mutate(lab = paste0("Surface de ",lab," - ",buffer," m buffer")) %>%
  mutate(type = "Land cover") %>%
    dplyr::select(-buffer)


  
corr_lsm_univ <- corr_lsm1 %>% 
  mutate(name = gsub("_"," ",name)) %>%
  mutate(function_name = paste(word(name,1),word(name,2),word(name,3),sep="_")) %>%
  mutate(buffer = as.numeric(word(name,4))) %>%
  mutate(layer_id = as.numeric(word(name,5))) %>%
  mutate(pixval = as.numeric(word(name,6)))

cor_df2 <- corr_lsm_univ %>%
  group_by(layer_id,pixval) %>%
  summarise(abs_corr=max(abs_corr)) %>%
  as_tibble() %>%
  mutate(to_keep = TRUE)

corr_lsm_univ <- corr_lsm_univ %>%
  left_join(cor_df2) %>%
  filter(to_keep==TRUE)

cor_df_prd <- corr_lsm_univ %>%
  filter(is.na(pixval)) %>%
  filter(abs_corr==max(abs_corr))

corr_lsm_univ <- corr_lsm_univ %>%
  filter(!is.na(pixval)) %>%
  bind_rows(cor_df_prd) %>%
  mutate(name = gsub(" ","_", name)) %>%
  left_join(lco_metadata) %>%
  dplyr::select(name,correlation,pixlabel,buffer) %>%
  rename(lab = pixlabel) %>%
  mutate(lab = gsub("_"," ",lab)) %>%
  mutate(lab = paste0("Surface de ",lab," - ",buffer," m buffer")) %>%
  mutate(type = "Land cover") %>%
  dplyr::select(-buffer)


## spatial-only variables
expl_vars_to_test = setdiff(colnames(env_spatial),"idpointdecapture")
corr_env_spatial <- fun_feature_forward_selection(
  df = th_trmetrics_entomo_postedecapture, 
  stat_method = "spearman", 
  mod = mod, 
  type = "univariate_selection", 
  expl_vars_to_test = expl_vars_to_test)

corr_env_spatial <- corr_env_spatial %>% filter(pval <= 0.2,abs_corr>=0.2)
corr_env_spatial$var <- substr(corr_env_spatial$name,1,3)
corr_env_spatial$buffer <- sub('.*\\_', '', corr_env_spatial$name)

cor_df2 <- corr_env_spatial %>%
  group_by(var) %>%
  summarise(correlation=max(abs_corr)) %>%
  left_join(corr_env_spatial) %>%
  mutate(correlation=ifelse(is.na(name),-correlation,correlation)) %>%
  left_join(corr_env_spatial, by=c("var","correlation"))

if(nrow(cor_df2)>0){
  corr_env_spatial <- cor_df2 %>% 
    left_join(prediction_vars, by = c("var" = "code")) %>%
    mutate(lab = paste0(short_name, " - ", buffer.y," m buffer")) %>%
    dplyr::select(lab,correlation,name.y) %>%
    rename(name=name.y) %>%
    mutate(type = "Other spatial variables")
} else {
  corr_env_spatial <- NULL
}



## nightcatch_postedecapture variables
expl_vars_to_test = setdiff(colnames(th_env_nightcatch_postedecapture),c("idpostedecapture","heuredecapture"))
corr_env_nightcatch_postedecapture <- fun_feature_forward_selection(
  df = th_trmetrics_entomo_postedecapture, 
  stat_method = "spearman", 
  mod = mod, 
  type = "univariate_selection", 
  expl_vars_to_test = expl_vars_to_test)

corr_env_nightcatch_postedecapture <- corr_env_nightcatch_postedecapture %>% filter(pval <= 0.2,abs_corr>=0.2)
if(nrow(corr_env_nightcatch_postedecapture)>0){
  
  corr_env_nightcatch_postedecapture <- corr_env_nightcatch_postedecapture %>% 
    left_join(prediction_vars, by = c("name" = "code")) %>%
    dplyr::select(short_name,correlation,name) %>%
    dplyr::rename(lab = short_name) %>%
    mutate(type = "Micro-climatic conditions during the night of catch")
  
}

## nightcatch
expl_vars_to_test = setdiff(colnames(th_env_nightcatch),"idpointdecapture")
corr_env_nightcatch <- fun_feature_forward_selection(
  df = th_trmetrics_entomo_postedecapture, 
  stat_method = "spearman", 
  mod = mod, 
  type = "univariate_selection", 
  expl_vars_to_test = expl_vars_to_test)

corr_env_nightcatch <- corr_env_nightcatch %>% filter(pval <= 0.2,abs_corr>=0.2)
if(nrow(corr_env_nightcatch)>0){
  
  corr_env_nightcatch <- corr_env_nightcatch %>% 
    left_join(prediction_vars, by = c("name" = "code")) %>%
    dplyr::select(short_name,correlation,name) %>%
    dplyr::rename(lab = short_name) %>%
    mutate(type = "Micro-climatic conditions during the night of catch")
  
}

## static variables
expl_vars_to_test = c("WMD","BDE","VCT","LUS")
corr_env_static <- fun_feature_forward_selection(
  df = th_trmetrics_entomo_postedecapture, 
  stat_method = "spearman", 
  mod = mod, 
  type = "univariate_selection", 
  expl_vars_to_test = expl_vars_to_test)

corr_env_static <- corr_env_static %>% filter(pval <= 0.2, abs_corr>=0.2)
if(nrow(corr_env_static)>0){
  
  corr_env_static <- corr_env_static %>% 
    left_join(prediction_vars, by = c("name" = "code")) %>%
    dplyr::select(short_name,correlation,name) %>%
    dplyr::rename(lab = short_name) %>%
    mutate(type = "Other spatial variables")
  
}


df_corr <- rbind(df_temporal, corr_lsm, corr_env_spatial, corr_env_nightcatch_postedecapture, corr_env_nightcatch, corr_env_static)
df_corr$name <- as.character(df_corr$name)

df_corr_univ <- rbind(df_temporal_corr,corr_lsm_univ, corr_env_spatial, corr_env_nightcatch_postedecapture, corr_env_nightcatch, corr_env_static)
df_corr_univ$name <- as.character(df_corr_univ$name)

## multicollinearity
var_to_remove <- NULL
p <- NULL
if(nrow(df_corr)>1){
m <- th_trmetrics_entomo_postedecapture[,df_corr$name] %>%
  cor(.,method = "pearson", use = "na.or.complete")
index <- which(abs(m) > .7 & abs(m) < 1,arr.ind = T) 
p <- cbind.data.frame(stock1 = rownames(m)[index[,1]], stock2 = colnames(m)[index[,2]])


if(nrow(p)>0){
  
p <- p %>%
  mutate(name1 = gsub("_"," ",stock1)) %>%
  mutate(name1 = word(name1,1)) %>%
  mutate(name2 = gsub("_"," ",stock2)) %>%
  mutate(name2 = word(name2,1)) %>%
  mutate(name1 = ifelse(name1 == "RFD1", "RFD1F", name1)) %>%
  mutate(name2 = ifelse(name2 == "RFD1", "RFD1F", name2)) %>%
  mutate(name1 = ifelse(name1 == "TMAX7", "TMAX1", name1)) %>%
  mutate(name2 = ifelse(name2 == "TMAX7", "TMAX1", name2)) %>%
  mutate(name1 = ifelse(name1 == "TMIN7", "TMIN1", name1)) %>%
  mutate(name2 = ifelse(name2 == "TMIN7", "TMIN1", name2)) %>%
  mutate(name1 = ifelse(name1 == "TAMP7", "TAMP1", name1)) %>%
  mutate(name2 = ifelse(name2 == "TAMP7", "TAMP1", name2)) %>%
  mutate(name1 = ifelse(name1 == "SMO7", "SMO1", name1)) %>%
  mutate(name2 = ifelse(name2 == "SMO7", "SMO1", name2)) %>%
  left_join(prediction_vars %>% dplyr::select(code,priority), by = c("name1"="code")) %>%
  dplyr::rename(priority1 = priority) %>%
  left_join(prediction_vars %>% dplyr::select(code,priority), by = c("name2"="code")) %>%
  dplyr::rename(priority2 = priority) %>%
  mutate(priority1 = ifelse(name1 == "lsm", 16, priority1)) %>%
  mutate(priority2 = ifelse(name2 == "lsm", 16, priority2)) %>%
  mutate(var_to_remove="")

  for(i in 1:nrow(p)){
    if(p$priority1[i] < p$priority2[i]){
      var_to_remove <- c(var_to_remove,as.character(p$stock2[i]))
    } else if (p$priority1[i] > p$priority2[i]) {
      var_to_remove <- c(var_to_remove,as.character(p$stock1[i]))
    }
    p$var_to_remove[i] <- var_to_remove
  }

var_to_remove <- unique(var_to_remove)

}

}
df_corr_multiv <- df_corr %>% 
  dplyr::filter(!(name %in% var_to_remove))


## multivariate model
# create indices for cross-validation
indices_spatial <- CAST::CreateSpacetimeFolds(th_trmetrics_entomo_postedecapture, spacevar = "codevillage", k = length(unique(th_trmetrics_entomo_postedecapture$codevillage)))
indices_temporal <- CAST::CreateSpacetimeFolds(th_trmetrics_entomo_postedecapture, timevar = "nummission", k = length(unique(th_trmetrics_entomo_postedecapture$nummission)))
indices_spatiotemporal <- CAST::CreateSpacetimeFolds(th_trmetrics_entomo_postedecapture, timevar = "nummission", spacevar = "codevillage", k = 3, seed = 10) # set seed for reproducibility as folds of spatiotemporal cv can change 


#predictors_multivariate <- c(df_corr_multiv$name,"X_32630","Y_32630","int_ext","VCP", colnames(th_trmetrics_entomo_postedecapture[which(grepl("VCM",colnames(th_trmetrics_entomo_postedecapture)))]))

if(model_type == "glmm"){
  
  predictors_multivariate <- c(df_corr_multiv$name,"VCM","int_ext")
  
  th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% mutate_if(is.character, as.factor)
  th_trmetrics_entomo_postedecapture$pointdecapture2 <- as.factor(paste0(th_trmetrics_entomo_postedecapture$codevillage,th_trmetrics_entomo_postedecapture$pointdecapture))
  th_trmetrics_entomo_postedecapture_mod1 <- th_trmetrics_entomo_postedecapture %>% mutate_at(df_corr_multiv$name, scale)
  
  # univariate selection
  # make a dataframe with the variables names to be used in the model, will also receive the results
  dep_var <- "resp_var" # dependent variable (to explain)
  ind_vars <- df_corr_multiv$name # independent variables (explanatory)
  results <- expand_grid(dep_var,ind_vars)
  
  # function that return the p-value of the variable
  p_value <- function(dep_var, ind_var, data){
    formule <- as.formula(paste(dep_var," ~ ", ind_var, "+ (1|codevillage/pointdecapture2)")) # write the formula for the model
    
    if(mod == "abundance"){
    res <- data %>%
        glmmTMB::glmmTMB(formule, data = ., family = truncated_nbinom2) %>% # fit the model
      Anova() %>% # test the significance of the variable
      nth(3) # get the p-value
    } else if (mod %in% c("presence","abundance", "late_prec_aggressiveness", "exophagy", "physiological_resistance_kdrw", "physiological_resistance_kdre")){
      res <- data %>%
        glmmTMB::glmmTMB(formule, data = ., family = binomial(link = "logit")) %>% # fit the model
        Anova() %>% # test the significance of the variable
        nth(3) # get the p-value
    }
    return(res)
  }
  
  # fill with the results (i.e the p_value from the model test )
  possible_p_value <- possibly(p_value, otherwise = NA_real_)
  
  #results <- results %>%
    #mutate(pvalue = map_dbl(ind_vars, ~possible_p_value(dep_var, ., th_trmetrics_entomo_postedecapture_mod1))) #%>%
    #filter(pvalue <= 0.25)
  
  var_to_keep <- c(results$ind_vars,"int_ext","VCM")
  
  # if(intervention == "post_intervention"){
  #   var_to_keep <- c(var_to_keep, "VCM")
  # }
  
  form <- as.formula(paste("resp_var ~ ",paste(var_to_keep, collapse = "+")))
  
  var_to_force <- c("VCM","LUS","RFH","WSP","NMA","NMT","NMH")
  
  if(mod %in% c("presence","abundance")){
    if(code_pays == "BF"){
     var_to_force <- c(var_to_force, "lsm_c_pland_2000_3_4","lsm_c_pland_2000_5_19","lsm_c_pland_2000_5_22","lsm_c_pland_2000_3_12") #foret_ripicole, prairie_marecageuse, marais, eaux_dormantes
    } else if(code_pays == "CI"){
      #var_to_force <- c(var_to_force, ) 
    }
  } else if (mod %in% c("physiological_resistance_kdrw","physiological_resistance_kdre")){
    if(code_pays == "BF"){
      var_to_force <- c(var_to_force, "lsm_c_pland_2000_4_16","lsm_c_pland_2000_4_11","lsm_c_pland_2000_4_1") # coton, riz, cultures irriguées
    } else if(code_pays == "CI"){
      #var_to_force <- c(var_to_force, )
    } 
  }
 

  ## multicollinearity ??
  
  
  form_forc <- as.formula(paste(" ~", paste(var_to_force, collapse = "+"), "+ (1|codevillage/pointdecapture2)"))
  var_to_keep <- setdiff(var_to_keep,var_to_force)
  
  th_trmetrics_entomo_postedecapture_mod2 <- th_trmetrics_entomo_postedecapture %>% mutate_at(results$ind_vars, ~scale(., center = TRUE, scale = FALSE)) %>% dplyr::select(c("resp_var","codevillage","pointdecapture2",var_to_force,var_to_keep))
  
  if(mod == "abundance"){
    th_mod1 <- buildglmmTMB(form, include = ~ (1|codevillage/pointdecapture2), data = th_trmetrics_entomo_postedecapture_mod2, family = truncated_nbinom2)
    th_mod2 <- buildglmmTMB(form, include = form_forc, data = th_trmetrics_entomo_postedecapture_mod2, family = truncated_nbinom2)
  } else if (mod %in% c("presence","abundance", "late_prec_aggressiveness", "exophagy", "physiological_resistance_kdrw","physiological_resistance_kdre")){
    th_mod1 <- buildglmmTMB(form, include = ~ (1|codevillage/pointdecapture2), data = th_trmetrics_entomo_postedecapture_mod2, family = binomial(link = "logit"))
    th_mod2 <- buildglmmTMB(form, include = form_forc, data = th_trmetrics_entomo_postedecapture_mod2, family = binomial(link = "logit"))
  }
  
  
   ## cross validation
  # if(intervention == "all"){
     
     # leave-one-village-out : predict on a new village but on known missions
  #   cv_llo <- fun_glmm_cross_validation(indices_spatial, th_mod1, mod, th_trmetrics_entomo_postedecapture, results$ind_vars)
  #    leave-one-mission-out : predict on known villages but on unknown missions
    # cv_lto <- fun_glmm_cross_validation(indices_temporal, th_mod1, mod, th_trmetrics_entomo_postedecapture, results$ind_vars)
  #   # leave-one-village-and-mission-out : predict on unknown villages and on unknown missions
  #   cv_llto <- fun_glmm_cross_validation(indices_spatiotemporal, th_mod1, mod, th_trmetrics_entomo_postedecapture, results$ind_vars)
  # 
  # } else {
  #   cv_llo <- cv_lto <- cv_llto <- NA
  # }
  cv_lto<-NULL
  
}
  

if(model_type == "rf"){
  
  #predictors_multivariate <- c(df_corr_multiv$name,"X_32630","Y_32630","VCP", colnames(th_trmetrics_entomo_postedecapture[which(grepl("VCM",colnames(th_trmetrics_entomo_postedecapture)))]))
  predictors_multivariate <- c(df_corr_multiv$name,"VCP", colnames(th_trmetrics_entomo_postedecapture[which(grepl("VCM",colnames(th_trmetrics_entomo_postedecapture)))]))
  
  if(mod == "abundance"){
    tr_spatial = trainControl(method="cv",
                              index = indices_spatial$index, 
                              indexOut = indices_spatial$indexOut,
                              savePredictions = 'final')
    tr_temporal = trainControl(method="cv",
                               index = indices_temporal$index, 
                               indexOut = indices_temporal$indexOut,
                               savePredictions = 'final')
    tr_spatiotemporal = trainControl(method="cv",
                               index = indices_spatiotemporal$index, 
                               indexOut = indices_spatiotemporal$indexOut,
                               savePredictions = 'final')
    
    
    met = "Rsquared"
    
    th_trmetrics_entomo_postedecapture_mod1 <- th_trmetrics_entomo_postedecapture
    th_trmetrics_entomo_postedecapture_mod1$resp_var <- log(th_trmetrics_entomo_postedecapture_mod1$resp_var)
    
  } else if (mod %in% c("presence","abundance", "late_prec_aggressiveness", "exophagy", "physiological_resistance_kdrw","physiological_resistance_kdre")){
    tr_spatial = trainControl(method="cv",
                              index = indices_spatial$index, 
                              indexOut = indices_spatial$indexOut,
                              sampling = "up",
                              summaryFunction = prSummary,
                              classProbs = TRUE,
                              savePredictions = 'final')
    tr_temporal = trainControl(method="cv",
                               index = indices_temporal$index, 
                               indexOut = indices_temporal$indexOut,
                               sampling = "up",
                               summaryFunction = prSummary,
                               classProbs = TRUE,
                               savePredictions = 'final')
    tr_spatiotemporal = trainControl(method="cv",
                               index = indices_spatiotemporal$index, 
                               indexOut = indices_spatiotemporal$indexOut,
                               sampling = "up",
                               summaryFunction = prSummary,
                               classProbs = TRUE,
                               savePredictions = 'final')
    
    met = "AUC"
    
    th_trmetrics_entomo_postedecapture_mod1 <- th_trmetrics_entomo_postedecapture
    th_trmetrics_entomo_postedecapture_mod1$resp_var <- ifelse(th_trmetrics_entomo_postedecapture_mod1$resp_var==0,"Absence","Presence")
    th_trmetrics_entomo_postedecapture_mod1$resp_var <- as.factor(th_trmetrics_entomo_postedecapture_mod1$resp_var)
    
  }
  
  th_mod <- caret::train(x = th_trmetrics_entomo_postedecapture_mod1[,predictors_multivariate], y = th_trmetrics_entomo_postedecapture_mod1$resp_var,
                           method="ranger",tuneLength=10,metric=met,
                           trControl=tr_spatiotemporal,importance = "permutation")
  #model_ffs_spatial <- CAST::ffs(predictors = th_trmetrics_entomo_postedecapture_mod1[,predictors_multivariate], response = th_trmetrics_entomo_postedecapture_mod1$resp_var, method = "ranger", tuneLength = 5, trControl = tr_spatial, metric = met)
  #model_ffs_temporal <- CAST::ffs(predictors = th_trmetrics_entomo_postedecapture_mod1[,predictors_multivariate], response = th_trmetrics_entomo_postedecapture_mod1$resp_var, method = "ranger", tuneLength = 5, trControl = tr_temporal, metric = met)
  #model_ffs_spatiotemporal <- CAST::ffs(predictors = th_trmetrics_entomo_postedecapture_mod1[,predictors_multivariate], response = th_trmetrics_entomo_postedecapture_mod1$resp_var, method = "ranger", tuneLength = 5, trControl = tr_spatiotemporal, metric = met)
  
  if(mod == "presence"){
    cv_lto = max(th_mod$results$AUC)
  } else if (mod == "abundance"){
    cv_lto = max(th_mod$results$Rsquared)
  }
  
  # to get predictions out of resampling : model_ffs_spatial$pred
  
  #cv_llo <- list(model_ffs_spatial$results,cbind(th_trmetrics_entomo_postedecapture_mod1,model_ffs_spatial$pred))
  #cv_lto <- list(model_ffs_temporal$results,cbind(th_trmetrics_entomo_postedecapture_mod1,model_ffs_temporal$pred))
  #cv_llto <- list(model_ffs_spatiotemporal$results,cbind(th_trmetrics_entomo_postedecapture_mod1,model_ffs_spatiotemporal$pred))
  
  #th_mod <- list(model_ffs_spatial=model_ffs_spatial, model_ffs_temporal=model_ffs_temporal, model_ffs_spatiotemporal=model_ffs_spatiotemporal)
}
  
  th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% 
    dplyr::select(codevillage,nummission,idpointdecapture,int_ext,resp_var,X_4326,Y_4326,mean_date_mission) %>%
    mutate(mean_date_mission = as.Date(mean_date_mission, origin = "1970-01-01"))
  
  return(list(df = th_trmetrics_entomo_postedecapture, ccm_plots = ccm_plots, ts_plots = ts_plots, df_corr_univ = df_corr_univ, df_corr_multiv = df_corr_multiv, correlated_variables = p, model = th_mod1, model_forced = th_mod2, cv_lto = cv_lto))
              #, cv_llo = cv_llo, cv_lto = cv_lto, cv_llto = cv_llto
              

}





df_input_params_glmm <- tibble(response_var = character(), code_pays = character(), mod = character(), model_type = character(), intervention = character(), predictive_type = character(), timevars_selection = character())
df_input_params_glmm <- df_input_params_glmm %>%
  add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "presence", model_type = "glmm", intervention = "all", predictive_type = "roi_villages", timevars_selection = "7day") %>%
  add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "abundance", model_type = "glmm", intervention = "all", predictive_type = "roi_villages", timevars_selection = "7day") %>%
  add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "late_prec_aggressiveness", model_type = "glmm", intervention = "all", predictive_type = "roi_villages", timevars_selection = "7day") %>%
  add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "exophagy", model_type = "glmm", intervention = "all", predictive_type = "roi_villages", timevars_selection = "7day") %>%
  #add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "physiological_resistance_kdrw","physiological_resistance_kdre", model_type = "glmm", intervention = "all", predictive_type = "roi_villages", timevars_selection = "7day") %>%
  #add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "physiological_resistance_kdre","physiological_resistance_kdre", model_type = "glmm", intervention = "all", predictive_type = "roi_villages", timevars_selection = "7day") %>%
  add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "presence", model_type = "glmm", intervention = "all", predictive_type = "roi_villages", timevars_selection = "7day") %>%
  add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "abundance", model_type = "glmm", intervention = "all", predictive_type = "roi_villages", timevars_selection = "7day") %>%
  add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "late_prec_aggressiveness", model_type = "glmm", intervention = "all", predictive_type = "roi_villages", timevars_selection = "7day") %>%
  add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "exophagy", model_type = "glmm", intervention = "all", predictive_type = "roi_villages", timevars_selection = "7day") %>%
  add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "physiological_resistance_kdrw", model_type = "glmm", intervention = "all", predictive_type = "roi_villages", timevars_selection = "7day") %>%
  add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "physiological_resistance_kdre", model_type = "glmm", intervention = "all", predictive_type = "roi_villages", timevars_selection = "7day") %>%
  add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "presence", model_type = "glmm", intervention = "all", predictive_type = "roi_villages", timevars_selection = "7day") %>%
  add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "abundance", model_type = "glmm", intervention = "all", predictive_type = "roi_villages", timevars_selection = "7day") %>%
  add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "late_prec_aggressiveness", model_type = "glmm", intervention = "all", predictive_type = "roi_villages", timevars_selection = "7day") %>%
  add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "exophagy", model_type = "glmm", intervention = "all", predictive_type = "roi_villages", timevars_selection = "7day") %>%
  add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "physiological_resistance_kdrw", model_type = "glmm", intervention = "all", predictive_type = "roi_villages", timevars_selection = "7day") %>% 
  add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "physiological_resistance_kdre", model_type = "glmm", intervention = "all", predictive_type = "roi_villages", timevars_selection = "7day")

model_results <- df_input_params_glmm %>%
  mutate(results = pmap(list(response_var, code_pays, mod, model_type, intervention, predictive_type, timevars_selection), ~fun_workflow_model(..1,..2,..3,..4,..5,..6,..7)))


model_results <- model_results %>%
  mutate(df = map(results, ~pluck(.,"df"))) %>%
  mutate(df_corr_univ = map(results, ~pluck(.,"df_corr_univ"))) %>%
  mutate(df_corr_multiv = map(results, ~pluck(.,"df_corr_multiv"))) %>%
  mutate(correlated_variables = map(results, ~pluck(.,"correlated_variables"))) %>%
  mutate(model = map(results, ~pluck(.,"model"))) %>%
  mutate(model_forced = map(results, ~pluck(.,"model_forced"))) %>%
  mutate(ccm_plots = map(results, ~pluck(.,"ccm_plots"))) %>%
  mutate(ts_plots = map(results, ~pluck(.,"ts_plots"))) %>%
  mutate(cv_lto = map(results, ~pluck(.,"cv_lto"))) %>%
  dplyr::select(-results)
  


saveRDS(model_results,"/home/ptaconet/Bureau/data_analysis/model_results.rds")




df_input_params_rf <- df_input_params_glmm
df_input_params_rf <- df_input_params_rf %>%
  add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "abundance", model_type = "rf", intervention = "all", predictive_type = "roi_villages") %>%
  add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "presence", model_type = "rf", intervention = "all", predictive_type = "roi_villages")








