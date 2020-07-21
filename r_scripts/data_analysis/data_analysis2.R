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

### connect to the database
path_to_db <- "data/react_db/react_db.gpkg" 
react_gpkg <- DBI::dbConnect(RSQLite::SQLite(),dbname = path_to_db) 


### open the tables
## dates and positions of the entomological missions (1 row = 1 point de capture)
entomo_csh_metadata_l1 <- dbReadTable(react_gpkg, 'entomo_csh_metadata_l1') %>% filter(!(nummission %in% c("11","12","13","15")))

## table containing the response variables (ie variables to model)
trmetrics_entomo_postedecapture <- dbReadTable(react_gpkg, 'trmetrics_entomo_postedecapture') %>% dplyr::select(-fid) %>% left_join(entomo_csh_metadata_l1 %>% dplyr::select(idpointdecapture, codevillage, pointdecapture, codepays, nummission, period_interv)) %>% filter(!is.na(codevillage))

## table of exhaustive definitions of the explanatory variables
googlesheets4::sheets_deauth()
prediction_vars <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1dIeSOa2WinXvOQGLmIjA0gFdsHnb6zMMsDME-G5pyMc/edit?usp=sharing", sheet = "var_explication", col_types="c")


### source home-made functions 
source("r_scripts/data_analysis_tests/functions_script_data_analysis.R")


###### input parameters
response_var <- "ma_funestus_ss"  # "ma_gambiae_ss"
code_pays <- "BF"            # "BF"
mod <- "abundance" # abundance, presence        si abondance + GLMM : glmm negatif binomial zero tronqué ; si présence + GLMM : glmm binomial ; si abondance + RF : RF regression ; si abondance + RF : RF classification 
model_type <- "glmm"  # glmm, rf       si RF : VCM en dummy variables
intervention <- "pre_intervention"  # pre_intervention, post_intervention, all
predictive_type <- "roi_villages" #  "roi_villages" (within the ROI, for the sampled villages)  or "roi_notvillages" (within the ROI, for the not sampled villages) or  "notroi" (outside the roi)


###### load the data

# load spatiotemporal data
env_spatiotemporal <- load_spatiotemporal_data(vars = c("SMO1","TMIN1","TMAX1","TAMP1","VNV8","RFD1_F","EVT8","WVV10","WVH10","WNW30"), 
                         buffers = c(2000),
                         lag_time_window = c(0,60),
                         summarize_days_to_week = TRUE,
                         code_pays = code_pays,
                         entomo_csh_metadata_l1 = entomo_csh_metadata_l1)

# load spatial data 
env_spatial_all <- load_spatial_data(code_pays)
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

# load response variable
th_trmetrics_entomo_postedecapture <- dbReadTable(react_gpkg, 'trmetrics_entomo_postedecapture') %>% 
  dplyr::select(-fid) %>% 
  left_join(entomo_csh_metadata_l1 %>% dplyr::select(idpointdecapture, codevillage, pointdecapture, codepays, nummission, period_interv)) %>% 
  filter(!is.na(codevillage)) %>%
  filter(codepays == code_pays)

th_trmetrics_entomo_postedecapture$resp_var <- th_trmetrics_entomo_postedecapture[,response_var]

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

mean_date_mission <- entomo_csh_metadata_l1 %>% mutate(date_capture = as.Date(date_capture)) %>% group_by(codepays,nummission) %>% summarise(mean_date_mission=mean(date_capture)) %>% as_tibble() %>% filter(codepays==code_pays) %>% dplyr::select(-codepays) 


######## join response variable with explanatory variables

th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% 
  left_join(mean_date_mission) %>%
  left_join(mean_coords_points_4326) %>%
  left_join(mean_coords_points_32630) %>%
  mutate(int_ext = substr( idpostedecapture,nchar(idpostedecapture),nchar(idpostedecapture))) %>%
  dplyr::select(idpostedecapture,idpointdecapture,int_ext,pointdecapture,codevillage,codepays,nummission,mean_date_mission,X_4326,Y_4326,X_32630,Y_32630,resp_var) %>%
  left_join(env_spatiotemporal) %>%
  left_join(env_spatial) %>%
  left_join(th_env_nightcatch_postedecapture) %>%
  left_join(th_env_nightcatch) %>%
  left_join(th_env_static) %>%
  left_join(env_landcover) %>%
  mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))
th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture[, colSums(is.na(th_trmetrics_entomo_postedecapture)) != nrow(th_trmetrics_entomo_postedecapture)]

######## feature forward selection of temporal variables

if(mod == "abundance"){
  logtransform_resp_var = TRUE
  time_vars <- c("RFD1","TMAX1","TMIN1","TAMP1","SMO1","EVT8","VNV8","WVV10","WVH10","WNW30")
} else if (mod == "presence"){
  logtransform_resp_var = FALSE
  time_vars <- c("RFD1","TMAX7","TMIN7","TAMP7","SMO7","EVT8","VNV8","WVV10","WVH10","WNW30")
}

df_temporal <- data.frame(var = character(), col_to_keep = character(), correlation = numeric())

### select the first temporal variable 
for(i in 1:length(time_vars)){
  
  cat("Calculating CCM for variable ",time_vars[i],"\n")
  expl_vars_to_test =  colnames(th_trmetrics_entomo_postedecapture[which(grepl(time_vars[i],colnames(th_trmetrics_entomo_postedecapture)))])
  corr <- fun_feature_forward_selection(
    stat_method = "spearman", 
    spearman_factor = "codevillage", 
    mod = mod, 
    type = "univariate_selection", 
    expl_vars_to_keep = NULL, 
    expl_vars_to_test = expl_vars_to_test)
  
  corr$time_lag_1 <- as.numeric(sub('.*\\_', '', corr$name))
  corr$time_lag_2 <- as.numeric(stringr::str_match( corr$name, '([^_]+)(?:_[^_]+){1}$')[,2])
  corr$diff_lag <- corr$time_lag_1 - corr$time_lag_2
  corr <- arrange(corr, time_lag_1, time_lag_2)
  
  corr$correlation[which(corr$pval > 0.05)] <- NA
  
  # plot the CCM
  #fun_ccm_plot(corr, "max", "Spearman\ncorrelation")
  
  # keep best time lag
  if(time_vars[i] %in% c("RFD1","TMAX1","TMIN1","TAMP1","SMO1")){
    corr <- corr %>% filter(diff_lag >= 7)
  }
  corr <- corr %>% filter(!is.na(correlation), pval <= 0.05) 
  
  if(nrow(corr) > 0){
    col_to_keep <- corr$name[which.max(abs(corr$abs_corr))]
    correlation <- corr$correlation[which.max(abs(corr$abs_corr))]
  } else {
    col_to_keep <- NA
    correlation <- NA
  }
  
  th_df_temporal <- data.frame(var = time_vars[i], col_to_keep = col_to_keep, correlation = correlation)
  
  df_temporal <- rbind(df_temporal, th_df_temporal)
  
}


best_time_var <- as.character(df_temporal$var[which.max(abs(df_temporal$correlation))])
cols_to_keep_best_time_var <- as.character(df_temporal$col_to_keep[which.max(abs(df_temporal$correlation))])

### now forward selection to select the next 2 temporal variables
# 2nd time var
time_vars <- setdiff(time_vars, best_time_var)
ffs_temp_it2 <- fun_ffs_tempvar(time_vars, cols_to_keep_best_time_var, logtransform_resp_var)
best_time_var <- as.character(ffs_temp_it2$var[which.min(ffs_temp_it2$res)])
cols_to_keep_best_time_var <- c(cols_to_keep_best_time_var, as.character(ffs_temp_it2$name[which.min(ffs_temp_it2$res)]))

# 3d time var
time_vars <- setdiff(time_vars, best_time_var)
ffs_temp_it3 <- fun_ffs_tempvar(time_vars, cols_to_keep_best_time_var, logtransform_resp_var)
best_time_var <- as.character(ffs_temp_it3$var[which.min(ffs_temp_it3$res)])
cols_to_keep_best_time_var <- c(cols_to_keep_best_time_var, as.character(ffs_temp_it3$name[which.min(ffs_temp_it2$res)]))
time_vars <- setdiff(time_vars, best_time_var)









######## selection of spatial variables

corr_lsm <- NULL
corr_env_spatial <- NULL
corr_env_nightcatch <- NULL
corr_env_nightcatch_postedecapture <- NULL
corr_env_static <- NULL

## filter landcover variables with spearman coefficient 

lco_metadata <- dbReadTable(react_gpkg, 'lco_metadata') # table containing pixel value and label for each land cover map
metrics_defs <- landscapemetrics::list_lsm() # list of landscape metrics

expl_vars_to_test =  colnames(th_trmetrics_entomo_postedecapture[which(grepl("lsm",colnames(th_trmetrics_entomo_postedecapture)))])
expl_vars_to_test_lcid <- as.numeric(word(gsub("_"," ",expl_vars_to_test),5))
expl_vars_to_test_buffer <- as.numeric(word(gsub("_"," ",expl_vars_to_test),4))

if(predictive_type == "notroi"){
  expl_vars_to_test <- expl_vars_to_test[which(expl_vars_to_test_lcid %in% c(11,12))]
} else {
  if(code_pays == "BF"){
    expl_vars_to_test <- expl_vars_to_test[which(expl_vars_to_test_lcid %in% c(1,2,3,4,5))]
  } else if (code_pays == "CI"){
    expl_vars_to_test <- expl_vars_to_test[which(expl_vars_to_test_lcid %in% c(6,7,8,9,10))]
  }
}


corr_lsm <- fun_feature_forward_selection(
  stat_method = "spearman", 
  mod = mod, 
  type = "univariate_selection", 
  df = th_trmetrics_entomo_postedecapture, 
  expl_vars_to_test = expl_vars_to_test)

corr_lsm <- corr_lsm %>% filter(abs_corr > 0.2, pval <= 0.05)

if(nrow(corr_lsm) > 0){
  
  m <- th_trmetrics_entomo_postedecapture[,corr_lsm$name] %>%
    cor(.,method = "spearman", use = "na.or.complete")
  index <- which(abs(m) > .7 & abs(m) < 1,arr.ind = T) 
  p <- cbind.data.frame(stock1 = rownames(m)[index[,1]], stock2 = colnames(m)[index[,2]])
  
  p <- p %>%
    mutate(name1 = gsub("_"," ",stock1)) %>%
    mutate(function_name1 = paste(word(name1,1),word(name1,2),word(name1,3),sep="_")) %>%
    mutate(buffer1 = as.numeric(word(name1,4))) %>%
    mutate(layer_id = as.numeric(word(name1,5))) %>%
    mutate(pixval = as.numeric(word(name1,6))) %>%
    left_join(corr_lsm %>% dplyr::select(name,correlation), by = c("stock1"="name")) %>%
    left_join(lco_metadata %>% dplyr::select(pixval,pixlabel,layer_id,priority)) %>%
    dplyr::rename(layer_id1 = layer_id, pixval1 = pixval, pixlabel1 = pixlabel, priority1 = priority, correlation1 =  correlation) %>%
    mutate(name2 = gsub("_"," ",stock2)) %>%
    mutate(function_name2 = paste(word(name2,1),word(name2,2),word(name2,3),sep="_")) %>%
    mutate(buffer2 = as.numeric(word(name2,4))) %>%
    mutate(layer_id = as.numeric(word(name2,5))) %>%
    mutate(pixval = as.numeric(word(name2,6))) %>%
    left_join(corr_lsm %>% dplyr::select(name,correlation), by = c("stock2"="name")) %>%
    left_join(lco_metadata %>% dplyr::select(pixval,pixlabel,layer_id,priority)) %>%
    dplyr::rename(layer_id2 = layer_id, pixval2 = pixval, pixlabel2 = pixlabel, priority2 = priority, correlation2 =  correlation) %>%
    mutate(priority1 = ifelse(is.na(priority1),100,priority1)) %>%
    mutate(priority2 = ifelse(is.na(priority2),100,priority2)) 
  
  
  var_to_remove <- NULL
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
  
  corr_lsm <- corr_lsm %>% 
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
    dplyr::select(name,correlation,pixlabel) %>%
    rename(lab = pixlabel) %>%
    mutate(type = "Land cover")
  
}


## spatial-only variables
expl_vars_to_test = setdiff(colnames(env_spatial),"idpointdecapture")
corr_env_spatial <- fun_feature_forward_selection(
  stat_method = "spearman", 
  mod = mod, 
  type = "univariate_selection", 
  df = th_trmetrics_entomo_postedecapture, 
  expl_vars_to_test = expl_vars_to_test)

corr_env_spatial <- corr_env_spatial %>% filter(pval<=0.05,abs_corr>=0.2)
corr_env_spatial$var <- substr(corr_env_spatial$name,1,3)
corr_env_spatial$buffer <- sub('.*\\_', '', corr_env_spatial$name)

cor_df2 <- corr_env_spatial %>%
  group_by(var) %>%
  summarise(correlation=max(abs_corr)) %>%
  left_join(corr_env_spatial) %>%
  mutate(correlation=ifelse(is.na(name),-correlation,correlation)) %>%
  left_join(corr_env_spatial, by=c("var","correlation"))

if(nrow(cor_df2)>1){
  
  corr_env_spatial <- cor_df2 %>% 
    left_join(prediction_vars, by = c("var" = "code")) %>%
    mutate(lab = paste0(short_name, " - \n", buffer.y," m")) %>%
    dplyr::select(lab,correlation,name.y) %>%
    rename(name=name.y) %>%
    mutate(type = "Other spatial variables")
  
}



## nightcatch_postedecapture variables
expl_vars_to_test = setdiff(colnames(th_env_nightcatch_postedecapture),"idpostedecapture")
corr_env_nightcatch_postedecapture <- fun_feature_forward_selection(
  stat_method = "spearman", 
  mod = mod, 
  type = "univariate_selection", 
  df = th_trmetrics_entomo_postedecapture, 
  expl_vars_to_test = expl_vars_to_test)

corr_env_nightcatch_postedecapture <- corr_env_nightcatch_postedecapture %>% filter(pval<=0.05,abs_corr>=0.2)
if(nrow(corr_env_nightcatch_postedecapture)>0){
  
  corr_env_nightcatch_postedecapture <- corr_env_nightcatch_postedecapture %>% 
    left_join(prediction_vars, by = c("name" = "code")) %>%
    dplyr::select(short_name,correlation,name) %>%
    dplyr::rename(lab = short_name) %>%
    mutate(type = "Micro-climatic conditions \n during the night of catch")
  
}

## nightcatch
expl_vars_to_test = setdiff(colnames(th_env_nightcatch),"idpointdecapture")
corr_env_nightcatch <- fun_feature_forward_selection(
  stat_method = "spearman", 
  mod = mod, 
  type = "univariate_selection", 
  df = th_trmetrics_entomo_postedecapture, 
  expl_vars_to_test = expl_vars_to_test)

corr_env_nightcatch <- corr_env_nightcatch %>% filter(pval<=0.05,abs_corr>=0.2)
if(nrow(corr_env_nightcatch)>0){
  
  corr_env_nightcatch <- corr_env_nightcatch %>% 
    left_join(prediction_vars, by = c("name" = "code")) %>%
    dplyr::select(short_name,correlation,name) %>%
    dplyr::rename(lab = short_name) %>%
    mutate(type = "Micro-climatic conditions \n during the night of catch")
  
}

## static variables
expl_vars_to_test = c("WMD","BDE","VCT")
corr_env_static <- fun_feature_forward_selection(
  stat_method = "spearman", 
  mod = mod, 
  type = "univariate_selection", 
  df = th_trmetrics_entomo_postedecapture, 
  expl_vars_to_test = expl_vars_to_test)

corr_env_static <- corr_env_static %>% filter(pval<=0.05, abs_corr>=0.2)
if(nrow(corr_env_static)>0){
  
  corr_env_static <- corr_env_static %>% 
    left_join(prediction_vars, by = c("name" = "code")) %>%
    dplyr::select(short_name,correlation,name) %>%
    dplyr::rename(lab = short_name) %>%
    mutate(type = "Other spatial-only variables")
  
}



df_corr <- rbind(corr_lsm, corr_env_spatial, corr_env_nightcatch_postedecapture, corr_env_nightcatch, corr_env_static)












