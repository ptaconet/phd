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
trmetrics_entomo_postedecapture <- dbReadTable(react_gpkg, 'trmetrics_entomo_postedecapture') %>% dplyr::select(-fid) %>% left_join(entomo_csh_metadata_l1 %>% dplyr::select(idpointdecapture, codevillage, pointdecapture, codepays, nummission)) %>% filter(!is.na(codevillage))

## tables containing the explanatory variables
# spatiotemporal explanatory variables
env_spatiotemporal <- dbReadTable(react_gpkg, 'env_spatiotemporal') %>% dplyr::select(-fid) %>% mutate(date = as.Date(date)) %>% dplyr::rename(idpointdecapture = id) %>% filter(var %in% c("SMO1","TMIN1","TMAX1","TAMP1","VNV8","RFD1_F","EVT8","LIG30","WVV10","WVH10"), buffer==2000, lag_time <= 60)

fun_summarize_week <- function(var_to_summarize){
  
  env_spatiotemporal_summarize <- env_spatiotemporal %>%
    filter(var==var_to_summarize) %>%
    group_by(idpointdecapture,buffer,lag_n = lubridate::week(date)) %>%
    summarise(val=mean(val, na.rm = T),date = min(date)) %>%
    group_by(idpointdecapture,buffer) %>%
    mutate(lag_n=seq(n()-1,0,-1)) %>%
    mutate(var = gsub("1","7",var_to_summarize), lag_time = NA) %>%
    as_tibble()
  
  return(env_spatiotemporal_summarize)
  
}

env_spatiotemporal <- env_spatiotemporal %>%
  bind_rows(fun_summarize_week("TMAX1")) %>%
  bind_rows(fun_summarize_week("TMIN1")) %>%
  bind_rows(fun_summarize_week("TAMP1")) %>%
  bind_rows(fun_summarize_week("SMO1"))

# spatial-only explanatory variables
env_spatial <- dbReadTable(react_gpkg,'env_spatial') %>% dplyr::select(-fid) %>% dplyr::rename(idpointdecapture = id)

# non-spatial explanatory variables
env_static <- dbReadTable(react_gpkg, 'env_static') %>% dplyr::select(-fid) %>% dplyr::rename(idpointdecapture = id)

# variables for the night of catch
env_nightcatch <-  dbReadTable(react_gpkg, 'env_nightcatch') %>% dplyr::select(-fid) %>% dplyr::rename(idpointdecapture = id)

# variables for the night of catch at the postedecapture level
env_nightcatch_postedecapture <- dbReadTable(react_gpkg,"env_nightcatch_postedecapture") %>% dplyr::select(-fid)

# landcover variables
env_landcover <-  dbReadTable(react_gpkg, 'env_landcover') %>% dplyr::select(-fid) %>% dplyr::rename(idpointdecapture = id) %>% filter(buffer>100, !(metric %in% c("ed","np")))

mean_date_mission <- entomo_csh_metadata_l1 %>% mutate(date_capture = as.Date(date_capture)) %>% group_by(codepays,nummission) %>% summarise(mean_date_mission=mean(date_capture)) %>% as_tibble()


## table of exhaustive definitions of the explanatory variables
googlesheets4::sheets_deauth()
prediction_vars <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1dIeSOa2WinXvOQGLmIjA0gFdsHnb6zMMsDME-G5pyMc/edit?usp=sharing", sheet = "var_explication", col_types="c")


### source home-made functions 
source("r_scripts/data_analysis_tests/functions_script_data_analysis.R")


###### prepare data
# spatiotemporal
env_spatiotemporal <- env_spatiotemporal %>%
  mutate(buffer = as.character(buffer)) %>%
  right_join(entomo_csh_metadata_l1[,c("idpointdecapture","codepays")]) %>%
  left_join(prediction_vars[,c("code","type_group1","short_name","temporal_aggregation_days")], by = c("var" = "code"))

env_spatiotemporal <- env_spatiotemporal %>%
  dplyr::select(-c(lag_time, date)) %>%
  group_by(type_group1, var, codepays, buffer) %>%
  tidyr::nest(predictive_df = c(idpointdecapture, lag_n , val)) %>%
  mutate(fun_summarize_ccm = ifelse(var %in% c("RFD1_F","RFD1_L","RFD7_F","RFD7_L"), "sum", "mean")) %>%
  arrange(type_group1, var, codepays, as.numeric(buffer), fun_summarize_ccm) #%>%
  # filter(!(var %in% c("RFD1_L","TAMP7","TMAX7","TMIN7"))) %>%
  # filter(!(var %in% c("RFD1_F","SMO1","VEV8","VNV8","EVT8","TMAX1","TMIN1","TAMP1") && buffer %in% c("500","1000"))) %>%
  # filter(!(var %in% c("VNV30","VMV30","WNW30","WVV10","WVH10") && buffer %in% c("500","1000","2000")))


env_spatiotemporal <- env_spatiotemporal %>% 
  mutate(predictive_df = pmap(list(predictive_df, var, buffer, fun_summarize_ccm), ~fun_ccm_df(..1, ..2, ..3, function_to_apply = ..4))) 

fun_extract_env_spatiotemporal <- function(env_spatiotemporal,code_pays){
  env_spatiotemporal <- env_spatiotemporal %>%
    filter(codepays==code_pays)
  th_env_spatiotemporal <- env_spatiotemporal$predictive_df[[1]]
  for(i in 2:nrow(env_spatiotemporal)){
    th_env_spatiotemporal <- left_join(th_env_spatiotemporal,env_spatiotemporal$predictive_df[[i]])
  }
  return(th_env_spatiotemporal)
}

env_spatiotemporal <- rbind(fun_extract_env_spatiotemporal(env_spatiotemporal,"BF"),fun_extract_env_spatiotemporal(env_spatiotemporal,"CI"))

# landscape metrics

metrics_defs <- landscapemetrics::list_lsm() # list of landscape metrics
env_landcover_bf <- env_landcover %>%
  filter(layer_id %in% c(1,2,3,4,5,11,12)) %>%
  left_join(metrics_defs) %>%
  dplyr::select(-c(level,metric,name,type)) %>%
  pivot_wider(names_from = c(function_name,buffer,layer_id,pixval), values_from = val, names_sep = "_", values_fill = list(val = 0)) %>%
  mutate_all(funs(replace_na(.,0)))

env_landcover_ci <- env_landcover %>%
  filter(layer_id %in% c(6,7,8,9,10,11,12)) %>%
  left_join(metrics_defs) %>%
  dplyr::select(-c(level,metric,name,type)) %>%
  pivot_wider(names_from = c(function_name,buffer,layer_id,pixval), values_from = val, names_sep = "_", values_fill = list(val = 0)) %>%
  mutate_all(funs(replace_na(.,0)))

# all other data
env_spatial <- env_spatial %>% pivot_wider(names_from = c("var","buffer"), values_from = val) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
th_env_nightcatch_postedecapture <- env_nightcatch_postedecapture %>% pivot_wider(names_from = var, values_from = val) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
th_env_nightcatch  <- env_nightcatch %>% pivot_wider(names_from = var, values_from = val) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
th_env_static <- env_static %>% pivot_wider(names_from = var, values_from = val) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>% mutate(VCT=as.numeric(VCT), WMD=as.numeric(WMD),BDE=as.numeric(BDE),VCP=ifelse(VCP=="TRUE",1,0))

# spatial coordinates
mean_coords_points_4326 = st_read(path_to_db, 'entomo_csh_metadata_l1', crs = 4326) %>%
  group_by(codevillage,pointdecapture) %>%
  summarise(X_4326=mean(X),Y_4326=mean(Y)) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("X_4326", "Y_4326"), crs = 4326)

mean_coords_points_4326$X_4326 = as.numeric(st_coordinates(mean_coords_points_4326)[,1])
mean_coords_points_4326$Y_4326 = as.numeric(st_coordinates(mean_coords_points_4326)[,2])
mean_coords_points_4326 = st_drop_geometry(mean_coords_points_4326) %>% as_tibble() %>% mutate(codevillage=as.character(codevillage), pointdecapture=as.character(pointdecapture))

mean_coords_points_32630 = st_read(path_to_db, 'entomo_csh_metadata_l1', crs = 4326) %>%
  group_by(codevillage,pointdecapture) %>%
  summarise(X_32630=mean(X),Y_32630=mean(Y)) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("X_32630", "Y_32630"), crs = 4326) %>%
  st_transform(32630)

mean_coords_points_32630$X_32630 = as.numeric(st_coordinates(mean_coords_points_32630)[,1])
mean_coords_points_32630$Y_32630 = as.numeric(st_coordinates(mean_coords_points_32630)[,2])
mean_coords_points_32630 = st_drop_geometry(mean_coords_points_32630) %>% as_tibble() %>% mutate(codevillage=as.character(codevillage), pointdecapture=as.character(pointdecapture))

###### input parameters
response_var <- "ma_funestus_ss"  # "ma_gambiae_ss"
code_pays <- "BF"            # "BF"
mod <- "presence" # abundance
model_type <- "glmm"
model_purpose <- "explicative"  # explicative  or predictive
predictive_type <- "roi_villages" #  "roi_villages" (within the ROI, for the sampled villages)  or "roi_notvillages" (within the ROI, for the not sampled villages) or  "notroi" (outside the roi)



mean_date_mission <- mean_date_mission %>% filter(codepays==code_pays) %>% dplyr::select(-codepays)

th_trmetrics_entomo_postedecapture <- trmetrics_entomo_postedecapture %>% filter(codepays==code_pays)
th_trmetrics_entomo_postedecapture$resp_var <- th_trmetrics_entomo_postedecapture[,response_var]

if(mod == "abundance" ){
  th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% filter(resp_var > 0 )
} else if (mod == "presence" ){
  th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% mutate(resp_var = ifelse(resp_var == 0,0,1 ))
}

if(model_purpose == "predictive"){
  th_env_static <- th_env_static %>% mutate(v = 1) %>% pivot_wider(names_from = VCM,  values_from = v, values_fill = list(v = 0), names_prefix = "VCM_")
}

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
  left_join(th_env_static)


if(code_pays=="BF"){
  th_trmetrics_entomo_postedecapture <- left_join(th_trmetrics_entomo_postedecapture,env_landcover_bf)
} else if (code_pays=="CI"){
  th_trmetrics_entomo_postedecapture <- left_join(th_trmetrics_entomo_postedecapture,env_landcover_ci)
}


th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% 
  mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# if(model_purpose == "predictive" & mod == "abundance"){
#   th_trmetrics_entomo_postedecapture$resp_var <- log(th_trmetrics_entomo_postedecapture$resp_var)
# }



######## feature forward selection

# keep rainfall time lag with highest spearman coeff
expl_vars_to_test =  colnames(th_trmetrics_entomo_postedecapture[which(grepl("RFD1",colnames(th_trmetrics_entomo_postedecapture)))])
corr_rainfall <- fun_feature_forward_selection(
                              stat_method = "spearman", 
                              spearman_factor = "codevillage", 
                              mod = mod, 
                              type = "univariate_selection", 
                              expl_vars_to_keep = NULL, 
                              expl_vars_to_test = expl_vars_to_test)

corr_rainfall$time_lag_1 <- as.numeric(sub('.*\\_', '', corr_rainfall$name))
corr_rainfall$time_lag_2 <- as.numeric(stringr::str_match( corr_rainfall$name, '([^_]+)(?:_[^_]+){1}$')[,2])
corr_rainfall$diff_lag <- corr_rainfall$time_lag_1 - corr_rainfall$time_lag_2
corr_rainfall <- arrange(corr_rainfall, time_lag_1, time_lag_2)

corr_rainfall$correlation[which(corr_rainfall$pval > 0.05)] <- NA

# plot the CCM
#fun_ccm_plot(corr_rainfall, "max", "Spearman\ncorrelation")

# keep best time lag
corr_rainfall <- corr_rainfall %>% filter(!is.na(correlation), diff_lag >= 8, pval <= 0.05) 
if(nrow(corr_rainfall) > 0){
  col_to_keep_rainfall <- corr_rainfall$name[which.max(abs(corr_rainfall$abs_corr))]
} else {
  col_to_keep_rainfall <- NULL
}


# same but calculated with a random forest
# corr_rainfall_rf <- fun_feature_forward_selection(
#   stat_method = "rf", 
#   mod = mod, 
#   type = "univariate_selection", 
#   expl_vars_to_test = expl_vars_to_test,
#   expl_vars_to_keep =  c("X_32630","Y_32630"),
#   cross_validation_type = "temporal")
# 
# 
# corr_rainfall_rf$time_lag_1 <- as.numeric(sub('.*\\_', '', corr_rainfall_rf$name))
# corr_rainfall_rf$time_lag_2 <- as.numeric(stringr::str_match( corr_rainfall_rf$name, '([^_]+)(?:_[^_]+){1}$')[,2])
# corr_rainfall_rf$diff_lag <- corr_rainfall_rf$time_lag_1 - corr_rainfall_rf$time_lag_2
# corr_rainfall_rf <- arrange(corr_rainfall_rf, time_lag_1, time_lag_2)
# corr_rainfall_rf$correlation <- corr_rainfall_rf$abs_corr <- corr_rainfall_rf$res
# fun_ccm_plot(corr_rainfall_rf, "min", "RMSE")


### keep temperature max
# using glmm
if(model_type == "glmm"){ 
  
  expl_vars_to_test =  colnames(th_trmetrics_entomo_postedecapture[which(grepl("TMAX8",colnames(th_trmetrics_entomo_postedecapture)))])
  glm_base <- glmmTMB(as.formula(paste0("resp_var ~ ",col_to_keep_rainfall," + (1|codevillage/pointdecapture)")), data = df, family = nbinom2(link = "log"))
  
  modcomp_temp <- fun_feature_forward_selection(
    stat_method = "glmm", 
    mod = mod, 
    type = "model_comparison", 
    expl_vars_to_keep = col_to_keep_rainfall, 
    expl_vars_to_test = expl_vars_to_test)
  
  colnames(modcomp_temp)[8] <- "pval"
  modcomp_temp$time_lag_1 <- as.numeric(sub('.*\\_', '', modcomp_temp$name))
  modcomp_temp$time_lag_2 <- as.numeric(stringr::str_match(modcomp_temp$name, '([^_]+)(?:_[^_]+){1}$')[,2])
  modcomp_temp$diff_lag <- modcomp_temp$time_lag_1 - modcomp_temp$time_lag_2
  
  modcomp_temp_select <- modcomp_temp %>% filter(pval < 0.05, diff_aic < 0)
  
} else if (model_type == "rf"){
  
  if(mod == "abundance"){
    logtransform_resp_var = TRUE
    if(model_purpose == "predictive"){
      time_vars <- c("TMAX1","TMIN1","TAMP1","SMO1","EVT8","VNV8","LIG30","WVV10","WVH10")
    } else if(model_purpose == "explicative"){
      time_vars <- c("TMAX1","TMIN1","TAMP1","SMO1","EVT8","VNV8","LIG30")
    }
  } else if (mod == "presence"){
    logtransform_resp_var = FALSE
    if(model_purpose == "predictive"){
     time_vars <- c("TMAX7","TMIN7","TAMP7","SMO7","EVT8","VNV8","LIG30","WVV10","WVH10")
    } else if (model_purpose == "explicative"){
      time_vars <- c("TMAX7","TMIN7","TAMP7","SMO7","EVT8","VNV8","LIG30")
    }
  }
  
  fun_rf_time_comp <- function(var_to_test){
    
    modcomp <- fun_feature_forward_selection(
      stat_method = "rf", 
      mod = mod, 
      type = "univariate_selection", 
      expl_vars_to_test = colnames(th_trmetrics_entomo_postedecapture[which(grepl(var_to_test,colnames(th_trmetrics_entomo_postedecapture)))]),
      expl_vars_to_keep =  c(col_to_keep_rainfall,"X_32630", "Y_32630"),
      cross_validation_type = "temporal",
      logtransform_resp_var = logtransform_resp_var)
    
     modcomp$time_lag_1 <- as.numeric(sub('.*\\_', '', modcomp$name))
     modcomp$time_lag_2 <- as.numeric(stringr::str_match( modcomp$name, '([^_]+)(?:_[^_]+){1}$')[,2])
     modcomp$diff_lag <- modcomp$time_lag_1 - modcomp$time_lag_2

       if(var_to_test %in% c("TMAX1","TMIN1","TAMP1","SMO1")){
         modcomp <- modcomp %>% filter(diff_lag >= 7)
       }
       
       modcomp <- modcomp %>% filter(diff_res_w_basemod > 0) 
       if(nrow(modcomp) > 0){
         col_to_keep <- as.character(modcomp$name[which.max(modcomp$diff_res_w_basemod)])
         qual <- modcomp$res[which.max(modcomp$diff_res_w_basemod)]
       } else {
         col_to_keep <- NA
         qual <- NA
       }
    
       df_cols_to_keep_temporal <-  data.frame(var = var_to_test, col_to_keep = col_to_keep, best_res = qual)
       
    return(df_cols_to_keep_temporal)

  }
  
  df_cols_to_keep_temporal <- data.frame(var = character(), col_to_keep = character(), best_res = numeric())
  
  for(i in 1:length(time_vars)){
    cat('calculating column to keep for temporal variable ',time_vars[i],"\n")
    th_col_to_keep_temporal <- fun_rf_time_comp(time_vars[i])
    df_cols_to_keep_temporal <- rbind(df_cols_to_keep_temporal, th_col_to_keep_temporal)
    # for funestus abundance : cols_to_keep_temporal <- c("RFD1_F_2000_8_16", "TMAX1_2000_28_55", "TMIN1_2000_32_40", "TAMP1_2000_2_19",  "SMO1_2000_6_25" ,  "EVT8_2000_1_1"  ,  "VNV8_2000_0_2"   , "LIG30_2000_2_2" ,  "WVV10_2000_0_0" ,  "WVH10_2000_3_3")
    # for funestus presence : cols_to_keep_temporal <- c("RFD1_F_2000_11_22", "TMAX7_2000_7_10" ,  "TMIN7_2000_5_7" ,   "TAMP7_2000_5_8"  ,  "SMO7_2000_0_2" ,    "EVT8_2000_2_2"  ,   "VNV8_2000_0_0"   ,  "LIG30_2000_2_2"  ,  "WVV10_2000_4_5"   , "WVH10_2000_4_4")
  }
  
  
  ## multicollinearity analysis
  df_cols_to_keep_temporal <- df_cols_to_keep_temporal %>% filter(!is.na(best_res))
  
  prediction_vars2 <- prediction_vars %>%
    mutate(code = gsub("1","7",code)) %>%
    mutate(code = gsub("70","10",code)) %>%
    rbind(prediction_vars)

  m <- th_trmetrics_entomo_postedecapture[,as.character(df_cols_to_keep_temporal$col_to_keep)] %>%
    select_if(is.numeric) %>%
    cor(.,method = "spearman", use = "na.or.complete")
  index <- which(abs(m) > .7 & abs(m) < 1, arr.ind = T)
  p <- cbind.data.frame(stock1 = rownames(m)[index[,1]],stock2 = colnames(m)[index[,2]]) %>%
    mutate(name1 = gsub("_"," ",stock1)) %>%
    mutate(name2 = gsub("_"," ",stock2)) %>%
    mutate(name1 = word(name1,1)) %>%
    mutate(name2 = word(name2,1)) %>%
    left_join(prediction_vars2 %>% dplyr::select(code,priority), by = c("name1" = "code")) %>%
    dplyr::rename(priority1 = priority) %>%
    left_join(prediction_vars2 %>% dplyr::select(code,priority), by = c("name2" = "code")) %>%
    dplyr::rename(priority2 = priority)
    
  if(nrow(p) > 0){
    var_to_remove <- NULL
    for(i in 1:nrow(p)){
      if(p$priority1[i] < p$priority2[i]){
        var_to_remove <- c(var_to_remove,as.character(p$stock2[i]))
        } else if (p$priority1[i] > p$priority2[i]) {
        var_to_remove <- c(var_to_remove,as.character(p$stock1[i]))
        }
    }
    var_to_remove <- unique(var_to_remove)
    df_cols_to_keep_temporal <- df_cols_to_keep_temporal %>% filter(!(col_to_keep %in% var_to_remove))
  }

    cols_to_keep_temporal <- c(col_to_keep_rainfall, as.character(df_cols_to_keep_temporal$col_to_keep))

}


#### non-temporal variables
corr_lsm <- NULL
corr_env_spatial <- NULL
corr_env_nightcatch <- NULL
corr_env_nightcatch_postedecapture <- NULL
corr_env_static <- NULL

## filter landcover variables with spearman coefficient 

lco_metadata <- dbReadTable(react_gpkg, 'lco_metadata') # table containing pixel value and label for each land cover map
metrics_defs <- landscapemetrics::list_lsm() # list of landscape metrics
lco_priority <- read.csv("data/react_db/miscellaneous_data/landcover/lco_pix_priority.csv", stringsAsFactors = F, sep = ",")
lco_metadata <- lco_metadata %>% left_join(lco_priority)

expl_vars_to_test =  colnames(th_trmetrics_entomo_postedecapture[which(grepl("lsm",colnames(th_trmetrics_entomo_postedecapture)))])
expl_vars_to_test_lcid <- as.numeric(word(gsub("_"," ",expl_vars_to_test),5))

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

corr_env_static <- corr_env_static %>% filter(pval<=0.05,abs_corr>=0.2)
if(nrow(corr_env_static)>0){
  
  corr_env_static <- corr_env_static %>% 
    left_join(prediction_vars, by = c("name" = "code")) %>%
    dplyr::select(short_name,correlation,name) %>%
    dplyr::rename(lab = short_name) %>%
    mutate(type = "Other spatial-only variables")
  
}



df_corr <- rbind(corr_lsm, corr_env_spatial, corr_env_nightcatch_postedecapture, corr_env_nightcatch, corr_env_static)


#### 
predictors_multivariate <- c(cols_to_keep_temporal,df_corr$name,"X_32630","Y_32630","int_ext","VCP",colnames(th_trmetrics_entomo_postedecapture[which(grepl("VCM_",colnames(th_trmetrics_entomo_postedecapture)))]))




if(model_purpose == "predictive"){
  
  vars_to_remove <- c("NMT" ,  "NML" ,  "NMH"  ,   "NDP",  "RFH" ,  "WDR"   ,"WSP")
  
  if(predictive_type %in% c("roi_notvillages","notroi")){
  
  vars_to_remove <- c(vars_to_remove,colnames(th_trmetrics_entomo_postedecapture[which(grepl("POP",colnames(th_trmetrics_entomo_postedecapture)))]),
                      colnames(th_trmetrics_entomo_postedecapture[which(grepl("VCM_",colnames(th_trmetrics_entomo_postedecapture)))]),
                      "VCP","VCT","X_32630","Y_32630")
  }
  
  predictors_multivariate <- setdiff(predictors_multivariate, vars_to_remove)
  
}

#predictors_multivariate <- paste(predictors_multivariate,collapse="+")



library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

library(CAST)
library(caret)
indices_spatial <- CreateSpacetimeFolds(th_trmetrics_entomo_postedecapture, spacevar = "codevillage", k = length(unique(th_trmetrics_entomo_postedecapture$codevillage)))
indices_temporal <- CreateSpacetimeFolds(th_trmetrics_entomo_postedecapture, timevar = "nummission", k = length(unique(th_trmetrics_entomo_postedecapture$nummission)))
indices_spatiotemporal <- CreateSpacetimeFolds(th_trmetrics_entomo_postedecapture, timevar = "nummission", spacevar = "codevillage", k = 3) 


if(mod == "abundance"){
  tr_spatial = trainControl(method="cv",
                    index = indices_spatial$index, 
                    indexOut = indices_spatial$indexOut)
  tr_temporal = trainControl(method="cv",
                            index = indices_temporal$index, 
                            indexOut = indices_temporal$indexOut)
  
  met = "Rsquared"
  
  th_trmetrics_entomo_postedecapture$resp_var <- log(th_trmetrics_entomo_postedecapture$resp_var)
  
} else if (mod == "presence"){
  tr_spatial = trainControl(method="cv",
                    index = indices_spatial$index, 
                    indexOut = indices_spatial$indexOut,
                    sampling = "up",
                    summaryFunction = prSummary,
                    classProbs = TRUE)
  tr_temporal = trainControl(method="cv",
                            index = indices_temporal$index, 
                            indexOut = indices_temporal$indexOut,
                            sampling = "up",
                            summaryFunction = prSummary,
                            classProbs = TRUE)
  
  met = "AUC"
  
  th_trmetrics_entomo_postedecapture$resp_var <- ifelse(th_trmetrics_entomo_postedecapture$resp_var==0,"Absence","Presence")
  th_trmetrics_entomo_postedecapture$resp_var <- as.factor(th_trmetrics_entomo_postedecapture$resp_var)

}

#model <- caret::train(form = as.formula(paste0("resp_var ~ ",predictors_multivariate)), data = th_trmetrics_entomo_postedecapture, method="ranger", tuneLength = 5, trControl=tr)


model_ffs_spatial <- CAST::ffs(predictors = th_trmetrics_entomo_postedecapture[,predictors_multivariate], response = th_trmetrics_entomo_postedecapture$resp_var, method = "ranger", tuneLength = 5, trControl = tr_spatial, metric = met)
model_ffs_temporal <- CAST::ffs(predictors = th_trmetrics_entomo_postedecapture[,predictors_multivariate], response = th_trmetrics_entomo_postedecapture$resp_var, method = "ranger", tuneLength = 5, trControl = tr_temporal, metric = met)



model_ffs_temporal$selectedvars <- c("TMAX1_2000_28_55","lsm_c_pland_250_5_4","int_ext")
model_ffs_spatial$selectedvars <- c("lsm_c_pland_2000_5_22" , "lsm_c_pland_2000_5_23", "lsm_c_pland_1000_5_10", "VCM_Larvicide" )
  
#predictors_multivariate_final <- unique(c(model_ffs_spatial$selectedvars, model_ffs_temporal$selectedvars))

model_ffs_temporal <- caret::train(form = as.formula(paste0("resp_var ~ ",paste(model_ffs_temporal$selectedvars, collapse = "+"))), data = th_trmetrics_entomo_postedecapture, method="ranger", tuneLength = 10, trControl=tr_temporal, importance = "permutation")


th_trmetrics_entomo_postedecapture$pred_log <- predict(model_ffs_temporal)
th_trmetrics_entomo_postedecapture$pred <- exp(th_trmetrics_entomo_postedecapture$pred)


## residual analysis
plot(th_trmetrics_entomo_postedecapture$pred,th_trmetrics_entomo_postedecapture$resp_var)
model_ffs_temporal$y <- th_trmetrics_entomo_postedecapture$resp_var
model_ffs_temporal$x <- th_trmetrics_entomo_postedecapture[, c("TMAX1_2000_28_55","lsm_c_pland_250_5_4","int_ext")]
plotmo::plotres(model_ffs_temporal)

# variable importance plot
library(vip)
plot(model_ffs_temporal$finalModel)


# map
th_trmetrics_entomo_postedecapture_byvillage <- th_trmetrics_entomo_postedecapture %>%
  group_by(codevillage,nummission) %>%
  summarise(resp_var = sum(resp_var), pred = sum(pred), X = mean(X_4326), Y = mean(Y_4326), mean_date_mission = as.Date(mean(mean_date_mission), origin = "1970-01-01"))

library(ggmap)
library(patchwork)

roi <- st_read(path_to_db,"contexte_frontieresreact") %>% 
  filter(codepays==code_pays) %>%
  st_transform(4326) %>%
  st_bbox() %>%
  as.numeric()
myLocation <- c(roi[1], roi[2], roi[3], roi[4])
myMap <- get_map(location=myLocation, source="osm",crop=FALSE)


map_resp <- ggmap(myMap) + 
  geom_point(aes(x = X, y = Y, size = resp_var), data = th_trmetrics_entomo_postedecapture_byvillage, alpha = .5, color="darkred") + 
  facet_wrap(.~mean_date_mission)

map_pred <- ggmap(myMap) + 
  geom_point(aes(x = X, y = Y, size = pred), data = th_trmetrics_entomo_postedecapture_byvillage, alpha = .5, color="darkred") + 
  facet_wrap(.~mean_date_mission)

map_resp + map_pred

