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


fun_workflow_model <- function(response_var, 
                               code_pays, 
                               mod, 
                               lag_time_window = c(0,90),
                               buffer_sizes = c(500,1000,2000)){
  
  cat("Executing workflow for parameters : ", response_var, code_pays, mod,"\n")
  
  ###### load the data
  
  # load spatiotemporal data
  env_spatiotemporal <- load_spatiotemporal_data(vars = c("RFD1F","TMIN1","TMAX1","TAMP1","SMO1","VNV8","EVT8","WVV10","WVH10"),
                                                 buffers = c(2000),
                                                 lag_time_window = lag_time_window,
                                                 summarize_days_to_week = FALSE,
                                                 code_pays = code_pays,
                                                 entomo_csh_metadata_l1 = entomo_csh_metadata_l1)
  
  
  # load spatial data 
  # if(predictive_type == "notroi"){
  #   landcover_layers_to_keep <- c(11,12)
  # } else {
  if(code_pays == "BF"){
    landcover_layers_to_keep <- c(2,3,4,5,11,12)
  } else if (code_pays == "CI"){
    landcover_layers_to_keep <- c(7,8,9,10,11,12)
  }
  #}
  
  landcover_metrics_to_keep <- c("pland","prd","shdi","np")
  
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
  
  th_trmetrics_entomo_postedecapture <- dbReadTable(react_gpkg, 'trmetrics_entomo_postedecapture') %>% 
    dplyr::select(-fid) %>% 
    left_join(entomo_csh_metadata_l1 %>% dplyr::select(idpointdecapture, codevillage, pointdecapture, codepays, nummission, period_interv)) %>% 
    filter(!is.na(codevillage)) %>%
    filter(codepays == code_pays) %>%
    mutate(heuredecapture = NA)
  
  th_trmetrics_entomo_postedecapture$resp_var <- th_trmetrics_entomo_postedecapture[,response_var]
  
  
  if(mod == "abundance" ){
    th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% filter(resp_var > 0 )
  } else if (mod == "presence" ){
    th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% mutate(resp_var = ifelse(resp_var == 0,0,1 ))
  }
  
  th_env_static2 <- th_env_static %>% mutate(v = 1) %>% pivot_wider(names_from = VCM,  values_from = v, values_fill = list(v = 0), names_prefix = "VCM_")
  th_env_static <- th_env_static %>% dplyr::select(idpointdecapture,VCM)
  
  mean_date_mission <- entomo_csh_metadata_l1 %>% mutate(date_capture = as.Date(date_capture)) %>% dplyr::group_by(codepays,nummission) %>% dplyr::summarise(mean_date_mission=mean(date_capture)) %>% as_tibble() %>% filter(codepays==code_pays) %>% dplyr::select(-codepays) 
  
  
  ######## join response variable with explanatory variables
  
  th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% 
    left_join(mean_date_mission) %>%
    left_join(mean_coords_points_4326) %>%
    left_join(mean_coords_points_32630) %>%
    mutate(int_ext = substr( idpostedecapture,nchar(idpostedecapture),nchar(idpostedecapture))) %>%
    mutate(IEH = ifelse(int_ext == "i",1,0)) %>%
    left_join(env_spatiotemporal) %>%
    left_join(env_spatial) %>%
    left_join(th_env_nightcatch_postedecapture) %>%
    left_join(th_env_nightcatch) %>%
    left_join(th_env_static) %>%
    left_join(th_env_static2) %>%
    left_join(env_landcover) %>%
    mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture[, colSums(is.na(th_trmetrics_entomo_postedecapture)) != nrow(th_trmetrics_entomo_postedecapture)]
  
  
  time_vars_rf <- c("RFD1F","TMIN1","TMAX1","TAMP1","SMO1","VNV8","EVT8","WVV10","WVH10")
  
  if(code_pays == "BF"){ lcid <- 3 } else if (code_pays == "CI"){ lcid <- 8 }
  
  lsm_colnames <- colnames(th_trmetrics_entomo_postedecapture[which(grepl("lsm",colnames(th_trmetrics_entomo_postedecapture)))])
  lsm_colnames_opensource <- lsm_colnames[which(grepl("_11_",lsm_colnames))]
  lsm_colnames_notopensource <- lsm_colnames[which(grepl(paste0("_",lcid,"_"),lsm_colnames))]
  
  spatial_vars_rf_opensource <- c(lsm_colnames_opensource,
                                  colnames(th_trmetrics_entomo_postedecapture[which(grepl("WMD|TWI|WAC|WAD|WLS|WAL|HYS|POH|LIG|BCH|BDE",colnames(th_trmetrics_entomo_postedecapture)))]))
  
  spatial_vars_rf_notopensource <- c(unique(c(lsm_colnames_notopensource,lsm_colnames_opensource)),
                           colnames(th_trmetrics_entomo_postedecapture[which(grepl("WMD|TWI|WAC|WAD|WLS|WAL|HYS|POP|POH|LIG|BCH|BDE",colnames(th_trmetrics_entomo_postedecapture)))]))
  
  
  ###### selection of spatial variables
  
  spatial_predictors_rf_opensource <- fun_multicol(th_trmetrics_entomo_postedecapture, spatial_vars_rf_opensource)
  spatial_predictors_rf_notopensource <- fun_multicol(th_trmetrics_entomo_postedecapture, spatial_vars_rf_notopensource)
  
  
  spatial_predictors_rf_opensource <- fun_feature_forward_selection(
    df = th_trmetrics_entomo_postedecapture,
    stat_method = "rf",
    mod = mod,
    type = "model_comparison",
    expl_vars_to_keep = "nummission",
    expl_vars_to_test = spatial_predictors_rf_opensource,
    cross_validation_type = "spatial",
    tune_length = 3)
  
  spatial_predictors_rf_notopensource <- fun_feature_forward_selection(
    df = th_trmetrics_entomo_postedecapture,
    stat_method = "rf",
    mod = mod,
    type = "model_comparison",
    expl_vars_to_keep = "nummission",
    expl_vars_to_test = spatial_predictors_rf_notopensource,
    cross_validation_type = "spatial",
    tune_length = 3)
  
  
  spatial_predictors_rf_opensource <- spatial_predictors_rf_opensource %>%
    arrange(desc(diff_res_w_basemod)) %>%
    top_n(10)
  
  spatial_predictors_rf_notopensource <- spatial_predictors_rf_notopensource %>%
    arrange(desc(diff_res_w_basemod)) %>%
    top_n(10)
  
  spatial_predictors_rf_opensource <- as.character(spatial_predictors_rf_opensource$name)
  spatial_predictors_rf_notopensource <- as.character(spatial_predictors_rf_notopensource$name)
  

  ###### selection of temporal variables
  
   time_starts <- seq(0,90,7)
   time_ends <- rep(90, length(time_starts))
   
   df_times <- data.frame(time_starts = time_starts, time_ends = time_ends )
   
   df_times <- df_times %>%
       mutate(colnames_timevars = map2(time_starts, time_ends, ~fun_get_temporal_preds_columns(.x, .y, time_vars_rf, 7, th_trmetrics_entomo_postedecapture))) %>%
     mutate(time_preds = map(colnames_timevars, ~fun_get_time_preds_rf(th_trmetrics_entomo_postedecapture, .x, time_vars_rf, tune_length = 1, mod = mod)))
   
   rf_model <- df_times %>%
     mutate(model_notopensource = map(time_preds, ~fun_run_rf(th_trmetrics_entomo_postedecapture, .x, spatial_predictors_rf_notopensource, mod))) %>%
     mutate(model_opensource = map(time_preds, ~fun_run_rf(th_trmetrics_entomo_postedecapture, .x, spatial_predictors_rf_opensource, mod)))

   
   return(list(rf_model_notopensource = rf_model_notopensource, rf_model_opensource = rf_model_opensource))
   
}
   

df_input_params <- tibble(response_var = character(), code_pays = character(), mod = character())
df_input_params <- df_input_params %>%
  add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "presence") %>%
  add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "abundance") %>%
  add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "presence") %>%
  add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "abundance") %>%
  add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "presence") %>%
  add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "abundance")


th_model_results1 <- df_input_params[1,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))

th_model_results2 <- df_input_params[2,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))

th_model_results3 <- df_input_params[3,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))

th_model_results4 <- df_input_params[4,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))

th_model_results5 <- df_input_params[5,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
th_model_results6 <- df_input_params[6,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))


model_results <- rbind(th_model_results1,th_model_results2,th_model_results3,th_model_results4,th_model_results5,th_model_results6)
