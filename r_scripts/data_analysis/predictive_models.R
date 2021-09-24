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
entomo_csh_metadata_l1 <- dbReadTable(react_gpkg, 'entomo_csh_metadata_l1') %>% dplyr::select(-geom) %>% filter(!(nummission %in% c("11","12","13","15")))

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
                               lag_time_window = c(0,60),
                               buffer_sizes = c(250,2000),
                               spatial_granularity = "idpointdecapture"){
  
  
  cat("Executing workflow for parameters : ", response_var, code_pays, mod,"\n")
  
  ###### load the data
  
  time_vars = c("RFD1L","TMIN1","TMAX1","SMO1","TAMP1","VNV8","VEV8","EVT8","VNV30","VMV30","WNW30","WVV10","WVH10","DTL7")
  # load spatiotemporal data
  env_spatiotemporal <- load_spatiotemporal_data(vars = time_vars,
                                                 buffers = c(250,2000),
                                                 lag_time_window = lag_time_window,
                                                 summarize_days_to_week = FALSE,
                                                 code_pays = code_pays,
                                                 entomo_csh_metadata_l1 = entomo_csh_metadata_l1)
  
  
  
  if(code_pays == "BF"){
    landcover_layers_to_keep <- c(2,3,11)
  } else if (code_pays == "CI"){
    landcover_layers_to_keep <- c(7,8,11)
  }

  landcover_metrics_to_keep <- c("pland","prd")
  
  env_spatial_all <- load_spatial_data(code_pays, landcover_layers_to_keep, mod, landcover_metrics_to_keep, buffer_sizes = c(100,250,2000))
  env_landcover <- env_spatial_all[[1]]
  env_spatial <- env_spatial_all[[2]]
  th_env_nightcatch <- env_spatial_all[[4]]
  th_env_static <- env_spatial_all[[5]]
  rm(env_spatial_all)
  
  time_since_vc <- load_time_since_vc(code_pays, entomo_csh_metadata_l1)
  
  # load human beahviour use data
  hum_behav <- load_hmnbehav_data(code_pays, entomo_csh_metadata_l1)
  LUS = hum_behav[[1]]
  hum_behav_4_exophagy = hum_behav[[2]]
  hum_behav_4_earlylatebiting = hum_behav[[3]]
  rm(hum_behav)
  
  # load coordinates
  spatial_coordinates <- load_csh_sp_coord()
  mean_coords_points_4326 <- spatial_coordinates[[1]]
  mean_coords_points_32630 <- spatial_coordinates[[2]]
  rm(spatial_coordinates)
  
  th_trmetrics_entomo_postedecapture <- dbReadTable(react_gpkg, 'trmetrics_entomo_postedecapture') %>% 
    dplyr::select(-fid) %>% 
    left_join(entomo_csh_metadata_l1 %>% dplyr::select(idpointdecapture, codevillage, pointdecapture, codepays, nummission, period_interv, season)) %>% 
    filter(!is.na(codevillage)) %>%
    filter(codepays == code_pays)
  
  th_trmetrics_entomo_postedecapture$resp_var <- th_trmetrics_entomo_postedecapture[,response_var]


  if( spatial_granularity == "idpointdecapture"){
    th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
      group_by(idpointdecapture,codevillage, pointdecapture, codepays, nummission) %>%
      summarise(resp_var = mean(resp_var)) %>%
      as_tibble()
  } else if ( spatial_granularity == "codevillage"){
    th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
      group_by(codevillage, codepays, nummission) %>%
      summarise(resp_var = mean(resp_var)) %>%
      as_tibble()
  }
  
  
  if(mod %in% c("abundance","abundance_discrete") ){
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
    left_join(env_spatiotemporal) %>%
    left_join(env_spatial) %>%
    #left_join(th_env_nightcatch_postedecapture) %>%
    left_join(th_env_nightcatch) %>%
    left_join(th_env_static) %>%
    left_join(th_env_static2) %>%
    left_join(env_landcover) %>%
    left_join(hum_behav_4_earlylatebiting) %>%
    left_join(LUS) %>%
    mutate(VCM = case_when(VCM == "Ctrle" ~ "LLIN",
                           VCM == "IEC" ~ "LLIN + IEC",
                           VCM == "IRS" ~ "LLIN + IRS",
                           VCM == "IVM" ~ "LLIN + IVM",
                           VCM == 'Larvicide' ~ 'LLIN + Larv.')) %>%
    mutate(mission_village = paste0(nummission,codevillage)) %>%
   # mutate_if(is.numeric,funs(ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>% #mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
    left_join(time_since_vc)
  
  th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture[, colSums(is.na(th_trmetrics_entomo_postedecapture)) != nrow(th_trmetrics_entomo_postedecapture)]
  
  ## fill missing values
  cols_miss_vals <- names(which(sapply(th_trmetrics_entomo_postedecapture, anyNA)))
  th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
    mutate_at(cols_miss_vals,funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  
  #### set variables to keep for modeling
  
  predictors_temporal <- c("RFD1L","TMIN1","TMAX1","SMO1","TAMP1","VNV8","VEV8","EVT8","VNV30","VMV30","WNW30","WVV10","WVH10","DTL7")
  
  ## open source variables (spatial)
  predictors_spatial_opensource <- colnames(th_trmetrics_entomo_postedecapture)[grepl("TEL|TSL|TAS|WAC|TCI|TWI|WAD|WLS|WAL|POH|LMN|WMD",colnames(th_trmetrics_entomo_postedecapture))]
  predictors_spatial_opensource <- c(predictors_spatial_opensource,setdiff(c(colnames(env_landcover)[grepl("_11_",colnames(env_landcover))]),"idpointdepcapture"))
  
  # to collect variables (spatial)
  predictors_spatial_tocollect <- colnames(th_trmetrics_entomo_postedecapture)[grepl("POP|HYS|BCH|BDE",colnames(th_trmetrics_entomo_postedecapture))]
  predictors_spatial_tocollect <- c(predictors_spatial_tocollect, c("VCM","VCT","LUS"))
  if(code_pays=="BF"){
    predictors_spatial_tocollect <- c(predictors_spatial_tocollect,setdiff(c(colnames(env_landcover)[grepl("_3_",colnames(env_landcover))]),"idpointdepcapture"))
    predictors_spatial_tocollect <- setdiff(predictors_spatial_tocollect, predictors_spatial_tocollect[grepl("_3_12|3_5|3_10|3_6",predictors_spatial_tocollect)])
    predictors_spatial_tocollect <- c(predictors_spatial_tocollect, "lsm_c_pland_100_2_5", "lsm_c_pland_250_2_5","lsm_c_pland_2000_2_5")
  } else if(code_pays=="CI"){
    predictors_spatial_tocollect <- c(predictors_spatial_tocollect,setdiff(c(colnames(env_landcover)[grepl("_8_",colnames(env_landcover))]),"idpointdepcapture"))
    predictors_spatial_tocollect <- setdiff(predictors_spatial_tocollect, predictors_spatial_tocollect[grepl("_8_12|8_13|8_10|8_3",predictors_spatial_tocollect)])
    predictors_spatial_tocollect <- c(predictors_spatial_tocollect,"lsm_c_pland_2000_7_6")
  }
  predictors_spatial_tocollect <- c(predictors_spatial_tocollect,predictors_spatial_opensource)
  
  
  # filter spatial collinear variables
  predictors_spatial_tocollect <- fun_multicol(th_trmetrics_entomo_postedecapture, predictors_spatial_tocollect)
  predictors_spatial_opensource <- fun_multicol(th_trmetrics_entomo_postedecapture, predictors_spatial_opensource)
  
  # get correlation coefficient spatial variables
  spatial_corrs_spearman_opensource <- fun_feature_forward_selection(
    df = th_trmetrics_entomo_postedecapture, 
    stat_method = "spearman", 
    spearman_factor = NULL,
    mod = ifelse(mod=="presence","presence","abundance"), 
    type = "univariate_selection", 
    expl_vars_to_keep = NULL,
    expl_vars_to_test = predictors_spatial_opensource)
  
  spatial_corrs_spearman_tocollect <- fun_feature_forward_selection(
    df = th_trmetrics_entomo_postedecapture, 
    stat_method = "spearman", 
    spearman_factor = NULL,
    mod = ifelse(mod=="presence","presence","abundance"), 
    type = "univariate_selection", 
    expl_vars_to_keep = NULL,
    expl_vars_to_test = predictors_spatial_tocollect)

  spatial_corrs_spearman_opensource <- spatial_corrs_spearman_opensource %>%
    filter(name!="useless_col") %>%
    top_n(20,abs_corr) 
    
  
  spatial_corrs_spearman_tocollect <- spatial_corrs_spearman_tocollect %>%
    filter(name!="useless_col") %>%
    top_n(20,abs_corr)
  
  ## temporal 
    lags1 <- seq(0,112,7)
    lags2 <- c(0,seq(27,112,7))
  
  rf_opensource <- list()
  rf_opensource_simple <- list()
  rf_tocollect <- list()

    for(i in 1:5){
    
     cat(lags1[i],"\n")
      ## for rf_opensource and rf_tocollect : 
      
  colnames_tempvar_lag1 <- fun_get_temporal_preds_columns(lags1[i],60,c("RFD1L", "TMIN1", "TMAX1", "SMO1" , "TAMP1"),7,th_trmetrics_entomo_postedecapture, c(2000))
  colnames_tempvar_lag2 <- fun_get_temporal_preds_columns(lags2[i],60,c("VNV8" , "VEV8", "EVT8"),7,th_trmetrics_entomo_postedecapture, c(250,2000))
  colnames_tempvar_lag3 <- fun_get_temporal_preds_columns(lags1[i],60,c("VNV30", "VMV30", "WNW30", "WVV10", "WVH10","DTL7"),7,th_trmetrics_entomo_postedecapture, c(250,2000))
  
  expl_vars_to_test1 <- intersect(colnames_tempvar_lag1, colnames(th_trmetrics_entomo_postedecapture))
  expl_vars_to_test2 <- intersect(colnames_tempvar_lag2, colnames(th_trmetrics_entomo_postedecapture))
  expl_vars_to_test3 <- intersect(colnames_tempvar_lag3, colnames(th_trmetrics_entomo_postedecapture))
  
  expl_vars_to_test <- c(expl_vars_to_test1,expl_vars_to_test2, expl_vars_to_test3)
  
  temporal_corr_spearman <- fun_feature_forward_selection(
    df = th_trmetrics_entomo_postedecapture,
    stat_method = "spearman",
    spearman_factor = NULL,
    mod = ifelse(mod=="presence","presence","abundance"),
    type = "univariate_selection",
    expl_vars_to_keep = NULL,
    expl_vars_to_test = expl_vars_to_test)
  
  temporal_corr_spearman$time_lag_1 <- as.numeric(sub('.*\\_', '', temporal_corr_spearman$name))
  temporal_corr_spearman$time_lag_2 <- as.numeric(stringr::str_match( temporal_corr_spearman$name, '([^_]+)(?:_[^_]+){1}$')[,2])
  temporal_corr_spearman$diff_lag <- temporal_corr_spearman$time_lag_1 - temporal_corr_spearman$time_lag_2
  temporal_corr_spearman$var <- sub("\\_.*", "", temporal_corr_spearman$name)
  
  
  temporal_corr_spearman <- temporal_corr_spearman %>%
    group_by(var) %>%
    top_n(20,abs_corr)
  
  predictors_temporal <- fun_multicol(th_trmetrics_entomo_postedecapture, temporal_corr_spearman$name)
  
  predictors_opensource <- c(predictors_temporal,spatial_corrs_spearman_opensource$name)
  predictors_tocollect <- c(predictors_temporal,spatial_corrs_spearman_tocollect$name)
  
  predictors_opensource <- fun_multicol(th_trmetrics_entomo_postedecapture, predictors_opensource)
  predictors_tocollect <- fun_multicol(th_trmetrics_entomo_postedecapture, predictors_tocollect)

  rf_opensource[[i]] <- fun_compute_rf(th_trmetrics_entomo_postedecapture, predictors_opensource, cv_col = "by_ptcapt3", mod, featureselect = "rfe", species = response_var,tune_length = 1, type = 'predictive')
  rf_tocollect[[i]] <- fun_compute_rf(th_trmetrics_entomo_postedecapture, predictors_tocollect, cv_col = "by_ptcapt3", mod, featureselect = "rfe", species = response_var,tune_length = 1, type = 'predictive')
  
  
  ## for rf_opensource_simple : 

  colnames_tempvar_lag1 <- fun_get_temporal_preds_columns(lags1[i],60,c("RFD1L", "TMIN1", "TMAX1", "SMO1" , "TAMP1"),30,th_trmetrics_entomo_postedecapture, c(2000))
  colnames_tempvar_lag2 <- fun_get_temporal_preds_columns(lags2[i],120,c("VNV8" , "VEV8", "EVT8"),30,th_trmetrics_entomo_postedecapture, c(250,2000))
  colnames_tempvar_lag3 <- fun_get_temporal_preds_columns(lags1[i],60,c("VNV30", "VMV30", "WNW30", "WVV10", "WVH10","DTL7"),30,th_trmetrics_entomo_postedecapture, c(250,2000))
  
  colnames_tempvar_lag1 = data.frame(name = colnames_tempvar_lag1)
  colnames_tempvar_lag2 = data.frame(name = colnames_tempvar_lag2)
  colnames_tempvar_lag3 = data.frame(name = colnames_tempvar_lag3)
  
  colnames_tempvar_lag1$time_lag_1 <- as.numeric(sub('.*\\_', '', colnames_tempvar_lag1$name))
  colnames_tempvar_lag1$time_lag_2 <- as.numeric(stringr::str_match( colnames_tempvar_lag1$name, '([^_]+)(?:_[^_]+){1}$')[,2])
  colnames_tempvar_lag1$diff_lag <- colnames_tempvar_lag1$time_lag_1 - colnames_tempvar_lag1$time_lag_2
  colnames_tempvar_lag1$var <- sub("\\_.*", "", colnames_tempvar_lag1$name)
  colnames_tempvar_lag2$time_lag_1 <- as.numeric(sub('.*\\_', '', colnames_tempvar_lag2$name))
  colnames_tempvar_lag2$time_lag_2 <- as.numeric(stringr::str_match( colnames_tempvar_lag2$name, '([^_]+)(?:_[^_]+){1}$')[,2])
  colnames_tempvar_lag2$diff_lag <- colnames_tempvar_lag2$time_lag_1 - colnames_tempvar_lag2$time_lag_2
  colnames_tempvar_lag2$var <- sub("\\_.*", "", colnames_tempvar_lag2$name)
  colnames_tempvar_lag3$time_lag_1 <- as.numeric(sub('.*\\_', '', colnames_tempvar_lag3$name))
  colnames_tempvar_lag3$time_lag_2 <- as.numeric(stringr::str_match( colnames_tempvar_lag3$name, '([^_]+)(?:_[^_]+){1}$')[,2])
  colnames_tempvar_lag3$diff_lag <- colnames_tempvar_lag3$time_lag_1 - colnames_tempvar_lag3$time_lag_2
  colnames_tempvar_lag3$var <- sub("\\_.*", "", colnames_tempvar_lag3$name)
  
  colnames_tempvar_lag1 <- colnames_tempvar_lag1 %>% group_by(var) %>% top_n(2,desc(time_lag_2))
  colnames_tempvar_lag2 <- colnames_tempvar_lag2 %>% group_by(var) %>% top_n(2,desc(time_lag_2))
  colnames_tempvar_lag3 <- colnames_tempvar_lag3 %>% group_by(var) %>% top_n(2,desc(time_lag_2))
  
  expl_vars_to_test1 <- intersect(colnames_tempvar_lag1$name, colnames(th_trmetrics_entomo_postedecapture))
  expl_vars_to_test2 <- intersect(colnames_tempvar_lag2$name, colnames(th_trmetrics_entomo_postedecapture))
  expl_vars_to_test3 <- intersect(colnames_tempvar_lag3$name, colnames(th_trmetrics_entomo_postedecapture))
  
  expl_vars_to_test <- c(expl_vars_to_test1,expl_vars_to_test2, expl_vars_to_test3)
  
  temporal_corr_spearman <- fun_feature_forward_selection(
    df = th_trmetrics_entomo_postedecapture,
    stat_method = "spearman",
    spearman_factor = NULL,
    mod = ifelse(mod=="presence","presence","abundance"),
    type = "univariate_selection",
    expl_vars_to_keep = NULL,
    expl_vars_to_test = expl_vars_to_test)
  
  temporal_corr_spearman$time_lag_1 <- as.numeric(sub('.*\\_', '', temporal_corr_spearman$name))
  temporal_corr_spearman$time_lag_2 <- as.numeric(stringr::str_match( temporal_corr_spearman$name, '([^_]+)(?:_[^_]+){1}$')[,2])
  temporal_corr_spearman$diff_lag <- temporal_corr_spearman$time_lag_1 - temporal_corr_spearman$time_lag_2
  temporal_corr_spearman$var <- sub("\\_.*", "", temporal_corr_spearman$name)

  temporal_corr_spearman <- temporal_corr_spearman %>%
    group_by(var) %>%
    top_n(20,abs_corr)
  
  predictors_temporal <- fun_multicol(th_trmetrics_entomo_postedecapture, temporal_corr_spearman$name)
  
  predictors_opensource_simple <- c(predictors_temporal,spatial_corrs_spearman_opensource$name)
  predictors_opensource_simple <- fun_multicol(th_trmetrics_entomo_postedecapture, predictors_opensource_simple)

  rf_opensource_simple[[i]] <- fun_compute_rf(th_trmetrics_entomo_postedecapture, predictors_opensource_simple, cv_col = "by_ptcapt3", mod, featureselect = "rfe", species = response_var,tune_length = 1, type = 'predictive')

}

  names(rf_opensource) <- names(rf_opensource_simple) <- names(rf_tocollect) <- paste0(seq(0,4,1),"_week_before")

  return(list(rf_opensource = rf_opensource, rf_tocollect = rf_tocollect, rf_opensource_simple = rf_opensource_simple))

}


df_input_params <- tibble(response_var = character(), code_pays = character(), mod = character())
df_input_params <- df_input_params %>%
  add_row(response_var = "ma_an", code_pays = "BF", mod = "presence") %>%
  add_row(response_var = "ma_an", code_pays = "BF", mod = "abundance") %>%
  add_row(response_var = "ma_an", code_pays = "CI", mod = "presence") %>%
  add_row(response_var = "ma_an", code_pays = "CI", mod = "abundance") %>%
  add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "presence") %>%
  add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "presence") %>%
  add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "presence") %>%
  add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "abundance") %>%
  add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "abundance") %>%
  add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "abundance") %>%
  add_row(response_var = "ma_gambiae_sl", code_pays = "CI", mod = "presence") %>%
  add_row(response_var = "ma_funestus_ss", code_pays = "CI", mod = "presence") %>%
  add_row(response_var = "ma_gambiae_sl", code_pays = "CI", mod = "abundance") %>%
  add_row(response_var = "ma_funestus_ss", code_pays = "CI", mod = "abundance") 
model_results1 <- df_input_params[1,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
model_results2 <- df_input_params[2,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
model_results3 <- df_input_params[3,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
model_results4 <- df_input_params[4,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
model_results5 <- df_input_params[5,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
model_results6 <- df_input_params[6,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
model_results7 <- df_input_params[7,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
model_results8 <- df_input_params[8,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
model_results9 <- df_input_params[9,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
model_results10 <- df_input_params[10,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
model_results11 <- df_input_params[11,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
model_results12 <- df_input_params[12,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
model_results13 <- df_input_params[13,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
model_results14 <- df_input_params[14,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))

model_results <- rbind(model_results1,model_results2,model_results3,model_results4,model_results5,model_results6,model_results7,model_results8,model_results9,model_results10,model_results11,model_results12,model_results13,model_results14)

model_results <- model_results %>%
  mutate(rf_opensource = map(results, ~pluck(.,"rf_opensource"))) %>%
  mutate(rf_tocollect = map(results, ~pluck(.,"rf_tocollect"))) %>%
  mutate(rf_opensource_simple = map(results, ~pluck(.,"rf_opensource_simple"))) %>%
  dplyr::select(-results)

  saveRDS(model_results,"/home/ptaconet/Bureau/data_analysis/model_results_predictive1.rds")  
  