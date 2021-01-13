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
entomo_csh_metadata_l1 <- DBI::dbReadTable(react_gpkg, 'entomo_csh_metadata_l1') %>% dplyr::select(-geom) %>% dplyr::filter(!(nummission %in% c("11","12","13","15")))

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
                               lag_time_window = c(0,40),
                               buffer_sizes = c(250,500,1000,2000)){
  
  cat("Executing workflow for parameters : ", response_var, code_pays, mod,"\n")
  
  ###### load the data
  
  # load spatiotemporal data
  env_spatiotemporal <- load_spatiotemporal_data(vars = c("RFD1F","TMIN1","TMAX1"),
                                                 buffers = c(2000),
                                                 lag_time_window = c(0,40),
                                                 summarize_days_to_week = FALSE,
                                                 code_pays = code_pays,
                                                 entomo_csh_metadata_l1 = entomo_csh_metadata_l1)
  
  # load spatial data 
  # if(predictive_type == "notroi"){
  #   landcover_layers_to_keep <- c(11,12)
  # } else {
  if(code_pays == "BF"){
    landcover_layers_to_keep <- c(3)
  } else if (code_pays == "CI"){
    landcover_layers_to_keep <- c(8)
  }
  #}
  
  landcover_metrics_to_keep <- c("pland")
  
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
    left_join(entomo_csh_metadata_l1 %>% dplyr::select(idpointdecapture, codevillage, pointdecapture, codepays, nummission, period_interv, season)) %>% 
    filter(!is.na(codevillage)) %>%
    filter(codepays == code_pays)
  
  th_trmetrics_entomo_postedecapture$resp_var <- th_trmetrics_entomo_postedecapture[,response_var]
  
  
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
    mutate(int_ext = substr( idpostedecapture,nchar(idpostedecapture),nchar(idpostedecapture))) %>%
    mutate(IEH = ifelse(int_ext == "i",1,0)) %>%
    mutate(int_ext = ifelse(int_ext == "i","int.","ext.")) %>%
    #dplyr::select(idpostedecapture,idpointdecapture,int_ext,heuredecapture,IEH,pointdecapture,codevillage,codepays,nummission,mean_date_mission,X_4326,Y_4326,X_32630,Y_32630,resp_var) %>%
    left_join(env_spatiotemporal) %>%
    left_join(env_spatial) %>%
    left_join(th_env_nightcatch_postedecapture) %>%
    left_join(th_env_nightcatch) %>%
    left_join(th_env_static) %>%
    left_join(th_env_static2) %>%
    left_join(env_landcover) %>%
    mutate(VCM = case_when(VCM == "Ctrle" ~ "LLIN",
                           VCM == "IEC" ~ "LLIN + IEC",
                           VCM == "IRS" ~ "LLIN + IRS",
                           VCM == "IVM" ~ "LLIN + IVM")) %>%
    mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture[, colSums(is.na(th_trmetrics_entomo_postedecapture)) != nrow(th_trmetrics_entomo_postedecapture)]
  
  
  
  ## landcover 
  predictors <- setdiff(c(colnames(env_landcover), colnames(env_spatial), "WMD"),"idpointdecapture")
  predictors <- predictors[!grepl('TSL|TEL|TAS|TCI|TWI|WAL|HYS|LIG30|POH', predictors)]

  # spatial_corrs_spearman_withrandomeff <- fun_feature_forward_selection(
  #   df = th_trmetrics_entomo_postedecapture, 
  #   stat_method = "spearman", 
  #   spearman_factor = "codevillage",
  #   mod = mod, 
  #   type = "univariate_selection", 
  #   expl_vars_to_test = predictors)

  
  spatial_corrs_spearman <- fun_feature_forward_selection(
    df = th_trmetrics_entomo_postedecapture, 
    stat_method = "spearman", 
    spearman_factor = NULL,
    mod = ifelse(mod=="presence","presence","abundance"), 
    type = "univariate_selection", 
    expl_vars_to_keep = NULL,
    expl_vars_to_test = predictors)

  ## ccms
  time_vars <- c("RFD1F","TMIN1","TMAX1")
  df_temporal_corr <- data.frame(var = character(), col_to_keep = character(), correlation = numeric())
  ccm_plots_spearman <- list()
  ccm_plots_glmm <- list()
  ccms_spearman <- list()
  ccms_glmm <- list()
  
  for(i in 1:length(time_vars)){

    cat("Calculating CCM for variable ",time_vars[i],"\n")
    expl_vars_to_test =  colnames(th_trmetrics_entomo_postedecapture[which(grepl(time_vars[i],colnames(th_trmetrics_entomo_postedecapture)))])

    corr_spearman <- fun_feature_forward_selection(
      df = th_trmetrics_entomo_postedecapture,
      stat_method = "spearman",
      spearman_factor = "codevillage",
      mod = ifelse(mod=="presence","presence","abundance"),
      type = "univariate_selection",
      expl_vars_to_keep = NULL,
      expl_vars_to_test = expl_vars_to_test)

    corr_spearman$time_lag_1 <- as.numeric(sub('.*\\_', '', corr_spearman$name))
    corr_spearman$time_lag_2 <- as.numeric(stringr::str_match( corr_spearman$name, '([^_]+)(?:_[^_]+){1}$')[,2])
    corr_spearman$diff_lag <- corr_spearman$time_lag_1 - corr_spearman$time_lag_2
    corr_spearman <- arrange(corr_spearman, time_lag_1, time_lag_2)
    corr_spearman$var <- time_vars[i]

    ccms_spearman[[i]] <- corr_spearman
  }
  
  
  # RF
  
  if (mod == "abundance_discrete"){
    th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%  mutate(resp_var2 = case_when(resp_var == 1 ~ "low (1 bite)",
                                                                                                                                        resp_var > 1 & resp_var <= 5 ~ "medium (between 1 and 5 bites)",
                                                                                                                                        resp_var > 5 ~ "high (> 5 bites)")) %>%
      dplyr::select(-resp_var) %>%
      dplyr::rename(resp_var = resp_var2)
    
  }
  ######## feature forward selection of temporal variables
  cols_to_keep_timevar <- c("RFD1F_2000_14_21","TMIN1_2000_0_14","TMAX1_2000_0_14")
  
  predictors <- c("lsm_c_pland_2000_3_12","lsm_c_pland_2000_3_2","lsm_c_pland_2000_3_4","lsm_c_pland_2000_3_9","lsm_c_pland_2000_3_1",
                    "WMD","WLS_2000",
                    cols_to_keep_timevar,"VCM","int_ext")
  

  # spatial_vars <- fun_feature_forward_selection(
  #   df = th_trmetrics_entomo_postedecapture,
  #   stat_method = "rf",
  #   mod = mod,
  #   type = "model_comparison",
  #   expl_vars_to_keep = cols_to_keep_timevar,
  #   expl_vars_to_test = spatial_vars,
  #   cross_validation_type = "temporal",
  #   tune_length = 3)
  # 
  # spatial_vars <- spatial_vars %>%
  #   filter(diff_res_w_basemod > 0) %>%
  #   arrange(desc(diff_res_w_basemod)) %>%
  #   top_n(10)
  
  # spatial_vars <- as.character(spatial_vars$name)
  

  predictors <- fun_multicol(th_trmetrics_entomo_postedecapture, predictors)
  
  #rf_llo <- fun_compute_rf(th_trmetrics_entomo_postedecapture, predictors, cv_type = "llo", mod, featureselect = FALSE)
  rf_lto <- fun_compute_rf(th_trmetrics_entomo_postedecapture, predictors, cv_type = "lto", mod, featureselect = FALSE)
  #rf_lto_withfeatureselect <- fun_compute_rf(th_trmetrics_entomo_postedecapture, predictors, cv_type = "lto", mod, featureselect = TRUE)
  #rf_llto <- fun_compute_rf(th_trmetrics_entomo_postedecapture, predictors, cv_type = "llto", mod)

  return(list(spatial_corrs_spearman = spatial_corrs_spearman, temporal_corrs_spearman = ccms_spearman, rf_lto = rf_lto))
  
}


df_input_params_glmm <- tibble(response_var = character(), code_pays = character(), mod = character())
df_input_params_glmm <- df_input_params_glmm %>%
  add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "presence") %>%
  add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "presence") %>%
  add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "presence") %>%
  add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "abundance") %>%
  add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "abundance") %>%
  add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "abundance")
  
model_results1 <- df_input_params_glmm[1,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
model_results2 <- df_input_params_glmm[2,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
model_results3 <- df_input_params_glmm[3,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
model_results4 <- df_input_params_glmm[4,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
model_results5 <- df_input_params_glmm[5,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
model_results6 <- df_input_params_glmm[6,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))

model_results <- rbind(model_results1,model_results2,model_results3,model_results4,model_results5,model_results6)

model_results <- model_results %>%
  mutate(spatial_corrs_spearman = map(results, ~pluck(.,"spatial_corrs_spearman"))) %>%
  mutate(temporal_corrs_spearman = map(results, ~pluck(.,"temporal_corrs_spearman"))) %>%
  mutate(rf_lto = map(results, ~pluck(.,"rf_lto"))) %>%
  #mutate(rf_llo = map(results, ~pluck(.,"rf_llo"))) %>%
  dplyr::select(-results)

saveRDS(model_results,"/home/ptaconet/Bureau/data_analysis/model_results_univanalysis4.rds")
