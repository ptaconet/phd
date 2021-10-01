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
                               spatial_granularity = "idpointdecapture",
                               mod_train=res){
  
  
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
  
  cols_miss_vals <- names(which(sapply(th_trmetrics_entomo_postedecapture, anyNA)))
  th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
    mutate_at(cols_miss_vals,funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  
  cols_miss_vals <- names(which(sapply(th_trmetrics_entomo_postedecapture, anyNaN)))
  th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
    mutate_at(cols_miss_vals,funs(ifelse(is.nan(.), mean(., na.rm = TRUE), .)))
  
  if(response_var=="ma_gambiae_sl"){
    response_var <- "ma_gambiae_ss"
  } else if(response_var=="ma_gambiae_ss"){
    response_var <- "ma_gambiae_sl"
  }
  
  mod_train <- mod_train %>% filter(response_var==!!response_var,code_pays!=!!code_pays,mod==!!mod)
  
  rf_opensource <- list()
  rf_opensource_simple <- list()
  
  if(mod=="abundance"){
    th_trmetrics_entomo_postedecapture$resp_var <- log(th_trmetrics_entomo_postedecapture$resp_var)+1
  }
  
  for(i in 1:5){
   
    if(mod=="presence"){
      th_trmetrics_entomo_postedecapture$pred_opensource <- predict(mod_train$rf_opensource[[1]][[i]]$mod,th_trmetrics_entomo_postedecapture, type = "prob")[,"Presence"]
      th_trmetrics_entomo_postedecapture$pred_opensource_simple <- predict(mod_train$rf_opensource_simple[[1]][[i]]$mod,th_trmetrics_entomo_postedecapture, type = "prob")[,"Presence"]
    } else if(mod=="abundance"){
      th_trmetrics_entomo_postedecapture$pred_opensource <- predict(mod_train$rf_opensource[[1]][[i]]$mod,th_trmetrics_entomo_postedecapture)
      th_trmetrics_entomo_postedecapture$pred_opensource_simple <- predict(mod_train$rf_opensource_simple[[1]][[i]]$mod,th_trmetrics_entomo_postedecapture)
      }
    
    rf_opensource[[i]] <- th_trmetrics_entomo_postedecapture %>% 
      dplyr::select( codevillage, pointdecapture, nummission, resp_var,pred_opensource) %>%
      rename( pred=pred_opensource, obs = resp_var)
    
    rf_opensource_simple[[i]] <- th_trmetrics_entomo_postedecapture %>% 
      dplyr::select( codevillage, pointdecapture, nummission, resp_var,pred_opensource_simple) %>%
      rename( pred=pred_opensource_simple, obs = resp_var)
    
    rf_opensource[[i]] <- list(rf_opensource[[i]])
    names(rf_opensource[[i]]) <- "df_cv"
    
    rf_opensource_simple[[i]] <- list(rf_opensource_simple[[i]])
    names(rf_opensource_simple[[i]]) <- "df_cv"
    
  }
  
  names(rf_opensource) <- names(rf_opensource_simple) <- paste0(seq(0,4,1),"_week_before")
  
  return(list(rf_opensource = rf_opensource, rf_opensource_simple = rf_opensource_simple))
  
}

res <-  readRDS("/home/ptaconet/Bureau/data_analysis/model_results_predictive2.rds")
  
df_input_params <- tibble(response_var = character(), code_pays = character(), mod = character())
df_input_params <- df_input_params %>%
  add_row(response_var = "ma_an", code_pays = "BF", mod = "presence") %>%
  add_row(response_var = "ma_an", code_pays = "BF", mod = "abundance") %>%
  add_row(response_var = "ma_an", code_pays = "CI", mod = "presence") %>%
  add_row(response_var = "ma_an", code_pays = "CI", mod = "abundance") %>%
  add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "presence") %>%
  add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "presence") %>%
  add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "abundance") %>%
  add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "abundance") %>%
  add_row(response_var = "ma_gambiae_sl", code_pays = "CI", mod = "presence") %>%
  add_row(response_var = "ma_funestus_ss", code_pays = "CI", mod = "presence") %>%
  add_row(response_var = "ma_gambiae_sl", code_pays = "CI", mod = "abundance") %>%
  add_row(response_var = "ma_funestus_ss", code_pays = "CI", mod = "abundance") 


amodel_results1 <- df_input_params[1,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
 amodel_results2 <- df_input_params[2,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
amodel_results3 <- df_input_params[3,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
amodel_results4 <- df_input_params[4,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
amodel_results5 <- df_input_params[5,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
amodel_results6 <- df_input_params[6,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
amodel_results7 <- df_input_params[7,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
amodel_results8 <- df_input_params[8,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
amodel_results9 <- df_input_params[9,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
amodel_results10 <- df_input_params[10,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
amodel_results11 <- df_input_params[11,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))
amodel_results12 <- df_input_params[12,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))

amodel_results <- rbind(amodel_results1,amodel_results2,amodel_results3,amodel_results4,amodel_results5,amodel_results6,amodel_results7,amodel_results8,amodel_results9,amodel_results10,amodel_results11,amodel_results12)

amodel_results <- amodel_results %>%
  mutate(rf_opensource = map(results, ~pluck(.,"rf_opensource"))) %>%
  mutate(rf_opensource_simple = map(results, ~pluck(.,"rf_opensource_simple"))) %>%
  dplyr::select(-results)

saveRDS(amodel_results,"/home/ptaconet/Bureau/data_analysis/model_results_predictive_switch_areas2.rds")  
