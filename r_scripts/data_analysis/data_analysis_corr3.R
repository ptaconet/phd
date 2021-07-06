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
library(MuMIn)


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
                               lag_time_window = c(0,42),
                               buffer_sizes = c(250,500,1000,2000)){
  
  cat("Executing workflow for parameters : ", response_var, code_pays, mod,"\n")
  
  ###### load the data
  
  # load spatiotemporal data
  env_spatiotemporal <- load_spatiotemporal_data(vars = c("RFD1F","TMIN1","TMAX1","TAMP1"),
                                                 buffers = c(2000),
                                                 lag_time_window = c(0,42),
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
  
  #landcover_metrics_to_keep <- c("pland","prd","shdi")
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
  
  # load human beahviour use data
  hum_behav <- load_hmnbehav_data(code_pays, entomo_csh_metadata_l1)
  LUS = hum_behav[[1]]
  hum_behav_4_exophagy = hum_behav[[2]]
  hum_behav_4_earlylatebiting = hum_behav[[3]]
  rm(hum_behav)
  
  # load response variable
  if(mod %in% c("presence","abundance","presence_abundance")){
    th_trmetrics_entomo_postedecapture <- dbReadTable(react_gpkg, 'trmetrics_entomo_postedecapture') %>% 
      dplyr::select(-fid) %>% 
      left_join(entomo_csh_metadata_l1 %>% dplyr::select(idpointdecapture, codevillage, pointdecapture, codepays, nummission, period_interv)) %>% 
      filter(!is.na(codevillage)) %>%
      filter(codepays == code_pays) %>%
      
   #   filter(as.numeric(nummission) %in% c(1,5,6)) %>% # dry
    #   filter(as.numeric(nummission) %in% c(2,3,4,7)) %>% # wet
      #  filter(as.numeric(nummission) %in% c(1,2,5,6,7)) %>% # wet
      
      mutate(heuredecapture = NA)
    
    th_trmetrics_entomo_postedecapture$resp_var <- th_trmetrics_entomo_postedecapture[,response_var]
  } else if(mod %in% c("physiological_resistance_kdrw","physiological_resistance_kdre","exophagy","early_late_biting")){
    
    if(response_var == "ma_funestus_ss"){
      response_var <- "An.funestus_ss"
    } else if(response_var == "ma_coluzzi"){
      response_var <- "An.coluzzii"
    } else if(response_var == "ma_gambiae_ss"){
      response_var <- "An.gambiae_ss"
    }
    
    th_trmetrics_entomo_postedecapture <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
      dplyr::select(-fid) %>% 
      filter(codepays == code_pays, pcr_espece == response_var, nummission <= 8) %>%
      mutate(nummission = as.character(nummission))
    
    # add physiological resistance (column PHY)
    th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
      mutate(kdrw = ifelse(kdrw == "RR","1",kdrw)) %>%
      mutate(kdrw = ifelse(kdrw == "RS","0.5",kdrw)) %>%
      mutate(kdrw = ifelse(kdrw == "SS","0",kdrw)) %>%
      mutate(kdre = ifelse(kdre == "RR","1",kdre)) %>%
      mutate(kdre = ifelse(kdre == "RS","0.5",kdre)) %>%
      mutate(kdre = ifelse(kdre == "SS","0",kdre)) %>%
      mutate(PHY_kdrw = as.numeric(kdrw)) %>%
      mutate(PHY_kdre = as.numeric(kdre))
    
    t = entomo_csh_metadata_l1 %>%
      filter(codepays == code_pays) %>%
      mutate(periode = ifelse(period_interv=="pre_intervention","preinterv","postinterv")) %>%
      mutate(date_capture = as.Date(date_capture)) %>%
      mutate(month = lubridate::month(date_capture)) %>%
      mutate(saison = ifelse(month <= 4 | month >=11 , "seche","pluies")) %>%
      dplyr::select(idpointdecapture,codevillage,periode,saison)
    
    HBB <- dbReadTable(react_gpkg, 'entomo_comportementhumain_l0') %>% 
      dplyr::filter(codepays == code_pays) %>%
      mutate(hcoucher = as.numeric(substr(hcoucher,1,2)),hlever=as.numeric(substr(hlever,1,2))) %>%
      mutate(hcoucher = ifelse(hcoucher <=17, 20, hcoucher), hlever=ifelse(hlever>=11 | hlever<=3,6,hlever)) %>%
      dplyr::group_by(codevillage,periode,saison) %>%
      dplyr::summarise(hcoucher=round(mean(hcoucher)),hlever=round(mean(hlever)))
    
    # add early_late biting (column ELB)
    th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
      left_join(t) %>%
      left_join(HBB) %>%
      mutate(ELB = ifelse(heuredecapture >= hlever & heuredecapture <= hcoucher,1,0)) %>%
      dplyr::select(-c("periode","saison","hcoucher","hlever"))
    
    # add exophagy (column EXO)
    th_trmetrics_entomo_postedecapture$EXO <- th_trmetrics_entomo_postedecapture$postedecapture
    
    if(mod=="early_late_biting"){
      
      th_trmetrics_entomo_postedecapture$resp_var <- th_trmetrics_entomo_postedecapture$ELB
      th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
        left_join(hum_behav_4_earlylatebiting)
      
    } else if (mod=="exophagy"){
      
      th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
        mutate(resp_var = ifelse(EXO == "e",1,0)) %>%
        left_join(hum_behav_4_exophagy)
      
    } else if (mod=="physiological_resistance_kdrw"){
      th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
        mutate(resp_var = PHY_kdrw)
    } else if (mod=="physiological_resistance_kdre"){
      th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
        mutate(resp_var = PHY_kdre)
    } else if (mod=="physiological_resistance_ace1"){
      th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
        mutate(resp_var = ifelse(ace1 == "RR",1,0)) %>%
        mutate(resp_var = ifelse(ace1 == "RS",0.5,resp_var))
    }
    
  }
  
  
  if(mod == "abundance" ){
    th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% filter(resp_var > 0 )
  } else if (mod == "presence" ){
    th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% mutate(resp_var = ifelse(resp_var == 0,0,1 ))
  } else if(mod == "presence_abundance"){
  
  }
  
  th_env_static2 <- th_env_static %>% mutate(v = 1) %>% pivot_wider(names_from = VCM,  values_from = v, values_fill = list(v = 0), names_prefix = "VCM_")
  th_env_static <- th_env_static %>% dplyr::select(idpointdecapture,VCM)
  
  mean_date_mission <- entomo_csh_metadata_l1 %>% mutate(date_capture = as.Date(date_capture)) %>% dplyr::group_by(codepays,nummission) %>% dplyr::summarise(mean_date_mission=mean(date_capture)) %>% as_tibble() %>% filter(codepays==code_pays) %>% dplyr::select(-codepays) 
  
  
  func <- function(x){
    ret <- broom.mixed::tidy(x, conf.int = TRUE, exponentiate = ifelse(mod == "abundance",FALSE,TRUE))
    return(ret)
  }
  
  possible_ <- possibly(func, otherwise = NULL)
  
  
  func2 <- function(x){
    df = x$frame
    ret = MuMIn::r.squaredGLMM(x)[1,1]
    return(ret)
  }
  
  possible2_ <- possibly(func2, otherwise = NULL)
  
  ######## join response variable with explanatory variables
  
  th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% 
    left_join(mean_date_mission) %>%
    left_join(mean_coords_points_4326) %>%
    left_join(mean_coords_points_32630) %>%
    mutate(int_ext = substr( idpostedecapture,nchar(idpostedecapture),nchar(idpostedecapture))) %>%
    mutate(IEH = ifelse(int_ext == "i",1,0)) %>%
    #dplyr::select(idpostedecapture,idpointdecapture,int_ext,heuredecapture,IEH,pointdecapture,codevillage,codepays,nummission,mean_date_mission,X_4326,Y_4326,X_32630,Y_32630,resp_var) %>%
    left_join(env_spatiotemporal) %>%
    left_join(env_spatial) %>%
    left_join(th_env_nightcatch_postedecapture) %>%
    left_join(th_env_nightcatch) %>%
    left_join(th_env_static) %>%
    left_join(th_env_static2) %>%
    left_join(env_landcover) %>%
    left_join(LUS) %>%
    mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  
  th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture%>%
    mutate_at(vars(starts_with("WMD")),~./100) %>% 
    mutate_at(vars(starts_with("WAC_")),~./100) %>% 
    mutate_at(vars(starts_with("WLS_")),~./1000) %>%
    mutate_at(vars(starts_with("BDE")),~./100) %>%
    mutate_at(vars(ends_with("_3_12")), ~ifelse(.>0, "presence","absence")) %>% 
    mutate_at(vars(ends_with("_3_5")), ~ifelse(.>0, "presence","absence"))
    
  
  th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture[, colSums(is.na(th_trmetrics_entomo_postedecapture)) != nrow(th_trmetrics_entomo_postedecapture)]
  
  
  spatial_corrs_glmm <- temporal_corrs_glmm <- temporal_corrs_spearman <- spatial_corrs_spearman <- NULL

    ## landcover 
  colnames_tempvar2 <- fun_get_temporal_preds_columns(0,20,c("VNV30","VMV30","WNW30","BRI30"),7,th_trmetrics_entomo_postedecapture, c(100,250,500,1000,2000))
  colnames_tempvar2 <- colnames_tempvar2[which(grepl("_0_1",colnames_tempvar2))]
  
  predictors <- setdiff(c(colnames(env_landcover), colnames(env_spatial), "WMD", "BDE"),"idpointdecapture")

  # spatial_corrs_spearman <- fun_feature_forward_selection(
  #   df = th_trmetrics_entomo_postedecapture, 
  #   stat_method = "spearman", 
  #   spearman_factor = "codevillage",
  #   mod = mod, 
  #   type = "univariate_selection", 
  #   expl_vars_to_test = predictors)
  
  
    spatial_corrs_glmm <- fun_feature_forward_selection(
      df = th_trmetrics_entomo_postedecapture, 
      stat_method = "glmm", 
      mod = mod, 
      type = "univariate_selection", 
      expl_vars_to_test = predictors)

    spatial_corrs_glmm <- spatial_corrs_glmm[!sapply(spatial_corrs_glmm,is.null)]
    aics_spatial <- data.frame(term = names(spatial_corrs_glmm), aic = purrr::map_dbl(spatial_corrs_glmm, ~AIC(.x)), bic = purrr::map_dbl(spatial_corrs_glmm, ~BIC(.x)), rsquared = purrr::map_dbl(spatial_corrs_glmm, possible2_))

    spatial_corrs_glmm <- purrr::map(spatial_corrs_glmm, possible_) %>%
      do.call(rbind.data.frame, .) %>%
      filter(effect == "fixed" & term!="(Intercept)") %>%
      filter(!grepl("VCM|int_ext",term)) %>%
      mutate(term = gsub("presence","",term)) %>%
      left_join(aics_spatial)

   
  ## ccms
  time_vars <- c("RFD1F","TMIN1","TMAX1","TAMP1")
  df_temporal_corr <- data.frame(var = character(), col_to_keep = character(), correlation = numeric())
  ccm_plots_spearman <- list()
  ccm_plots_glmm <- list()
  ccms_spearman <- list()
  ccms_glmm <- list()
  plots_timevar <- list()
  
  for(i in 1:length(time_vars)){
    
    cat("Calculating CCM for variable ",time_vars[i],"\n")
    expl_vars_to_test =  colnames(th_trmetrics_entomo_postedecapture[which(grepl(time_vars[i],colnames(th_trmetrics_entomo_postedecapture)))])

    plot_timevar <- fun_spatiotemporal_plots(th_trmetrics_entomo_postedecapture,mod,time_vars[i],code_pays)
    
    
    # corr_spearman <- fun_feature_forward_selection(
    #   df = th_trmetrics_entomo_postedecapture,
    #   stat_method = "spearman",
    #   spearman_factor = "codevillage",
    #   mod = mod,
    #   type = "univariate_selection",
    #   expl_vars_to_keep = NULL,
    #   expl_vars_to_test = expl_vars_to_test)
    # 
    # corr_spearman$time_lag_1 <- as.numeric(sub('.*\\_', '', corr_spearman$name))
    # corr_spearman$time_lag_2 <- as.numeric(stringr::str_match( corr_spearman$name, '([^_]+)(?:_[^_]+){1}$')[,2])
    # corr_spearman$diff_lag <- corr_spearman$time_lag_1 - corr_spearman$time_lag_2
    # corr_spearman <- arrange(corr_spearman, time_lag_1, time_lag_2)
    # corr_spearman$correlation[which(corr_spearman$pval > 0.05)] <- NA
    # corr_spearman$var <- time_vars[i]
    # 
    # ccms_spearman[[i]] <- corr_spearman

    
      corr_glmm <- fun_feature_forward_selection(
        df = th_trmetrics_entomo_postedecapture,
        stat_method = "glmm",
        mod = mod,
        type = "univariate_selection",
        expl_vars_to_test = expl_vars_to_test)
     
      corr_glmm <- corr_glmm[!sapply(corr_glmm,is.null)]
      aics <- data.frame(name =  names(corr_glmm), aic = purrr::map_dbl(corr_glmm, ~AIC(.x)), bic = purrr::map_dbl(corr_glmm, ~BIC(.x)), rsquared = purrr::map_dbl(corr_glmm, possible2_))
      
       glmms_univs <- purrr::map(corr_glmm, possible_) %>%
         do.call(rbind.data.frame, .) %>%
         filter(effect == "fixed" & term!="(Intercept)") %>%
         filter(!grepl("VCM|int_ext",term)) %>%
         #mutate(estimate = ifelse(p.value <= 0.2,estimate,NA)) %>%
         rename(name = term, correlation = estimate) %>%
         mutate(abs_corr = abs(correlation)) %>%
         left_join(aics)
      
       glmms_univs$name <- gsub("presence","",glmms_univs$name)
      glmms_univs$time_lag_1 <- as.numeric(sub('.*\\_', '', glmms_univs$name))
      glmms_univs$time_lag_2 <- as.numeric(stringr::str_match( glmms_univs$name, '([^_]+)(?:_[^_]+){1}$')[,2])
      glmms_univs$diff_lag <- glmms_univs$time_lag_1 - glmms_univs$time_lag_2
      glmms_univs <- arrange(glmms_univs, time_lag_1, time_lag_2)
      glmms_univs$var <- time_vars[i]
      
      plots_timevar[[i]] <- plot_timevar
      ccms_glmm[[i]] <- glmms_univs
     
    
     
    
  }
  
  return(list(spatial_corrs_spearman = spatial_corrs_spearman, spatial_corrs_glmm = spatial_corrs_glmm, temporal_corrs_spearman = ccms_spearman, temporal_corrs_glmm = ccms_glmm, plots_timevar = plots_timevar))
  
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
  mutate(spatial_corrs_glmm = map(results, ~pluck(.,"spatial_corrs_glmm"))) %>%
  mutate(temporal_corrs_spearman = map(results, ~pluck(.,"temporal_corrs_spearman"))) %>%
  mutate(temporal_corrs_glmm = map(results, ~pluck(.,"temporal_corrs_glmm")))  %>%
  dplyr::select(-results)

saveRDS(model_results,"/home/ptaconet/Bureau/data_analysis/model_results_univanalysis_glmm5.rds")  # 
