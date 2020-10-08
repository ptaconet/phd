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
                               model_type, 
                               intervention, 
                               predictive_type, 
                               timevars_selection = "7day", 
                               lag_time_window = c(0,28),
                               buffer_sizes = c(2000)){
  
  cat("Executing workflow for parameters : ", response_var, code_pays, mod, model_type, intervention, predictive_type,"\n")
  
  ###### load the data
  
  # load spatiotemporal data
  env_spatiotemporal <- load_spatiotemporal_data(vars = c("RFD1F","TMIN1","TMAX1","TAMP1","SMO1"),
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
      landcover_layers_to_keep <- c(2,3,4,5)
    } else if (code_pays == "CI"){
      landcover_layers_to_keep <- c(7,8,9,10)
    }
  }
  
  landcover_metrics_to_keep <- c("pland","prd","shdi")
  
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
  hum_behav = hum_behav[[2]]
    
  # load response variable
  if(mod %in% c("presence","abundance")){
    th_trmetrics_entomo_postedecapture <- dbReadTable(react_gpkg, 'trmetrics_entomo_postedecapture') %>% 
      dplyr::select(-fid) %>% 
      left_join(entomo_csh_metadata_l1 %>% dplyr::select(idpointdecapture, codevillage, pointdecapture, codepays, nummission, period_interv)) %>% 
      filter(!is.na(codevillage)) %>%
      filter(codepays == code_pays) %>%
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
          left_join(hum_behav)
      
    } else if (mod=="exophagy"){
      
      th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
        mutate(resp_var = ifelse(EXO == "e",1,0)) %>%
        left_join(hum_behav)
      
    } else if (mod=="physiological_resistance_kdrw"){
      th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
        mutate(resp_var = ifelse(kdrw == "RR",1,0)) %>%
        mutate(resp_var = ifelse(kdrw == "RS",0.5,resp_var))
    } else if (mod=="physiological_resistance_kdre"){
      th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
        mutate(resp_var = ifelse(kdre == "RR",1,0)) %>%
        mutate(resp_var = ifelse(kdre == "RS",0.5,resp_var))
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
  th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture[, colSums(is.na(th_trmetrics_entomo_postedecapture)) != nrow(th_trmetrics_entomo_postedecapture)]
  
  
  ### selection of predictors
  if(mod %in% c("presence","abundance")){
    
    if(code_pays == "BF"){ lcid <- 3 } else if (code_pays == "CI"){ lcid <- 8 }
    
    predictors <- c(paste0(c("RFD1F","TMIN1","TMAX1","TAMP1","SMO1"),"_2000_0_28"),
                    "BCH_2000","BDE", colnames(th_trmetrics_entomo_postedecapture[which(grepl(paste0("lsm_c_pland_2000_",lcid,"|lsm_l_prd_2000_",lcid,"|lsm_l_shdi_2000_",lcid),colnames(th_trmetrics_entomo_postedecapture)))]),
                    #paste0(c("TEL","TSL","TCI"),"_2000"),
                    "WMD",paste0(c("TWI","WAC","WAD","WLS","WAL","HYS"),"_2000"),
                    "RFH",
                    "POP_2000",
                    "VCM","int_ext"
                    )
    glmm_varstoforce <- c("VCM")
    glmm_varstointegrate <- NULL
    predictors_nightscience <- c("WSP","NMA","NMT","NMH","LMN")
    
    } else if (mod %in% c("early_late_biting","exophagy")){
      
      if(code_pays == "BF"){ lcid <- 2 } else if (code_pays == "CI"){ lcid <- 7 }
      
      predictors <- c("WSP","NMA","NMT","NMH","RFH","VCM","HBB","HBI","LUS")
      glmm_varstointegrate <- c("HBB","HBI","LUS")
      glmm_varstoforce <- c("VCM")
      
        if(mod == "early_late_biting"){
          predictors <- c(predictors,"int_ext")
        }
      
      predictors_nightscience <- c(paste0(c("RFD1F","TMIN1","TMAX1","TAMP1","SMO1"),"_2000_0_28"), colnames(th_trmetrics_entomo_postedecapture[which(grepl(paste0("lsm_c_pland_2000_",lcid,"|lsm_l_prd_2000_",lcid,"|lsm_l_shdi_2000_",lcid),colnames(th_trmetrics_entomo_postedecapture)))]) )
      
      if(response_var != "An.funestus_ss"){
        predictors_nightscience <- c("PHY_kdre","PHY_dkrw",predictors_nightscience)
      }
      
    } else if(mod %in% c("physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1")){
      
      if(code_pays == "BF"){ 
        lcid <- 2 
        lsm_agri <- c("lsm_c_pland_2000_3_1","lsm_c_pland_2000_3_11")
      } else if (code_pays == "CI"){ 
          lcid <- 7 
          lsm_agri <- c("lsm_c_pland_2000_8_4","lsm_c_pland_2000_8_7","lsm_c_pland_2000_8_8")
      }
      
      predictors <- c("VCM","LUS","int_ext",lsm_agri)
      glmm_varstointegrate <- NULL
      glmm_varstoforce <- c("VCM")
      predictors_nightscience <- c("ELB","WSP","NMA","NMT","NMH","LMN","RFH",paste0(c("RFD1F","TMIN1","TMAX1","TAMP1","SMO1"),"_2000_0_28"), colnames(th_trmetrics_entomo_postedecapture[which(grepl(paste0("lsm_c_pland_2000_",lcid,"|lsm_l_prd_2000_",lcid,"|lsm_l_shdi_2000_",lcid),colnames(th_trmetrics_entomo_postedecapture)))]))
    
    }
  
    
  ###### univariate models ######
  # glmms
  df <- th_trmetrics_entomo_postedecapture %>% 
    mutate(pointdecapture2 = as.factor(paste0(codevillage,pointdecapture))) %>%
    dplyr::select(resp_var,codevillage,pointdecapture2,unique(c(predictors,predictors_nightscience))) %>% 
    mutate(resp_var = as.character(resp_var)) %>%
    mutate_if(is.numeric, ~scale(.)) %>%
    mutate(resp_var = as.numeric(resp_var))

  if(mod %in% c("physiological_resistance_kdrw","physiological_resistance_kdre","exophagy","early_late_biting","presence")){
    glmms_univs <- future_map(colnames(df[4:ncol(df)]), ~glmmTMB(as.formula(paste0("resp_var ~ ",.x," + (1|codevillage/pointdecapture2)")), data = df, family = binomial(link = "logit")))
  } else if (mod == "abundance"){
    glmms_univs <- future_map(colnames(df[4:ncol(df)]), ~glmmTMB(as.formula(paste0("resp_var ~ ",.x," + (1|codevillage/pointdecapture2)")), data = df, family = truncated_nbinom2))
  }
  
  glmms_univs <- glmms_univs %>%
    purrr::map(.,~broom.mixed::tidy(., conf.int = TRUE, exponentiate = ifelse(mod == "abundance",FALSE,TRUE))) %>%
    do.call(rbind.data.frame, .) %>%
    filter(effect == "fixed" & term!="(Intercept)")
  
  # spearman correlations
  predictors_numeric <- th_trmetrics_entomo_postedecapture %>%
    dplyr::select(unique(c(predictors,predictors_nightscience))) %>%
    dplyr::select_if(is.numeric)
  predictors_numeric <- colnames(predictors_numeric)
  predictors_character <- th_trmetrics_entomo_postedecapture %>%
    dplyr::select(unique(c(predictors,predictors_nightscience))) %>%
    dplyr::select_if(is.character)
  predictors_character <- colnames(predictors_character)
  
  if("VCM" %in% predictors_character){
    predictors_numeric <- c(predictors_numeric, colnames(th_trmetrics_entomo_postedecapture[which(grepl("VCM_",colnames(th_trmetrics_entomo_postedecapture)))]))
  }
  if("int_ext" %in% predictors_character){
    predictors_numeric <- c(predictors_numeric, "IEH")
  }
  
  spearmancorr_univ <- fun_feature_forward_selection(
    df = th_trmetrics_entomo_postedecapture, 
    stat_method = "spearman", 
    mod = mod, 
    type = "univariate_selection", 
    expl_vars_to_test = predictors_numeric)
  
  
    ###### GLMM multivariate model  ######

  #### based on glmm pval univariate analysis
  # get numerical predictors
     predictors_numeric <- th_trmetrics_entomo_postedecapture %>%
       dplyr::select(predictors) %>%
       dplyr::select_if(is.numeric)
     predictors_numeric <- colnames(predictors_numeric)
     predictors_character <- th_trmetrics_entomo_postedecapture %>%
       dplyr::select(predictors) %>%
       dplyr::select_if(is.character)
     predictors_character <- colnames(predictors_character)
     
  ### filter predictors with spearman correlation coefficient
     corrs <- fun_feature_forward_selection(
      df = th_trmetrics_entomo_postedecapture, 
      stat_method = "spearman", 
      mod = mod, 
      type = "univariate_selection", 
      expl_vars_to_test = predictors_numeric)
    
    corrs_filt <- corrs %>% filter(pval <= 0.2, abs_corr >= 0.2)
    
    vars_multiv <- fun_multicol(corrs_filt$name)
    glmm_varstointegrate <- fun_multicollinearity(th_trmetrics_entomo_postedecapture, glmm_varstointegrate)
    vars_multiv_spearmanfilt <- unique(c(vars_multiv,predictors_character,glmm_varstointegrate))
    
    ## run GLMM with spearman filter (forcing introduction of some variables)
    glmm_mod_spearmanfilt <- fun_compute_glmm(th_trmetrics_entomo_postedecapture, vars_multiv_spearmanfilt, predictors_forced = glmm_varstoforce, mod)
    
    #### based on glmm pval univariate analysis
    pvals_filts <- glmms_univs %>% filter(p.value <= 0.2, !grepl("VCM",term), !grepl("int_ext",term))
    
  # multicollinearity among predictors
    vars_multiv <- fun_multicol(pvals_filts$term)
    glmm_varstointegrate <- fun_multicollinearity(th_trmetrics_entomo_postedecapture, glmm_varstointegrate)
    vars_multiv_pvalsfilt <- unique(c(vars_multiv,predictors_character,glmm_varstointegrate))

    glmm_mod_pvalfilt <- fun_compute_glmm(th_trmetrics_entomo_postedecapture, vars_multiv_pvalsfilt, predictors_forced = glmm_varstoforce, mod)
    
    
    ###### RF multivariate model  ######
    
    fun_rf <- function(predictors, predictors_nightscience = NULL, cv = NULL){
      
      fun_multicol <- function(preds){
        
        predictors_numeric <- th_trmetrics_entomo_postedecapture %>%
          dplyr::select(preds) %>%
          dplyr::select_if(is.numeric)
        predictors_numeric <- colnames(predictors_numeric)
        predictors_character <- th_trmetrics_entomo_postedecapture %>%
          dplyr::select(preds) %>%
          dplyr::select_if(is.character)
        predictors_character <- colnames(predictors_numeric)
        
              ## multicollinearity among predictors
      lsm_vars <- predictors_numeric[grepl("lsm", predictors_numeric)]
      other_sp_vars_num <- predictors_numeric[!grepl("lsm", predictors_numeric)]
      if(length(lsm_vars) > 0){
        lsm_vars <- fun_multicollinearity_lsm(th_trmetrics_entomo_postedecapture, lsm_vars)
      } else {lsm_vars = NULL}
      vars_multiv_num <- c(other_sp_vars_num, lsm_vars)
      vars_multiv_num <- fun_multicollinearity(th_trmetrics_entomo_postedecapture, vars_multiv_num)
      vars_multiv <- c(vars_multiv_num, predictors_character)

        return(vars_multiv)
        
      }
      
      predictors <- fun_multicol(predictors)
      
      if(!is.null(predictors_nightscience)){
        predictors_nightscience <- fun_multicol(predictors_nightscience)
        
        var_to_remove <- NULL
        vars_to_test <- c(predictors, predictors_nightscience)
        vars_to_keep <- vars_to_test
        p <- NULL
        m <- th_trmetrics_entomo_postedecapture[,vars_to_test] %>%
          cor(.,method = "pearson", use = "na.or.complete")
        index <- which(abs(m) > .7 & abs(m) < 1,arr.ind = T) 
        p <- cbind.data.frame(stock1 = rownames(m)[index[,1]], stock2 = colnames(m)[index[,2]])
        if(nrow(p)>0){
          for(i in 1:nrow(p)){
            if(p$stock1[i] %in% predictors){
              var_to_remove <- c(var_to_remove,as.character(p$stock2[i]))
            } else if (p$stock1[i] %in% predictors_nightscience) {
              var_to_remove <- c(var_to_remove,as.character(p$stock1[i]))
            }
            p$var_to_remove[i] <- var_to_remove
          }
          
          var_to_remove <- unique(var_to_remove)
          vars_to_keep <- setdiff(vars_to_test, var_to_remove)
        }
        predictors <- vars_to_keep
        
      }

      rf_mod <- fun_compute_rf(th_trmetrics_entomo_postedecapture, predictors, cv_type = cv, mod)
      
      return(rf_mod)
    }
    
    
    if("VCM" %in% predictors){
      predictors <- setdiff(predictors,"VCM")
      predictors <- c(predictors, colnames(th_trmetrics_entomo_postedecapture[which(grepl("VCM_",colnames(th_trmetrics_entomo_postedecapture)))]))
    }
    if("int_ext" %in% predictors){
      predictors <- setdiff(predictors,"int_ext")
      predictors <- c(predictors, "IEH")
    }
    
    ## RF day science
    rf_dayscience <- fun_rf(predictors, NULL, "random")
    
    ## RF night science
    rf_nightscience <- fun_rf(predictors, predictors_nightscience, "random")
    
    
    # importance
    # randomForest::varImpPlot(rf_dayscience$mod$fit, type = 1, scale = F)
    # mod <- Predictor$new(rf_dayscience$mod$fit, data = rf_dayscience$df_mod)
    # ia <- Interaction$new(mod, feature = "lsm_c_pland_2000_3_12")
    # partial(rf_dayscience$mod, pred.var = c("lsm_c_pland_2000_3_12", "RFD1F_2000_0_28"), plot = TRUE, 
    #        chull = TRUE, train = rf_dayscience$df_mod, prob = TRUE, plot.engine = "ggplot2",
    #        palette = "magma")
    
    return(list(univ_glmm = glmms_univ, 
                univ_spearmancorr = spearmancorr_univ, 
                multiv_glmm_spearmanfilt = glmm_mod_spearmanfilt, 
                multiv_glmm_pvalfilt = glmm_mod_pvalfilt, 
                multiv_rf_dayscience = rf_dayscience, 
                multiv_rf_nightscience = rf_nightscience))
    
  }

  
  
  
  
  
  