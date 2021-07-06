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
                                 purpose,
                                 lag_time_window = c(0,42),
                                 buffer_sizes = c(2000)){
    
    cat("Executing workflow for parameters : ", response_var, code_pays, mod, purpose, "\n")
    
    ###### load the data
    
    # load spatiotemporal data
    env_spatiotemporal <- load_spatiotemporal_data(vars = c("RFD1F","TMIN1","TMAX1","TAMP1","VNV8","EVT8"),
                                                   buffers = c(2000),
                                                   lag_time_window = lag_time_window,
                                                   summarize_days_to_week = FALSE,
                                                   code_pays = code_pays,
                                                   entomo_csh_metadata_l1 = entomo_csh_metadata_l1)
  
    env_spatiotemporal2 <- load_spatiotemporal_data(vars = c("WVV10","WVH10","WNW30"),
                                                   buffers = buffer_sizes,
                                                   lag_time_window = lag_time_window,
                                                   summarize_days_to_week = FALSE,
                                                   code_pays = code_pays,
                                                   entomo_csh_metadata_l1 = entomo_csh_metadata_l1)
    
    # load spatial data 
    # if(predictive_type == "notroi"){
    #   landcover_layers_to_keep <- c(11,12)
    # } else {
      if(code_pays == "BF"){
        #landcover_layers_to_keep <- c(2,3,4,5,11,12)
        landcover_layers_to_keep <- c(2,3)
      } else if (code_pays == "CI"){
        #landcover_layers_to_keep <- c(7,8,9,10,11,12)
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
    
    # load human beahviour use data
    hum_behav <- load_hmnbehav_data(code_pays, entomo_csh_metadata_l1)
    LUS = hum_behav[[1]]
    hum_behav_4_exophagy = hum_behav[[2]]
    hum_behav_4_earlylatebiting = hum_behav[[3]]
    rm(hum_behav)
    
    # load response variable
    if(mod %in% c("presence","abundance","abundance_discrete")){
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
    } else if (mod == "abundance_discrete"){
      
      if(response_var == "ma_funestus_ss"){
        upperbound_medium <- 4
      } else if(response_var == "ma_coluzzi"){
        upperbound_medium <- 4
      } else if(response_var == "ma_gambiae_ss"){
        upperbound_medium <- 4
      }

      th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% filter(resp_var > 0 ) %>% mutate(resp_var2 = case_when(resp_var == 1 ~ "low (1 bite)",
                                                                                                                                          resp_var > 1 & resp_var <= upperbound_medium ~ paste0("medium (between 1 and ",upperbound_medium," bites)"),
                                                                                                                                          resp_var > upperbound_medium ~ paste0("high (> ",upperbound_medium," bites)"))) %>%
        dplyr::select(-resp_var) %>%
        dplyr::rename(resp_var = resp_var2)

    }
    
    # if(response_var == "ma_funestus_ss"){
    #   upperbound_medium <- 4
    #   upperbound_high <- 6 
    # } else if(response_var == "ma_coluzzi"){
    #   upperbound_medium <- 3
    #   upperbound_high <- 6 
    # } else if(response_var == "ma_gambiae_ss"){
    #   upperbound_medium <- 2
    #   upperbound_high <- 4
    # }
    # 
    # th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% filter(resp_var > 0 ) %>% mutate(resp_var2 = case_when(resp_var == 1 ~ "low (1 bite)",
    #                                                                                                                                     resp_var > 1 & resp_var <= upperbound_medium ~ paste0("medium (between 1 and ",upperbound_medium," bites)"),
    #                                                                                                                                     resp_var > upperbound_medium & resp_var <= upperbound_high ~ paste0("high (between ",upperbound_medium," and ",upperbound_high," bites)"),
    #                                                                                                                                     resp_var > upperbound_high ~ paste0("very high (> ",upperbound_high," bites)"))) %>%
    #   
    #   dplyr::select(-resp_var) %>%
    #   dplyr::rename(resp_var = resp_var2)
    # 
    

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
      left_join(env_spatiotemporal2) %>%
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
    if(mod %in% c("presence","abundance","abundance_discrete")){
      
      if(code_pays =="BF"){
        th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
          mutate(still_water_presence = ifelse(lsm_c_pland_2000_3_12>0,"presence","absence")) #%>%
          #mutate(still_water_presence = ifelse(still_water_presence == "presence" & grepl("BOH|DIA",idpointdecapture),"dam",still_water_presence)) %>%
          #mutate(still_water_presence = ifelse(still_water_presence == "presence" & !(grepl("BOH|DIA",idpointdecapture)),"small_reservoir",still_water_presence)) %>%
          #mutate(running_water_presence = ifelse(lsm_c_pland_2000_3_5>0,"presence","absence"))
      }
      
      
      if(code_pays == "BF"){ 
        lcid <- 3 
        spatial_vars_to_rm <- c("lsm_c_pland_2000_3_10","lsm_c_pland_2000_3_8","lsm_c_pland_2000_3_5","lsm_c_pland_2000_3_6","lsm_c_pland_2000_3_7","lsm_c_pland_2000_3_11",
                                "lsm_c_pland_500_3_10","lsm_c_pland_500_3_8","lsm_c_pland_500_3_5","lsm_c_pland_500_3_6","lsm_c_pland_500_3_7","lsm_c_pland_500_3_11","POP_500","lsm_c_pland_500_3_1","lsm_c_pland_500_3_3")
      } else if (code_pays == "CI"){ 
          lcid <- 8 
        }
      
      if(purpose == "explicative"){
                time_vars <- c("RFD1F","TMAX1")
  
         # spatial_vars <- c("BCH_2000","BDE", 
         #                     colnames(th_trmetrics_entomo_postedecapture[which(grepl(paste0("lsm_c_pland_2000_",lcid),colnames(th_trmetrics_entomo_postedecapture)))]),
         #                     "still_water_presence","running_water_presence","WMD",paste0(c("WAC","WAD","WLS","HYS","POP"),"_2000"),
         #                     "RFH","NMT","NMH","LMN")
         
          spatial_vars <- c(
                  colnames(th_trmetrics_entomo_postedecapture[which(grepl("lsm_c_pland_2000_3_",colnames(th_trmetrics_entomo_postedecapture)))]),
                  
                  colnames(th_trmetrics_entomo_postedecapture[which(grepl("lsm_c_pland_500_3_",colnames(th_trmetrics_entomo_postedecapture)))]),
                  
                  colnames(th_trmetrics_entomo_postedecapture[which(grepl("WAC|WLS|POP|BCH",colnames(th_trmetrics_entomo_postedecapture)))]),
                  "WMD", "still_water_presence"#,
                  #"RFH","NMT","LMN"
                  )
                
         
         spatial_vars <- setdiff(spatial_vars, spatial_vars_to_rm)
      
      } else if (purpose == "predictive"){
        
        time_vars <- c("RFD1F","TMIN1","TMAX1","TAMP1","SMO1","VNV8","EVT8","WVV10","WVH10","WNW30")
        
        # lcid <- 11
        
         lsm_colnames <- colnames(th_trmetrics_entomo_postedecapture[which(grepl("lsm",colnames(th_trmetrics_entomo_postedecapture)))])
         lsm_colnames <- lsm_colnames[which(grepl(paste0("_",lcid,"_"),lsm_colnames))]
         
         spatial_vars <- c(lsm_colnames,colnames(th_trmetrics_entomo_postedecapture[which(grepl("WMD|TWI|WAC|WAD|WLS|WAL|POH|LIG|LMN",colnames(th_trmetrics_entomo_postedecapture)))]))
         spatial_vars <- setdiff(spatial_vars,c("lsm_c_pland_2000_3_10","lsm_c_pland_2000_3_11"))
         
      }
  
  
      } else if (mod %in% c("early_late_biting","exophagy")){
        
        if(code_pays == "BF"){ lcid <- 2 } else if (code_pays == "CI"){ lcid <- 7 }
        
        predictors_with_interactions <- c("VCM","HBB","HBI","LUS")
        
        predictors_without_interactions <- c("WSP","NMA","NMT","NMH","RFH",
                        paste0(c("RFD1F","TMIN1","TMAX1"),"_2000_0_42"), 
                        colnames(th_trmetrics_entomo_postedecapture[which(grepl(paste0("lsm_c_pland_2000_",lcid,"|lsm_l_shdi_2000_",lcid),colnames(th_trmetrics_entomo_postedecapture)))])
        )
  
          if(mod == "early_late_biting"){
            predictors_without_interactions <- c(predictors_without_interactions,"int_ext")
          }
        
        if(response_var != "An.funestus_ss"){
          predictors_without_interactions <- c("PHY_kdre","PHY_kdrw",predictors_without_interactions)
        }
        
      } else if(mod %in% c("physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1")){
        
        if(code_pays == "BF"){ 
          lcid <- 2 
          lsm_agri <- c("lsm_c_pland_2000_3_1","lsm_c_pland_2000_3_11")
        } else if (code_pays == "CI"){ 
            lcid <- 7 
            lsm_agri <- c("lsm_c_pland_2000_8_4","lsm_c_pland_2000_8_7","lsm_c_pland_2000_8_8")
        }
        
        predictors_with_interactions <- c("VCM",lsm_agri)
        
        
        predictors_without_interactions <- c("int_ext",
                        "ELB","WSP","NMA","NMT","NMH","NML","LMN","RFH",
                        paste0(c("RFD1F","TMIN1","TMAX1"),"_2000_0_42"), 
                        colnames(th_trmetrics_entomo_postedecapture[which(grepl(paste0("lsm_c_pland_2000_",lcid,"|lsm_l_shdi_2000_",lcid),colnames(th_trmetrics_entomo_postedecapture)))])
        )
        
      }
  
    
    ######## ######## ########  
    ######## glmm
    ######## ######## ########
    
    ######## feature forward selection of temporal variables
    
    if(mod != "abundance_discrete"){
    
    colnames_tempvar <- fun_get_temporal_preds_columns(0,42,time_vars,7,th_trmetrics_entomo_postedecapture)
    
    df_temporal <- NULL
    df_temporal_corr <- data.frame(var = character(), col_to_keep = character(), correlation = numeric())
    
    ### select the first temporal variable
    for(i in 1:length(time_vars)){
      
      cat("Calculating CCM for variable ",time_vars[i],"\n")
  
      expl_vars_to_test <- intersect(colnames_tempvar, colnames(th_trmetrics_entomo_postedecapture[which(grepl(time_vars[i],colnames(th_trmetrics_entomo_postedecapture)))]))
  
      corr <- fun_feature_forward_selection(
        df = th_trmetrics_entomo_postedecapture,
        stat_method = "spearman",
        spearman_factor = "codevillage",
        mod = mod,
        type = "univariate_selection",
        expl_vars_to_keep = NULL,
        expl_vars_to_test = expl_vars_to_test)
      
      corr <- corr %>% filter(pval <= 0.2)
      
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
    
    cols_to_keep_timevar <- as.character(df_temporal_corr$col_to_keep[which.max(abs(df_temporal_corr$correlation))])
  
    ### forward selection to select the next 2 temporal variables
    # 2nd time var
    
    #time_vars_it2 <- setdiff(time_vars, word(gsub("_"," ", cols_to_keep_timevar,1)))
    #ffs_temp_it2 <- fun_ffs_tempvar(df = th_trmetrics_entomo_postedecapture, model_type = "glmm", mod, time_vars, cols_to_keep_timevar, colnames_tempvar = colnames_tempvar)
    
    time_vars_it2 <- setdiff(time_vars, word(gsub("_"," ", cols_to_keep_timevar,1)))
    ffs_temp_it2 <- fun_ffs_tempvar(df = th_trmetrics_entomo_postedecapture, model_type = "glmm", mod, time_vars_it2, cols_to_keep_timevar, colnames_tempvar = colnames_tempvar)
    
    
    cols_to_keep_timevar <- c(cols_to_keep_timevar, as.character(ffs_temp_it2$name[which.min(ffs_temp_it2$res)]))
  
    # # 3d time var
    # time_vars_it3 <- setdiff(time_vars, word(gsub("_"," ", cols_to_keep_timevar,1)))
    #  ffs_temp_it3 <- fun_ffs_tempvar(df = th_trmetrics_entomo_postedecapture, model_type = "glmm", mod, time_vars, cols_to_keep_timevar, colnames_tempvar = colnames_tempvar)
    #  cols_to_keep_timevar <- c(cols_to_keep_timevar, as.character(ffs_temp_it3$name[which.min(ffs_temp_it3$res)]))
    # 
    #  
    spatial_vars2 <- setdiff(spatial_vars,c("lsm_c_pland_2000_3_12"))
    predictors <- c(spatial_vars2, cols_to_keep_timevar)
    #predictors <- c(spatial_vars, cols_to_keep_timevar)
    
    ###### univariate models ######
    
    ###### univariate models ######
    # glmms
    df <- th_trmetrics_entomo_postedecapture %>% 
      mutate(pointdecapture2 = as.factor(paste0(codevillage,pointdecapture))) %>%
      dplyr::select(resp_var,codevillage,pointdecapture2, predictors) %>% 
      mutate(resp_var = as.character(resp_var)) %>%
      mutate_if(is.numeric, ~scale(.)) %>%
      mutate(resp_var = as.numeric(resp_var))
    
    if(mod %in% c("physiological_resistance_kdrw","physiological_resistance_kdre","exophagy","early_late_biting","presence")){
      #glmms_univs <- map(colnames(df[4:ncol(df)]), ~glmmTMB(as.formula(paste0("resp_var ~ ",.x," + (1|codevillage/pointdecapture2)")), data = df, family = binomial(link = "logit")))
      func <- function(x){
        ret <- glmmTMB(as.formula(paste0("resp_var ~ ",x," + (1|codevillage/pointdecapture2)")), data = df, family = binomial(link = "logit"))
        return(ret)
      }
    } else if (mod == "abundance"){
      #glmms_univs <- map(colnames(df[4:ncol(df)]), ~glmmTMB(as.formula(paste0("resp_var ~ ",.x," + (1|codevillage/pointdecapture2)")), data = df, family = truncated_nbinom2))
      func <- function(x){
        ret <- glmmTMB(as.formula(paste0("resp_var ~ ",x," + (1|codevillage/pointdecapture2)")), data = df, family = truncated_nbinom2)
        return(ret)
      }
    }
    possible_a <- possibly(func, otherwise = NA_real_)
    glmms_univs <- map(colnames(df[4:ncol(df)]), possible_a)
    
      glmms_univs <- glmms_univs %>%
      purrr::map(.,~broom.mixed::tidy(., conf.int = TRUE, exponentiate = ifelse(mod == "abundance",FALSE,TRUE)))
                                                                                
    for(i in 1:length(glmms_univs)){
      if(nrow(glmms_univs[[i]])<=1){
        glmms_univs[[i]] = NA
      }
    }
                                                                                
    glmms_univs <- glmms_univs %>%
      do.call(rbind.data.frame, .) %>%
      filter(effect == "fixed" & term!="(Intercept)") %>%
      #filter(p.value <= 0.2, !grepl("VCM",term)) %>%
      mutate(term = gsub("presencepresence","presence",term))
  
  
    ###### GLMM multivariate model  ######
  
    # multicollinearity among predictors
      vars_multiv <- unique(glmms_univs$term)
      if(nrow(glmms_univs)>1){
        vars_multiv <- fun_multicol(th_trmetrics_entomo_postedecapture, unique(glmms_univs$term))
      }
    
    vars_multiv <- c(vars_multiv,"int_ext","VCM")
    preds_forced <- NULL
    
      ## run GLMM 
     if(purpose == "explicative"){
       preds_forced <- c("VCM","int_ext")
     } else if(purpose == "predictive"){
       preds_forced <- c("int_ext")
     }
    
    
    glmm_model <- fun_compute_glmm(th_trmetrics_entomo_postedecapture, vars_multiv, predictors_forced = preds_forced, mod, cv_type = c("llo","lto","llto"))
    
    } else {
      glmm_model <- NULL
    }
    
    
    rf_llo <- NULL
    rf_lto <- NULL
    rf_llto <- NULL
    
      
      #############
    ###### RF ######
      #############
      
    ######## feature forward selection of temporal variables
    colnames_timevars <- fun_get_temporal_preds_columns(0, 42, time_vars, 7, th_trmetrics_entomo_postedecapture)
    cols_to_keep_timevar <- fun_get_time_preds_rf(th_trmetrics_entomo_postedecapture, colnames_timevars,time_vars, tune_length = 1, mod = mod)
    
    ##### selection of spatial variables
    # if(purpose == "predictive"){
    #   spatial_vars <- setdiff(spatial_vars,c("still_water_presence","running_water_presence","RFH","NMA","NMT","NMH","LMN"))
    # } else {
    #   spatial_vars <- setdiff(spatial_vars,c("still_water_presence","running_water_presence","RFH","NMA","NMT","NMH","LMN"))
    # }
    #  spatial_vars <- c(
    #   "lsm_c_pland_2000_3_2","lsm_c_pland_2000_3_4","lsm_c_pland_2000_3_9","lsm_c_pland_2000_3_12",
    #   colnames(th_trmetrics_entomo_postedecapture[which(grepl("WAC|WLS|POP",colnames(th_trmetrics_entomo_postedecapture)))]),
    #   "WMD"
    # )
    spatial_vars <- setdiff(spatial_vars,"still_water_presence")
     
    spatial_vars <- fun_multicol(th_trmetrics_entomo_postedecapture, spatial_vars)
      
      # spatial_vars <- fun_feature_forward_selection(
      #   df = th_trmetrics_entomo_postedecapture,
      #   stat_method = "rf",
      #   mod = mod,
      #   type = "model_comparison",
      #   expl_vars_to_keep = "nummission",
      #   expl_vars_to_test = spatial_vars,
      #   cross_validation_type = "spatial",
      #   tune_length = 3)
    
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
     #  spatial_vars <- spatial_vars %>%
     #    filter(diff_res_w_basemod > 0) %>%
     #    arrange(desc(diff_res_w_basemod)) %>%
     #    top_n(10)
     #  
     #  spatial_vars <- as.character(spatial_vars$name)
     # 
      
      if(purpose == "explicative"){
        predictors <- c(cols_to_keep_timevar, spatial_vars, "VCM","int_ext")
      } else if (purpose == "predictive"){
        predictors <- c(cols_to_keep_timevar, spatial_vars)
      }
      
      predictors <- fun_multicol(th_trmetrics_entomo_postedecapture, predictors)
      #predictors <- c(predictors,"X_32630","Y_32630")
      
      
      #rf_llo <- fun_compute_rf(th_trmetrics_entomo_postedecapture, predictors, cv_type = "llo", mod)
      rf_lto <- fun_compute_rf(th_trmetrics_entomo_postedecapture, predictors, cv_type = "lto", mod)
      #rf_llto <- fun_compute_rf(th_trmetrics_entomo_postedecapture, predictors, cv_type = "llto", mod)

    
    #}
       return(list(glmm_model = glmm_model,
                   rf_llo = rf_llo,
                   rf_lto = rf_lto,
                   rf_llto = rf_llto))
      
  }
  
    
    
    
  df_input_params_glmm <- tibble(response_var = character(), code_pays = character(), mod = character(), purpose = character())
  df_input_params_glmm <- df_input_params_glmm %>%
    add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "abundance", purpose = "explicative") %>%
    add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "abundance", purpose = "explicative") %>%
    add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "abundance", purpose = "explicative") %>%
    add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "presence", purpose = "explicative") %>%
    add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "presence", purpose = "explicative") %>%
    add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "presence", purpose = "explicative") %>%
    add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "abundance_discrete", purpose = "explicative") %>%
    add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "abundance_discrete", purpose = "explicative") %>%
    add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "abundance_discrete", purpose = "explicative") %>%
    add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "abundance", purpose = "predictive") %>%
    add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "abundance", purpose = "predictive") %>%
    add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "abundance", purpose = "predictive") %>%
    add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "presence", purpose = "predictive") %>%
    add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "presence", purpose = "predictive") %>%
    add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "presence", purpose = "predictive")
  
  
  th_model_results1 <- df_input_params_glmm[1,] %>%
    mutate(results = pmap(list(response_var, code_pays, mod, purpose), ~fun_workflow_model(..1,..2,..3,..4)))
  
  th_model_results2 <- df_input_params_glmm[2,] %>%
    mutate(results = pmap(list(response_var, code_pays, mod, purpose), ~fun_workflow_model(..1,..2,..3,..4)))
  
  th_model_results3 <- df_input_params_glmm[3,] %>%
    mutate(results = pmap(list(response_var, code_pays, mod, purpose), ~fun_workflow_model(..1,..2,..3,..4)))
  
  th_model_results4 <- df_input_params_glmm[4,] %>%
    mutate(results = pmap(list(response_var, code_pays, mod, purpose), ~fun_workflow_model(..1,..2,..3,..4)))
  
    th_model_results5 <- df_input_params_glmm[5,] %>%
    mutate(results = pmap(list(response_var, code_pays, mod, purpose), ~fun_workflow_model(..1,..2,..3,..4)))
  
  th_model_results6 <- df_input_params_glmm[6,] %>%
    mutate(results = pmap(list(response_var, code_pays, mod, purpose), ~fun_workflow_model(..1,..2,..3,..4)))

  th_model_results7 <- df_input_params_glmm[7,] %>%
    mutate(results = pmap(list(response_var, code_pays, mod, purpose), ~fun_workflow_model(..1,..2,..3,..4)))
  th_model_results8 <- df_input_params_glmm[8,] %>%
    mutate(results = pmap(list(response_var, code_pays, mod, purpose), ~fun_workflow_model(..1,..2,..3,..4)))
  th_model_results9 <- df_input_params_glmm[9,] %>%
    mutate(results = pmap(list(response_var, code_pays, mod, purpose), ~fun_workflow_model(..1,..2,..3,..4)))
  
  
  model_results <- rbind(th_model_results1,th_model_results2,th_model_results3,th_model_results4,th_model_results5,th_model_results6,th_model_results7,th_model_results8,th_model_results9)
  
  
  model_results <- model_results %>%
    mutate(glmm_model = map(results, ~pluck(.,"glmm_model"))) %>%
    mutate(rf_model_llo = map(results, ~pluck(.,"rf_llo"))) %>%
    mutate(rf_model_lto = map(results, ~pluck(.,"rf_lto"))) %>%
    mutate(rf_model_llto = map(results, ~pluck(.,"rf_llto"))) %>%
    dplyr::select(-c(results))
  
  saveRDS(model_results,"/home/ptaconet/Bureau/data_analysis/model_results_newnewanalysis_13.rds")
  
  # model_results_newnewanalysis_9.rds = 2 vars temporelles en univarié, 1 seul buffer de 2000 m
  # model_results_newnewanalysis_10.rds = 3 vars temporelles en univarié, 1 seul buffer de 2000 m
  # model_results_newnewanalysis_11.rds = 2 vars temporelles en univarié, 2 buffers (500 m, 2000m)
  # model_results_newnewanalysis_12.rds = 2 vars temporelles différentes en univarié, 1 buffer
  # model_results_newnewanalysis_13.rds = 2 vars temporelles différentes en univarié, 1 buffer, sans les vars micro clim
  
  
th_model_results10 <- df_input_params_glmm[10,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod, purpose), ~fun_workflow_model(..1,..2,..3,..4)))
th_model_results11 <- df_input_params_glmm[11,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod, purpose), ~fun_workflow_model(..1,..2,..3,..4)))
th_model_results12 <- df_input_params_glmm[12,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod, purpose), ~fun_workflow_model(..1,..2,..3,..4)))
th_model_results13 <- df_input_params_glmm[13,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod, purpose), ~fun_workflow_model(..1,..2,..3,..4)))
th_model_results14 <- df_input_params_glmm[14,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod, purpose), ~fun_workflow_model(..1,..2,..3,..4)))
th_model_results15 <- df_input_params_glmm[15,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod, purpose), ~fun_workflow_model(..1,..2,..3,..4)))

model_results <- rbind(th_model_results1,th_model_results2,th_model_results3,th_model_results4,th_model_results5,th_model_results6,th_model_results7,th_model_results8,th_model_results9,th_model_results10,th_model_results11,th_model_results12,th_model_results13,th_model_results14,th_model_results15)

model_results <- model_results %>%
  mutate(glmm_model = map(results, ~pluck(.,"glmm_model"))) %>%
  mutate(rf_model_llo = map(results, ~pluck(.,"rf_llo"))) %>%
  mutate(rf_model_lto = map(results, ~pluck(.,"rf_lto"))) %>%
  mutate(rf_model_llto = map(results, ~pluck(.,"rf_llto"))) %>%
  dplyr::select(-c(results))

saveRDS(model_results,"/home/ptaconet/Bureau/data_analysis/model_results_newnewanalysis_8.rds")
