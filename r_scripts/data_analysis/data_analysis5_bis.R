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
                                   period_interv = "all",
                                   lag_time_window = c(0,30),
                                   buffer_sizes = c(2000)){
      
      cat("Executing workflow for parameters : ", response_var, code_pays, mod,period_interv,"\n")
      
      ###### load the data
      
      # load spatiotemporal data
      env_spatiotemporal <- load_spatiotemporal_data(vars = c("RFD1F","TMIN1","TMAX1"),
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
      th_env_static$VCT <- NULL
      popani <- env_spatial_all[[6]]
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
      hum_behav = hum_behav[[3]]

      # load time since vector control measure
      time_since_vc <- load_time_since_vc(code_pays, entomo_csh_metadata_l1)
      
      # load response variable

        if(response_var == "ma_funestus_ss"){
          response_var <- ifelse(code_pays == "BF","An.funestus_ss","An.funestus")
        } else if(response_var == "ma_coluzzi"){
          response_var <- "An.coluzzii"
        } else if(response_var == "ma_gambiae_ss"){
          response_var <- "An.gambiae_ss"
        } else if(response_var == "ma_gambiae_sl"){
          response_var <- "An.gambiae s.l."
        }
        
        if(code_pays == "BF"){
        th_trmetrics_entomo_postedecapture <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
          dplyr::select(-fid) %>% 
          filter(codepays == code_pays, pcr_espece == response_var, nummission <= 8) %>%
          mutate(nummission = as.character(nummission))
        } else if(code_pays == "CI"){
          th_trmetrics_entomo_postedecapture <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
            dplyr::select(-fid) %>% 
            filter(codepays == code_pays, especeanoph == response_var, nummission <= 8) %>%
            mutate(nummission = as.character(nummission))
        }
        
        
        # add early_late biting (column ELB)
         ELB <- th_trmetrics_entomo_postedecapture %>%
           left_join(hum_behav_4_exophagy) %>%
           mutate(ELB = case_when(HBB > 50 ~ "nocturnal",
                                  HBB < 50 & heuredecapture > 15 ~ "early_biting",
                                  HBB < 50 & heuredecapture < 10 ~ "late_biting")) %>%
           dplyr::select(idpointdecapture,heuredecapture,ELB)
        
         th_trmetrics_entomo_postedecapture$ELB <- ELB$ELB
         

        # fill missing values KDR and ACE1
        th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
          mutate(kdrw = ifelse(is.na(kdrw),"RS",kdrw)) %>%
          mutate(kdre = ifelse(is.na(kdre),"RS",kdre)) %>%
          mutate(ace1 = ifelse(is.na(ace1),"RS",ace1))
          
        
        
            early_b <- th_trmetrics_entomo_postedecapture %>%
              mutate(ELB = ifelse(ELB == "late_biting","nocturnal",ELB)) %>%
              group_by(nummission, codevillage, pointdecapture, ELB) %>%
              summarise(n=n()) %>%
              pivot_wider(names_from = ELB, values_from = n, values_fill = 0) %>%
              mutate(tot = nocturnal+early_biting) %>%
              mutate(ELB = early_biting / tot) %>%
              as_tibble() %>%
              dplyr::select(nummission,codevillage,pointdecapture,ELB )

            
            
              late_b <- th_trmetrics_entomo_postedecapture %>%
              mutate(ELB = ifelse(ELB == "early_biting","nocturnal",ELB)) %>%
              group_by(nummission, codevillage, pointdecapture, ELB) %>%
              summarise(n=n()) %>%
              pivot_wider(names_from = ELB, values_from = n, values_fill = 0) %>%
              mutate(tot = nocturnal+late_biting) %>%
              mutate(LBT = late_biting / tot ) %>%
              as_tibble() %>%
              dplyr::select(nummission,codevillage,pointdecapture,LBT )
            


          exo <- th_trmetrics_entomo_postedecapture %>%
            group_by(nummission, codevillage, pointdecapture,postedecapture) %>%
            summarise(n=n()) %>%
            pivot_wider(names_from = postedecapture, values_from = n, values_fill = 0) %>%
            mutate(tot = e+i) %>%
            mutate(EXO = e / tot) %>%
            as_tibble() %>%
            dplyr::select(nummission,codevillage,pointdecapture,EXO,tot )
          

          if(response_var %in% c("An.coluzzii","An.gambiae_ss")){
            
          kdrw <- th_trmetrics_entomo_postedecapture %>%
            group_by(codevillage, nummission,  pointdecapture, kdrw) %>%
            summarise(n=n()) %>%
            as_tibble() %>%
            filter(!is.na(kdrw)) %>%
            pivot_wider(names_from = kdrw, values_from = n, values_fill = list(n = 0) ) %>%
            mutate(KDRW = (2*RR + RS) / (2*(RR+RS+SS))) %>%
            as_tibble() %>%
            dplyr::select(nummission,codevillage,pointdecapture,KDRW )
          
          

          kdre <- th_trmetrics_entomo_postedecapture %>%
            group_by(codevillage, nummission,  pointdecapture, kdre) %>%
            summarise(n=n()) %>%
            as_tibble() %>%
            filter(!is.na(kdre)) %>%
            pivot_wider(names_from = kdre, values_from = n, values_fill = list(n = 0) ) %>%
            mutate(KDRE = (2*0 + RS) / (2*(0+RS+SS))) %>%
            as_tibble()  %>%
            dplyr::select(nummission,codevillage,pointdecapture,KDRE )
          

          ace1 <- th_trmetrics_entomo_postedecapture %>%
            group_by(codevillage, nummission,  pointdecapture, ace1) %>%
            summarise(n=n()) %>%
            as_tibble() %>%
            filter(!is.na(ace1)) %>%
            pivot_wider(names_from = ace1, values_from = n, values_fill = list(n = 0) ) %>%
            mutate(ACE1 = (2*0 + RS) / (2*(0+RS+SS))) %>%
            as_tibble() %>%
            dplyr::select(nummission,codevillage,pointdecapture,ACE1 )
          
          
          exo <- exo %>% left_join(kdre) %>% left_join(kdrw) %>% left_join(ace1)
          }
      
      th_env_static2 <- th_env_static %>% mutate(v = 1) %>% pivot_wider(names_from = VCM,  values_from = v, values_fill = list(v = 0), names_prefix = "VCM_")
      th_env_static <- th_env_static %>% dplyr::select(idpointdecapture,VCM)
      
      mean_date_mission <- entomo_csh_metadata_l1 %>% mutate(date_capture = as.Date(date_capture)) %>% dplyr::group_by(codepays,nummission) %>% dplyr::summarise(mean_date_mission=mean(date_capture)) %>% as_tibble() %>% filter(codepays==code_pays) %>% dplyr::select(-codepays) 
    
      
        ######## join response variable with explanatory variables
      
      th_trmetrics_entomo_postedecapture <- early_b %>% 
        mutate(idpointdecapture = paste0(nummission,codevillage,pointdecapture)) %>%
        left_join(late_b) %>%
        left_join(exo) %>%
        left_join(mean_date_mission) %>%
        left_join(mean_coords_points_4326) %>%
        left_join(mean_coords_points_32630) %>%
        left_join(env_spatiotemporal) %>%
        left_join(env_spatial) %>%
        left_join(th_env_nightcatch_postedecapture) %>%
        left_join(th_env_nightcatch) %>%
        left_join(th_env_static) %>%
        left_join(th_env_static2) %>%
        left_join(env_landcover) %>%
        left_join(hum_behav) %>%
        left_join(popani) %>%
        left_join(LUS) %>%
        left_join(time_since_vc) %>%
        mutate(VCM = case_when(VCM == "Ctrle" ~ "LLIN",
                               VCM == "IEC" ~ "LLIN + IEC",
                               VCM == "IRS" ~ "LLIN + IRS",
                               VCM == "IVM" ~ "LLIN + IVM",
                               VCM == 'Larvicide' ~ 'LLIN + Larv.')) %>%
        mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
        mutate(TMAX1_2000_0_30 = (TMAX1_2000_0_30+TMIN1_2000_0_30)/2)
      
      th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture[, colSums(is.na(th_trmetrics_entomo_postedecapture)) != nrow(th_trmetrics_entomo_postedecapture)]
      
      
      if(mod == "exophagy_reg"){
        th_trmetrics_entomo_postedecapture$resp_var <- th_trmetrics_entomo_postedecapture$EXO
      }      
      if(mod == "early_biting_reg"){
        th_trmetrics_entomo_postedecapture$resp_var <- th_trmetrics_entomo_postedecapture$ELB
      }
      if(mod == "late_biting_reg"){
        th_trmetrics_entomo_postedecapture$resp_var <- th_trmetrics_entomo_postedecapture$LBT
      }
      if(mod == "physiological_resistance_kdrw_reg"){
        th_trmetrics_entomo_postedecapture$resp_var <- th_trmetrics_entomo_postedecapture$KDRW
      }
      if(mod == "physiological_resistance_kdre_reg"){
        th_trmetrics_entomo_postedecapture$resp_var <- th_trmetrics_entomo_postedecapture$KDRE
      }
      if(mod == "physiological_resistance_ace1_reg"){
        th_trmetrics_entomo_postedecapture$resp_var <- th_trmetrics_entomo_postedecapture$ACE1
      }
      
      if(period_interv!="all"){
        th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% dplyr::filter(periode==period_interv)
      }
      
      if(code_pays == "BF"){
        th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
          mutate(season = case_when(nummission %in% c("3","4") ~ "wet",
                                    nummission %in% c("1","5","6") ~ "dry-cold",
                                    nummission %in% c("2","7") ~ "dry-hot")) %>%
          mutate(season = fct_relevel(season,c("wet","dry-cold","dry-hot")))
      } else if(code_pays=="CI"){
        th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
          mutate(season = case_when(nummission %in% c("","") ~ "wet",
                                    nummission %in% c("","") ~ "dry-cold",
                                    nummission %in% c("","") ~ "dry-hot")) %>%
          mutate(season = fct_relevel(season,c("wet","dry-cold","dry-hot")))
        
      }
    
    
      
      ### selection of predictors
       if (mod %in% c("exophagy_reg")){
         
          #predictors <- c("NMT","NMH","NML","NMA","DNMT","DNMH","DNML","WSP","RFH","LMN","VCM","VCT","VCT2","HBB","HBI","LUS","LIG30_2000","RFD1F_2000_0_30","TMIN1_2000_0_30","TMAX1_2000_0_30","TMAX1_2000_0_0","RFD1F_2000_0_0","POP","ANI","POPANI","BDE","WMD","lsm_c_pland_2000_3_9")
          predictors <- c("NMT","NMH","NML","DNMT","DNMH","DNML","WSP","RFH","VCM","VCT","VCT2","HBB","HBI","LUS","LMN","RFD1F_2000_0_30","TMAX1_2000_0_30","TMAX1_2000_0_0","RFD1F_2000_0_0","POPANI","BDE","WMD","ELB","LBT")  # predictors = same as dayscience + some landscape and meteorological variables + physiological resistances
          
          if(response_var != "An.funestus_ss"){
            predictors <- c(predictors,"KDRE","KDRW")
          }
  
             glmm_varstoforce <- NULL
          
        } else if (mod %in% c("early_late_biting_reg","early_biting_reg","late_biting_reg")){
          
          predictors <- c("VCM","VCT","VCT2","HBB","HBI","LUS","LMN","RFD1F_2000_0_30","TMAX1_2000_0_30","TMAX1_2000_0_0","RFD1F_2000_0_0","POPANI","BDE","WMD","EXO") 

          if(response_var != "An.funestus_ss"){
            predictors <- c(predictors,"KDRE","KDRW")
          }
          glmm_varstoforce <- NULL
          
          
          } else if(mod %in% c("physiological_resistance_kdrw_reg","physiological_resistance_kdre_reg","physiological_resistance_ace1_reg")){
          
          if(code_pays == "BF"){ 
            lcid <- 3
            th_trmetrics_entomo_postedecapture$lsm_c_pland_2000_3_1 <- rowSums(cbind(th_trmetrics_entomo_postedecapture$lsm_c_pland_2000_3_1,th_trmetrics_entomo_postedecapture$lsm_c_pland_2000_3_11),na.rm = TRUE)
            lsm_agri <- c("lsm_c_pland_2000_3_1")
          } else if (code_pays == "CI"){ 
              lcid <- 8
              lsm_agri <- c("lsm_c_pland_2000_8_4","lsm_c_pland_2000_8_7","lsm_c_pland_2000_8_8")
          }
          
          glmm_varstoforce <- NULL
          predictors <-c(lsm_agri,"VCM","VCT","VCT2","LUS","EXO")
          
        }
    
      
      glmms_univs <- NULL
      glmm <- NULL
      rf_dayscience <- NULL
      rf_nightscience <- NULL
      rf_selectvar <- NULL
      
      ###### univariate models ######
      # glmms
      df <- th_trmetrics_entomo_postedecapture %>% 
        mutate(pointdecapture2 = as.factor(paste0(codevillage,pointdecapture))) %>%
        dplyr::select(resp_var,codevillage,pointdecapture2,predictors) %>% 
        mutate(resp_var = as.character(resp_var)) %>%
        mutate_if(is.numeric, ~scale(.)) %>%
        mutate(resp_var = as.numeric(resp_var))
    
        func <- function(x){
          ret <- glmmTMB(as.formula(paste0("resp_var ~ ",x," + (1|codevillage/pointdecapture2)")), data = df, family = binomial(link = "logit"))
          return(ret)
        }
        
      possible_a <- possibly(func, otherwise = NA_real_)
      glmms_univs <- map(colnames(df[4:ncol(df)]), possible_a)
      
      i <- 1
      while (i <= length(glmms_univs)){
        if(is.na(summary(glmms_univs[[i]])$AICtab[1])){
          glmms_univs[[i]] <- NULL
        }
        i = i+1
      }
      i <- 1
      while (i <= length(glmms_univs)){
        if(is.na(summary(glmms_univs[[i]])$AICtab[1])){
          glmms_univs[[i]] <- NULL
        }
        i = i+1
      }
      
      
      func2 <- function(x){
        ret <- broom.mixed::tidy(x, conf.int = TRUE, exponentiate = TRUE)
        return(ret)
      }
      
      possible_b <- possibly(func2, otherwise = NULL)
      glmms_univs <- map(glmms_univs, possible_b) %>%
        do.call(rbind.data.frame, .) %>%
        filter(effect == "fixed" & term!="(Intercept)")
  
  
        ###### GLMM multivariate model  ######
    
        #### based on glmm pval univariate analysis
          pvals_filts <- glmms_univs %>% filter(term %in% predictors, p.value <= 0.3, !grepl("VCM",term))
  
        
      # multicollinearity among predictors
        vars_multiv <- pvals_filts$term
        if(nrow(pvals_filts)>1){
          vars_multiv <- fun_multicol(th_trmetrics_entomo_postedecapture, pvals_filts$term)
        }
        if(length(vars_multiv)>1){
         predictors <- fun_multicol(th_trmetrics_entomo_postedecapture, vars_multiv)
        } else {
          predictors <- vars_multiv
        }
  
        predictors_microclim <- c("NMT","NMH","DNMT","DNMH")
        predictors_microclim <- intersect(predictors_microclim,predictors)
        if(length(predictors_microclim)==0){
          predictors_microclim <- NULL
        }
  
        pvals_filt_vcm <- glmms_univs %>% filter(p.value <= 0.3, grepl("VCM",term))

        if(nrow(pvals_filt_vcm)>0){
          predictors <- c(predictors, "VCM")
          pvals_filts <- rbind(pvals_filts, pvals_filt_vcm)
        }
  
        if(length(predictors)>=2){
          
          th_trmetrics_entomo_postedecapture$int_ext <- "i"
         glmm <- fun_compute_glmm(th_trmetrics_entomo_postedecapture, unique(predictors), mod = mod, predictors_interaction = predictors_microclim, cv_type = "llo")
        
        rf_nightscience <- fun_compute_rf(th_trmetrics_entomo_postedecapture, unique(predictors), cv_col = "codevillage", mod, featureselect = FALSE)
        
    
        # rf_nightscience$mod$pred %>% group_by(Resample) %>% summarise(n=n())  -> number of samples for each fold
        # rf_nightscience$mod$resample -> predictive quality for each resampling
        
        }
          
         return(list(glmms_univs = pvals_filts,
                     glmm = glmm, 
                     rf_dayscience = rf_dayscience,
                     rf_nightscience = rf_nightscience,
                     rf_selectvar = rf_selectvar))
        
      }
    
      
      
      
    df_input_params_glmm <- tibble(response_var = character(), code_pays = character(), mod = character(), period_interv = character())
    df_input_params_glmm <- df_input_params_glmm %>%
      add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "exophagy_reg", period_interv = "all") %>%
      add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "exophagy_reg", period_interv = "all") %>%
      add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "exophagy_reg", period_interv = "all") %>%
      add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "early_biting_reg", period_interv = "all") %>%
      add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "early_biting_reg", period_interv = "all") %>%
      add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "early_biting_reg", period_interv = "all")%>%
      add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "late_biting_reg", period_interv = "all") %>%
      add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "late_biting_reg", period_interv = "all") %>%
      add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "late_biting_reg", period_interv = "all")  %>%
      add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "physiological_resistance_kdrw_reg", period_interv = "all") %>%
      add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "physiological_resistance_kdrw_reg", period_interv = "all") %>%
      add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "physiological_resistance_kdre_reg", period_interv = "all") %>%
      add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "physiological_resistance_kdre_reg", period_interv = "all") %>%
      add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "physiological_resistance_ace1_reg", period_interv = "all") %>%
      add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "physiological_resistance_ace1_reg", period_interv = "all") %>%
      add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "exophagy", period_interv="postinterv") %>%
      add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "exophagy", period_interv="postinterv") %>%
      add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "exophagy", period_interv="postinterv") %>%
      add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "early_biting", period_interv="postinterv") %>%
      add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "early_biting", period_interv="postinterv") %>%
      add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "early_biting", period_interv="postinterv")%>%
      add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "late_biting", period_interv="postinterv") %>%
      add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "late_biting", period_interv="postinterv") %>%
      add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "late_biting", period_interv="postinterv")  %>%
      add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "physiological_resistance_kdrw", period_interv = "postinterv") %>%
      add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "physiological_resistance_kdrw", period_interv = "postinterv") %>%
      add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "physiological_resistance_kdre", period_interv = "postinterv") %>%
      add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "physiological_resistance_kdre", period_interv = "postinterv") %>%
      add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "physiological_resistance_ace1", period_interv = "postinterv") %>%
      add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "physiological_resistance_ace1", period_interv = "postinterv")
       
    th_model_results1 <- df_input_params_glmm[1,] %>%
      mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          
          th_model_results2 <- df_input_params_glmm[2,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          
          th_model_results3 <- df_input_params_glmm[3,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
           
            th_model_results4 <- df_input_params_glmm[4,] %>%
             mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
           
           th_model_results5 <- df_input_params_glmm[5,] %>%
             mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
           
            th_model_results6 <- df_input_params_glmm[6,] %>%
             mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          
          th_model_results7 <- df_input_params_glmm[7,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          
          th_model_results8 <- df_input_params_glmm[8,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          
          th_model_results9 <- df_input_params_glmm[9,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          
          th_model_results10 <- df_input_params_glmm[10,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          
          th_model_results11 <- df_input_params_glmm[11,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          
          th_model_results12 <- df_input_params_glmm[12,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          
          th_model_results13 <- df_input_params_glmm[13,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          
          th_model_results14 <- df_input_params_glmm[14,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          
          th_model_results15 <- df_input_params_glmm[15,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          
          th_model_results16 <- df_input_params_glmm[16,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          th_model_results17 <- df_input_params_glmm[17,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          th_model_results18 <- df_input_params_glmm[18,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          th_model_results19 <- df_input_params_glmm[19,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          th_model_results20 <- df_input_params_glmm[20,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          th_model_results21 <- df_input_params_glmm[21,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          th_model_results22 <- df_input_params_glmm[22,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          th_model_results23 <- df_input_params_glmm[23,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          th_model_results24 <- df_input_params_glmm[24,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          th_model_results25 <- df_input_params_glmm[25,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          th_model_results26 <- df_input_params_glmm[26,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          th_model_results27 <- df_input_params_glmm[27,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          th_model_results28 <- df_input_params_glmm[28,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          th_model_results29 <- df_input_params_glmm[29,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          th_model_results30 <- df_input_params_glmm[30,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,period_interv), ~fun_workflow_model(..1,..2,..3,..4)))
          
    
        model_results <- rbind(th_model_results1,th_model_results2,th_model_results3,th_model_results4,th_model_results5,th_model_results6,th_model_results7,th_model_results8,th_model_results9,th_model_results10,th_model_results11,th_model_results12,th_model_results13,th_model_results14,th_model_results15,th_model_results16,th_model_results17,th_model_results18,th_model_results19,th_model_results20,th_model_results21,th_model_results22,th_model_results23,th_model_results24)
    
    model_results <- model_results %>%
      mutate(glmms_univs = map(results, ~pluck(.,"glmms_univs"))) %>%
      mutate(glmm = map(results, ~pluck(.,"glmm"))) %>%
      mutate(rf_dayscience = map(results, ~pluck(.,"rf_dayscience"))) %>%
      mutate(rf_nightscience = map(results, ~pluck(.,"rf_nightscience"))) %>%
      mutate(rf_selectvar = map(results, ~pluck(.,"rf_selectvar"))) %>%
      dplyr::select(-results)
    
    saveRDS(model_results,"/home/ptaconet/Bureau/data_analysis/model_results_resistances7.rds")
    
      # model_results_resistances4.rds : glmm + rf   
    # model_results_resistances5.rds : rf only
    #model_results_resistances6.rds  : glmm + rf (corrigé)
    #model_results_resistances7.rds  : glmm + rf (threshold pval = 0.2)
    
    
    #5,11,12,14,15,19,20,25,26,28,29,30