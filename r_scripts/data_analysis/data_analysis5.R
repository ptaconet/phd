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
                                     periodinterv = "all",
                                     lag_time_window = c(0,30),
                                     buffer_sizes = c(2000)){
        
        cat("Executing workflow for parameters : ", response_var, code_pays, mod,periodinterv,"\n")
        
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
            landcover_layers_to_keep <- c(3,4)
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
        th_env_nightcatch$NMA <- NULL
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
            hum_behav_4_earlylatebiting = hum_behav[[3]]
            rm(hum_behav)
        
        # load time since vector control measure
        time_since_vc <- load_time_since_vc(code_pays, entomo_csh_metadata_l1)
        
        # load abundance 
        abundance <- dbReadTable(react_gpkg, 'trmetrics_entomo_postedecapture')
        abundance$MA <- abundance[,response_var]
        abundance <- abundance %>% dplyr::select(idpostedecapture,MA)
        
        # load response variable
        if(mod %in% c("presence","abundance")){
          th_trmetrics_entomo_postedecapture <- dbReadTable(react_gpkg, 'trmetrics_entomo_postedecapture') %>% 
            dplyr::select(-fid) %>% 
            left_join(entomo_csh_metadata_l1 %>% dplyr::select(idpointdecapture, codevillage, pointdecapture, codepays, nummission, period_interv)) %>% 
            filter(!is.na(codevillage)) %>%
            filter(codepays == code_pays) %>%
            mutate(heuredecapture = NA)
          
          th_trmetrics_entomo_postedecapture$resp_var <- th_trmetrics_entomo_postedecapture[,response_var]
        } else if(mod %in% c("physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1","exophagy","early_late_biting","early_biting","late_biting")){
          
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
            left_join(entomo_csh_metadata_l1 %>% dplyr::select(idpointdecapture, period_interv)) %>% 
            filter(codepays == code_pays, pcr_espece == response_var, nummission <= 8) %>%
            mutate(nummission = as.character(nummission))
          } else if(code_pays == "CI"){
            th_trmetrics_entomo_postedecapture <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>% 
              dplyr::select(-fid) %>% 
              left_join(entomo_csh_metadata_l1 %>% dplyr::select(idpointdecapture, period_interv)) %>% 
              filter(codepays == code_pays, especeanoph == response_var, nummission <= 8) %>%
              mutate(nummission = as.character(nummission))
          }
          
          # add physiological resistance (column PHY)
          # th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
          #   mutate(kdrw = ifelse(kdrw == "RR","1",kdrw)) %>%
          #   mutate(kdrw = ifelse(kdrw == "RS","0.5",kdrw)) %>%
          #   mutate(kdrw = ifelse(kdrw == "SS","0",kdrw)) %>%
          #   mutate(kdre = ifelse(kdre == "RR","1",kdre)) %>%
          #   mutate(kdre = ifelse(kdre == "RS","0.5",kdre)) %>%
          #   mutate(kdre = ifelse(kdre == "SS","0",kdre)) %>%
          #   mutate(ace1 = ifelse(ace1 == "RR","1",ace1)) %>%
          #   mutate(ace1 = ifelse(ace1 == "RS","0.5",ace1)) %>%
          #   mutate(ace1 = ifelse(ace1 == "SS","0",ace1)) %>%
          #   mutate(PHY_kdrw = as.numeric(kdrw)) %>%
          #   mutate(PHY_kdre = as.numeric(kdre)) %>%
          #   mutate(PHY_ace1 = as.numeric(ace1))
            
           # t = entomo_csh_metadata_l1 %>%
           #   filter(codepays == code_pays) %>%
           #   mutate(periode = ifelse(period_interv=="pre_intervention","preinterv","postinterv")) %>%
           #   mutate(date_capture = as.Date(date_capture)) %>%
           #   mutate(month = lubridate::month(date_capture)) %>%
           #   mutate(saison = ifelse(month <= 4 | month >=11 , "seche","pluies")) %>%
           #   dplyr::select(idpointdecapture,codevillage,periode,saison)
          # 
          # HBB <- dbReadTable(react_gpkg, 'entomo_comportementhumain_l0') %>% 
          #   dplyr::filter(codepays == code_pays) %>%
          #   mutate(hcoucher = as.numeric(substr(hcoucher,1,2)),hlever=as.numeric(substr(hlever,1,2))) %>%
          #   mutate(hcoucher = ifelse(hcoucher <=17, 20, hcoucher), hlever=ifelse(hlever>=11 | hlever<=3,6,hlever)) %>%
          #   dplyr::group_by(codevillage,periode,saison) %>%
          #   dplyr::summarise(hcoucher=round(mean(hcoucher)),hlever=round(mean(hlever)))
          
          # add early_late biting (column ELB)
           ELB <- th_trmetrics_entomo_postedecapture %>%
             left_join(hum_behav_4_exophagy) %>%
             mutate(ELB = case_when(HBB > 50 | is.na(HBB) ~ "nocturnal",
                                    HBB < 50 & heuredecapture > 15 ~ "early_biting",
                                    HBB < 50 & heuredecapture < 10 ~ "late_biting")) %>%
             mutate(ELB = fct_relevel(ELB,c("nocturnal","early_biting","late_biting"))) %>%
             dplyr::select(idpointdecapture,heuredecapture,ELB)
          
           th_trmetrics_entomo_postedecapture$ELB <- ELB$ELB
           
          # add exophagy (column EXO)
          th_trmetrics_entomo_postedecapture$EXO <- th_trmetrics_entomo_postedecapture$postedecapture
          
          # fill missing values KDR and ACE1
          th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
            mutate(kdrw = ifelse(is.na(kdrw),"RS",kdrw)) %>%
            mutate(kdre = ifelse(is.na(kdre),"RS",kdre)) %>%
            mutate(ace1 = ifelse(is.na(ace1),"RS",ace1))
            
          # add physiological resistance
           th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
             mutate(kdrw = ifelse(kdrw == "RR","1",kdrw)) %>%
             mutate(kdrw = ifelse(kdrw == "RS","0.5",kdrw)) %>%
             mutate(kdrw = ifelse(kdrw == "SS","0",kdrw)) %>%
             mutate(kdre = ifelse(kdre == "RR","1",kdre)) %>%
             mutate(kdre = ifelse(kdre == "RS","0.5",kdre)) %>%
             mutate(kdre = ifelse(kdre == "SS","0",kdre)) %>%
             mutate(ace1 = ifelse(ace1 == "RR","1",ace1)) %>%
             mutate(ace1 = ifelse(ace1 == "RS","0.5",ace1)) %>%
             mutate(ace1 = ifelse(ace1 == "SS","0",ace1)) %>%
             mutate(kdrw = as.numeric(kdrw)) %>%
             mutate(kdre = as.numeric(kdre)) %>%
             mutate(ace1 = as.numeric(ace1))
          
          
          if(mod %in% c("early_late_biting","early_biting","late_biting")){
            
            if(mod == "early_biting"){
              th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
                #filter(ELB != "late_biting") %>%
                mutate(resp_var = ifelse(ELB == "early_biting",1,0)) 
            }
            if(mod == "late_biting"){
              th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
                #filter(ELB != "early_biting") %>%
                mutate(resp_var = ifelse(ELB == "late_biting",1,0)) 
            }
            if(mod == "early_late_biting"){
               th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
                 mutate(resp_var = ELB) 
            }
            
              th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
                left_join(hum_behav_4_earlylatebiting)
            
          } else if (mod=="exophagy"){
            
            th_env_nightcatch$RFH <- th_env_nightcatch$WSP <-  th_env_nightcatch$NMA <- NULL
                th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
              mutate(resp_var = ifelse(EXO == "e",1,0)) %>%
              left_join(hum_behav_4_exophagy)
            
          } else if (mod=="physiological_resistance_kdrw"){
            th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
               mutate(resp_var = kdrw)  %>%
              left_join(hum_behav_4_exophagy)#%>%
              # mutate(resp_var = ifelse(resp_var == "RR","1",resp_var)) %>%
              # mutate(resp_var = ifelse(resp_var == "RS","0.5",resp_var)) %>%
              # mutate(resp_var = ifelse(resp_var == "SS","0",resp_var)) %>%
              # mutate(resp_var = as.numeric(resp_var))
          } else if (mod=="physiological_resistance_kdre"){
            th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
               mutate(resp_var = kdre)  %>%
              left_join(hum_behav_4_exophagy)#%>%
              # mutate(resp_var = ifelse(resp_var == "RR","1",resp_var)) %>%
              # mutate(resp_var = ifelse(resp_var == "RS","0.5",resp_var)) %>%
              # mutate(resp_var = ifelse(resp_var == "SS","0",resp_var)) %>%
              # mutate(resp_var = as.numeric(resp_var))
          } else if (mod=="physiological_resistance_ace1"){
            th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
               mutate(resp_var = ace1)  %>%
              left_join(hum_behav_4_exophagy)#%>%
              # mutate(resp_var = ifelse(resp_var == "RR","1",resp_var)) %>%
              # mutate(resp_var = ifelse(resp_var == "RS","0.5",resp_var)) %>%
              # mutate(resp_var = ifelse(resp_var == "SS","0",resp_var)) %>%
              # mutate(resp_var = as.numeric(resp_var))
          }
          
        }
        
        
        th_env_static2 <- th_env_static %>% mutate(v = 1) %>% pivot_wider(names_from = VCM,  values_from = v, values_fill = list(v = 0), names_prefix = "VCM_")
        th_env_static <- th_env_static %>% dplyr::select(idpointdecapture,VCM)
        
        mean_date_mission <- entomo_csh_metadata_l1 %>% mutate(date_capture = as.Date(date_capture)) %>% dplyr::group_by(codepays,nummission) %>% dplyr::summarise(mean_date_mission=mean(date_capture)) %>% as_tibble() %>% filter(codepays==code_pays) %>% dplyr::select(-codepays) 
      
        
          ######## join response variable with explanatory variables
        
        th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% 
          left_join(abundance) %>%
          left_join(mean_date_mission) %>%
          left_join(mean_coords_points_4326) %>%
          left_join(mean_coords_points_32630) %>%
          mutate(int_ext = substr( idpostedecapture,nchar(idpostedecapture),nchar(idpostedecapture))) %>%
          mutate(IEH = ifelse(int_ext == "i",1,0)) %>%
          mutate(int_ext = fct_relevel(int_ext,c("i","e"))) %>%
          #dplyr::select(idpostedecapture,idpointdecapture,int_ext,heuredecapture,IEH,pointdecapture,codevillage,codepays,nummission,mean_date_mission,X_4326,Y_4326,X_32630,Y_32630,resp_var) %>%
          left_join(env_spatiotemporal) %>%
          left_join(env_spatial) %>%
          left_join(th_env_nightcatch_postedecapture) %>%
          left_join(th_env_nightcatch) %>%
          left_join(th_env_static) %>%
          left_join(th_env_static2) %>%
          left_join(env_landcover) %>%
          left_join(popani) %>%
          left_join(LUS) %>%
          left_join(time_since_vc) %>%
          mutate(pointdecapture2 = as.factor(paste0(codevillage,pointdecapture))) %>%
          mutate(VCM = case_when(VCM %in% c("Ctrle","IEC") ~ "LLIN",
                                 #VCM == "IEC" ~ "LLIN + IEC",
                                 VCM == "IRS" ~ "LLIN + IRS",
                                 VCM == "IVM" ~ "LLIN + IVM",
                                 VCM == 'Larvicide' ~ 'LLIN + Larv.')) %>%
          mutate_if(is.numeric,funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))
        
        th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture[, colSums(is.na(th_trmetrics_entomo_postedecapture)) != nrow(th_trmetrics_entomo_postedecapture)]
        
        if(periodinterv!="all"){
          th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% dplyr::filter(period_interv==periodinterv)
        }
        
        if(code_pays == "BF"){
          th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
            mutate(season = case_when(nummission %in% c("3","4") ~ "wet",
                                      nummission %in% c("1","5","6") ~ "dry-cold",
                                      nummission %in% c("2","7") ~ "dry-hot")) %>%
            mutate(season = fct_relevel(season,c("wet","dry-cold","dry-hot"))) %>%
            mutate(period_interv = ifelse(nummission %in% c("1","2","3"),'pre-intervention','post-intervention')) %>% 
            mutate(period_interv = fct_relevel(period_interv,c("pre-intervention","post-intervention")))
        } else if(code_pays=="CI"){
          th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
            mutate(season = case_when(nummission %in% c("1","5") ~ "wet",
                                      nummission %in% c("2","6","7") ~ "dry-cold",
                                      nummission %in% c("3","4","8") ~ "dry-hot")) %>%
            mutate(season = fct_relevel(season,c("wet","dry-cold","dry-hot"))) %>%
            mutate(period_interv = ifelse(nummission %in% c("1","2","3","4"),'pre-intervention','post-intervention')) %>% 
            mutate(period_interv = fct_relevel(period_interv,c("pre-intervention","post-intervention")))
          
        }
        
        if(code_pays == "CI"){
        th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
          mutate(VCM = ifelse(period_interv=="pre-intervention","< LLIN dist.",VCM)) %>%
          mutate(VCM = fct_relevel(VCM, c("< LLIN dist.","LLIN","LLIN + IEC","LLIN + IRS","LLIN + Larv.")))
        th_trmetrics_entomo_postedecapture$VCM <- droplevels(th_trmetrics_entomo_postedecapture$VCM)
        
        }
        
        ### createfolds for resampling resistance models
      
        # {point de capture, mission} où il y a eu + de 10 piqures : 1 fold à part entière
        # si moins de 10 piqures sur un {point de capture, mission} : on regroupe tous les points de capture du village (toutes missions confondues) dans un fold
        # si moins de 10 piqures sur tous les points de capture du village (toutes missions confondues) : on regroupe aléatoirement pour faire de folds de la médiane du nb de piqures par point de capture
        
        
        #folds <- create_folds(th_trmetrics_entomo_postedecapture,15)
         folds <- create_folds2(th_trmetrics_entomo_postedecapture)
      
      th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
        left_join(folds)
        
      
      if(mod %in% c("physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1","exophagy")){
        th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% 
          mutate(trchehorairecapt = case_when(heuredecapture >=15 & heuredecapture <=19 ~ "17 - 19",
                                              heuredecapture > 19 & heuredecapture <=24 ~ "20 - 23",
                                              heuredecapture >=0 & heuredecapture <=3 ~ "00 - 03",
                                              heuredecapture >3 & heuredecapture <=6 ~ "04 - 06",
                                              heuredecapture >6 & heuredecapture <=10 ~ "07 - 09"
          )) %>%
          mutate(trchehorairecapt = forcats::fct_relevel(trchehorairecapt,c("17 - 19","20 - 23","00 - 03","04 - 06","07 - 09")))
      }
        
        ### selection of predictors
         if (mod %in% c("exophagy")){
            
            #predictors <- c("NMT","NMH","NML","NMA","DNMT","DNMH","DNML","WSP","RFH","LMN","VCM","VCT","VCT2","HBB","HBI","LUS","LIG30_2000","RFD1F_2000_0_30","TMIN1_2000_0_30","TMAX1_2000_0_30","TMAX1_2000_0_0","RFD1F_2000_0_0","POP","ANI","POPANI","BDE","WMD","lsm_c_pland_2000_3_9")
                    predictors <- c("NMTI","NMHI","NMLE","NMA","DNMT","DNMH","DNML","WSP","RFHP","VCM","HBB","HBI","LUS","LMN","TMAX1_2000_0_0","RFD1F_2000_0_0","BDE","BCH_2000","WMD","POP", "RFD1F_2000_0_30","TMAX1_2000_0_30","TMIN1_2000_0_30")
            
            if(response_var != "An.funestus_ss" & code_pays=="BF"){
              predictors <- c(predictors,"kdre","kdrw")
            }
             # if(code_pays=="BF"){
             #   predictors <- c(predictors,"POPANI")
             # }
            if(code_pays=="BF"){
              glmm_varstoforce <- c("VCM","VCT")
            } else if (code_pays=="CI"){
              glmm_varstoforce <- c("VCM")
            }

               # correct RFH
               th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
                 mutate(RFHP=ifelse(RFHP ==2, "Presence","Absence")) %>%
                 mutate(RFHP=ifelse(is.na(RFHP), "Absence",RFHP)) %>%
                 mutate(RFHP=fct_relevel(RFHP,"Absence","Presence"))
               
               
          } else if (mod %in% c("early_late_biting","early_biting","late_biting")){
            
            predictors <- c("VCM","LUS","TMAX1_2000_0_0","RFD1F_2000_0_0","BDE","BCH_2000","WMD","int_ext","POP","RFD1F_2000_0_30","TMAX1_2000_0_30","TMIN1_2000_0_30") #,"HBB2","HBI2"
  
            if(response_var != "An.funestus_ss" & code_pays=="BF"){
              predictors <- c(predictors,"kdre","kdrw")
            }
            if(mod == "late_biting"){
              predictors <- c(predictors,"NMT","NMH","NML","NMA","RFH","WSP")
            }
             # if(code_pays=="BF"){
             #   predictors <- c(predictors,"POPANI")
             # }
            
            if(code_pays=="BF"){
              glmm_varstoforce <- c("VCM","VCT")
            } else if (code_pays=="CI"){
              glmm_varstoforce <- c("VCM")
            }

            
            } else if(mod %in% c("physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1")){
            
            if(code_pays == "BF"){ 
              #lcid <- 3
              #th_trmetrics_entomo_postedecapture$lsm_c_pland_2000_3_1 <- rowSums(cbind(th_trmetrics_entomo_postedecapture$lsm_c_pland_2000_3_1,th_trmetrics_entomo_postedecapture$lsm_c_pland_2000_3_11),na.rm = TRUE)
              lsm_agri <- c("lsm_c_pland_2000_4_1","lsm_c_pland_2000_4_11","lsm_c_pland_2000_4_16")#c("lsm_c_pland_2000_3_1")
              
              th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
                mutate(lsm_c_pland_2000_4_16 = ifelse(lsm_c_pland_2000_4_16>0,"Presence","Absence")) %>%
                mutate(lsm_c_pland_2000_4_16=fct_relevel(lsm_c_pland_2000_4_16,"Absence","Presence")) 
              
            } else if (code_pays == "CI"){ 
                lcid <- 8
                lsm_agri <- c("lsm_c_pland_2000_8_4","lsm_c_pland_2000_8_7","lsm_c_pland_2000_8_8")
            }
            
              if(code_pays=="BF"){
                glmm_varstoforce <- c("VCM","VCT","lsm_c_pland_2000_4_1","lsm_c_pland_2000_4_11","lsm_c_pland_2000_4_16")  #"lsm_c_pland_2000_3_1"
              } else if (code_pays=="CI"){
                glmm_varstoforce <- c("VCM","VCT")
              }
            predictors <-c(lsm_agri,"VCM","HBB","HBI","MA","POP","LUS","RFD1F_2000_0_30","TMAX1_2000_0_30","TMIN1_2000_0_30")#,"NMT","NMH","NMA","NML")  #   enlever int_ext  # ajouter abondance de moustique sur la nuit de capture (si ++ de pop -> mutation favorable   cout de la mutation )   # ajouter micro-clim
            
             # if(code_pays=="BF"){
             #   predictors <- c(predictors,"POPANI")
             # }
            
            if(mod == "physiological_resistance_kdre"){
              predictors <- c(predictors,"kdrw")
            }            
            if(mod == "physiological_resistance_ace1"){
              predictors <- c(predictors,"kdrw","kdre")
            }

            # transform df to model the allel
              th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% 
                mutate(resp_var = case_when(resp_var == 1 ~ "RR",
                                            resp_var == 0.5  ~ "RS",
                                            resp_var == 0  ~ "SS")) %>%
                mutate(resp_var1 = substr(resp_var,0,1)) %>%
                mutate(resp_var2 = substr(resp_var,2,2)) %>%
                pivot_longer(c(resp_var1,resp_var2)) %>%
                mutate(resp_var = ifelse(value == "R",1,0))
 
          }
      
        all_predictors  = predictors
        
        pvals_filts <- NULL
        glmm_lrt <- NULL
        glmm_aic <- NULL
        rf <- NULL
        rf_selectvar <- NULL
        rf_allpredictors <- NULL
        
       if(response_var=="An.funestus" & code_pays=="CI"){
         th_trmetrics_entomo_postedecapture = th_trmetrics_entomo_postedecapture %>% filter(VCM %in% c("< LLIN dist.","LLIN + IRS"))
       }
        
        
        ###### univariate models ######
        # glmms
        df <- th_trmetrics_entomo_postedecapture %>% 
          mutate(pointdecapture2 = as.factor(paste0(codevillage,pointdecapture))) %>%
          dplyr::select(resp_var,codevillage,pointdecapture2,predictors) %>% 
          mutate(resp_var = as.character(resp_var)) %>%
          mutate_if(is.numeric, ~scale(.)) %>%
          mutate(resp_var = as.numeric(resp_var))
      
        if(mod %in% c("physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1","exophagy","early_late_biting","early_biting","late_biting","presence")){
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
        
        if(response_var!="An.gambiae s.l."){
          glmms_univs <- map(colnames(df[4:ncol(df)]), possible_a)
        } else {
          glmms_univs <- future_map(colnames(df[4:ncol(df)]), possible_a)
        }
        
        i <- 1
        while (i <= length(glmms_univs)){
          if(is.na(glmms_univs[[i]])){
            glmms_univs[[i]] <- NULL
          } else {
          if(is.na(summary(glmms_univs[[i]])$AICtab[1])){
            glmms_univs[[i]] <- NULL
          }
          }
          i = i+1
        }
        
        i <- 1
        while (i <= length(glmms_univs)){
          if(is.na(glmms_univs[[i]])){
            glmms_univs[[i]] <- NULL
          } else {
            if(is.na(summary(glmms_univs[[i]])$AICtab[1])){
              glmms_univs[[i]] <- NULL
            }
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
            pvals_filts <- glmms_univs %>% filter(term %in% predictors, p.value <= 0.2, !grepl("VCM",term), !grepl("trchehorairecapt",term))
    
          
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
    
          predictors_microclim <- c("NMT","NMH","NMTI","NMHI")
          predictors_microclim <- intersect(predictors_microclim,predictors)
          if(length(predictors_microclim)==0){
            predictors_microclim <- NULL
          }
          
          th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% 
            mutate(kdrw = case_when(kdrw == 1 ~ "RR",
                                    kdrw == 0.5  ~ "RS",
                                    kdrw == 0  ~ "SS")) %>% 
            mutate(kdre = case_when(kdre == 1 ~ "RR",
                                    kdre == 0.5  ~ "RS",
                                    kdre == 0  ~ "SS")) %>%
            mutate(ace1 = case_when(ace1 == 1 ~ "RR",
                                    ace1 == 0.5  ~ "RS",
                                    ace1 == 0  ~ "SS")) %>%
            mutate(kdrw = fct_relevel(kdrw, c("SS","RS","RR"))) %>%
            mutate(kdre = fct_relevel(kdre, c("SS","RS","RR"))) %>%
            mutate(ace1 = fct_relevel(ace1, c("SS","RS","RR")))
    
          pvals_filt_vcm <- glmms_univs %>% filter(p.value <= 0.2, grepl("VCM",term))
          if(nrow(pvals_filt_vcm)>0){
            predictors <- c(predictors, "VCM")
            pvals_filts <- rbind(pvals_filts, pvals_filt_vcm)
          }
          
          pvals_filt_int_ext <- glmms_univs %>% filter(p.value <= 0.2, grepl("int_ext",term))
          if(nrow(pvals_filt_int_ext)>0){
            predictors <- c(predictors, "int_ext")
            pvals_filts <- rbind(pvals_filts, pvals_filt_int_ext)
          }
          
          pvals_filt_elb <- glmms_univs %>% filter(p.value <= 0.2, grepl("ELB",term))
          if(nrow(pvals_filt_elb)>0){
            predictors <- c(predictors, "ELB")
            pvals_filts <- rbind(pvals_filts, pvals_filt_elb)
          }
          
          pvals_filt_rfh <- glmms_univs %>% filter(p.value <= 0.2, grepl("RFHP",term))
          if(nrow(pvals_filt_rfh)>0){
            predictors <- c(predictors, "RFHP")
            pvals_filts <- rbind(pvals_filts, pvals_filt_rfh)
          }
          
          pvals_filt_periodinterv <- glmms_univs %>% filter(p.value <= 0.2, grepl("period_interv",term))
          if(nrow(pvals_filt_periodinterv)>0){
            predictors <- c(predictors, "period_interv")
            pvals_filts <- rbind(pvals_filts, pvals_filt_periodinterv)
          }
          
          pvals_filt_season <- glmms_univs %>% filter(p.value <= 0.2, grepl("season",term))
          if(nrow(pvals_filt_season)>0){
            predictors <- c(predictors, "season")
            pvals_filts <- rbind(pvals_filts, pvals_filt_season)
          }
          
          
          
          # if(response_var == "An.funestus_ss" & code_pays = "BF" & mod = "exophagy"){
          #   predictors <- setdiff(predictors,c("",""))
          # }
          # if(response_var = "An.gambiae_ss" & code_pays = "BF" & mod = "exophagy"){
          #   predictors <- setdiff(predictors,c("",""))
          # }
          # if(response_var = "An.coluzzii" & code_pays = "BF" & mod = "exophagy"){
          #   predictors <- setdiff(predictors,c("",""))
          # }
          # if(response_var = "An.funestus_ss" & code_pays = "BF" & mod = "late_biting"){
          #   predictors <- setdiff(predictors,c("",""))
          # }
          # if(response_var = "An.gambiae_ss" & code_pays = "BF" & mod = "physiological_resistance_kdrw"){
          #   predictors <- setdiff(predictors,c("",""))
          # }
          # if(response_var = "An.coluzzii" & code_pays = "BF" & mod = "physiological_resistance_kdrw"){
          #   predictors <- setdiff(predictors,c("",""))
          # }
          # if(response_var = "An.gambiae_ss" & code_pays = "BF" & mod = "physiological_resistance_kdre"){
          #   predictors <- setdiff(predictors,c("",""))
          # }
          # if(response_var = "An.coluzzii" & code_pays = "BF" & mod = "physiological_resistance_kdre"){
          #   predictors <- setdiff(predictors,c("",""))
          # }
          # if(response_var = "An.gambiae_ss" & code_pays = "BF" & mod = "physiological_resistance_ace1"){
          #   predictors <- setdiff(predictors,c("",""))
          # }
          # if(response_var = "An.funestus" & code_pays = "CI" & mod = "exophagy"){
          #   predictors <- setdiff(predictors,c("",""))
          # }
          # if(response_var = "An.funestus" & code_pays = "CI" & mod = "early_biting"){
          #   predictors <- setdiff(predictors,c("",""))
          # }
          # if(response_var = "An.gambiae s.l." & code_pays = "CI" & mod = "exophagy"){
          #   predictors <- setdiff(predictors,c("",""))
          # }
          # if(response_var = "An.gambiae s.l." & code_pays = "CI" & mod = "early_biting"){
          #   predictors <- setdiff(predictors,c("",""))
          # }
          # if(response_var = "An.gambiae s.l." & code_pays = "CI" & mod = "late_biting_biting"){
          #   predictors <- setdiff(predictors,c("",""))
          # }
          # 
          tictoc::tic()
          if(length(predictors)>=1){
            glmm_aic <- fun_compute_glmm(df = th_trmetrics_entomo_postedecapture, predictors = unique(predictors), mod = mod,predictors_forced = glmm_varstoforce, crit_selection = "AIC",cv_col = NULL) # cv_col = "by_ptcapt"
             }
          tictoc::toc()
          
          # if(mod == "physiological_resistance_kdrw"){
          #   th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% mutate(resp_var = ifelse(resp_var==0.5,0,resp_var))
          # } 
          # if (mod %in% c("physiological_resistance_kdre","physiological_resistance_ace1")) {
          #   th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% mutate(resp_var = ifelse(resp_var==0.5,1,resp_var))   ###### tenter avec 1 ligne par allèle
          # }
          
          predictors <- c(predictors,glmm_varstoforce)
          
          tictoc::tic()
          
          if(code_pays=="CI" & response_var== "An.gambiae s.l."){
            tlength <- 1 #2
          } else {
            tlength <- 7
          }
          
          if(length(predictors)>2){
            rf <- fun_compute_rf(th_trmetrics_entomo_postedecapture, unique(predictors), cv_col = "by_ptcapt", mod, featureselect = "false", species = response_var,tune_length = tlength)
          }
          tictoc::toc()
          
            ##################rf_selectvar <- fun_compute_rf(th_trmetrics_entomo_postedecapture, unique(predictors), cv_col = "by_ptcapt", mod, featureselect = TRUE)
          #}
          # rf_nightscience$mod$pred %>% group_by(Resample) %>% summarise(n=n())  -> number of samples for each fold
          # rf_nightscience$mod$resample -> predictive quality for each resampling
          

          #######################all_predictors <- fun_multicol(th_trmetrics_entomo_postedecapture, all_predictors)
          #######################rf_allpredictors <- fun_compute_rf(th_trmetrics_entomo_postedecapture, all_predictors, cv_col = "by_ptcapt", mod, featureselect = FALSE)

            
           return(list(glmms_univs = glmms_univs,
                       #glmm_multiv_lrt = glmm_lrt, 
                       glmm_multiv_aic = glmm_aic, 
                       rf = rf
                       #rf_selectvar = rf_selectvar,
                       #rf_allpredictors = rf_allpredictors
                       ))
          
        }
      
        
      df_input_params_glmm <- tibble(response_var = character(), code_pays = character(), mod = character(), periodinterv = character())
      df_input_params_glmm <- df_input_params_glmm %>%
        add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "exophagy", periodinterv = "all") %>%
        add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "exophagy", periodinterv = "all") %>%
        add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "exophagy", periodinterv = "all") %>%
        #add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "early_biting", periodinterv = "all") %>%
        #add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "early_biting", periodinterv = "all") %>%
        #add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "early_biting", periodinterv = "all")%>%
        add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "late_biting", periodinterv = "all") %>%
        #add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "late_biting", periodinterv = "all") %>%
        #add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "late_biting", periodinterv = "all")  %>%
        add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "physiological_resistance_kdrw", periodinterv = "all") %>%
        add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "physiological_resistance_kdrw", periodinterv = "all") %>%
        add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "physiological_resistance_kdre", periodinterv = "all") %>%
        add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "physiological_resistance_kdre", periodinterv = "all") %>%
        add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "physiological_resistance_ace1", periodinterv = "all") %>%
        #add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "physiological_resistance_ace1", periodinterv = "all") %>%
        add_row(response_var = "ma_funestus_ss", code_pays = "CI", mod = "exophagy", periodinterv = "all") %>%
        add_row(response_var = "ma_funestus_ss", code_pays = "CI", mod = "early_biting", periodinterv = "all") %>%
        #add_row(response_var = "ma_funestus_ss", code_pays = "CI", mod = "late_biting", periodinterv = "all") %>%
        add_row(response_var = "ma_gambiae_sl", code_pays = "CI", mod = "exophagy", periodinterv = "all") %>%
        add_row(response_var = "ma_gambiae_sl", code_pays = "CI", mod = "early_biting", periodinterv = "all") %>%
        add_row(response_var = "ma_gambiae_sl", code_pays = "CI", mod = "late_biting", periodinterv = "all")

             
          th_model_results1 <- df_input_params_glmm[1,] %>%
            mutate(results = pmap(list(response_var, code_pays, mod,periodinterv), ~fun_workflow_model(..1,..2,..3,..4)))
          saveRDS(th_model_results1,"/home/ptaconet/Bureau/data_analysis/th_model_results1.rds")
          
                th_model_results2 <- df_input_params_glmm[2,] %>%
                  mutate(results = pmap(list(response_var, code_pays, mod,periodinterv), ~fun_workflow_model(..1,..2,..3,..4)))
                saveRDS(th_model_results2,"/home/ptaconet/Bureau/data_analysis/th_model_results2.rds")
                
                th_model_results3 <- df_input_params_glmm[3,] %>%
                  mutate(results = pmap(list(response_var, code_pays, mod,periodinterv), ~fun_workflow_model(..1,..2,..3,..4)))
                saveRDS(th_model_results3,"/home/ptaconet/Bureau/data_analysis/th_model_results3.rds")
                
                  th_model_results4 <- df_input_params_glmm[4,] %>%
                   mutate(results = pmap(list(response_var, code_pays, mod,periodinterv), ~fun_workflow_model(..1,..2,..3,..4)))
                  saveRDS(th_model_results4,"/home/ptaconet/Bureau/data_analysis/th_model_results4.rds")
                    
                   th_model_results5 <- df_input_params_glmm[5,] %>%
                     mutate(results = pmap(list(response_var, code_pays, mod,periodinterv), ~fun_workflow_model(..1,..2,..3,..4)))
                   saveRDS(th_model_results5,"/home/ptaconet/Bureau/data_analysis/th_model_results5.rds")
                   
                    th_model_results6 <- df_input_params_glmm[6,] %>%
                     mutate(results = pmap(list(response_var, code_pays, mod,periodinterv), ~fun_workflow_model(..1,..2,..3,..4)))
                    saveRDS(th_model_results6,"/home/ptaconet/Bureau/data_analysis/th_model_results6.rds")
                    
                  th_model_results7 <- df_input_params_glmm[7,] %>%
                    mutate(results = pmap(list(response_var, code_pays, mod,periodinterv), ~fun_workflow_model(..1,..2,..3,..4)))
                  saveRDS(th_model_results7,"/home/ptaconet/Bureau/data_analysis/th_model_results7.rds")
                  
                  th_model_results8 <- df_input_params_glmm[8,] %>%
                    mutate(results = pmap(list(response_var, code_pays, mod,periodinterv), ~fun_workflow_model(..1,..2,..3,..4)))
                  saveRDS(th_model_results8,"/home/ptaconet/Bureau/data_analysis/th_model_results8.rds")
                  
                  th_model_results9 <- df_input_params_glmm[9,] %>%
                    mutate(results = pmap(list(response_var, code_pays, mod,periodinterv), ~fun_workflow_model(..1,..2,..3,..4)))
                  saveRDS(th_model_results9,"/home/ptaconet/Bureau/data_analysis/th_model_results9.rds")
                  
                  th_model_results10 <- df_input_params_glmm[10,] %>%
                    mutate(results = pmap(list(response_var, code_pays, mod,periodinterv), ~fun_workflow_model(..1,..2,..3,..4)))
                  saveRDS(th_model_results10,"/home/ptaconet/Bureau/data_analysis/th_model_results10.rds")
                  
                  th_model_results11 <- df_input_params_glmm[11,] %>%
                    mutate(results = pmap(list(response_var, code_pays, mod,periodinterv), ~fun_workflow_model(..1,..2,..3,..4)))
                  saveRDS(th_model_results11,"/home/ptaconet/Bureau/data_analysis/th_model_results11.rds")
                    
                            th_model_results12 <- df_input_params_glmm[12,] %>%
                              mutate(results = pmap(list(response_var, code_pays, mod,periodinterv), ~fun_workflow_model(..1,..2,..3,..4)))
                            saveRDS(th_model_results12,"/home/ptaconet/Bureau/data_analysis/th_model_results12.rds")
                              
                              th_model_results13 <- df_input_params_glmm[13,] %>%
                                mutate(results = pmap(list(response_var, code_pays, mod,periodinterv), ~fun_workflow_model(..1,..2,..3,..4)))
                              saveRDS(th_model_results13,"/home/ptaconet/Bureau/data_analysis/th_model_results13.rds")
                              
                              th_model_results14 <- df_input_params_glmm[14,] %>%
                                mutate(results = pmap(list(response_var, code_pays, mod,periodinterv), ~fun_workflow_model(..1,..2,..3,..4)))
                              saveRDS(th_model_results14,"/home/ptaconet/Bureau/data_analysis/th_model_results14.rds")
                      
                    th_model_results15 <- df_input_params_glmm[15,] %>%
                      mutate(results = pmap(list(response_var, code_pays, mod,periodinterv), ~fun_workflow_model(..1,..2,..3,..4)))
                    saveRDS(th_model_results15,"/home/ptaconet/Bureau/data_analysis/th_model_results15.rds")
                    
                  th_model_results16 <- df_input_params_glmm[16,] %>%
                    mutate(results = pmap(list(response_var, code_pays, mod,periodinterv), ~fun_workflow_model(..1,..2,..3,..4)))
                  saveRDS(th_model_results16,"/home/ptaconet/Bureau/data_analysis/th_model_results16.rds")
                  
                th_model_results18 <- df_input_params_glmm[18,] %>%
                  mutate(results = pmap(list(response_var, code_pays, mod,periodinterv), ~fun_workflow_model(..1,..2,..3,..4)))
                saveRDS(th_model_results18,"/home/ptaconet/Bureau/data_analysis/th_model_results18.rds")
                
                th_model_results20 <- df_input_params_glmm[20,] %>%
                  mutate(results = pmap(list(response_var, code_pays, mod,periodinterv), ~fun_workflow_model(..1,..2,..3,..4)))
                saveRDS(th_model_results20,"/home/ptaconet/Bureau/data_analysis/th_model_results20.rds")
                
                   
                     th_model_results17 <- df_input_params_glmm[17,] %>%
                     mutate(results = pmap(list(response_var, code_pays, mod,periodinterv), ~fun_workflow_model(..1,..2,..3,..4)))
                     saveRDS(th_model_results17,"/home/ptaconet/Bureau/data_analysis/th_model_results17.rds")
                     
                   th_model_results19 <- df_input_params_glmm[19,] %>%
                     mutate(results = pmap(list(response_var, code_pays, mod,periodinterv), ~fun_workflow_model(..1,..2,..3,..4)))
                   saveRDS(th_model_results19,"/home/ptaconet/Bureau/data_analysis/th_model_results19.rds")
                   
                   
                   th_model_results21 <- df_input_params_glmm[21,] %>%
                     mutate(results = pmap(list(response_var, code_pays, mod,periodinterv), ~fun_workflow_model(..1,..2,..3,..4)))
                   saveRDS(th_model_results21,"/home/ptaconet/Bureau/data_analysis/th_model_results21.rds")
                   
 
          
          
         # model_results <- rbind(th_model_results1,th_model_results2,th_model_results3,th_model_results4,th_model_results5,th_model_results6,th_model_results7,th_model_results8,th_model_results9,th_model_results10,th_model_results11,th_model_results12,th_model_results13,th_model_results14,th_model_results15,th_model_results16,th_model_results17,th_model_results18,th_model_results19,th_model_results20,th_model_results21)#,th_model_results22,th_model_results23,th_model_results24,th_model_results25,th_model_results26,th_model_results27,th_model_results28,th_model_results29,th_model_results30,th_model_results31,th_model_results32,th_model_results33,th_model_results34,th_model_results35,th_model_results36)
       model_results <- rbind(th_model_results1,th_model_results2,th_model_results3,th_model_results4,th_model_results5,th_model_results6,th_model_results7,th_model_results8,th_model_results9,th_model_results10,th_model_results11,th_model_results12,th_model_results13,th_model_results14,th_model_results15,th_model_results16,th_model_results18,th_model_results20)#,th_model_results22,th_model_results23,th_model_results24,th_model_results25,th_model_results26,th_model_results27,th_model_results28,th_model_results29,th_model_results30,th_model_results31,th_model_results32,th_model_results33,th_model_results34,th_model_results35,th_model_results36)
                  
       
       model_results <- rbind(
             readRDS("/home/ptaconet/Bureau/data_analysis/th_model_results1.rds"),
         readRDS("/home/ptaconet/Bureau/data_analysis/th_model_results2.rds"),
         readRDS("/home/ptaconet/Bureau/data_analysis/th_model_results3.rds"),
         readRDS("/home/ptaconet/Bureau/data_analysis/th_model_results4.rds"),
         readRDS("/home/ptaconet/Bureau/data_analysis/th_model_results5.rds"),
         readRDS("/home/ptaconet/Bureau/data_analysis/th_model_results6.rds"),
         readRDS("/home/ptaconet/Bureau/data_analysis/th_model_results7.rds"),
         readRDS("/home/ptaconet/Bureau/data_analysis/th_model_results8.rds"),
         readRDS("/home/ptaconet/Bureau/data_analysis/th_model_results9.rds"),
         readRDS("/home/ptaconet/Bureau/data_analysis/th_model_results10.rds"),
         readRDS("/home/ptaconet/Bureau/data_analysis/th_model_results11.rds"),
         readRDS("/home/ptaconet/Bureau/data_analysis/th_model_results12.rds"),
         readRDS("/home/ptaconet/Bureau/data_analysis/th_model_results13.rds"),
         readRDS("/home/ptaconet/Bureau/data_analysis/th_model_results14.rds")
         # readRDS("/home/ptaconet/Bureau/data_analysis/th_model_results15.rds"),
         # readRDS("/home/ptaconet/Bureau/data_analysis/th_model_results16.rds"),
         # readRDS("/home/ptaconet/Bureau/data_analysis/th_model_results17.rds"),
         # readRDS("/home/ptaconet/Bureau/data_analysis/th_model_results18.rds"),
         # readRDS("/home/ptaconet/Bureau/data_analysis/th_model_results19.rds"),
         # readRDS("/home/ptaconet/Bureau/data_analysis/th_model_results20.rds"),
         # readRDS("/home/ptaconet/Bureau/data_analysis/th_model_results21.rds")
       )
       
      model_results <- model_results %>%
        mutate(glmms_univs = map(results, ~pluck(.,"glmms_univs"))) %>%
        #mutate(glmm_lrt = map(results, ~pluck(.,"glmm_multiv_lrt"))) %>%
        mutate(glmm_aic = map(results, ~pluck(.,"glmm_multiv_aic"))) %>%
        mutate(rf = map(results, ~pluck(.,"rf"))) %>%
        #mutate(rf_selectvar = map(results, ~pluck(.,"rf_selectvar"))) %>%
        #mutate(rf_allpredictors = map(results, ~pluck(.,"rf_allpredictors"))) %>%
        dplyr::select(-results)
      
      saveRDS(model_results,"/home/ptaconet/Bureau/data_analysis/model_results_resistances13.rds")
      
        # model_results_resistances4.rds : glmm + rf   
    # model_results_resistances5.rds : rf only
    #mode l_results_resistances6.rds  : glmm + rf (corrigé)
    #model_results_resistances7.rds  : glmm + rf (threshold pval = 0.2) BF + CI
    #model_results_resistances8.rds  : idem 7 avec les changements de Nico + CV LTO
      #model_results_resistances9.rds  : idem 8 + cv leave-idspoindecapture-out
      
      