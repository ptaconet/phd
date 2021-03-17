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
                               lag_time_window = c(0,30),
                               buffer_sizes = c(2000)){
  
  cat("Executing workflow for parameters : ", response_var, code_pays, mod,"\n")
  
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
  hum_behav_4_earlylatebiting = hum_behav[[3]]
  rm(hum_behav)
  
  # load time since vector control measure
  time_since_vc <- load_time_since_vc(code_pays, entomo_csh_metadata_l1)
  
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
       mutate(ELB = case_when(HBB > 50 ~ "nocturnal",
                              HBB < 50 & heuredecapture > 15 ~ "early_biting",
                              HBB < 50 & heuredecapture < 10 ~ "late_biting")) %>%
       dplyr::select(idpointdecapture,heuredecapture,ELB)
    
     th_trmetrics_entomo_postedecapture$ELB <- ELB$ELB
     
    # add exophagy (column EXO)
    th_trmetrics_entomo_postedecapture$EXO <- th_trmetrics_entomo_postedecapture$postedecapture
    
    # fill missing values KDR and ACE1
    th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
      mutate(kdrw = ifelse(is.na(kdrw),"RS",kdrw)) %>%
      mutate(kdre = ifelse(is.na(kdre),"RS",kdre)) %>%
      mutate(ace1 = ifelse(is.na(ace1),"RS",ace1))
      
    if(mod %in% c("early_late_biting","early_biting","late_biting")){
      
      th_env_nightcatch$RFH <- th_env_nightcatch$WSP <- NULL
      
      if(mod == "early_biting"){
        th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
          filter(ELB != "late_biting") %>%
          mutate(resp_var = ifelse(ELB == "early_biting",1,0)) 
      }
      if(mod == "late_biting"){
        th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
          filter(ELB != "early_biting") %>%
          mutate(resp_var = ifelse(ELB == "late_biting",1,0)) 
      }
      if(mod == "early_late_biting"){
        th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
          mutate(resp_var = ifelse(ELB != "nocturnal",1,0)) 
      }
      
        th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
          left_join(hum_behav_4_exophagy)
      
    } else if (mod=="exophagy"){
      
      th_env_nightcatch$RFH <- th_env_nightcatch$WSP <- NULL
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
        mutate(resp_var = PHY_ace1)
    }
    
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
    left_join(popani) %>%
    left_join(LUS) %>%
    left_join(time_since_vc) %>%
    mutate(WSP = ifelse(postedecapture == "i",0,WSP), RFH = ifelse(postedecapture == "i",0,RFH)) %>%
    mutate(VCM = case_when(VCM == "Ctrle" ~ "LLIN",
                           VCM == "IEC" ~ "LLIN + IEC",
                           VCM == "IRS" ~ "LLIN + IRS",
                           VCM == "IVM" ~ "LLIN + IVM")) %>%
    mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture[, colSums(is.na(th_trmetrics_entomo_postedecapture)) != nrow(th_trmetrics_entomo_postedecapture)]
  
  a = th_trmetrics_entomo_postedecapture %>% 
    group_by(nummission,codevillage) %>% 
    summarise(n=n()) %>%
    mutate(mission_village = ifelse(n>=10,paste0(codevillage,nummission),"all_other")) %>%
    dplyr::select(nummission,codevillage,mission_village)
  
  th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>%
    left_join(a)
  
  ### selection of predictors
   if (mod %in% c("exophagy","early_late_biting","early_biting","late_biting")){
      
      predictors <- c("NMT","NMH","NML","NMA","DNMT","DNMH","DNML","WSP","RFH","LMN","VCM","VCT","VCT2","HBB","HBI","LUS","LIG30_2000","RFD1F_2000_0_30","TMIN1_2000_0_30","TMAX1_2000_0_30","TMAX1_2000_0_0","RFD1F_2000_0_0","POP","ANI","POPANI","BDE","WMD","lsm_c_pland_2000_3_9")
      glmm_varstoforce <- c("VCM")
      
      if(response_var != "An.funestus_ss"){
        predictors <- c(predictors,"kdre","kdrw")
      }
      
    } else if(mod %in% c("physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1")){
      
      if(code_pays == "BF"){ 
        lcid <- 2 
        lsm_agri <- c("lsm_c_pland_2000_3_1","lsm_c_pland_2000_3_11")
      } else if (code_pays == "CI"){ 
          lcid <- 7 
          lsm_agri <- c("lsm_c_pland_2000_8_4","lsm_c_pland_2000_8_7","lsm_c_pland_2000_8_8")
      }
      
      predictors <- c("VCM","int_ext",lsm_agri)
      glmm_varstointegrate <- NULL
      glmm_varstoforce <- c("VCM")
      predictors_nightscience <- c("ELB","WSP","NMA","NMT","NMH","NML","LMN","RFH",paste0(c("RFD1F","TMIN1","TMAX1","TAMP1","SMO1"),"_2000_0_28"), colnames(th_trmetrics_entomo_postedecapture[which(grepl(paste0("lsm_c_pland_2000_",lcid,"|lsm_l_prd_2000_",lcid,"|lsm_l_shdi_2000_",lcid),colnames(th_trmetrics_entomo_postedecapture)))]))
    
    }
  
    
  ###### univariate models ######
  # glmms
  df <- th_trmetrics_entomo_postedecapture %>% 
    mutate(pointdecapture2 = as.factor(paste0(codevillage,pointdecapture))) %>%
    dplyr::select(resp_var,codevillage,pointdecapture2,predictors) %>% 
    mutate(resp_var = as.character(resp_var)) %>%
    mutate_if(is.numeric, ~scale(.)) %>%
    mutate(resp_var = as.numeric(resp_var))

  if(mod %in% c("physiological_resistance_kdrw","physiological_resistance_kdre","exophagy","early_late_biting","early_biting","late_biting","presence")){
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
  
  i <- 1
  while (i <= length(glmms_univs)){
    if(is.na(summary(glmms_univs[[i]])$AICtab[1])){
      glmms_univs[[i]] <- NULL
    }
    i = i+1
  }
  
    glmms_univs <- glmms_univs %>%
      purrr::map(.,~broom.mixed::tidy(., conf.int = TRUE, exponentiate = ifelse(mod == "abundance",FALSE,TRUE))) %>%
      do.call(rbind.data.frame, .) %>%
      filter(effect == "fixed" & term!="(Intercept)")

  
    ###### GLMM multivariate model  ######

    #### based on glmm pval univariate analysis
    pvals_filts <- glmms_univs %>% filter(term %in% predictors, p.value <= 0.5, !grepl("VCM",term))
    
    
  # multicollinearity among predictors
    vars_multiv <- pvals_filts$term
    if(nrow(pvals_filts)>1){
      vars_multiv <- fun_multicol(th_trmetrics_entomo_postedecapture, pvals_filts$term)
    }

    
    predictors <- fun_multicol(th_trmetrics_entomo_postedecapture, vars_multiv)
    
    ##  GLMM 
    glmm <- fun_compute_glmm(th_trmetrics_entomo_postedecapture, predictors, predictors_forced = glmm_varstoforce, mod)

    ## RF
    rf <- fun_compute_rf(th_trmetrics_entomo_postedecapture, predictors, cv_type = "llto2", mod, featureselect = FALSE)
    rf_selectvar <- fun_compute_rf(th_trmetrics_entomo_postedecapture, predictors, cv_type = "llto2", mod, featureselect = TRUE)
    
    
    
    # importance
    # randomForest::varImpPlot(rf_dayscience$mod$fit, type = 1, scale = F)
    # mod <- Predictor$new(rf_dayscience$mod$fit, data = rf_dayscience$df_mod)
    # ia <- Interaction$new(mod, feature = "lsm_c_pland_2000_3_12")
    # partial(rf_dayscience$mod, pred.var = c("lsm_c_pland_2000_3_12", "RFD1F_2000_0_28"), plot = TRUE, 
    #        chull = TRUE, train = rf_dayscience$df_mod, prob = TRUE, plot.engine = "ggplot2",
    #        palette = "magma")
    
    
     return(list(glmms_univs = glmms_univs,
                 glmm = glmm, 
                 rf = rf,
                 rf_selectvar = rf_selectvar))
    
  }

  
  
  
df_input_params_glmm <- tibble(response_var = character(), code_pays = character(), mod = character())
df_input_params_glmm <- df_input_params_glmm %>%
  add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "exophagy") %>%
  add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "exophagy") %>%
  add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "exophagy") %>%
  add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "early_late_biting") %>%
  add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "early_late_biting") %>%
  add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "early_late_biting")%>%
  add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "early_biting") %>%
  add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "early_biting") %>%
  add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "early_biting")%>%
  add_row(response_var = "ma_funestus_ss", code_pays = "BF", mod = "late_biting") %>%
  add_row(response_var = "ma_gambiae_ss", code_pays = "BF", mod = "late_biting") %>%
  add_row(response_var = "ma_coluzzi", code_pays = "BF", mod = "late_biting")
  

th_model_results1 <- df_input_params_glmm[1,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))

th_model_results2 <- df_input_params_glmm[2,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))

th_model_results3 <- df_input_params_glmm[3,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))

th_model_results4 <- df_input_params_glmm[4,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))

th_model_results5 <- df_input_params_glmm[5,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))

th_model_results6 <- df_input_params_glmm[6,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))

th_model_results7 <- df_input_params_glmm[7,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))

th_model_results8 <- df_input_params_glmm[8,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))

th_model_results9 <- df_input_params_glmm[9,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))

th_model_results10 <- df_input_params_glmm[10,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))

th_model_results11 <- df_input_params_glmm[11,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))

th_model_results12 <- df_input_params_glmm[12,] %>%
  mutate(results = pmap(list(response_var, code_pays, mod), ~fun_workflow_model(..1,..2,..3)))

model_results <- rbind(th_model_results1,th_model_results2,th_model_results3,th_model_results4,th_model_results5,th_model_results6,th_model_results7,th_model_results8,th_model_results9,th_model_results10,th_model_results11,th_model_results12)

model_results <- model_results %>%
  mutate(glmms_univs = map(results, ~pluck(.,"glmms_univs"))) %>%
  mutate(glmm = map(results, ~pluck(.,"glmm"))) %>%
  mutate(rf = map(results, ~pluck(.,"rf"))) %>%
  mutate(rf_selectvar = map(results, ~pluck(.,"rf_selectvar"))) %>%
  dplyr::select(-results)

saveRDS(model_results,"/home/ptaconet/Bureau/data_analysis/model_results_resistances.rds")

  