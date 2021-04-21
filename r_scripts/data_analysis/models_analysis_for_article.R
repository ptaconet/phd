library(tidyverse)
library(patchwork)
library(DBI)
library(sjPlot) # see : https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html
library(stringr)
library(sf)
library(ggmap)
library(precrec)
library(plotROC)
library(ggsn)

### connect to the database
path_to_db <- "data/react_db/react_db.gpkg" 
react_gpkg <- DBI::dbConnect(RSQLite::SQLite(),dbname = path_to_db) 


## table of exhaustive definitions of the explanatory variables
googlesheets4::sheets_deauth()
prediction_vars <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1dIeSOa2WinXvOQGLmIjA0gFdsHnb6zMMsDME-G5pyMc/edit?usp=sharing", sheet = "var_explication", col_types="c")

### source home-made functions 
source("r_scripts/data_analysis_tests/functions_script_data_analysis.R")
source("r_scripts/data_analysis_tests/functions_models_analysis.R")


######################
###### maps and univariate analysis######
######################


res <-  readRDS("/home/ptaconet/Bureau/data_analysis/model_results_univanalysis10.rds")


model_univanalysis_results <- res %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus",
    response_var == "ma_gambiae_ss" ~ "An. gambiae s.s.",
    response_var == "ma_coluzzi" ~ "An. coluzzii",
  response_var == "ma_gambiae_sl" ~ "An. gambiae s.l.")) %>%
  mutate(spatial_corrs_spearman = map(spatial_corrs_spearman, ~fun_get_predictors_labels(table = ., vector_predictors_name = "name"))) %>%
  mutate(spatial_corrs_spearman = map(spatial_corrs_spearman, ~mutate(., buffer = word(gsub("_"," ",name), ifelse(label_group %in% c("Landscape - other","Landscape - wetlands"), 4, 2))))) %>%
  mutate(spatial_corrs_spearman = map(spatial_corrs_spearman, ~mutate(., buffer = ifelse(is.na(buffer), word(gsub("_"," ",name), 2), buffer)))) %>%
  mutate(spatial_corrs_spearman =  pmap(list(spatial_corrs_spearman, response_var, code_pays, mod), ~mutate(..1, species = ..2, country = ..3, indicator = ..4))) %>%
  mutate(temporal_corrs_spearman =  map(temporal_corrs_spearman, ~do.call(rbind.data.frame,.))) %>%
  mutate(temporal_corrs_spearman = map(temporal_corrs_spearman, ~fun_get_predictors_labels(table = ., vector_predictors_name = "var"))) %>%
  mutate(temporal_corrs_spearman =  pmap(list(temporal_corrs_spearman, response_var, code_pays, mod), ~mutate(..1, species = ..2, country = ..3, indicator = ..4))) %>%
  
  mutate(spatial_corrs_glmm = map(spatial_corrs_glmm, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(spatial_corrs_glmm = map(spatial_corrs_glmm, ~mutate(., buffer = word(gsub("_"," ",term), ifelse(label_group %in% c("Landscape - other","Landscape - wetlands"), 4, 2))))) %>%
  mutate(spatial_corrs_glmm = map(spatial_corrs_glmm, ~mutate(., buffer = ifelse(is.na(buffer), word(gsub("_"," ",term), 2), buffer)))) %>%
  mutate(spatial_corrs_glmm =  pmap(list(spatial_corrs_glmm, response_var, code_pays, mod), ~mutate(..1, species = ..2, country = ..3, indicator = ..4))) %>%
  mutate(temporal_corrs_glmm =  map(temporal_corrs_glmm, ~do.call(rbind.data.frame,.))) %>%
  mutate(temporal_corrs_glmm = map(temporal_corrs_glmm, ~fun_get_predictors_labels(table = ., vector_predictors_name = "var"))) %>%
  mutate(temporal_corrs_glmm =  pmap(list(temporal_corrs_glmm, response_var, code_pays, mod), ~mutate(..1, species = ..2, country = ..3, indicator = ..4)))
  


univ_spearman_spatial <- do.call(rbind.data.frame, model_univanalysis_results$spatial_corrs_spearman) %>% 
      # filter(buffer %in% c(NA,250,500,1000,2000), name %in% c("WMD","BDE","BCH","WLS_2000","lsm_c_pland_2000_2_5","lsm_c_pland_2000_3_3","lsm_c_pland_2000_3_2","lsm_c_pland_2000_3_4","lsm_c_pland_2000_3_9","lsm_c_pland_2000_3_1","lsm_c_pland_2000_3_5",
      #                                                     "WLS_1000","lsm_c_pland_1000_2_5","lsm_c_pland_1000_3_3","lsm_c_pland_1000_3_2","lsm_c_pland_1000_3_4","lsm_c_pland_1000_3_9","lsm_c_pland_1000_3_1","lsm_c_pland_1000_3_5",
      #                                                     "WLS_500","lsm_c_pland_500_2_5","lsm_c_pland_500_3_3","lsm_c_pland_500_3_2","lsm_c_pland_500_3_4","lsm_c_pland_500_3_9","lsm_c_pland_500_3_1","lsm_c_pland_500_3_5",
      #                                                     "WLS_250","lsm_c_pland_250_2_5","lsm_c_pland_250_3_3","lsm_c_pland_250_3_2","lsm_c_pland_250_3_4","lsm_c_pland_250_3_9","lsm_c_pland_250_3_1","lsm_c_pland_250_3_5")) %>%
  mutate(model = "spearman univariate") %>%
  mutate(indicator = ifelse(indicator == "presence","Presence","Abundance")) %>%
  mutate(buffer = ifelse(is.na(buffer),"2000",buffer)) %>%
  dplyr::rename(pval = p) %>%
  mutate(correlation = ifelse(pval >= 0.2 | abs(correlation)<=0.1, NA, correlation)) %>%
  #nest(-c(country))
  nest(-c(country))


univ_glmm_spatial <- do.call(rbind.data.frame, model_univanalysis_results$spatial_corrs_glmm) %>% 
  mutate(model = "glmm univariate") %>%
  mutate(indicator = ifelse(indicator == "presence","Presence","Abundance")) %>%
  mutate(buffer = ifelse(is.na(buffer),"2000",buffer)) %>%
  dplyr::rename(pval = p.value, correlation = estimate) %>%
  mutate(correlation = ifelse(pval >= 0.2, NA, correlation)) %>%
  nest(-c(country))


univ_spearman_temporal <-  do.call(rbind.data.frame, model_univanalysis_results$temporal_corrs_spearman) %>% 
  mutate(r= ifelse(r>0.7,0.70,r)) %>%
  filter(var %in% c("RFD1F", "TMIN1", "TMAX1")) %>%
  mutate(correlation = ifelse(p<=0.2 | correlation >= 0.1,correlation,NA) ) %>%
  mutate(time_lag_1=ifelse(grepl("1",name),time_lag_1/7,time_lag_1),time_lag_2=ifelse(grepl("1",name),time_lag_2/7,time_lag_2),diff_lag=ifelse(grepl("1",name),diff_lag/7,diff_lag)) %>%
  mutate(model = "spearman univariate") %>%
  nest(-c(indicator,country,species,var))


univ_glmm_temporal <-  do.call(rbind.data.frame, model_univanalysis_results$temporal_corrs_glmm) %>% 
  filter(var %in% c("RFD1F", "TMIN1", "TMAX1")) %>%
  rename(correlation = estimate) %>%
  mutate(correlation = ifelse(p.value<=0.2,correlation,NA) ) %>%
  mutate(time_lag_1=ifelse(grepl("1",term),time_lag_1/7,time_lag_1),time_lag_2=ifelse(grepl("1",term),time_lag_2/7,time_lag_2),diff_lag=ifelse(grepl("1",term),diff_lag/7,diff_lag)) %>%
  mutate(model = "glmm univariate") %>%
  nest(-c(indicator,country,species,var))

#### spatial univariate


plots_univ_spearman_spatial <- univ_spearman_spatial %>%
  mutate(univ_spatial = pmap(list(data, country), ~fun_plot_tile_univ_spatial(df = ..1,country = ..4))) %>%
  dplyr::select(-data)

plots_univ_glmm_spatial <- univ_glmm_spatial %>%
  mutate(univ_spatial = pmap(list(data, country), ~fun_plot_tile_univ_spatial(df = ..1,country = ..4, metric_name = "glmm"))) %>%
  dplyr::select(-data)


###### temporal univariate

plots_univ_spearman_temporal <- univ_spearman_temporal %>%
  arrange(rev(indicator),species,var) %>%
  mutate(univ_temporal = pmap(list(data, indicator), ~fun_ccm_plot2(..1, ..1$label[1], time_frame = 1, indicator = ..2, metric_name = "Spearman"))) %>%
  mutate(species = fct_relevel(species,c("An. funestus","An. gambiae s.s.","An. gambiae s.l.","An. coluzzii"))) %>%
  arrange(species) %>%
  nest(-c(indicator,country)) %>%
  mutate(univ_temporal = map2(data,country, ~wrap_plots(.x$univ_temporal, nrow = ifelse(..2 == "BF",3,2), ncol = 3))) %>%  ## mettre nrow = 2 pour CI
  #mutate(univ_temporal = pmap(list(univ_temporal,species,country,indicator), ~..1 + plot_annotation(title = paste(..2,..3,..4, sep = " - "), subtitle = paste("CCM using spearman coefficient"), caption = "Only significant results (pval <= 0.05) are displayed (non-signficant results are greyed out)"))) %>%
  dplyr::select(-data)



plots_univ_glmm_temporal <- univ_glmm_temporal %>%
  arrange(rev(indicator),species,var) %>%
  mutate(univ_temporal = pmap(list(data,indicator), ~fun_ccm_plot2(..1, ..1$label[1], time_frame = 1, indicator = ..2, metric_name = "glmm"))) %>%
  mutate(species = fct_relevel(species,c("An. funestus","An. gambiae s.s.","An. gambiae s.l.","An. coluzzii"))) %>%
  arrange(species) %>%
  nest(-c(indicator,country)) %>%
  mutate(univ_temporal = map2(data,country, ~wrap_plots(.x$univ_temporal, nrow = ifelse(..2 == "BF",3,2), ncol = 3))) %>%  ## mettre nrow = 2 pour CI
  #mutate(univ_temporal = pmap(list(univ_temporal,species,country,indicator), ~..1 + plot_annotation(title = paste(..2,..3,..4, sep = " - "), subtitle = paste("CCM using spearman coefficient"), caption = "Only significant results (pval <= 0.05) are displayed (non-signficant results are greyed out)"))) %>%
  dplyr::select(-data)


# BF : 
wrap_plots(plots_univ_spearman_temporal$univ_temporal[1][[1]],plots_univ_spearman_temporal$univ_temporal[2][[1]], ncol = 2, nrow = 1) 

# CI : 
wrap_plots(plots_univ_spearman_temporal$univ_temporal[3][[1]],plots_univ_spearman_temporal$univ_temporal[4][[1]], ncol = 2, nrow = 1) 

######################
###### multivariate
######################
  
## pdps
pdps <- res %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(rf_plots = pmap(list(rf_llo,mod,response_var), ~fun_plot_pdp2(..1,..2,..3)))

plot_fun <- wrap_plots(pdps$rf_plots[[1]]$pdps, pdps$rf_plots[[4]]$pdps, nrow = 2, ncol = 1) # funestus
plot_gam <-wrap_plots(pdps$rf_plots[[2]]$pdps, pdps$rf_plots[[5]]$pdps, nrow = 2, ncol = 1) # gambiae ss.
plot_col <-wrap_plots(pdps$rf_plots[[3]]$pdps, pdps$rf_plots[[6]]$pdps, nrow = 2, ncol = 1) # coluzzii

ggsave("figure5.png",plot_fun, path = "/home/ptaconet/phd/figures_article1", width = 155,height = 210,units = "mm", dpi = 600)
ggsave("figure6.png",plot_gam, path = "/home/ptaconet/phd/figures_article1", width = 155,height = 210,units = "mm", dpi = 600)
ggsave("figure7.png",plot_col, path = "/home/ptaconet/phd/figures_article1", width = 155,height = 210,units = "mm", dpi = 600)

#wrap_plots(pdps$rf_plots_lto[[1]]$plot_interactions, pdps$rf_plots_lto[[2]]$plot_interactions, pdps$rf_plots_lto[[3]]$plot_interactions, pdps$rf_plots_lto[[4]]$plot_interactions, pdps$rf_plots_lto[[5]]$plot_interactions, pdps$rf_plots_lto[[6]]$plot_interactions, nrow = 3, ncol = 2, byrow = F)  + plot_layout(guides = 'collect')

# plots for models validation
model_validation <- res %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(df_cv = map(rf_llo, ~pluck(.,"df_cv"))) %>%
  dplyr::select(response_var, code_pays, mod, df_cv) %>%
  mutate(df_cv = map(df_cv, ~dplyr::select(., pred,obs, codevillage, nummission, pointdecapture, int_ext))) %>%
  mutate(df_cv = map2(df_cv,response_var, ~mutate(.x, species = .y))) %>%
  nest(-c(mod,code_pays))%>%
  mutate(df_cv2 = map(data, ~do.call(rbind.data.frame, .$df_cv))) %>%
  mutate(df_val = map2(df_cv2,mod, ~fun_prepare_df_perf(.x,.y))) %>%
  mutate(perf_metric = map2(df_cv2,mod,~fun_compute_perf_metric(..1,..2))) %>%
  mutate(plot_validation = pmap(list(df_val,perf_metric,mod), ~pmap(list(..1$data,..1$species,..2$data,..3), ~fun_plot_validation(..1,..2,..3,..4))))


# presence (courbes AUC)
 plots_validation_presence <- model_validation$df_cv2[[1]] %>%  # AUC plots
   nest(-c(species)) %>%
   mutate(plots_validation = map2(data, species, ~fun_plot_validation_presence(..1,..2)))


 
 # abundance (mean absolute error)
 plots_validation_abundance <- model_validation$df_cv2[[2]] %>% 
   nest(-c(species)) %>%
   mutate(plots_validation = map2(data, species, ~fun_plot_validation_abundance(..1,..2)))
 
 
wrap_plots(list(plots_validation_presence$plots_validation[[1]],
                 plots_validation_presence$plots_validation[[2]],
                 plots_validation_presence$plots_validation[[3]],
                 model_validation$plot_validation[[1]][[2]],
                 model_validation$plot_validation[[1]][[3]],
                 model_validation$plot_validation[[1]][[1]]),
            nrow = 2, ncol = 3) + plot_layout(guides = 'collect', heights = c(1, 2))



wrap_plots(list(plots_validation_abundance$plots_validation[[1]],
                 plots_validation_abundance$plots_validation[[2]],
                 plots_validation_abundance$plots_validation[[3]], 
                 model_validation$plot_validation[[2]][[2]],
                 model_validation$plot_validation[[2]][[3]],
                 model_validation$plot_validation[[2]][[1]]),
            nrow = 2, ncol = 3) + plot_layout(guides = 'collect', heights = c(1, 2))
 
 
######################
###### resistances
######################


res <-  readRDS("/home/ptaconet/Bureau/data_analysis/model_results_resistances7.rds")


to_rm <- NULL
for(i in 1:nrow(res)){
  if(is.null(res[i,]$rf_nightscience[[1]])){
    to_rm <- c(to_rm,i) 
  }
}
res <- res[-to_rm,]



## glmm + RF
glmm_plots <- res %>% 
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(glmm_multiv = map2(glmm, mod, ~broom.mixed::tidy(.x$mod@model, conf.int = TRUE, exponentiate = ifelse(.y == "abundance", FALSE, TRUE)))) %>%
  mutate(glmm_multiv = map(glmm_multiv, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(glmm_multiv_auc = map(glmm, ~MLmetrics::AUC(.$df_cv_llo$pred, .$df_cv_llo$obs))) %>%
  mutate(glmm_univ = map(glmms_univs, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(glmm_multiv_plot = pmap(list(glmm_multiv,mod,response_var,period_interv,glmm_multiv_auc), ~fun_glmm_dotpoint(..1,..2,..3,..4,..5,"multivariate"))) %>%
  mutate(glmm_univ_plot = pmap(list(glmm_univ,mod,response_var,period_interv), ~fun_glmm_dotpoint(..1,..2,..3,..4,NA,"univariate"))) %>%
  mutate(rf_nightscience_plots = pmap(list(rf_nightscience,mod,response_var,period_interv,glmm_univ_plot,glmm_multiv_plot), ~fun_plot_pdp2(..1,..2,..3,..4,"nightscience",..5,..6)))





glmm_plots <- res %>% 
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(glmm_multiv = map2(glmm, mod, ~broom.mixed::tidy(.x$mod@model, conf.int = TRUE, exponentiate = ifelse(.y == "abundance", FALSE, TRUE)))) %>%
  mutate(glmm_multiv = map(glmm_multiv, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(glmm_multiv_auc = map(glmm, ~MLmetrics::AUC(.$df_cv_llo$pred, .$df_cv_llo$obs))) %>%
  mutate(glmm_univ = map(glmms_univs, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(glmm_multiv_plot = pmap(list(glmm_multiv,mod,response_var,period_interv,glmm_multiv_auc), ~fun_glmm_dotpoint(..1,..2,..3,..4,..5,"multivariate"))) %>%
  mutate(glmm_univ_plot = pmap(list(glmm_univ,mod,response_var,period_interv), ~fun_glmm_dotpoint(..1,..2,..3,..4,NA,"univariate"))) %>%
  mutate(rf_nightscience_plots = pmap(list(rf_nightscience,mod,response_var,period_interv,glmm_univ_plot,glmm_multiv_plot), ~fun_plot_pdp2(..1,..2,..3,..4,"nightscience",..5,..6)))










## pdps
pdps_resistances <- res %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
 # mutate(rf_dayscience_plots = pmap(list(rf_dayscience ,mod,response_var,period_interv), ~fun_plot_pdp2(..1,..2,..3,..4,"dayscience"))) %>%
  mutate(rf_nightscience_plots = pmap(list(rf_nightscience,mod,response_var,period_interv), ~fun_plot_pdp2(..1,..2,..3,..4,"nightscience")))


pmap(list(pdps_resistances$rf_dayscience_plots,pdps_resistances$response_var,pdps_resistances$code_pays,pdps_resistances$mod,pdps_resistances$period_interv),~ggsave(paste(..2,..3,..4,..5,"dayscience",sep="_"),..1$pdps,"png",paste0("plots_resistance2/",..4)))
pmap(list(pdps_resistances$rf_nightscience_plots,pdps_resistances$response_var,pdps_resistances$code_pays,pdps_resistances$mod,pdps_resistances$period_interv),~ggsave(paste(..2,..3,..4,..5,"nightscience",sep="_"),..1$pdps,"png",paste0("plots_resistance2/",..4)))




# plots for models validation
model_validation <- res %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(df_cv = map(rf_nightscience, ~pluck(.,"df_cv"))) %>%
  dplyr::select(response_var, code_pays, mod, period_interv, df_cv) %>%
  mutate(df_cv = map(df_cv, ~dplyr::select(., pred,obs, codevillage, nummission, pointdecapture, int_ext))) %>%
  mutate(df_cv = map2(df_cv,response_var, ~mutate(.x, species = .y))) %>%
  nest(-c(mod,code_pays,period_interv))%>%
  mutate(df_cv2 = map(data, ~do.call(rbind.data.frame, .$df_cv))) %>%
  mutate(df_val = map2(df_cv2,mod, ~fun_prepare_df_perf(.x,.y))) %>%
  mutate(perf_metric = map2(df_cv2,mod,~fun_compute_perf_metric(..1,..2))) %>%
  mutate(plot_validation = pmap(list(df_val,perf_metric,mod), ~pmap(list(..1$data,..1$species,..2$data,..3), ~fun_plot_validation(..1,..2,..3,..4))))






## glmm
glmm_plots <- res %>% 
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(glmm_multiv = map2(glmm, mod, ~broom.mixed::tidy(.x$mod@model, conf.int = TRUE, exponentiate = ifelse(.y == "abundance", FALSE, TRUE)))) %>%
  mutate(glmm_multiv = map(glmm_multiv, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(glmm_multiv_auc = map(glmm, ~MLmetrics::AUC(.$df_cv_llo$pred, .$df_cv_llo$obs))) %>%
  mutate(glmm_univ = map(glmms_univs, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(glmm_multiv_plot = pmap(list(glmm_multiv,mod,response_var,period_interv,glmm_multiv_auc), ~fun_glmm_dotpoint(..1,..2,..3,..4,..5,"multivariate"))) %>%
  mutate(glmm_univ_plot = pmap(list(glmm_univ,mod,response_var,period_interv), ~fun_glmm_dotpoint(..1,..2,..3,..4,NA,"univariate")))
 

pmap(list(glmm_plots$glmm_univ_plot,glmm_plots$glmm_multiv_plot,glmm_plots$response_var,glmm_plots$code_pays,glmm_plots$mod,glmm_plots$period_interv),~ggsave(paste(..3,..4,..5,..6,"glmm",sep="_"),wrap_plots(..1, ..2, nrow = 1, ncol = 2) ,"png",paste0("plots_resistance2/",..5)))


# get rsquared from the multivariate models
df_mod=res$glmm[[1]]$mod@model$frame
r2 = MuMIn::r.squaredGLMM(res$glmm[[1]]$mod@model)
