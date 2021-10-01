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
  nest(-c(country))


univ_glmm_spatial <- do.call(rbind.data.frame, model_univanalysis_results$spatial_corrs_glmm) %>% 
  mutate(model = "glmm univariate") %>%
  mutate(indicator = ifelse(indicator == "presence","Presence","Abundance")) %>%
  mutate(indicator2 = indicator) %>%
  mutate(buffer = ifelse(is.na(buffer),"2000",buffer)) %>%
  dplyr::rename(pval = p.value, correlation = estimate) %>%
  mutate(correlation = ifelse(pval >= 0.2, NA, correlation)) %>%
  nest(-c(country,indicator2))


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
  mutate(univ_spatial = pmap(list(data, country), ~fun_plot_tile_univ_spatial(df = ..1,country = ..2))) %>%
  dplyr::select(-data)

plots_univ_glmm_spatial <- univ_glmm_spatial %>%
  mutate(univ_spatial = pmap(list(data, country, indicator2), ~fun_plot_tile_univ_spatial(df = ..1,country = ..2, metric_name = "glmm", indicator2 = ..3))) %>%
  dplyr::select(-data)

plots_univ_glmm_spatial$univ_spatial[[1]] + plots_univ_glmm_spatial$univ_spatial[[2]] # BF
plots_univ_glmm_spatial$univ_spatial[[3]] + plots_univ_glmm_spatial$univ_spatial[[4]] # CI

plots_univ_spearman_spatial$univ_spatial[[1]] #BF
plots_univ_spearman_spatial$univ_spatial[[2]] #CI

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
#wrap_plots(plots_univ_spearman_temporal$univ_temporal[1][[1]],plots_univ_spearman_temporal$univ_temporal[2][[1]], ncol = 2, nrow = 1) 
wrap_plots(plots_univ_glmm_temporal$univ_temporal[1][[1]],plots_univ_glmm_temporal$univ_temporal[2][[1]], ncol = 2, nrow = 1) 

# CI : 
#wrap_plots(plots_univ_spearman_temporal$univ_temporal[3][[1]],plots_univ_spearman_temporal$univ_temporal[4][[1]], ncol = 2, nrow = 1) 
wrap_plots(plots_univ_glmm_temporal$univ_temporal[3][[1]],plots_univ_glmm_temporal$univ_temporal[4][[1]], ncol = 2, nrow = 1) 


######################
###### multivariate
######################
  
## pdps
pdps <- res %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_gambiae_sl" ~ "An. gambiae sl.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(rf_plots = pmap(list(rf,mod,response_var), ~fun_plot_pdp2(..1,..2,..3)))

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
    response_var %in% c("ma_gambiae_ss","ma_gambiae_sl") ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii",
    response_var == "ma_an" ~ "all Anopheles")) %>%
  mutate(df_cv = map(rf, ~pluck(.,"df_cv"))) %>%
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

  res <-  readRDS("/home/ptaconet/Bureau/data_analysis/model_results_resistances20.rds") %>%
    mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii",
    response_var == "ma_gambiae_sl" ~ "An. gambiae s.l."))
    
  ## model performance
  fun_plot_mod_perf <- function(rf,glmm,response_var,code_pays){
    df_rf <- rf$df_cv
        df_mod <- glmm$mod@model$frame
    df_mod$pred <- predict(glmm$mod@model, type = 'response', newdata = df_mod)
    df_mod$obs <- df_mod$resp_var
    df_glmm <- df_mod
    
    df_rf <- df_rf %>% dplyr::select(pred, obs,codevillage) %>% mutate(model = "GLMM") %>%
      mutate(obs = ifelse(obs==0, "Absence", "Presence"))
    df_glmm <- df_glmm %>% dplyr::select(pred, obs,codevillage) %>% mutate(model = "RF") %>%
      mutate(obs = ifelse(obs==0, "Absence", "Presence"))
    
  
    p_glmm <- ggplot(df_glmm, aes(x = obs, y = pred)) + geom_boxplot(outlier.shape = NA, colour = "#E69F00") + geom_jitter(width = 0.25, size =.4, colour = "#E69F00", alpha = 0.3) + theme_bw() + ylim(c(0,1)) + ggtitle("GLMM") + xlab("observed") + ylab("predicted probability")  
  
    p_rf <- ggplot(df_rf, aes(x = obs, y = pred)) + geom_boxplot(outlier.shape = NA, colour = "#009E73") + geom_jitter(width = 0.25, size =.4, colour = "#009E73", alpha = 0.3) + theme_bw() + ylim(c(0,1)) + ggtitle("RF") + xlab("observed") + ylab("predicted probability") 
    
    p <- p_glmm + p_rf +  plot_annotation(title = paste0(code_pays, " - ", response_var ))
    
    return(p)
  }
  
  res <- res %>%
    mutate(code_pays2 = ifelse(code_pays=="CI","IC","BF")) %>%
    mutate(mod2 = case_when(mod=="exophagy" ~ 'Exophagy',
                           mod=='late_biting' ~ 'Late biting',
                           mod=='early_biting' ~ 'Early biting',
                           mod=='physiological_resistance_kdrw' ~ 'Kdr-w',
                           mod=='physiological_resistance_kdre' ~ 'Kdr-e',
                           mod=='physiological_resistance_ace1' ~ 'Ace-1')) %>%
    mutate(mod_pred = pmap(list(rf,glmm_aic,response_var,code_pays2), ~fun_plot_mod_perf(..1,..2,..3,..4)))
  
  pmap(list(res$mod_pred,res$response_var,res$code_pays,res$mod2),~ggsave(paste(..2,..3,..4,sep="_"),..1,"png",paste0("articles/article2/mod_evaluation/",..4),width = 7.68, height = 3.87))
  


## r squared and auc
df_perf_mod <- data.frame(species = character(), code_pays = character(), mod = character(), auc = numeric(), rsq = numeric())
for(i in 1:nrow(res)){
  
  auc_rf = MLmetrics::AUC(y_true = res$rf[[i]]$df_cv$obs, y_pred =res$rf[[i]]$df_cv$pred )
  df_mod = res$glmm_aic[[i]]$mod@model$frame
  r2 = MuMIn::r.squaredGLMM(res$glmm_aic[[i]]$mod@model)[1,1]
  
  auc_rf = round(auc_rf,2)
  r2 = round(r2,2)
  
  df_perf_mod <- add_row(df_perf_mod,species=res$response_var[i],code_pays=res$code_pays[i],mod=res$mod[i],auc=auc_rf,rsq=r2)
  
}

df_perf_mod$species <- gsub("An. gambiae ss.","An. gambiae s.s.",df_perf_mod$species)

df_perf_mod2 <- df_perf_mod %>%
  mutate(code_pays = ifelse(code_pays=="CI","IC","BF")) %>%
  mutate(mod = case_when(mod=="exophagy" ~ 'Exophagy',
                         mod=='late_biting' ~ 'Late biting',
                         mod=='early_biting' ~ 'Early biting',
                         mod=='physiological_resistance_kdrw' ~ 'Kdr-w',
                         mod=='physiological_resistance_kdre' ~ 'Kdr-e',
                         mod=='physiological_resistance_ace1' ~ 'Ace-1')) %>%
  mutate(mod = fct_relevel(mod,c("Exophagy","Early biting","Late biting","Kdr-w","Kdr-e","Ace-1"))) %>%
  #pivot_longer(c(auc  ,rsq)) %>%
  mutate(species_pays = paste0(code_pays,"\n",species))

p1 = ggplot(df_perf_mod2, aes(x=species_pays, y = auc)) + geom_col(fill = "#009E73", position = position_dodge2(width = 0.8, preserve = "single")) + ylim(c(0,1)) + theme_light() + facet_grid(.~mod , scales="free",space = "free") + theme(axis.title.x = element_blank()) + geom_hline(aes(yintercept = 0.5),linetype = "dashed", color = "darkred", size = 0.3)  + geom_hline(aes(yintercept = 0.6),linetype = "dashed", color = "darkred", size = 0.3)   + geom_hline(aes(yintercept = 0.7),linetype = "dashed", color = "darkred", size = 0.3)     
p2 = ggplot(df_perf_mod2, aes(x=species_pays, y = rsq)) + geom_col(fill = "#E69F00",position = position_dodge2(width = 0.8, preserve = "single")) + ylim(c(0,1)) + theme_light() + facet_grid(.~mod , scales="free",space = "free") + theme(axis.title.x = element_blank()) + geom_hline(aes(yintercept = 0.02),linetype = "dashed", color = "darkred", size = 0.3)  + geom_hline(aes(yintercept = 0.13),linetype = "dashed", color = "darkred", size = 0.3)   + geom_hline(aes(yintercept = 0.26),linetype = "dashed", color = "darkred", size = 0.3) 

p2/p1

fun_plot_distrib_resist <- function(df,response_var,code_pays,mod){
  df <- df %>%
    filter(resp_var==1) %>%
    count(resp_var,  codevillage) %>%
    mutate(Freq = n/sum(n))
  p <- ggplot(df,aes(y=Freq,x=reorder(codevillage,-Freq))) +  geom_col() + ylim(0,1) + ggtitle(paste0(mod," - ",code_pays," - ",response_var)) + theme_classic() + xlab("villages") + ylab("Frequency of the resistant class") + theme(axis.text.x = element_blank(),   axis.ticks.x = element_blank())
  return(p)
}

res <- res %>%
  mutate(df_mod = map(glmm_aic,~.$mod@model$frame)) %>%
  mutate(df_mod_plot = pmap(list(df_mod, response_var, code_pays, mod), ~fun_plot_distrib_resist(..1, ..2, ..3, ..4)))
wrap_plots(res$df_mod_plot)


  fun_get_tab_glmm_resist <- function(glmm_aic,response_var,code_pays,mod){
  
  glmm_tidy <- broom.mixed::tidy(glmm_aic$mod@model, conf.int = TRUE, exponentiate = TRUE)
  glmm_tidy <- glmm_tidy %>%
    mutate(p.value2 = case_when(
      p.value <= 0.001 ~ "***",
      p.value > 0.001 & p.value <= 0.01  ~  "**",
      p.value > 0.01 & p.value <= 0.05 ~ "*",
      p.value > 0.05 ~ ""
    )) %>%
    filter(effect=="fixed", term != "estimate")
  
  glmm_tidy <- fun_get_predictors_labels(glmm_tidy,"term")
  
  if(mod == "late_biting"){
    glmm_tidy <- glmm_tidy %>%
      mutate(label_detail = gsub("day of collection","day preceding collection",label_detail)) %>%
      mutate(label_detail = gsub("hour of collection","night of collection",label_detail))
  }
  
  if(!(mod %in% c("presence","abundance"))){
    glmm_tidy <- glmm_tidy %>%
      mutate(label_detail = gsub("b/w 0 and 4.28571428571429 weeks","(month preceding coll.",label_detail)) %>%
      mutate(label_detail = gsub("b/w 0 and 0 weeks","(day of collection",label_detail)) %>%
      mutate(label_detail = gsub("\n2000 m buffer","",label_detail))
  }
  
  if(mod %in% c("presence","abundance")){
    glmm_tidy <- glmm_tidy %>%
      mutate(label_detail = gsub("hour of collection","night of collection",label_detail))
  }
  
  glmm_tidy <- glmm_tidy %>%
    mutate(label_detail=gsub("\\n"," ",label_detail)) %>%
    mutate(label_detail=ifelse(grepl("\\(",label_detail),paste0(label_detail,")"),label_detail)) %>%
    mutate(label_detail = paste(label_detail,p.value2)) %>%
    mutate(species = response_var, codepays = code_pays, mod=mod ) 
  
  return(glmm_tidy)
  }

res <- res %>%
  mutate(glmm_aic_tab = pmap(list(glmm_aic,response_var,code_pays,mod),~fun_get_tab_glmm_resist(..1,..2,..3,..4)))

glmms_tab <- do.call(rbind.data.frame, res$glmm_aic_tab) %>% 
  mutate(model = paste(mod,codepays,species, sep = " - ")) %>%
  dplyr::filter(!(is.na(label))) %>%
  dplyr::select(model,label_detail, unit,estimate,conf.low,conf.high,p.value) %>%
  mutate(estimate = round(estimate,5), conf.high = round(conf.high,5), conf.low = round(conf.low,5), p.value = round(p.value,3)) %>%
  mutate(unit = ifelse(unit=='LLIN only','comp. to LLIN only',unit)) %>%
  mutate(unit = ifelse(is.na(unit),'comp. to interior',unit)) %>%
  mutate(label_detail = ifelse(label_detail=="Place ***",'Place (exterior) ***',label_detail))

write.csv(glmms_tab,"/home/ptaconet/phd/articles/article2/glmms_tab2.csv",row.names = F)

  # selected plots
  
          glmm_plots <- res %>% 
          mutate(model_plots = pmap(list(rf,glmm_aic, mod,response_var,code_pays), ~fun_plot_pdp5(..1,..2,..3,..4,..5,get_all_plots=FALSE)))
        
        pmap(list(glmm_plots$model_plots,glmm_plots$response_var,glmm_plots$code_pays,glmm_plots$mod),~ggsave(paste(..2,..3,..4,"_smallmod",sep="_"),..1,"png",paste0("plots_resistance5_selectvar2/",..4),width = 1.4, height = 14.4, units = "in"))  #height = 10  -> pour 9 plots sur la colonne
        
          
          # all the plots
          glmm_plots <- res %>% filter(code_pays=="CI") %>%
            mutate(model_plots = pmap(list(rf,glmm_aic, mod,response_var,code_pays), ~fun_plot_pdp5(..1,..2,..3,..4,..5,get_all_plots=TRUE)))
          
            pmap(list(glmm_plots$model_plots,glmm_plots$response_var,glmm_plots$code_pays,glmm_plots$mod),~ggsave(paste(..2,..3,..4,"_fullmod",sep="_"),..1,"png",paste0("plots_resistance5_selectvar2/",..4),width = 1.4, height = 16.6, units = "in"))
          
    
# varimplot
glmm_plots <- glmm_plots %>%
  filter(!(response_var == "An. coluzzii" & code_pays=="BF" & mod == "early_biting")) %>%
  mutate(df_varimp = map(model_plots, ~pluck(.,"df_varimp")))
  
df_varimp = do.call(rbind.data.frame, glmm_plots$df_varimp) %>%
  mutate(model = paste0(country," - ", species))

 ggplot(df_varimp , aes(y=reorder(label, value), x = value, fill = name,group = name, label = label)) + 
  geom_bar(position="dodge", stat="identity") + 
  scale_fill_manual("model", values = c("GLMM" = "#E69F00", "RF" = "#009E73")) + 
  theme_light() + 
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom") + 
  facet_grid(label_group~model, scales = "free_y")


 p1=ggplot(df_varimp %>% filter(name == "RF") , aes(y=label, x = value, label = label)) + 
   geom_bar(position="dodge", stat="identity") + 
   scale_fill_manual("model", values = c("GLMM" = "#E69F00", "RF" = "#009E73")) + 
   theme_light() + 
   theme(axis.title.y = element_blank(),
         axis.title.x = element_blank(),
         legend.position = "bottom") + 
   facet_grid(label_group~model, scales = "free_y")
 
 p2=ggplot(df_varimp %>% filter(name == "GLMM") , aes(y=label, x = value, label = label)) + 
   geom_bar(position="dodge", stat="identity") + 
   scale_fill_manual("model", values = c("GLMM" = "#E69F00", "RF" = "#009E73")) + 
   theme_light() + 
   theme(axis.title.y = element_blank(),
         axis.title.x = element_blank(),
         legend.position = "bottom") + 
   facet_grid(label_group~model, scales = "free_y")
 

pmap(list(glmm_plots$model_plots,glmm_plots$response_var,glmm_plots$code_pays,glmm_plots$mod),~ggsave(paste(..2,..3,..4,sep="_"),..1,"png",paste0("plots_resistance5/",..4))) #, width = 15, height = 12

glmm_plots_selectvar <- res %>% 
  filter(!(response_var == "ma_gambiae_ss" & code_pays=="BF" & mod == "early_biting")) %>%
  filter(!(response_var == "ma_funestus_ss" & code_pays=="CI" & mod == "early_biting")) %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii",
    response_var == "ma_gambiae_sl" ~ "An. gambiae s.l.")) %>%
  mutate(model_plots = pmap(list(rf,glmm_aic, mod,response_var,code_pays), ~fun_plot_pdp4(..1,..2,..3,..4,..5,get_all_plots=FALSE)))

pmap(list(glmm_plots_selectvar$model_plots,glmm_plots_selectvar$response_var,glmm_plots_selectvar$code_pays,glmm_plots_selectvar$mod),~ggsave(paste(..2,..3,..4,sep="_"),..1,"png",paste0("plots_resistance5_selectvar/",..4)))


to_rm <- NULL
for(i in 1:nrow(res)){
  if(is.null(res[i,]$rf_nightscience[[1]])){
    to_rm <- c(to_rm,i) 
  }
}
res <- res[-to_rm,]



## glmm + RF
glmm_plots <- res %>% 
  filter(periodinterv=="all") %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus",
    response_var == "ma_gambiae_ss" ~ "An. gambiae s.s.",
    response_var == "ma_coluzzi" ~ "An. coluzzii",
    response_var == "ma_gambiae_sl" ~ "An. gambiae s.l.")) %>%
  mutate(glmm_multiv = map2(glmm, mod, ~broom.mixed::tidy(.x$mod@model, conf.int = TRUE, exponentiate = ifelse(.y == "abundance", FALSE, TRUE)))) %>%
  mutate(glmm_multiv = map(glmm_multiv, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(glmm_multiv_auc = map(glmm, ~MLmetrics::AUC(.$df_cv_llo$pred, .$df_cv_llo$obs))) %>%
  mutate(glmm_univ = map(glmms_univs, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(glmm_multiv_plot = pmap(list(glmm_multiv,mod,response_var,periodinterv,glmm_multiv_auc), ~fun_glmm_dotpoint(..1,..2,..3,..4,..5,"multivariate"))) %>%
  mutate(glmm_univ_plot = pmap(list(glmm_univ,mod,response_var,periodinterv), ~fun_glmm_dotpoint(..1,..2,..3,..4,NA,"univariate"))) %>%
  mutate(rf_nightscience_auc = map(rf_nightscience, ~MLmetrics::AUC(.$df_cv$pred, .$df_cv$obs))) %>%
  mutate(rf_allpredictors_auc = map(rf_allpredictors, ~MLmetrics::AUC(.$df_cv$pred, .$df_cv$obs))) %>%
  mutate(rf_nightscience_plots = pmap(list(rf_nightscience,mod,response_var,periodinterv,glmm_univ_plot,glmm_multiv_plot), ~fun_plot_pdp2(..1,..2,..3,..4,..5,..6))) %>%
  mutate(rf_allpredictors_plots = pmap(list(rf_allpredictors,mod,response_var,periodinterv,glmm_univ_plot,glmm_multiv_plot), ~fun_plot_pdp2(..1,..2,..3,..4,..5,..6)))






glmm_plots <- res %>% 
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(glmm_multiv = map2(glmm, mod, ~broom.mixed::tidy(.x$mod@model, conf.int = TRUE, exponentiate = ifelse(.y == "abundance", FALSE, TRUE)))) %>%
  mutate(glmm_multiv = map(glmm_multiv, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(glmm_multiv_auc = map(glmm, ~MLmetrics::AUC(.$df_cv_llo$pred, .$df_cv_llo$obs))) %>%
  mutate(glmm_univ = map(glmms_univs, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(glmm_multiv_plot = pmap(list(glmm_multiv,mod,response_var,periodinterv,glmm_multiv_auc), ~fun_glmm_dotpoint(..1,..2,..3,..4,..5,"multivariate"))) %>%
  mutate(glmm_univ_plot = pmap(list(glmm_univ,mod,response_var,periodinterv), ~fun_glmm_dotpoint(..1,..2,..3,..4,NA,"univariate"))) %>%
  mutate(rf_nightscience_plots = pmap(list(rf_nightscience,mod,response_var,periodinterv,glmm_univ_plot,glmm_multiv_plot), ~fun_plot_pdp2(..1,..2,..3,..4,"nightscience",..5,..6)))










## pdps
pdps_resistances <- res %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
 # mutate(rf_dayscience_plots = pmap(list(rf_dayscience ,mod,response_var,periodinterv), ~fun_plot_pdp2(..1,..2,..3,..4,"dayscience"))) %>%
  mutate(rf_nightscience_plots = pmap(list(rf_nightscience,mod,response_var,periodinterv), ~fun_plot_pdp2(..1,..2,..3,..4,"nightscience")))


pmap(list(pdps_resistances$rf_dayscience_plots,pdps_resistances$response_var,pdps_resistances$code_pays,pdps_resistances$mod,pdps_resistances$period_interv),~ggsave(paste(..2,..3,..4,..5,"dayscience",sep="_"),..1$pdps,"png",paste0("plots_resistance2/",..4)))
pmap(list(pdps_resistances$rf_nightscience_plots,pdps_resistances$response_var,pdps_resistances$code_pays,pdps_resistances$mod,pdps_resistances$period_interv),~ggsave(paste(..2,..3,..4,..5,"nightscience",sep="_"),..1$pdps,"png",paste0("plots_resistance2/",..4)))




# plots for models validation
model_validation <- res %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(df_cv = map(rf_nightscience, ~pluck(.,"df_cv"))) %>%
  dplyr::select(response_var, code_pays, mod, periodinterv, df_cv) %>%
  mutate(df_cv = map(df_cv, ~dplyr::select(., pred,obs, codevillage, nummission, pointdecapture, int_ext))) %>%
  mutate(df_cv = map2(df_cv,response_var, ~mutate(.x, species = .y))) %>%
  nest(-c(mod,code_pays,periodinterv))%>%
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
  mutate(glmm_multiv_plot = pmap(list(glmm_multiv,mod,response_var,periodinterv,glmm_multiv_auc), ~fun_glmm_dotpoint(..1,..2,..3,..4,..5,"multivariate"))) %>%
  mutate(glmm_univ_plot = pmap(list(glmm_univ,mod,response_var,periodinterv), ~fun_glmm_dotpoint(..1,..2,..3,..4,NA,"univariate")))
 

pmap(list(glmm_plots$glmm_univ_plot,glmm_plots$glmm_multiv_plot,glmm_plots$response_var,glmm_plots$code_pays,glmm_plots$mod,glmm_plots$periodinterv),~ggsave(paste(..3,..4,..5,..6,"glmm",sep="_"),wrap_plots(..1, ..2, nrow = 1, ncol = 2) ,"png",paste0("plots_resistance2/",..5)))


# get rsquared from the multivariate models
df_mod=res$glmm[[1]]$mod@model$frame
r2 = MuMIn::r.squaredGLMM(res$glmm[[1]]$mod@model)









######################
###### predictive analysis######
######################


res <-  readRDS("/home/ptaconet/Bureau/data_analysis/model_results_predictive2.rds") %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus",
    response_var == "ma_gambiae_ss" ~ "An. gambiae s.s.",
    response_var == "ma_gambiae_sl" ~ "An. gambiae s.s.",
    response_var == "ma_coluzzi" ~ "An. coluzzii",
    response_var == "ma_an" ~ "Anopheles genus"))


res_switch <-  readRDS("/home/ptaconet/Bureau/data_analysis/model_results_predictive_switch_areas2.rds") %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus",
    response_var == "ma_gambiae_ss" ~ "An. gambiae s.s.",
    response_var == "ma_gambiae_sl" ~ "An. gambiae s.s.",
    response_var == "ma_coluzzi" ~ "An. coluzzii",
    response_var == "ma_an" ~ "Anopheles genus"))

# sur 1 meme plot : lineplot évolution des AUC selon les 3 modèles (opensource, opensource simple, tocollect). 

df_cv <- NULL
df_imp <- NULL

for(i in 1:nrow(res)){
  
  for(j in 1:5){
   a <- res$rf_opensource[[i]][[j]]$df_cv
   b <- res$rf_tocollect[[i]][[j]]$df_cv
   c <- res$rf_opensource_simple[[i]][[j]]$df_cv
   
   if(i<13){
   a1 <- res_switch$rf_opensource[[i]][[j]]$df_cv
   c1 <- res_switch$rf_opensource_simple[[i]][[j]]$df_cv
   }
   
   imp_opensource <- as.data.frame(ranger::importance(res$rf_opensource[[i]][[j]]$mod$finalModel))
   imp_tocollect <- as.data.frame(ranger::importance(res$rf_tocollect[[i]][[j]]$mod$finalModel))
   imp_opensource_simple <- as.data.frame(ranger::importance(res$rf_opensource_simple[[i]][[j]]$mod$finalModel))
   
   imp_opensource$var <- rownames(imp_opensource)
   colnames(imp_opensource) <- c("importance","var")
   imp_tocollect$var <- rownames(imp_tocollect)
   colnames(imp_tocollect) <- c("importance","var")
   imp_opensource_simple$var <- rownames(imp_opensource_simple)
   colnames(imp_opensource_simple) <- c("importance","var")
   

   a$data_tocollect <-  imp_opensource$data_tocollect <- "Open source - complex model"
   b$data_tocollect <- imp_tocollect$data_tocollect <- "To collect"
   c$data_tocollect <- imp_opensource_simple$data_tocollect <- "Open source - simple model"
   a1$data_tocollect <-  imp_opensource$data_tocollect <- "Open source - complex model - trained on the other area"
   c1$data_tocollect <- imp_opensource_simple$data_tocollect <- "Open source - simple model - trained on the other area"
   
   a$weeks_before <-  b$weeks_before <-  c$weeks_before <-  a1$weeks_before <-  c1$weeks_before <- imp_opensource$weeks_before <- imp_tocollect$weeks_before <- imp_opensource_simple$weeks_before <-  j - 1
   a$mod <-  b$mod <-  c$mod <- imp_opensource$mod <- imp_tocollect$mod <- imp_opensource_simple$mod <- res$mod[[i]]
   a$code_pays <-  b$code_pays <-  c$code_pays <-   imp_opensource$code_pays <- imp_tocollect$code_pays <- imp_opensource_simple$code_pays <- res$code_pays[[i]]
   a$species <-  b$species <-  c$species <-  imp_opensource$species <- imp_tocollect$species <- imp_opensource_simple$species <- res$response_var[[i]]
   
   if(i<13){
     a1$mod <- c1$mod <- res_switch$mod[[i]]
     a1$code_pays <- c1$code_pays <- res_switch$code_pays[[i]]
     a1$species <-  c1$species <- res_switch$response_var[[i]]
      df_cv <- bind_rows(df_cv,a,b,c,a1,c1)
   } else {
     df_cv <- bind_rows(df_cv,a,b,c)
   }
   
   df_imp <- bind_rows(df_imp,imp_opensource,imp_tocollect,imp_opensource_simple)
   
  }

}

df_cv <- df_cv %>%
  mutate(data_tocollect = fct_relevel(data_tocollect,c("To collect","Open source - complex model","Open source - simple model","Open source - complex model - trained on the other area","Open source - simple model - trained on the other area"))) %>%
  mutate(species = fct_relevel(species,c("Anopheles genus","An. gambiae s.s.","An. funestus","An. coluzzii")))
  

df_cv_quality_presence <-  df_cv %>%
  filter(mod=="presence", weeks_before > 0) %>%
  nest(-c(species,data_tocollect, weeks_before,code_pays,mod)) %>%
  mutate(perf_metrics = map(data,~fun_compute_perf_metric_predictive(.)))  %>%
  mutate(df_cv = map(perf_metrics, ~pluck(.,"df_cv"))) %>%
  mutate(AUC = as.numeric(map(perf_metrics, ~pluck(.,"ROC_AUC")))) %>%
  mutate(PR_AUC = as.numeric(map(perf_metrics, ~pluck(.,"PR_AUC")))) %>%
  mutate(f1score = as.numeric(map(perf_metrics, ~pluck(.,"f1score")))) %>%
  mutate(recall = as.numeric(map(perf_metrics, ~pluck(.,"recall")))) %>%
  mutate(precision = as.numeric(map(perf_metrics, ~pluck(.,"precision")))) %>%
  mutate(specificity = as.numeric(map(perf_metrics, ~pluck(.,"specificity")))) %>%
  mutate(sensitivity = as.numeric(map(perf_metrics, ~pluck(.,"sensitivity")))) %>%
  dplyr::select(-c(data,perf_metrics))

p1=ggplot(df_cv_quality_presence, aes(x=weeks_before,y=AUC,colour=data_tocollect )) +  
  annotate("rect", xmin=-Inf, xmax=Inf, ymin= 0.5, ymax=0.65, alpha=0.2, fill="red") +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin= 0.65, ymax=0.85, alpha=0.2, fill="blue") +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin= 0.85, ymax=1, alpha=0.2, fill="green") + 
  geom_line() +  geom_point() + facet_wrap(species~code_pays, ncol = 2 ) + ylim(c(0.5,1)) + theme_light() + theme(legend.position="bottom")
  
df_cv_quality_presence2 <- df_cv %>%
  filter(mod=="presence") %>%
  group_by(species,data_tocollect, weeks_before,nummission,code_pays,mod, codevillage) %>%
  summarise(pred=sum(pred), obs=sum(obs)) %>%
  pivot_longer(c(pred,obs))

p2 = ggplot(df_cv_quality_presence2 %>% filter(weeks_before == 0, code_pays == "CI", species == "An. gambiae s.s.", data_tocollect == "To collect"), aes(x=nummission,y=value,colour=name)) + geom_line(aes(group=name)) +  geom_point() + facet_wrap(.~codevillage) + theme_light()


# abondance
df_cv_quality_abundance <- df_cv %>%
filter(mod=="abundance") %>%
  mutate(pred=exp(pred-1),obs=exp(obs-1)) %>%
  group_by(species,data_tocollect, weeks_before,code_pays,mod) %>%
  summarise(mae = round(MLmetrics::MAE(y_true = obs ,y_pred = pred),2),
            mse =  round(MLmetrics::MSE(y_true = obs ,y_pred = pred),2),
            rmse =  round(MLmetrics::RMSE(y_true = obs ,y_pred = pred),2),
            rsq =  round(MLmetrics::R2_Score(y_true = obs ,y_pred = pred),2)) %>%
  as_tibble()

ggplot(df_cv_quality_abundance, aes(x=weeks_before,y=mae,colour=data_tocollect)) + geom_line() +  geom_point() + facet_wrap(species~code_pays, ncol = 2, scales = 'free' ) + theme_light() + theme(legend.position="bottom")


df_cv_quality_abundance <- df_cv %>%
  filter(mod=="abundance") %>%
  #mutate(pred=exp(pred-1),obs=exp(obs-1)) %>%
  group_by(species,data_tocollect, weeks_before,nummission,code_pays,mod, codevillage) %>%
  summarise(pred=sum(pred), obs=sum(obs)) %>%
  pivot_longer(c(pred,obs))

ggplot(df_cv_quality_abundance %>% filter(weeks_before == 0, code_pays == "CI", species == "Anopheles genus", data_tocollect == "To collect"), aes(x=nummission,y=value,colour=name)) + geom_line(aes(group=name)) +  geom_point() + facet_wrap(.~codevillage) + theme_light()

df_cv_quality_abundance <- df_cv %>%
  filter(mod=="abundance") %>%
  #mutate(pred=exp(pred-1),obs=exp(obs-1)) %>%
  pivot_longer(c(pred,obs))

ggplot(df_cv_quality_abundance %>% filter(weeks_before == 0, code_pays == "CI", species == "Anopheles genus", data_tocollect == "Open source - simple model"), aes(x=nummission,y=value,colour=name)) + geom_boxplot()  + facet_wrap(.~codevillage) + theme_light()


df_cv_quality_abundance <- df_cv %>%
  filter(mod=="abundance") %>%
  mutate(pred=exp(pred-1),obs=exp(obs-1)) %>%
  mutate(residuals = obs - pred) %>%
  mutate(groups = case_when(obs<=1 ~ "<=1",
                            obs>1 & obs<=3 ~ "2-3",
                            obs>3 & obs<=10 ~ "4-10",
                            obs>10 & obs<=50 ~ "11-50",
                            obs>50 ~ ">50"
  )) %>%
  mutate(groups = fct_relevel(groups, c("<=1","2-3","4-10","11-50",">50")))

ggplot() + 
  geom_boxplot(data = df_cv_quality_abundance %>% filter(species=="Anopheles genus", data_tocollect == "To collect",weeks_before>0 ), aes(x=weeks_before , y=residuals, group = weeks_before)) + 
  geom_jitter(data = df_cv_quality_abundance %>% filter(species=="Anopheles genus", data_tocollect == "To collect",weeks_before>0) , aes(x=weeks_before , y=residuals, group = weeks_before), position = position_jitter(width = .15), size = 0.3) + 
  #stat_summary(data = df_cv_quality_abundance %>% filter(species=="Anopheles genus", data_tocollect == "To collect",weeks_before>0), aes(x=weeks_before , y=residuals), fun=median, geom="point", size=2, color="black") +
  facet_wrap( code_pays ~ groups, ncol = 5, scales = "free") +
  theme_bw() + 
  xlab("Weeks before") + 
  ylab("Residuals (obs - pred)") + 
  # geom_label(data = df_metrics_perf,
  #            size = 2.5,
  #            mapping = aes(x = groups, y = max(df$residuals,na.rm = T), label = paste0('MAE = ',mae,'\nn = ',n),
  #                          vjust = 1)) +
  # ggtitle(spec) + 
  geom_hline(yintercept=0, linetype="dashed") + 
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))


## inter-changer les modèles BF et CI


## variables retained and var imp
df_imp <- fun_get_predictors_labels(table = df_imp, vector_predictors_name = "var")

df_imp <- df_imp %>%
  group_by(label,label_group,data_tocollect,weeks_before,mod,code_pays,species) %>%
  summarise(importance = sum(importance))

ggplot(df_imp %>% filter(mod=="presence", code_pays=="BF", data_tocollect == "Open source - complex model"), aes(x=weeks_before,y=label, fill = sqrt(importance), group = label_group)) + geom_tile(color = "white", show.legend = TRUE, size = 0.4) + facet_grid(rows = "label_group", scales = "free") +  theme_bw() + scale_fill_continuous(type = "viridis")
ggplot(df_imp %>% filter(mod=="presence", code_pays=="BF", data_tocollect == "Open source - complex model"), aes(x=weeks_before,y=label, size = importance, group = label_group)) + geom_point() + facet_grid(rows = "label_group", scales = "free") +  theme_bw()

## observed vs predicted



## maps obs. vs. pred
df_cv_quality_abundance <- df_cv %>%
  filter(mod=="abundance") %>%
  #mutate(pred=exp(pred-1),obs=exp(obs-1)) %>%
  group_by(species,data_tocollect, weeks_before,nummission,code_pays,mod, codevillage) %>%
  summarise(pred=sum(pred), obs=sum(obs)) %>%
  pivot_longer(c(pred,obs))

m1 <- fun_map2("BF",df_cv_quality_abundance %>% filter(species=="Anopheles genus", data_tocollect == "To collect"))
m2 <- fun_map2("CI",df_cv_quality_abundance %>% filter(species=="Anopheles genus", data_tocollect == "To collect"))


