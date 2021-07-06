## dot plot of the predictors removed during the feature selection process

res <-  readRDS("/home/ptaconet/Bureau/data_analysis/model_results_univanalysis9.rds")

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
  mutate(temporal_corrs_spearman =  pmap(list(temporal_corrs_spearman, response_var, code_pays, mod), ~mutate(..1, species = ..2, country = ..3, indicator = ..4)))
  

landscape <- do.call(rbind.data.frame, model_univanalysis_results$spatial_corrs_spearman) %>% 
  mutate(model = "spearman univariate") %>%
  mutate(buffer = ifelse(is.na(buffer),"2000",buffer)) %>%
  dplyr::rename(pval = p) %>%
  mutate(correlation = ifelse(pval >= 0.2 | abs(correlation)<=0.1, NA, correlation)) %>%  
  filter(buffer %in% c(NA,250,500,1000,2000), name %in% c("WMD","BDE","BCH_2000","WLS_2000","lsm_c_pland_2000_2_5","lsm_c_pland_2000_3_3","lsm_c_pland_2000_3_2","lsm_c_pland_2000_3_4","lsm_c_pland_2000_3_9","lsm_c_pland_2000_3_1","lsm_c_pland_2000_3_7","lsm_c_pland_2000_3_11","lsm_c_pland_2000_3_8","lsm_c_pland_2000_3_6","lsm_c_pland_2000_3_10",
                                                                                                                      "WLS_1000","lsm_c_pland_1000_2_5","lsm_c_pland_1000_3_3","lsm_c_pland_1000_3_2","lsm_c_pland_1000_3_4","lsm_c_pland_1000_3_9","lsm_c_pland_1000_3_1","lsm_c_pland_1000_3_7","lsm_c_pland_1000_3_11","lsm_c_pland_1000_3_8","lsm_c_pland_1000_3_6","lsm_c_pland_1000_3_10",
                                                                                                                      "WLS_500","lsm_c_pland_500_2_5","lsm_c_pland_500_3_3","lsm_c_pland_500_3_2","lsm_c_pland_500_3_4","lsm_c_pland_500_3_9","lsm_c_pland_500_3_1","lsm_c_pland_500_3_7","lsm_c_pland_500_3_11","lsm_c_pland_500_3_8","lsm_c_pland_500_3_6","lsm_c_pland_500_3_10",
                                                                                                                      "WLS_250","lsm_c_pland_250_2_5","lsm_c_pland_250_3_3","lsm_c_pland_250_3_2","lsm_c_pland_250_3_4","lsm_c_pland_250_3_9","lsm_c_pland_250_3_1","lsm_c_pland_250_3_7","lsm_c_pland_250_3_11","lsm_c_pland_250_3_8","lsm_c_pland_250_3_6","lsm_c_pland_250_3_10")) %>%
  mutate(label_detail=ifelse(name=="WLS_2000", "Length of the streams 2000 m buffer",label_detail)) %>%
  mutate(label_detail=ifelse(name=="WLS_1000", "Length of the streams 1000 m buffer",label_detail)) %>%
  mutate(label_detail=ifelse(name=="WLS_500", "Length of the streams 500 m buffer",label_detail)) %>%
  mutate(label_detail=ifelse(name=="WLS_250", "Length of the streams 250 m buffer",label_detail)) %>%
  mutate(label_detail= gsub("\\n"," ",label_detail)) %>%
  mutate(type = 'landscape variables') %>%
  dplyr::select(name,label_detail,pval,rho,type,indicator,species) %>% dplyr::rename(r=rho,p=pval) %>%
  nest(-c(species,indicator)) %>%
  rename(landscape = data)




climate <-  do.call(rbind.data.frame, model_univanalysis_results$temporal_corrs_spearman) %>% 
  mutate(r= ifelse(r>0.7,0.70,r)) %>%
  filter(var %in% c("RFD1F", "TMIN1", "TMAX1")) %>%
  mutate(correlation = ifelse(p<=0.2 | correlation >= 0.1,correlation,NA) ) %>%
  mutate(day_start = as.numeric(word(name,3,sep = "\\_"))) %>%
  mutate(day_end = as.numeric(word(name,4,sep = "\\_"))) %>%
  mutate(label_detail = paste0(label, "\nb/w ",day_start," and ",day_end," weeks")) %>%
  mutate(label_detail= gsub("\\n"," ",label_detail)) %>%
  mutate(type = 'meteorological variables') %>%
  dplyr::select(name,label_detail,p,r,type,indicator,species) %>%
  nest(-c(species,indicator)) %>%
  rename(climate = data)


df <- left_join(landscape,climate) %>% 
  mutate(data = map2(landscape,climate,~bind_rows(.x,.y))) %>%
  rename(response_var = species, mod = indicator) %>%
  left_join(model_univanalysis_results)

for(i in 1:nrow(df)){
  vars_ret <- colnames(df$rf_llo[[i]]$df_mod)
  df$data[[i]] <- df$data[[i]] %>% 
    mutate(multivariate_model = ifelse(name %in% vars_ret,"Variable retained","Variable excluded")) %>%
    mutate(colors = ifelse(multivariate_model=="retained","blue","grey60"))
}

i=3
p1 <- ggplot(df$data[[i]], aes(x = r, y = label_detail, fill = multivariate_model, col = multivariate_model)) +  
  theme_bw() + 
  facet_grid(type~.,scales="free_y", space="free_y") + 
  geom_point() +
  geom_vline(aes(xintercept = 0),lwd=0.2,linetype = "dashed") +
  ggtitle(paste0(df$response_var[[i]], " - ", df$mod[[i]], " model")) +
  theme(axis.text.y = element_text(size = 7)) + 
  ylab("Variable") + 
  xlab("Spearman correlation with the response variable") + 
  xlim(-0.55,0.55)

i=6
p2 <- ggplot(df$data[[i]], aes(x = r, y = label_detail, fill = multivariate_model, col = multivariate_model)) +  
  theme_bw() + 
  facet_grid(type~.,scales="free_y", space="free_y") + 
  geom_point() +
  geom_vline(aes(xintercept = 0),lwd=0.2,linetype = "dashed") +
  ggtitle(paste0(df$response_var[[i]], " - ", df$mod[[i]], " model")) +
  theme(axis.text.y = element_text(size = 7)) + 
  ylab("Variable") + 
  xlab("Spearman correlation with the response variable") + 
  xlim(-0.55,0.55)

wrap_plots(p1,p2) + plot_layout(guides = 'collect')
