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

path_to_db <- "data/react_db/react_db.gpkg"
react_gpkg <- DBI::dbConnect(RSQLite::SQLite(),dbname = path_to_db)

# open tables
trmetrics_entomo_pointdecapture <- dbReadTable(react_gpkg, 'trmetrics_entomo_pointdecapture') %>% dplyr::select(-fid) %>% rename(id = idpointdecapture)
trmetrics_entomo_postedecapture <- dbReadTable(react_gpkg, 'trmetrics_entomo_postedecapture') %>% dplyr::select(-fid) %>% rename(id = idpointdecapture)
entomo_csh_metadata_l1 <- dbReadTable(react_gpkg, 'entomo_csh_metadata_l1') %>% dplyr::select(-fid)
recensement_villages_l1 <- st_read(path_to_db,"recensement_villages_l1", quiet=T) %>%
  filter(!is.na(intervention)) %>%
  dplyr::select("codevillage","codepays","nomvillage","population","intervention","date_debut_interv","date_fin_interv","X","Y")
env_timeseries <-  dbReadTable(react_gpkg, 'env_timeseries') %>% dplyr::select(-fid) %>% filter(var!="WMW30")
env_staticnobuffer <-  dbReadTable(react_gpkg, 'env_staticnobuffer') %>% dplyr::select(-fid)
env_nightcatch <-  dbReadTable(react_gpkg, 'env_nightcatch') %>% dplyr::select(-fid)

# get the number of human - nights
idpointdecaptures <- dbReadTable(react_gpkg, 'entomo_csh_metadata_l1') %>%
  dplyr::select(idpointdecapture,nummission,codevillage,pointdecapture,codepays,date_capture)

# get mean date for each entomo mission in each country
mean_date_by_mission <- entomo_csh_metadata_l1 %>%
  group_by(codepays,nummission) %>%
  summarise(date = mean(as.Date(date_capture))) %>%
  as_tibble()


# get response variable table
googlesheets4::sheets_deauth()
response_vars <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1dIeSOa2WinXvOQGLmIjA0gFdsHnb6zMMsDME-G5pyMc/edit?usp=sharing", sheet = "var_reponse2", col_types="c")
prediction_vars <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1dIeSOa2WinXvOQGLmIjA0gFdsHnb6zMMsDME-G5pyMc/edit?usp=sharing", sheet = "var_prediction", col_types="c")

# join codepays and variable type to the explanatory vars
env_timeseries <- env_timeseries %>%
  mutate(buffer = as.character(buffer)) %>%
  right_join(idpointdecaptures[,c("idpointdecapture","codepays")], by =  c("id" = "idpointdecapture")) %>%
  left_join(prediction_vars[,c("code","type")], by = c("var" = "code"))
  
  
# retructurate in lists
env_timeseries2 <- env_timeseries %>%
  dplyr::select(-c(lag_time, date)) %>%
  group_by(type, var, codepays, buffer) %>%
  tidyr::nest(predictive_df = c(id, lag_n , val)) %>%
  mutate(fun_summarize = ifelse(var %in% c("RFD1","RFD7"), "sum", "mean")) %>%
  arrange(type, var, codepays, as.numeric(buffer), fun_summarize)

# function to create the data.frame for CCM
fun_ccm_df <- function(df_timeseries, var, buffer, function_to_apply = "mean"){
  
  df_timeseries_wide <- df_timeseries %>%
    arrange(lag_n) %>%
    pivot_wider(values_from = val, names_from = lag_n, names_prefix = paste0(var,"_", buffer,"_"))

    
  max_col <- ncol(df_timeseries_wide)
  
  for(i in 2:(max_col-1)){
    for(j in (i+1):max_col){
      column_name <- paste0(colnames(df_timeseries_wide[i]),"_",(j-2))
      if(function_to_apply=="mean"){
        df_timeseries_wide[column_name] <- rowMeans(df_timeseries_wide[,i:j], na.rm = T)
      } else if (function_to_apply=="sum"){
        df_timeseries_wide[column_name] <- rowSums(df_timeseries_wide[,i:j], na.rm = T)
      }
    }
  }
  
  for(i in 2:max_col){
    colnames(df_timeseries_wide)[i] <- paste0(colnames(df_timeseries_wide)[i],"_",sub('.*\\_', '', colnames(df_timeseries_wide)[i]))
  }
  
  return(df_timeseries_wide)
  
}

# function to get correlation matrix for the CCM
fun_ccm_corrmat <- function(df_timeseries_wide, df_response_var, column_response_var, filter_0){
  
  df_response_var <- df_response_var %>%
    select(id, column_response_var) %>%
    rename(response_val = column_response_var)
  
  # df_timeseries_wide <- df_timeseries_wide %>%
  #  left_join(df_response_var, by = "id") 
  
  df_timeseries_wide <- df_response_var %>%
    right_join(df_timeseries_wide, by = "id") 
  
  if(filter_0){
    df_timeseries_wide <- df_timeseries_wide %>% filter(response_val > 0)
  }
  
  correlation <- sapply(df_timeseries_wide[,3:(ncol(df_timeseries_wide))], function(x) cor(x, df_timeseries_wide$response_val, method = "spearman", use = "na.or.complete"))# Spearman’s rank order correlation was applied because mosquito capture rates as well as some environmental quantities, especially daytime length and precipitation, are non-Gaussian distributed.
  #correlation_pval <- sapply(df_timeseries_wide[,2:(ncol(df_timeseries_wide)-1)], function(x) cor.test(x, df_timeseries_wide$response_val, method = "spearman", use = "na.or.complete", exact = FALSE))
  correlation_df <- as.data.frame(correlation)
  correlation_df$time_lag_1 <- as.numeric(sub('.*\\_', '', rownames(correlation_df)))
  correlation_df$time_lag_2 <- as.numeric(stringr::str_match(rownames(correlation_df), '([^_]+)(?:_[^_]+){1}$')[,2])  
  correlation_df <- arrange(correlation_df, time_lag_1, time_lag_2)
  correlation_df$abs_corr <- abs(correlation_df$correlation)
  
  return(correlation_df)
  
}
  
# function to plot the CCM the 
fun_ccm_plot <- function(correlation_df, var, buffer, country){
  
  abs_corr <- correlation_df %>% filter(abs_corr == max(abs_corr))

  if(nrow(correlation_df)>10){
    abs_corr2 <- correlation_df %>% arrange(desc(abs_corr)) %>% top_frac(.03) # 3 % top correlations will be blacked borders
  } else {
    abs_corr2 <- abs_corr
  }
  #abs_corr2 <- correlation_df %>% arrange(desc(abs_corr)) %>% top_frac(.05) # 5 % top correlations will be blacked borders
  #abs_corr2 <- correlation_df %>% arrange(desc(abs_corr)) %>% top_n(3) # 3 top correlations will be blacked borders
  
   ccm_plot <- ggplot(data = correlation_df, aes(time_lag_1, time_lag_2, fill = correlation)) +
     geom_tile(color = "white", show.legend = FALSE, size = 0.05) +
     scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                          midpoint = 0, limit = c(-1,1), space = "Lab", 
                          name="Spearman\ncorrelation") +
     geom_tile(data = abs_corr2 , color = "black", size = 0.3, show.legend = FALSE) +
     geom_tile(data = abs_corr , color = "deeppink3", size = 0.6, show.legend = FALSE) +
     theme_minimal() + 
     theme(plot.title = element_text(size = 10, hjust = 0.5),
           axis.title = element_text(size = 8)
           ) +
     #ggtitle(paste0("CCM for : area = ",country," ; variable = ",var," ; buffer = ",buffer," m")) +
     ggtitle(paste0(country," - " ,buffer," m")) +
     annotate("text", size = 3,x = min(correlation_df$time_lag_1), y = max(correlation_df$time_lag_2), vjust = "inward", hjust = "inward", label = paste0("r(0,0) = ", round(correlation_df$correlation[1],3),"\nr(",abs_corr$time_lag_1,",",abs_corr$time_lag_2,") = ",round(abs_corr$correlation,3))) +
     coord_fixed()
   
   return(ccm_plot)
  
}


env_timeseries2 <- env_timeseries2 %>%
  mutate(predictive_df = pmap(list(predictive_df, var, buffer, fun_summarize), ~fun_ccm_df(..1, ..2, ..3, function_to_apply = ..4))) %>% # transform the predictive vars (widening it)
  mutate(ccm_corrmat = map(predictive_df, ~fun_ccm_corrmat(., trmetrics_entomo_postedecapture, "ma_an", TRUE))) %>%  # calculate correlation between ma_an and the predictive vars
  mutate(ccm_plot = pmap(list(ccm_corrmat, var, buffer, codepays), ~fun_ccm_plot(..1,..2,..3,..4))) # plot CCM
  
vars <- unique(env_timeseries2$var)

for(i in 1:length(vars)){

  th_timeseries_expl_bf <- env_timeseries %>%
    filter(var == vars[i], codepays == "BF") %>%
    group_by(date, buffer) %>%
    summarise(val = mean(val)) %>%
    as_tibble() %>%
    mutate(date = as.Date(date))
  
  th_timeseries_resp_bf <- trmetrics_entomo_postedecapture %>%
    left_join(entomo_csh_metadata_l1[c("idpointdecapture","codepays","date_capture","nummission")], by = c("id" = "idpointdecapture")) %>%
    filter(codepays == "BF") %>%
    left_join(mean_date_by_mission %>% filter(codepays=="BF"), by = "nummission")
  
  scaleFactor_bf <- max(th_timeseries_expl_bf$val, na.rm = T) / max(th_timeseries_resp_bf$ma_an, na.rm = T)
  
  th_timeplot_bf <-  ggplot() + 
    geom_line(aes(x = th_timeseries_expl_bf$date, y = th_timeseries_expl_bf$val, color = th_timeseries_expl_bf$buffer), size = 0.5, show.legend = FALSE) +
    geom_boxplot(aes(x = th_timeseries_resp_bf$date, y = th_timeseries_resp_bf$ma_an * scaleFactor_bf, group = th_timeseries_resp_bf$date), show.legend = FALSE, outlier.shape=NA) + 
    geom_jitter(aes(x = th_timeseries_resp_bf$date, y = th_timeseries_resp_bf$ma_an * scaleFactor_bf, group = th_timeseries_resp_bf$date), position=position_jitter(2), cex=0.3) + 
    #geom_flat_violin(aes(x = th_timeseries_resp_bf$date, y = th_timeseries_resp_bf$ma_an * scaleFactor_bf, group = th_timeseries_resp_bf$date), position = position_nudge(x = .25, y = -1), adjust =2, trim = TRUE)+
    scale_y_continuous(name = vars[i], sec.axis = sec_axis(~./scaleFactor_bf, name = "ma_an")) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme_minimal()
  
  
  
  th_env_timeseries <- env_timeseries2 %>% filter(var == vars[i], codepays == "BF")
  
  th_patchwork_bf <- th_env_timeseries$ccm_plot[[1]]
  if(nrow(th_env_timeseries)>1){
    for(j in 2:nrow(th_env_timeseries)){
     th_patchwork_bf <- th_patchwork_bf + th_env_timeseries$ccm_plot[[j]]
    }
  }
  
  
  th_timeseries_expl_ci <- env_timeseries %>%
    filter(var == vars[i], codepays == "CI") %>%
    group_by(date, buffer) %>%
    summarise(val = mean(val)) %>%
    as_tibble() %>%
    mutate(date = as.Date(date))
  
  th_timeseries_resp_ci <- trmetrics_entomo_postedecapture %>%
    left_join(entomo_csh_metadata_l1[c("idpointdecapture","codepays","date_capture","nummission")], by = c("id" = "idpointdecapture")) %>%
    filter(codepays == "CI") %>%
    left_join(mean_date_by_mission %>% filter(codepays=="CI"), by = "nummission")
  
  scaleFactor_ci <- max(th_timeseries_expl_ci$val, na.rm = T) / max(th_timeseries_resp_ci$ma_an, na.rm = T)
  
  th_timeplot_ci <-  ggplot() + 
    geom_line(aes(x = th_timeseries_expl_ci$date, y = th_timeseries_expl_ci$val, color = th_timeseries_expl_ci$buffer), size = 0.5, show.legend = FALSE) +
    geom_boxplot(aes(x = th_timeseries_resp_ci$date, y = th_timeseries_resp_ci$ma_an * scaleFactor_ci, group = th_timeseries_resp_ci$date), show.legend = FALSE, outlier.shape = NA) + 
    geom_jitter(aes(x = th_timeseries_resp_ci$date, y = th_timeseries_resp_ci$ma_an * scaleFactor_ci, group = th_timeseries_resp_ci$date), position=position_jitter(2), cex=0.3) + 
    #geom_flat_violin(aes(x = th_timeseries_resp_bf$date, y = th_timeseries_resp_bf$ma_an * scaleFactor_bf, group = th_timeseries_resp_bf$date), position = position_nudge(x = .25, y = -1), adjust =2, trim = TRUE)+
    scale_y_continuous(name = vars[i], sec.axis = sec_axis(~./scaleFactor_ci, name = "ma_an")) +
    scale_x_date(date_labels = "%m/%Y", date_breaks = "2 months") +
    theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
    theme_minimal()
  
  
  th_env_timeseries <- env_timeseries2 %>% filter(var == vars[i], codepays == "CI")
  th_patchwork_ci <- th_env_timeseries$ccm_plot[[1]]
  if(nrow(th_env_timeseries) > 0){
    for(j in 2:nrow(th_env_timeseries)){
      th_patchwork_ci <- th_patchwork_ci + th_env_timeseries$ccm_plot[[j]]
    }
  }

  th_patchwork_timeseries <- th_timeplot_bf + th_timeplot_ci
  
  th_final_patchwork <- th_patchwork_timeseries / 
     th_patchwork_bf / th_patchwork_ci  +
    plot_annotation(
      title = prediction_vars$short_name[which(prediction_vars$code == vars[i])],
      subtitle = paste0("time step = ",prediction_vars$temporal_aggregation_days[which(prediction_vars$code == vars[i])]," days"), 
      theme = theme(plot.title = element_text(hjust = 0.5),
                    plot.subtitle = element_text(hjust = 0.5))) + 
    plot_layout(heights = 1)
  
}






env_timeseries3 <- env_timeseries2 %>%
  mutate(ccm_maxcorr_vcor = map_dbl(ccm_corrmat, function(x) x$correlation[which.max(abs(x$correlation))])) %>%
  mutate(ccm_max_corr_lag1 = map_dbl(ccm_corrmat, function(x) x$time_lag_1[which.max(abs(x$correlation))])) %>%
  mutate(ccm_max_corr_lag2 = map_dbl(ccm_corrmat, function(x) x$time_lag_2[which.max(abs(x$correlation))])) %>%
  select(-c(predictive_df, ccm_corrmat, ccm_plot)) %>%
  as_tibble()


########## static

env_staticnobuffer2 <- env_staticnobuffer %>%
  pivot_wider(names_from = var, values_from = val) %>%
  right_join(trmetrics_entomo_postedecapture) %>%
  left_join(entomo_csh_metadata_l1, by = c("id" = "idpointdecapture")) %>%
  group_split(codepays) %>%
  map(~select(.,ma_an,WMD,BDE,VCT)) %>%
  map(~mutate_all(.,as.numeric))

res <- env_staticnobuffer2 %>%
  map(.,~cor(.,method = "spearman", use = "na.or.complete"))

library(corrplot)
corrplot(res[[1]],
         type = "upper", 
         diag= FALSE,
         tl.col = "dark grey", tl.srt = 45)


########### env nightcatch

env_nightcatch2 <- env_nightcatch %>%
  pivot_wider(names_from = var, values_from = val) %>%
  right_join(trmetrics_entomo_postedecapture) %>%
  left_join(entomo_csh_metadata_l1, by = c("id" = "idpointdecapture")) %>%
  group_split(codepays) %>%
  map(~select(.,ma_an,RFH,WDR,WSP,LMN))
  
res <- env_nightcatch2 %>%
  map(.,~cor(.,method = "spearman", use = "na.or.complete"))

corrplot(res[[1]],
         type = "upper", 
         diag= FALSE,
         tl.col = "dark grey", tl.srt = 45)
