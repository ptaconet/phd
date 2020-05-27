
# function to create the data.frame for CCM
fun_ccm_df <- function(df_timeseries, var, buffer, function_to_apply){
  
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
fun_ccm_corrmat_sup0 <- function(df_timeseries_wide, df_response_var, column_response_var, method){
  
  df_response_var <- df_response_var %>%
    dplyr::select(idpointdecapture, column_response_var, codevillage) %>%
    rename(response_val = column_response_var)
  
  # df_timeseries_wide <- df_timeseries_wide %>%
  #  left_join(df_response_var, by = "id") 
  
  df_timeseries_wide <- df_response_var %>%
    right_join(df_timeseries_wide, by = "idpointdecapture") %>%
    filter(response_val > 0)
  df_timeseries_wide$codevillage <- as.factor(df_timeseries_wide$codevillage)
  df <- df_timeseries_wide %>% dplyr::select(codevillage,response_val)
  
  if(method %in% c("spearman","kendall")){
  # correlation <- sapply(df_timeseries_wide[,5:(ncol(df_timeseries_wide))], function(x) cor(x, df_timeseries_wide$response_val, method = "spearman", use = "na.or.complete"))# Spearman’s rank order correlation was applied because mosquito capture rates as well as some environmental quantities, especially daytime length and precipitation, are non-Gaussian distributed.
  # correlation_pval <- sapply(df_timeseries_wide[,5:(ncol(df_timeseries_wide))], function(x) cor.test(x, df_timeseries_wide$response_val, method = "spearman", use = "na.or.complete", exact = FALSE))
  # correlation_pval_df <- correlation_pval["p.value",] %>% as.data.frame() %>% pivot_longer(colnames(correlation_pval)) %>% as_tibble()
  # colnames(correlation_pval_df) <- c("name","pval")
  # correlation_df <- as.data.frame(correlation)
  # correlation_df$name <- rownames(correlation_df)
    correlation <- sapply(df_timeseries_wide[,4:(ncol(df_timeseries_wide))], function(x) correlation::correlation(cbind(as.data.frame(x),df), method = method, multilevel = TRUE))
  } else if (method == "distance"){
    correlation <- sapply(df_timeseries_wide[,4:(ncol(df_timeseries_wide))], function(x) correlation::correlation(cbind(as.data.frame(x),df), method = "distance", include_factors = FALSE))
  }
  
  correlation <- as.data.frame(correlation)
  correlation_df <- as.data.frame(correlation["r",]) %>% pivot_longer(everything()) %>% mutate(correlation = as.numeric(value)) %>% select(-value)
  correlation_pval_df <- as.data.frame(correlation["p",]) %>% pivot_longer(everything()) %>% mutate(pval = as.numeric(value)) %>% select(-value)
  
  
  correlation_df <- left_join(correlation_df,correlation_pval_df, by="name")
  correlation_df$time_lag_1 <- as.numeric(sub('.*\\_', '', correlation_df$name))
  correlation_df$time_lag_2 <- as.numeric(stringr::str_match( correlation_df$name, '([^_]+)(?:_[^_]+){1}$')[,2])  
  correlation_df <- arrange(correlation_df, time_lag_1, time_lag_2)
  correlation_df$abs_corr <- abs(correlation_df$correlation)
  correlation_df$name <- NULL
  
  correlation_df$correlation[which(correlation_df$pval > 0.05)] <- NA
  correlation_df$abs_corr[which(correlation_df$pval > 0.05)] <- NA
  
  return(correlation_df)
  
}

fun_ccm_corrmat_0 <- function(df_timeseries_wide, df_response_var, column_response_var){
  
  df_response_var <- df_response_var %>%
    select(id, column_response_var) %>%
    rename(response_val = column_response_var) %>%
    mutate(response_val = as.factor(response_val))
  
  df_timeseries_wide <- df_response_var %>%
    right_join(df_timeseries_wide, by = "id")
  
  pval <-  sapply(df_timeseries_wide[,3:(ncol(df_timeseries_wide))], function(x) kruskal.test(x, df_timeseries_wide$response_val))
  pval_df <- pval["p.value",] %>% as.data.frame() %>% pivot_longer(colnames(pval)) %>% as_tibble()
  colnames(pval_df) <- c("name","pval")
  pval_df$time_lag_1 <- as.numeric(sub('.*\\_', '', pval_df$name))
  pval_df$time_lag_2 <- as.numeric(stringr::str_match( pval_df$name, '([^_]+)(?:_[^_]+){1}$')[,2])  
  pval_df <- arrange(pval_df, time_lag_1, time_lag_2)
  pval_df$name <- NULL
  
  pval_df$pval[which(pval_df$pval > 0.05)] <- NA
  
  return(pval_df)
  
}



#### plots

# function to plot the CCM (simple plot : only the CCM)
fun_ccm_plot <- function(correlation_df, var, buffer, country){
  
  abs_corr <- correlation_df %>% filter(abs_corr == max(abs_corr, na.rm = T))
  
  if(nrow(correlation_df)>10){
    abs_corr2 <- correlation_df %>% arrange(desc(abs_corr)) %>% top_frac(.03) # 3 % top correlations will be blacked borders
  } else {
    abs_corr2 <- abs_corr
  }
  #abs_corr2 <- correlation_df %>% arrange(desc(abs_corr)) %>% top_frac(.05) # 5 % top correlations will be blacked borders
  #abs_corr2 <- correlation_df %>% arrange(desc(abs_corr)) %>% top_n(3) # 3 top correlations will be blacked borders
  
  ccm_plot <- ggplot(data = correlation_df, aes(time_lag_1, time_lag_2, fill = correlation)) +
    geom_tile(color = "white", show.legend = TRUE, size = 0.05) +
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
    ggtitle(paste0(country," - CCM for buffer " ,buffer," m")) +
    annotate("text", size = 3,x = min(correlation_df$time_lag_1), y = max(correlation_df$time_lag_2), vjust = "inward", hjust = "inward", label = paste0("r(0,0) = ", round(correlation_df$correlation[1],3),"\nr(",abs_corr$time_lag_1,",",abs_corr$time_lag_2,") = ",round(abs_corr$correlation,3))) +
    coord_fixed()
  
  return(ccm_plot)
  
}

# function to plot the time series of the response variable and the explanatory variable

fun_spatiotemparal_plots <- function(resp_var, expl_var, code_pays){
  
  th_pred_var <- prediction_vars %>% filter(code==expl_var)
  
  trmetrics_entomo_postedecapture$resp_var <- trmetrics_entomo_postedecapture[,resp_var]
  
  mean_date_by_mission <- entomo_csh_metadata_l1 %>%
    group_by(codepays,nummission) %>%
    summarise(date = mean(as.Date(date_capture))) %>%
    as_tibble()
  
  th_timeseries_expl_bf <- env_spatiotemporal %>%
    filter(var == expl_var, codepays == code_pays) %>%
    group_by(date, buffer) %>%
    summarise(val = mean(val, na.rm = T)) %>%
    as_tibble() %>%
    mutate(date = as.Date(date))
  
  
  th_timeseries_resp_bf <- trmetrics_entomo_postedecapture %>%
    left_join(entomo_csh_metadata_l1[c("idpointdecapture","codepays","date_capture","nummission")], by = c("id" = "idpointdecapture")) %>%
    filter(codepays == code_pays, resp_var > 0) %>%
    left_join(mean_date_by_mission %>% filter(codepays==code_pays), by = "nummission")
  
  scaleFactor_bf <- max(th_timeseries_expl_bf$val, na.rm = T) / max(th_timeseries_resp_bf$resp_var, na.rm = T)
  
  th_timeplot_bf <-  ggplot() + 
    geom_line(aes(x = th_timeseries_expl_bf$date, y = th_timeseries_expl_bf$val, color = th_timeseries_expl_bf$buffer), size = 0.5, show.legend = FALSE) +
    geom_boxplot(aes(x = th_timeseries_resp_bf$date, y = th_timeseries_resp_bf$resp_var * scaleFactor_bf, group = th_timeseries_resp_bf$date), show.legend = FALSE, outlier.shape=NA) + 
    geom_jitter(aes(x = th_timeseries_resp_bf$date, y = th_timeseries_resp_bf$resp_var * scaleFactor_bf, group = th_timeseries_resp_bf$date), position=position_jitter(2), cex=0.3) + 
    #geom_flat_violin(aes(x = th_timeseries_resp_bf$date, y = th_timeseries_resp_bf$ma_an * scaleFactor_bf, group = th_timeseries_resp_bf$date), position = position_nudge(x = .25, y = -1), adjust =2, trim = TRUE)+
    scale_y_continuous(name = paste0(th_pred_var$short_name," (",th_pred_var$unit,")"), sec.axis = sec_axis(~./scaleFactor_bf, name = paste0(resp_var," (positive counts only)"))) +
    scale_x_date(name = "date",date_labels = "%m/%Y", date_breaks = "2 months") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme_minimal(base_size = 10) +
    ggtitle("BF")
  
  
  
  th_env_spatiotemporal <- env_spatiotemporal2 %>% filter(var == expl_var, codepays == code_pays) %>% 
    ungroup() %>% 
    arrange(desc(abs(ccm_maxcorr_vcor))) %>% 
    slice(1L)
  # th_patchwork_bf <- th_env_spatiotemporal$ccm_plot_sup0[[1]]
  # if(nrow(th_env_spatiotemporal)>1){
  #   for(j in 2:nrow(th_env_spatiotemporal)){
  #    th_patchwork_bf <- th_patchwork_bf + th_env_spatiotemporal$ccm_plot_sup0[[j]]
  #   }
  # }
  th_ccm_bf <- th_env_spatiotemporal$ccm_plot_sup0[[nrow(th_env_spatiotemporal)]]
  
  th_var_df <- th_env_spatiotemporal$var_df[[nrow(th_env_spatiotemporal)]]
  
  th_var_plot_bf <- th_env_spatiotemporal$predictive_df[[nrow(th_env_spatiotemporal)]] %>%
    right_join(th_timeseries_resp_bf, by = "id") %>%
    dplyr::select(resp_var,th_var_df) %>%
    set_names(c(resp_var,th_pred_var$code))
  
  th_plot_bf <- ggplot(th_var_plot_bf, aes_string(y=th_pred_var$code, x=resp_var)) + geom_point(size = 1, alpha = 0.5) + theme_minimal(base_size = 9) + ggtitle(paste0(code_pays," - ",th_pred_var$short_name," for time lags (",th_env_spatiotemporal$ccm_maxcorr_lag1,",",th_env_spatiotemporal$ccm_maxcorr_lag2,") and buffer size ",th_env_spatiotemporal$buffer)) + ylab(paste0(th_pred_var$short_name," (",th_pred_var$unit,")"))
  
  trmetrics_entomo_postedecapture$resp_var <- NULL
  
  return(list(plot1 = th_timeplot_bf, plot2 = th_plot_bf))
  
  # th_patchwork_timeseries <- th_timeplot_bf + th_timeplot_ci
  # th_patchwork_ccm <- th_ccm_bf + th_ccm_ci
  # th_plot <- th_plot_bf + th_plot_ci
  # 
  # th_final_patchwork <- th_patchwork_timeseries / th_patchwork_ccm / th_plot +
  #   plot_annotation(
  #     title = paste0(th_pred_var$long_name," (",th_pred_var$code,")"),
  #     subtitle = paste0("source time step = ",th_pred_var$temporal_aggregation_days," days ; source spatial resolution = ",th_pred_var$spatial_resolution_m," meters"), 
  #     theme = theme(plot.title = element_text(hjust = 0.5),
  #                   plot.subtitle = element_text(hjust = 0.5))) + 
  #   plot_layout(heights = 1)
  
}

# function used in landscape metrics analysis
fun_tidy_corr <- function(correlation, col_name){
  
  correlation_df <- correlation %>%
    as.data.frame()
  colnames(correlation_df) <- col_name
  correlation_df$name2 <- rownames(correlation_df)
  correlation_df <- correlation_df %>%
    mutate(name2 = gsub("@"," ",name2)) %>%
    mutate(function_name = word(name2,1)) %>%
    mutate(buffer = as.numeric(word(name2,2))) %>%
    mutate(layer_id = as.numeric(word(name2,3))) %>%
    mutate(pixval = as.numeric(word(name2,4))) %>%
    left_join(lco_metadata %>% dplyr::select(pixval,layer_id,pixlabel,classif_label)) %>%
    left_join(metrics_defs %>% dplyr::select(function_name,name)) %>%
    dplyr::select(-c(layer_id,pixval))
  
  return(correlation_df)
  
}