
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
fun_ccm_corrmat <- function(df_timeseries_wide, df_response_var, method ){
  
  df_response_var <- df_response_var %>%
    dplyr::select(idpointdecapture, resp_var, codevillage)
  
  # df_timeseries_wide <- df_timeseries_wide %>%
  #  left_join(df_response_var, by = "id") 
  
  df_timeseries_wide <- df_response_var %>%
    right_join(df_timeseries_wide, by = "idpointdecapture") 
  
  
  df_timeseries_wide$codevillage <- as.factor(df_timeseries_wide$codevillage)
  df <- df_timeseries_wide %>% dplyr::select(resp_var,codevillage)
  
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
  correlation_df <- as.data.frame(correlation["r",]) %>% pivot_longer(everything()) %>% mutate(correlation = as.numeric(value)) %>% dplyr::select(-value)
  correlation_pval_df <- as.data.frame(correlation["p",]) %>% pivot_longer(everything()) %>% mutate(pval = as.numeric(value)) %>% dplyr::select(-value)
  
  
  correlation_df <- left_join(correlation_df,correlation_pval_df, by="name")
  correlation_df$time_lag_1 <- as.numeric(sub('.*\\_', '', correlation_df$name))
  correlation_df$time_lag_2 <- as.numeric(stringr::str_match( correlation_df$name, '([^_]+)(?:_[^_]+){1}$')[,2])  
  correlation_df <- arrange(correlation_df, time_lag_1, time_lag_2)
  correlation_df$abs_corr <- abs(correlation_df$correlation)
  correlation_df$name <- NULL
  correlation_df$diff_lag <- correlation_df$time_lag_1 - correlation_df$time_lag_2
  
  correlation_df$correlation[which(correlation_df$pval > 0.05)] <- NA
  correlation_df$abs_corr[which(correlation_df$pval > 0.05)] <- NA
  
  return(correlation_df)
  
}


fun_ccm_select_lags <- function(ccm_corrmat, col_to_look, var){
  
  if(!all(is.na(ccm_corrmat$correlation))){
    ccm_corrmat <- ccm_corrmat %>% filter(!is.na(correlation))
    if(var %in% c("TMAX1","TMIN1","TAMP1","SMO1","RFD1F","RFD1L")){ # daily resolution data
      ccm_corrmat <- ccm_corrmat %>% filter(diff_lag>=8)
    }
    val <- as.numeric(ccm_corrmat[which.max(abs(ccm_corrmat$abs_corr)),col_to_look])
  }
    
   else {
    val <- NA
  }

  return(val)
}
  

# stat_method : "spearman","glmm","rf"
# mod : "presence","abundance"
# type : "univariate_selection", "model_comparison"
# df : input dataframe
# resp_var_colname : response variable column name
# expl_vars_to_keep : nom des colonnes des variables explicatives intégrées dans le modele
# expl_vars_to_test : nom des colonnes des variables explicative à tester
# cross_validation_type = "spatial","temporal", "spatiotemporal"

# stat_method = "spearman"
# spearman_factor = "codevillage"
# mod = "abundance"
# df = th_trmetrics_entomo_postedecapture
# resp_var_colname = "resp_var"
# expl_vars_to_keep = NULL
# expl_vars_to_test = colnames(df[which(grepl("RFD1",colnames(df)))])

fun_feature_forward_selection <- function(stat_method, spearman_factor = NULL, mod = NULL, type, df, expl_vars_to_keep = NULL, expl_vars_to_test, cross_validation_type = NULL, logtransform_resp_var = FALSE){
  
  
  if(logtransform_resp_var == TRUE){
    df$resp_var <- log(df$resp_var)
  }
  
  # spearman coeff
  if(stat_method == "spearman"){
    library(correlation)
    df <- df[,c("resp_var",spearman_factor,expl_vars_to_keep,expl_vars_to_test)]
    if(!is.null(spearman_factor)){ df[,spearman_factor] <- as.factor(df[,spearman_factor]) }
    correlation <- sapply(df[,ifelse(!is.null(spearman_factor),3,2):(ncol(df))], function(x) correlation::correlation(cbind(as.data.frame(x),df[,c("resp_var",spearman_factor)]), method = "spearman", multilevel = ifelse(!is.null(spearman_factor),TRUE,FALSE)))
    
    correlation <- as.data.frame(correlation)
    correlation_df <- as.data.frame(correlation["r",]) %>% pivot_longer(everything()) %>% mutate(correlation = as.numeric(value)) %>% dplyr::select(-value)
    correlation_pval_df <- as.data.frame(correlation["p",]) %>% pivot_longer(everything()) %>% mutate(pval = as.numeric(value)) %>% dplyr::select(-value)
    correlation_df <- left_join(correlation_df,correlation_pval_df, by="name")
    correlation_df$abs_corr <- abs(correlation_df$correlation)

    return(correlation_df)
  }
  
  if(stat_method == "glmm"){
    library(glmmTMB)
    library(purrr)
    library(future)
    if(type == "model_comparison"){
      to_scale <- df[,c(expl_vars_to_keep,expl_vars_to_test)]
      to_scale <- as.data.frame(scale(to_scale))
      
      pointdecapture2 <- as.data.frame( as.factor(paste0(df$codevillage,df$pointdecapture)))
      colnames(pointdecapture2) = "pointdecapture2"
      df <- cbind(df[,c("resp_var","codevillage")],pointdecapture2,to_scale)
      start <- 4 + length(expl_vars_to_keep)
      expl_vars_to_keep <- paste(expl_vars_to_keep,collapse = "*")
      plan(multiprocess)
      options(future.globals.maxSize= 20000*1024^2)
      if(mod %in% c("physiological_resistance_kdrw","exophagy","late_prec_aggressiveness","presence")){
        glm_base <- glmmTMB(as.formula(paste0("resp_var ~ ",expl_vars_to_keep," + (1|codevillage/pointdecapture2)")), data = df, family = binomial(link = "logit"))
        func <- function(x){
          ret <- anova(glmmTMB(as.formula(paste0("resp_var ~ ",expl_vars_to_keep,"*",x," + (1|codevillage/pointdecapture2)")), data = df, family = binomial(link = "logit")), glm_base)
          return(ret)
        }
        possible_anova <- possibly(func, otherwise = NA_real_)
        mod_comp <- future_map(colnames(df[start:ncol(df)]), possible_anova)
      } else if (mod == "abundance"){
        glm_base <- glmmTMB(as.formula(paste0("resp_var ~ ",expl_vars_to_keep," + (1|codevillage/pointdecapture2)")), data = df, family = truncated_nbinom2)
          func <- function(x){
            ret <- anova(glmmTMB(as.formula(paste0("resp_var ~ ",expl_vars_to_keep,"*",x,"  + (1|codevillage/pointdecapture2)")), data = df, family = truncated_nbinom2), glm_base)
            return(ret)
          }
          possible_anova <- possibly(func, otherwise = NA_real_)
          mod_comp <- future_map(colnames(df[start:ncol(df)]), possible_anova)
      }
      
      na_indices <- which(is.na(mod_comp))
      if(length(na_indices) > 0){
        mod_comp <- mod_comp[-na_indices]
        expl_vars_to_test <- expl_vars_to_test[-na_indices]
      }
      aic_glm_base <- mod_comp[[1]]$AIC[1]
      AIC <- mod_comp %>% map(~pluck(.,"AIC")[2])
      pval <- mod_comp %>% map(~pluck(.,"Pr(>Chisq)")[2])
      name <- expl_vars_to_test
      
      mod_comp <- data.frame(name = name, res = unlist(AIC), pval = unlist(pval))
      mod_comp$diff_res_w_basemod <- mod_comp$res - aic_glm_base
      
      return(mod_comp)
      
    } else if(type == "univariate_selection"){
      to_scale <- df[,c(expl_vars_to_test)]
      to_scale <- as.data.frame(scale(to_scale))
      pointdecapture2 <- as.data.frame( as.factor(paste0(df$codevillage,df$pointdecapture)))
      colnames(pointdecapture2) = "pointdecapture2"
      df <- cbind(df[,c("resp_var","codevillage")],pointdecapture2,to_scale)
      if(mod %in% c("physiological_resistance_kdrw","exophagy","late_prec_aggressiveness","presence")){
        pvals <- future_map_dfr(colnames(df[4:ncol(df)]), ~Anova(glmmTMB(as.formula(paste0("resp_var ~ ",.x," + (1|codevillage/pointdecapture2)")), data = df, family = binomial(link = "logit")))[3])
      } else if (mod == "abundance"){
        pvals <- future_map_dfr(colnames(df[4:ncol(df)]), ~Anova(glmmTMB(as.formula(paste0("resp_var ~ ",.x," + (1|codevillage/pointdecapture2)")), data = df, family = truncated_nbinom2))[3])
      }
      
      pvals <- as.data.frame(pvals)
      pvals$name <- expl_vars_to_test
      colnames(pvals) <- c("pval","name")

      return(pvals)
    }
    
  }
  
  if(stat_method == "rf"){
    
    library(CAST)
    library(caret)
    
    if(cross_validation_type == "spatial"){
      indices <- CreateSpacetimeFolds(df, spacevar = "codevillage", k = length(unique(df$codevillage))) 
    } else if (cross_validation_type == "temporal"){
      indices <- CreateSpacetimeFolds(df, timevar = "nummission", k = length(unique(df$nummission))) 
    }
    
    if(mod == "abundance"){
      tr = trainControl(method="cv",
                      index = indices$index, 
                      indexOut = indices$indexOut)
      met = "Rsquared"
    } else if (mod %in% c("physiological_resistance_kdrw","exophagy","late_prec_aggressiveness","presence")){
      tr = trainControl(method="cv",
                        index = indices$index, 
                        indexOut = indices$indexOut,
                        sampling = "up",
                        summaryFunction = prSummary,
                        classProbs = TRUE)
      df <- df %>%
        mutate(resp_var = ifelse(resp_var==0,"Absence","Presence")) %>%
        mutate(resp_var = as.factor(resp_var))
      
      met = "AUC"
    }
    
    if(type == "model_comparison"){
      
      mod_base <- caret::train(form = as.formula(paste0("resp_var ~ ",paste(expl_vars_to_keep,collapse="+"))), data = df, method="ranger", tuneLength = 5, trControl=tr, metric = met)
      
      #predict(mod_base,type = "prob")
      #confusionMatrix(predict(mod_base), df$resp_var, positive = "Presence")
      
      if(mod == "abundance"){
        #qual_basemod <- min(mod_base$results$RMSE)
        qual_basemod <- max(mod_base$results$Rsquared)
      } else if (mod %in% c("physiological_resistance_kdrw","exophagy","late_prec_aggressiveness","presence")){
        qual_basemod <- max(mod_base$results$AUC)
      }
      
      fun_univ_rf <- function(predictors){
        predictors <- c(predictors,expl_vars_to_keep)
        predictors <- paste(predictors,collapse="+")
        model <- caret::train(form = as.formula(paste0("resp_var ~ ",predictors)), data = df, method="ranger", tuneLength = 5, trControl=tr, metric = met)
        if(mod == "abundance"){
          #ret <- min(model$results$RMSE)
          ret <- max(model$results$Rsquared)
        } else if (mod %in% c("physiological_resistance_kdrw","exophagy","late_prec_aggressiveness","presence")){
          ret <- max(model$results$AUC)
        }
        return(ret)
      }
      
      results <- future_map_dbl(expl_vars_to_test, ~fun_univ_rf(.x))
      results <- data.frame(name = expl_vars_to_test, res = results)
      results$diff_res_w_basemod <- results$res - qual_basemod
      
      return(results)
    }
    
    
  }
    
  
}





#### plots

# function to plot the CCM (simple plot : only the CCM)
fun_ccm_plot <- function(correlation_df, max_or_min, metric_name, var, filter_lags_ndays = 0){
  
  if(max_or_min=="max"){
    abs_corr <- correlation_df %>% filter(diff_lag >= filter_lags_ndays) %>% filter(abs_corr == max(abs_corr, na.rm = T))
  } else if (max_or_min=="min"){
    abs_corr <- correlation_df %>% filter(diff_lag >= filter_lags_ndays) %>% filter(abs_corr == min(abs_corr, na.rm = T))
  }
  
  if(nrow(correlation_df)>10){
    if(max_or_min=="max"){
     abs_corr2 <- correlation_df %>% arrange(desc(abs_corr)) %>% head(round(0.03*nrow(.))) # 3 % top correlations will be blacked borders
    } else if (max_or_min=="min"){
     abs_corr2 <- correlation_df %>% arrange(abs_corr) %>% head(round(0.03*nrow(.)))
    }
  } else {
    abs_corr2 <- abs_corr
  }

  ccm_plot <- ggplot(data = correlation_df, aes(time_lag_1, time_lag_2, fill = correlation)) +
    geom_tile(color = "white", show.legend = TRUE, size = 0.05) +
    #scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name = metric_name) +
    #scale_fill_gradient(low = "red", high = "white", space = "Lab", name="RMSE") +
    geom_tile(data = abs_corr2 , color = "black", size = 0.3, show.legend = FALSE) +
    geom_tile(data = abs_corr , color = "deeppink3", size = 0.6, show.legend = FALSE) +
    theme_minimal() + 
    theme(plot.title = element_text(size = 10, hjust = 0.5),
          axis.title = element_text(size = 8)
    ) +
    #ggtitle(paste0("CCM for : area = ",country," ; variable = ",var," ; buffer = ",buffer," m")) +
    ggtitle(var) +
    annotate("text", size = 3,x = min(correlation_df$time_lag_1), y = max(correlation_df$time_lag_2), vjust = "inward", hjust = "inward", label = paste0("r(0,0) = ", round(correlation_df$correlation[1],3),"\nr(",abs_corr$time_lag_1,",",abs_corr$time_lag_2,") = ",round(abs_corr$correlation,3))) +
    coord_fixed()
  
  if(metric_name=="Spearman\ncorrelation"){
    ccm_plot <- ccm_plot + 
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name = metric_name) 
  } else if(metric_name=="RMSE"){
    ccm_plot <- ccm_plot + 
     scale_fill_gradient(low = "red", high = "white", space = "Lab", name="RMSE") 
  }
  
  return(ccm_plot)
  
}

# function to plot the time series of the response variable and the explanatory variable
fun_spatiotemparal_plots <- function(th_trmetrics_entomo_postedecapture, mod, expl_var, codepays){
  
  th_pred_var <- prediction_vars %>% filter(code == expl_var)
  
  entomo_csh_metadata_l1_bis <- entomo_csh_metadata_l1 %>%
    mutate(idpointdecapture = substr(idpointdecapture,1,4))
  
  th_timeseries_resp_bf <- th_trmetrics_entomo_postedecapture %>%
    mutate(date = as.Date(mean_date_mission, origin = "1970-01-01"))
  
  env_spatiotemporal <- dbReadTable(react_gpkg, 'env_spatiotemporal') %>% 
    dplyr::select(-fid) %>% 
    mutate(date = as.Date(date)) %>% 
    dplyr::rename(idpointdecapture = id) %>% 
    filter(var %in% expl_var, buffer == 2000) %>% 
    left_join(entomo_csh_metadata_l1_bis) %>% 
    filter(codepays == code_pays) %>%
    group_by(date, codevillage) %>%
    summarise(val = mean(val, na.rm = T)) %>%
    as_tibble() %>%
    full_join(th_timeseries_resp_bf %>% dplyr::select(codevillage,date,resp_var))
  

  if(mod == "abundance"){
    
    # plot by village
    scaleFactor_bf <- max(env_spatiotemporal$val, na.rm = T) / max(env_spatiotemporal$resp_var, na.rm = T)

    th_timeplot_bf <-  ggplot(env_spatiotemporal) + 
      geom_line(aes(x = date, y = val), size = 0.5, show.legend = FALSE, color='steelblue') +
      geom_boxplot(aes(x = date, y = resp_var * scaleFactor_bf, group = date), show.legend = FALSE, outlier.shape=NA, size = 1.5) + 
      geom_jitter(aes(x = date, y = resp_var * scaleFactor_bf, group = date), position=position_jitter(3), cex=0.2) + 
      scale_y_continuous(name = paste0(th_pred_var$short_name," (",th_pred_var$unit,")"), sec.axis = sec_axis(~./scaleFactor_bf, name = "human biting rate (positive counts only)")) +
      scale_x_date(name = "date",date_labels = "%m/%Y", date_breaks = "20 months") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle(th_pred_var$long_name) +
      facet_wrap(.~codevillage)
  
    # plot for whole area
    env_spatiotemporal2 <- env_spatiotemporal %>%
      filter(!is.na(resp_var))
    
    env_spatiotemporal3 <- env_spatiotemporal %>%
      dplyr::select(-resp_var) %>%
      unique() %>%
      group_by(date) %>%
      summarise(val = mean(val))
    
    scaleFactor_bf <- max(env_spatiotemporal3$val, na.rm = T) / max(env_spatiotemporal2$resp_var, na.rm = T)
    
    th_timeplot_wholearea <-  ggplot() + 
      geom_line(aes(x = env_spatiotemporal3$date, y = env_spatiotemporal3$val), size = 0.5, show.legend = FALSE, color='steelblue') +
      geom_boxplot(aes(x = env_spatiotemporal2$date, y = env_spatiotemporal2$resp_var * scaleFactor_bf, group = env_spatiotemporal2$date), show.legend = FALSE, outlier.shape=NA) + 
      geom_jitter(aes(x = env_spatiotemporal2$date, y = env_spatiotemporal2$resp_var * scaleFactor_bf, group = env_spatiotemporal2$date), position=position_jitter(3), cex=0.2) + 
      scale_y_continuous(name = paste0(th_pred_var$short_name," (",th_pred_var$unit,")"), sec.axis = sec_axis(~./scaleFactor_bf, name = "human biting rate (positive counts only)")) +
      scale_x_date(name = "date",date_labels = "%m/%Y", date_breaks = "2 months") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme_minimal(base_size = 10) +
      ggtitle(th_pred_var$long_name)
    
  
  } else if (mod == "presence"){
    
    env_spatiotemporal <- env_spatiotemporal %>%
      mutate(bites = ifelse(resp_var == 1, "aa_presence", "absence")) %>%
      mutate(number = ifelse(is.na(resp_var), 0, 1))
    
    env_spatiotemporal <- env_spatiotemporal[rep(seq_len(nrow(env_spatiotemporal)), 10), ]
    
    scaleFactor_bf <- max(env_spatiotemporal$val, na.rm = T) / 8

    th_timeplot_bf <- ggplot(env_spatiotemporal) + 
      geom_line(aes(x = date, y = val), size = 0.5, show.legend = FALSE, color='steelblue') +
      geom_histogram(aes(x = date, fill = bites), position = position_stack(reverse = TRUE), alpha = 0.8, na.rm = TRUE, size = 2) +
      scale_color_manual(values=c("#00BFC4", "#F8766D", "#00BFC4")) + 
      scale_fill_manual(values=c("#00BFC4", "#F8766D", "#00BFC4")) + 
      labs(fill = "Biting status") +
      scale_y_continuous(limits = c(0,max(env_spatiotemporal$val, na.rm = T)), name = paste0(th_pred_var$short_name," (",th_pred_var$unit,")"), sec.axis = sec_axis(~./scaleFactor_bf, name = "Number of sampling sites", labels = c("2","4","6","8",""))) +
      scale_x_date(name = "date",date_labels = "%m/%Y", date_breaks = "20 months") +
      #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      #theme_minimal(base_size = 10) + 
      ggtitle(th_pred_var$long_name) + 
      facet_wrap(.~codevillage)
    

    # plot for whole area
    th_timeseries_resp_bf <- th_timeseries_resp_bf %>%
           mutate(bites = ifelse(resp_var == 1, "presence", "absence")) %>%
           mutate(n = 1) %>%
           filter(bites == "presence")
    
    env_spatiotemporal3 <- env_spatiotemporal %>%
      dplyr::select(-resp_var) %>%
      unique() %>%
      group_by(date) %>%
      summarise(val = mean(val))
    
     scale <- th_timeseries_resp_bf %>% 
         group_by(nummission, bites) %>%
       summarise(tot = sum(n))    
     
     scaleFactor_bf <- max(env_spatiotemporal3$val, na.rm = T) / max(scale$tot, na.rm = T)

     th_timeseries_resp_bf <- th_timeseries_resp_bf %>%
      group_by(date, bites) %>%
      summarise(val = sum(n)) %>%
       as_tibble()

     
     th_timeplot_wholearea <- NULL
    # df_to_plot <- env_spatiotemporal3 %>% left_join(th_timeseries_resp_bf)
    # 
    # th_timeplot_bf <- ggplot() + 
    #   geom_line(aes(x = env_spatiotemporal3$date, y = env_spatiotemporal3$val), size = 0.5, show.legend = FALSE, color='steelblue') +
    #   geom_bar(aes(x = th_timeseries_resp_bf$date, y = th_timeseries_resp_bf$val, fill = th_timeseries_resp_bf$bites), position = 'dodge', stat = 'identity', alpha = 0.8, na.rm = TRUE, size = 2) +
    #   labs(fill = "Biting status") +
    #   scale_y_continuous(name = paste0(th_pred_var$short_name," (",th_pred_var$unit,")"), sec.axis = sec_axis(~./scaleFactor_bf, name = "Number of sites with presence of bites (>= 1 bite) ")) +
    #   scale_x_date(name = "date",date_labels = "%m/%Y", date_breaks = "2 months") +
    #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    #   theme_minimal(base_size = 10) + 
    #   ggtitle(th_pred_var$long_name)
    # 
    # 
    #     th_timeplot_bf <- ggplot() + 
    #       geom_line(aes(x = env_spatiotemporal3$date, y = env_spatiotemporal3$val), size = 0.5, show.legend = FALSE, color='steelblue') +
    #       geom_bar(aes(x = th_timeseries_resp_bf$date, y = th_timeseries_resp_bf$val, fill = th_timeseries_resp_bf$bites), position = 'dodge', stat = 'identity', alpha = 0.8, na.rm = TRUE, size = 2) +
    #       labs(fill = "Biting status") +
    #   scale_y_continuous(name = paste0(th_pred_var$short_name," (",th_pred_var$unit,")"), sec.axis = sec_axis(~./scaleFactor_bf, name = "Number of sites with presence of bites (>= 1 bite) ")) +
    #   scale_x_date(name = "date",date_labels = "%m/%Y", date_breaks = "2 months") +
    #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    #   theme_minimal(base_size = 10) + 
    #   ggtitle(th_pred_var$long_name)

  }
  
  
  # th_env_spatiotemporal2 <- th_env_spatiotemporal %>% filter(var == expl_var) %>% 
  #   ungroup() %>% 
  #   arrange(desc(abs(ccm_maxcorr_vcor))) %>% 
  #   slice(1L)
  # th_patchwork_bf <- th_env_spatiotemporal$ccm_plot_sup0[[1]]
  # if(nrow(th_env_spatiotemporal)>1){
  #   for(j in 2:nrow(th_env_spatiotemporal)){
  #    th_patchwork_bf <- th_patchwork_bf + th_env_spatiotemporal$ccm_plot_sup0[[j]]
  #   }
  # }
  # th_ccm_bf <- th_env_spatiotemporal2$ccm_plot_sup0[[nrow(th_env_spatiotemporal2)]]
  # 
  # th_var_df <- th_env_spatiotemporal2$var_df[[nrow(th_env_spatiotemporal2)]]
  # 
  # th_var_plot_bf <- th_env_spatiotemporal2$predictive_df[[nrow(th_env_spatiotemporal2)]] %>%
  #   right_join(th_timeseries_resp_bf, by = "idpointdecapture") %>%
  #   dplyr::select(resp_var,!!th_var_df) %>%
  #   set_names(c(response_var,th_pred_var$code))
  # 
  # th_plot_bf <- ggplot(th_var_plot_bf, aes_string(y=th_pred_var$code, x=response_var)) + geom_point(size = 1, alpha = 0.5) + theme_minimal(base_size = 9) + ggtitle(paste0(code_pays," - ",th_pred_var$short_name," for time lags (",th_env_spatiotemporal$ccm_maxcorr_lag1,",",th_env_spatiotemporal$ccm_maxcorr_lag2,") and buffer size ",th_env_spatiotemporal$buffer)) + ylab(paste0(th_pred_var$short_name," (",th_pred_var$unit,")"))
  # 
  # 
  # return(list(plot1 = th_timeplot_bf, plot2 = th_plot_bf))
  
  return(list(th_timeplot_village = th_timeplot_bf,th_timeplot_wholearea = th_timeplot_wholearea))
  
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


fun_remove_var_buffer <- function(env_spatiotemporal_abundance, vars_to_test, corr_threshold){ ######## à finir
  
  rul <- env_spatiotemporal_abundance %>% 
    filter(var %in% vars_to_test) %>%
    arrange(buffer)
  
  if(nrow(rul) > 1){
    for(i in 1:(nrow(rul)-1)){
      corr <- as.numeric(cor(dplyr::select(rul$predictive_df[[i]], rul$var_df[[i]]), dplyr::select(rul$predictive_df[[i+1]], rul$var_df[[i+1]]), method = "spearman", use = "na.or.complete"))
      if(corr >= corr_threshold){
        env_spatiotemporal_abundance <- env_spatiotemporal_abundance %>% filter(!(var==vars_to_test && buffer==rul$buffer[i]))
      }
    }
  }
  
  return(env_spatiotemporal_abundance)
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



lsm_rules <- function(lay){
  
  var_to_rm <- NULL
  
  # if same layer, same lsm, same class, different buffer : keep biggest buffer  (for easier collection)
  if(lay$pixval.x==lay$pixval.y && lay$layer_id.x==lay$layer_id.y && lay$function_name.x==lay$function_name.y){
    if(lay$buffer.x > lay$buffer.y){ var_to_rm <- lay$stock2  } else { var_to_rm <- lay$stock1 }
  }
  
  # if same layer, same buffer, same class, different lsm : keep pland  (for easier interpretation)
  if(lay$pixval.x==lay$pixval.y && lay$layer_id.x==lay$layer_id.y && lay$buffer.x==lay$buffer.y){
    if(lay$function_name.x == "lsm_c_pland"){ var_to_rm <- lay$stock2  } else { var_to_rm <- lay$stock1 }
  }
  
  # if same layer, same buffer, same lsm, different class : keep pland  (for easier interpretation)
  if(lay$pixval.x==lay$pixval.y && lay$layer_id.x==lay$layer_id.y && lay$buffer.x==lay$buffer.y){
    if(lay$function_name.x == "lsm_c_pland"){ var_to_rm <- lay$stock2  } else { var_to_rm <- lay$stock1 }
  }
  
  if(is.null(var_to_rm)) { stop("error")}
  return(var_to_rm)
  
}









load_spatiotemporal_data <- function(vars, buffers, lag_time_window, summarize_days_to_week, code_pays, entomo_csh_metadata_l1){
  
  env_spatiotemporal <- dbReadTable(react_gpkg, 'env_spatiotemporal') %>% dplyr::select(-fid) %>% mutate(date = as.Date(date)) %>% dplyr::rename(idpointdecapture = id) %>% filter(var %in% vars, buffer %in% buffers, lag_time >= lag_time_window[1] , lag_time <= lag_time_window[2])
  
  if(summarize_days_to_week){
  
  fun_summarize_week <- function(var_to_summarize){
    
    if(var_to_summarize=="RFD1F"){
      env_spatiotemporal_summarize <- env_spatiotemporal %>%
        filter(var==var_to_summarize) %>%
        group_by(idpointdecapture,buffer,lag_n = lubridate::week(date)) %>%
        summarise(val=sum(val, na.rm = T),date = min(date)) %>%
        group_by(idpointdecapture,buffer) %>%
        mutate(lag_n=seq(n()-1,0,-1)) %>%
        mutate(var = gsub("1","7",var_to_summarize), lag_time = NA) %>%
        as_tibble()
    } else {
    env_spatiotemporal_summarize <- env_spatiotemporal %>%
      filter(var==var_to_summarize) %>%
      group_by(idpointdecapture,buffer,lag_n = lubridate::week(date)) %>%
      summarise(val=mean(val, na.rm = T),date = min(date)) %>%
      group_by(idpointdecapture,buffer) %>%
      mutate(lag_n=seq(n()-1,0,-1)) %>%
      mutate(var = gsub("1","7",var_to_summarize), lag_time = NA) %>%
      as_tibble()
    }
    return(env_spatiotemporal_summarize)
    
  }
  
  env_spatiotemporal <- env_spatiotemporal %>%
    bind_rows(fun_summarize_week("TMAX1")) %>%
    bind_rows(fun_summarize_week("TMIN1")) %>%
    bind_rows(fun_summarize_week("SMO1")) %>%
    bind_rows(fun_summarize_week("RFD1F"))
    
  }
  
  # spatiotemporal
  env_spatiotemporal <- env_spatiotemporal %>%
    mutate(buffer = as.character(buffer)) %>%
    right_join(entomo_csh_metadata_l1[,c("idpointdecapture","codepays")]) %>%
    left_join(prediction_vars[,c("code","type_group1","short_name","temporal_aggregation_days")], by = c("var" = "code"))
  
  env_spatiotemporal <- env_spatiotemporal %>%
    dplyr::select(-c(lag_time, date)) %>%
    group_by(type_group1, var, codepays, buffer) %>%
    tidyr::nest(predictive_df = c(idpointdecapture, lag_n , val)) %>%
    mutate(fun_summarize_ccm = ifelse(var %in% c("RFD1F","RFD1L","RFD7_F","RFD7_L"), "sum", "mean")) %>%
    arrange(type_group1, var, codepays, as.numeric(buffer), fun_summarize_ccm) #%>%
  # filter(!(var %in% c("RFD1_L","TAMP7","TMAX7","TMIN7"))) %>%
  # filter(!(var %in% c("RFD1_F","SMO1","VEV8","VNV8","EVT8","TMAX1","TMIN1","TAMP1") && buffer %in% c("500","1000"))) %>%
  # filter(!(var %in% c("VNV30","VMV30","WNW30","WVV10","WVH10") && buffer %in% c("500","1000","2000")))
  
  
  env_spatiotemporal <- env_spatiotemporal %>% 
    mutate(predictive_df = pmap(list(predictive_df, var, buffer, fun_summarize_ccm), ~fun_ccm_df(..1, ..2, ..3, function_to_apply = ..4))) %>%
    filter(codepays==code_pays)
  
    
  th_env_spatiotemporal <- env_spatiotemporal$predictive_df[[1]]
  
  for(i in 2:nrow(env_spatiotemporal)){
    th_env_spatiotemporal <- left_join(th_env_spatiotemporal,env_spatiotemporal$predictive_df[[i]])
  }
  
  return(th_env_spatiotemporal)
  
}




load_spatial_data <- function(code_pays, landcover_layers_to_keep, mod, landcover_metrics_to_keep = NULL){
  
  # extract ligth from satellite data
  LIG <- dbReadTable(react_gpkg, 'env_spatiotemporal') %>% dplyr::select(-fid) %>% mutate(date = as.Date(date)) %>% dplyr::rename(idpointdecapture = id) %>% filter(var == "LIG30", lag_n == 3, buffer >= 500) %>% dplyr::select(idpointdecapture,buffer,val,var)
  
  # spatial-only explanatory variables
  env_spatial <- dbReadTable(react_gpkg,'env_spatial') %>% dplyr::select(-fid) %>% dplyr::rename(idpointdecapture = id) %>% filter(buffer >= 500, var!="POH")
  
  # non-spatial explanatory variables
  env_static <- dbReadTable(react_gpkg, 'env_static') %>% dplyr::select(-fid) %>% dplyr::rename(idpointdecapture = id)
  
  # variables for the night of catch
  env_nightcatch <- dbReadTable(react_gpkg, 'env_nightcatch') %>% dplyr::select(-fid) %>% dplyr::rename(idpointdecapture = id) %>% dplyr::filter(var != "WDR")
  
  # variables for the night of catch at the postedecapture level
  if(mod %in% c("presence","abundance")){
    env_nightcatch_postedecapture <- dbReadTable(react_gpkg,"env_nightcatch_postedecapture") %>% dplyr::select(-fid)
    th_env_nightcatch_postedecapture <- env_nightcatch_postedecapture %>% pivot_wider(names_from = var, values_from = val) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
  } else if(mod %in% c("physiological_resistance_kdrw","exophagy","late_prec_aggressiveness")){
    th_env_nightcatch_postedecapture <- read.csv("/home/ptaconet/Bureau/data_anglique.csv") %>%
      dplyr::select(idpostedecapture,date_time,temperature_hygro,humidity_hygro,pointderosee_hygro,luminosite_hobo,pressure_baro) %>%
      dplyr::rename(NMT = temperature_hygro, NMH = humidity_hygro, NDP = pointderosee_hygro, NML = luminosite_hobo, NMA = pressure_baro) %>%
      mutate(date = as.Date(date_time), heuredecapture = hour(date_time)) %>%
      dplyr::group_by(idpostedecapture,heuredecapture) %>%
      dplyr::summarise(NMT = mean(NMT,na.rm = T), NMH = mean(NMH,na.rm = T), NDP=mean(NDP,na.rm = T), NML=mean(NML,na.rm = T), NMA=mean(NMA,na.rm = T)) %>%
      as_tibble()
  }
  
  # landcover variables
  env_landcover <-  dbReadTable(react_gpkg, 'env_landcover') %>% dplyr::select(-fid) %>% dplyr::rename(idpointdecapture = id) %>% filter(buffer >= 500, layer_id %in% landcover_layers_to_keep)
  
  if(!is.null(landcover_metrics_to_keep)){
    env_landcover <- env_landcover %>% filter(metric %in% landcover_metrics_to_keep)
  }
  
  metrics_defs <- landscapemetrics::list_lsm() # list of landscape metrics
  

  if(code_pays == "BF"){
    lid <-  c(2,3,11,12)
  } else if (code_pays == "CI"){
    lid <-  c(7,8,11,12)
  }
  
  env_landcover <- env_landcover %>%
    left_join(metrics_defs) %>%
    dplyr::select(-c(level,metric,name,type)) %>%
    pivot_wider(names_from = c(function_name,buffer,layer_id,pixval), values_from = val, names_sep = "_", values_fill = list(val = 0)) %>%
    mutate_all(funs(replace_na(.,0)))

  # all other data
  env_spatial <- env_spatial %>% 
    #bind_rows(LIG) %>% 
    pivot_wider(names_from = c("var","buffer"), values_from = val) %>% 
    mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
  th_env_nightcatch  <- env_nightcatch %>% pivot_wider(names_from = var, values_from = val) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
  th_env_static <- env_static %>% pivot_wider(names_from = var, values_from = val) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>% mutate(VCT=as.numeric(VCT), WMD=as.numeric(WMD),BDE=as.numeric(BDE),VCP=ifelse(VCP=="TRUE",1,0))
  
  return(list(env_landcover, env_spatial, th_env_nightcatch_postedecapture, th_env_nightcatch, th_env_static))
  
}
  

load_llinuse_data <- function(code_pays,entomo_csh_metadata_l1){
  
tab = dbReadTable(react_gpkg, 'entomo_comportementhumain_l0') %>% 
  filter(codepays == code_pays) %>%
  group_by(codevillage,dateenquete,dormirssmoust) %>%
  summarise(n=n()) %>%
  as_tibble() %>%
  pivot_wider(names_from = dormirssmoust, values_from = n, values_fill = list(n = 0)) %>%
  mutate(LUS = oui / (non + oui) * 100) %>%
  mutate(dateenquete = as.Date(dateenquete)) %>%
  dplyr::select(codevillage,dateenquete,LUS)

t = entomo_csh_metadata_l1 %>%
  mutate(date_capture = as.Date(date_capture)) %>%
  left_join(tab, by = "codevillage") %>%
  mutate(dateDiff = abs(date_capture - dateenquete)) %>%
  group_by(codevillage, date_capture) %>%
  filter(dateDiff == min(dateDiff)) %>%
  as_tibble() %>%
  dplyr::select(idpointdecapture,LUS)

return(t)
}

load_csh_sp_coord <- function(){
  
  # spatial coordinates
  mean_coords_points_4326 = st_read(path_to_db, 'entomo_csh_metadata_l1', crs = 4326) %>%
    dplyr::group_by(codevillage,pointdecapture) %>%
    dplyr::summarise(X_4326=mean(X),Y_4326=mean(Y)) %>%
    st_drop_geometry() %>%
    st_as_sf(coords = c("X_4326", "Y_4326"), crs = 4326)
  
  mean_coords_points_4326$X_4326 = as.numeric(st_coordinates(mean_coords_points_4326)[,1])
  mean_coords_points_4326$Y_4326 = as.numeric(st_coordinates(mean_coords_points_4326)[,2])
  mean_coords_points_4326 = st_drop_geometry(mean_coords_points_4326) %>% as_tibble() %>% mutate(codevillage=as.character(codevillage), pointdecapture=as.character(pointdecapture))
  
  mean_coords_points_32630 = st_read(path_to_db, 'entomo_csh_metadata_l1', crs = 4326) %>%
    dplyr::group_by(codevillage,pointdecapture) %>%
    dplyr::summarise(X_32630=mean(X),Y_32630=mean(Y)) %>%
    st_drop_geometry() %>%
    st_as_sf(coords = c("X_32630", "Y_32630"), crs = 4326) %>%
    st_transform(32630)
  
  mean_coords_points_32630$X_32630 = as.numeric(st_coordinates(mean_coords_points_32630)[,1])
  mean_coords_points_32630$Y_32630 = as.numeric(st_coordinates(mean_coords_points_32630)[,2])
  mean_coords_points_32630 = st_drop_geometry(mean_coords_points_32630) %>% as_tibble() %>% mutate(codevillage=as.character(codevillage), pointdecapture=as.character(pointdecapture))
  
  return(list(mean_coords_points_4326, mean_coords_points_32630))
  
}


fun_ffs_tempvar <- function(df, model_type, mod, time_vars, cols_to_keep, timevars_selection = "1day"){
  
  modcomp <- data.frame(name = character(), res = numeric(), diff_res_w_basemod = numeric(), var = character())
  
  for(i in 1:length(time_vars)){
    cat('calculating column to keep for temporal variable ',time_vars[i],"\n")
    expl_vars_to_test =  colnames(df[which(grepl(time_vars[i],colnames(df)))])
    
    if(timevars_selection == "7day"){
      expl_vars_to_test <- c(paste0(time_vars[i],"_2000_0_7"),
                             paste0(time_vars[i],"_2000_7_14"),
                             paste0(time_vars[i],"_2000_14_21"),
                             paste0(time_vars[i],"_2000_21_28"),
                             paste0(time_vars[i],"_2000_0_14"),
                             paste0(time_vars[i],"_2000_0_21"),
                             paste0(time_vars[i],"_2000_0_28"),
                             paste0(time_vars[i],"_2000_7_21"),
                             paste0(time_vars[i],"_2000_7_28"),
                             paste0(time_vars[i],"_2000_14_21"),
                             paste0(time_vars[i],"_2000_14_28")
      )
    }
    
    if(grepl("1", time_vars[i])){
      time_lag1 <- as.numeric(sub('.*\\_', '', expl_vars_to_test))
      time_lag2 <- as.numeric(stringr::str_match(expl_vars_to_test, '([^_]+)(?:_[^_]+){1}$')[,2])
      diff_lag <- time_lag1 - time_lag2
      ##########expl_vars_to_test <- expl_vars_to_test[which(diff_lag >= 4)]
    }
    
    # remove colinear variables
    ind_to_rm <- NULL
    for(i in 1:length(expl_vars_to_test)){
      for(j in 1:length(cols_to_keep)){
        m <- cor(df[,expl_vars_to_test[i]],df[,cols_to_keep[j]], method = "pearson", use = "na.or.complete")
        if(abs(m) > 0.7){
          ind_to_rm <- c(ind_to_rm, i)
          }
      }
    }
    if(!is.null(ind_to_rm)){
      expl_vars_to_test <- expl_vars_to_test[-unique(ind_to_rm)]
    }
    
    if(model_type == "rf" & mod == "abundance"){
      logtransform_resp_var = TRUE
    } else {
      logtransform_resp_var = FALSE
    }
    
    if(model_type == "rf"){
      cols_to_keep <- c(cols_to_keep,"X_32630", "Y_32630")
      cross_validation_type <- "temporal"
    } else {
      cross_validation_type <- NULL
    }
    
    if(length(expl_vars_to_test) > 0){
      th_modcomp <- fun_feature_forward_selection(df = df, 
                                                stat_method = model_type,
                                                mod = mod, 
                                                type = "model_comparison",
                                                expl_vars_to_test = expl_vars_to_test, 
                                                expl_vars_to_keep = cols_to_keep, 
                                                logtransform_resp_var = logtransform_resp_var,
                                                cross_validation_type = cross_validation_type
                                                )
    th_modcomp$var <- time_vars[i]
    
    modcomp <- rbind(modcomp, th_modcomp)
    }
  }
  
  return(modcomp)
}


fun_glmm_cross_validation <- function(indices_cv, th_mod, mod, df, ind_vars_to_center){

  df <- df %>% dplyr::select(c("resp_var","codevillage","pointdecapture2","int_ext","VCM",ind_vars_to_center))
    
  df$pred <- NA
  
  for (i in 1:length(indices_cv$index)){
    df_train <- df[indices_cv$index[[i]],]
    df_test <- df[indices_cv$indexOut[[i]],]
    
    # here, scale the numerical explanatory variables for train and test datasets
    df_train <- df_train %>% mutate_at(ind_vars_to_center, ~scale(., center = TRUE, scale = FALSE))
    df_test <- df_test %>% mutate_at(ind_vars_to_center, ~scale(., center = TRUE, scale = FALSE))
    
    if(mod == "abundance"){
      th_mod_th_it <- glmmTMB(formula(th_mod), data = df_train, family = truncated_nbinom2)
      preds_th_it <- predict(th_mod_th_it, newdata = df_test, type = "response", allow.new.levels=TRUE)
    } else if (mod %in% c("physiological_resistance_kdrw","exophagy","late_prec_aggressiveness","presence")){
      th_mod_th_it <- glmmTMB(formula(th_mod), data = df_train, family = binomial(link = "logit"))
      preds_th_it <- predict(th_mod_th_it, newdata = df_test, type = "response", allow.new.levels=TRUE)
    }
    
    df$pred[indices_cv$indexOut[[i]]] <- preds_th_it # fill in df with predictions out of CV
    
  }
  
  if(mod == "abundance"){
    mean_metric <- cor(df$pred, df$resp_var, use = "na.or.complete")^2
  } else if (mod %in% c("physiological_resistance_kdrw","exophagy","late_prec_aggressiveness","presence")){
    mean_metric <- mltools::auc_roc(df$pred, df$resp_var)  
  }
  
  return(list(mean_metric = mean_metric, df_cv = df))
  
}
