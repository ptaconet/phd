
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

fun_feature_forward_selection <- function(stat_method, spearman_factor = NULL, mod = NULL, type, df, expl_vars_to_keep = NULL, expl_vars_to_test, cross_validation_type = NULL, logtransform_resp_var = FALSE, tune_length = 5){
  
  
  if(logtransform_resp_var == TRUE){
    df$resp_var <- log(df$resp_var)
  }
  
  # spearman coeff
  if(stat_method == "spearman"){
    library(correlation)
    df <- df[,c("resp_var",spearman_factor,expl_vars_to_keep,expl_vars_to_test)]
    if(!is.null(spearman_factor)){ df[,spearman_factor] <- as.factor(df[,spearman_factor]) }
    
    df$useless_col <-  sample(100, size = nrow(df), replace = TRUE)

    
    correlation <- try(sapply(df[,ifelse(!is.null(spearman_factor),3,2):(ncol(df))], function(x) correlation::correlation(cbind(as.data.frame(x),df[,c("resp_var",spearman_factor)]), method = "spearman", multilevel = ifelse(!is.null(spearman_factor),TRUE,FALSE))))
    
    if(inherits(correlation, "try-error")){
      correlation <- sapply(df[,ifelse(!is.null(spearman_factor),3,2):(ncol(df))], function(x) tryCatch(correlation::correlation(cbind(as.data.frame(x),df[,c("resp_var",spearman_factor)]), method = "spearman", multilevel = ifelse(!is.null(spearman_factor),TRUE,FALSE)), error=function(e) NULL))
      correlation <- correlation[!sapply(correlation,is.null)]
      cor_names <- names(correlation)
      correlation_df <- do.call(rbind.data.frame, correlation)
      correlation_df$correlation <- correlation_df$r
      correlation_df$abs_corr <- abs(correlation_df$correlation)
      correlation_df$name <- cor_names
      } else {
    
    correlation <- as.data.frame(correlation)
    correlation_df <- do.call(rbind.data.frame, correlation)
    correlation_df$correlation <- correlation_df$r
    correlation_df$abs_corr <- abs(correlation_df$correlation)
    correlation_df$name <- rownames(correlation_df)
    correlation_df <- correlation_df %>% filter(name!="useless_col")
      }

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
      if(mod %in% c("physiological_resistance_kdrw","physiological_resistance_kdre","exophagy","early_late_biting","presence")){
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
     
      vars_char <- df %>% dplyr::select(which(sapply(.,is.character))) %>% colnames() %>% intersect(expl_vars_to_test)
      vars_char <- df %>% dplyr::select(vars_char)
      vars_to_scale <- df %>% dplyr::select(which(sapply(.,is.numeric))) %>% colnames() %>% intersect(expl_vars_to_test)
      
      to_scale <- df[,c(vars_to_scale)]
      to_scale <- as.data.frame(scale(to_scale, center = TRUE, scale = TRUE))  # scale = FALSE
      pointdecapture2 <- as.data.frame( as.factor(paste0(df$codevillage,df$pointdecapture)))
      colnames(pointdecapture2) = "pointdecapture2"
      df <- cbind(df[,c("resp_var","codevillage","VCM","int_ext")],pointdecapture2,to_scale,vars_char)
      if(mod %in% c("physiological_resistance_kdrw","physiological_resistance_kdre","exophagy","early_late_biting","presence")){
        #pvals <- future_map_dfr(colnames(df[4:ncol(df)]), ~Anova(glmmTMB(as.formula(paste0("resp_var ~ ",.x," + (1|codevillage/pointdecapture2)")), data = df, family = binomial(link = "logit")))[3])
        func <- function(x){
          pval <- glmmTMB(as.formula(paste0("resp_var ~ ",x," + (1|codevillage/pointdecapture2)")), data = df, family = binomial(link = "logit"))
          return(pval)
        }
        } else if (mod == "abundance"){
        #pvals <- future_map_dfr(colnames(df[4:ncol(df)]), ~Anova(glmmTMB(as.formula(paste0("resp_var ~ ",.x," + (1|codevillage/pointdecapture2)")), data = df, family = truncated_nbinom2))[3])
          func <- function(x){
            pval <- glmmTMB(as.formula(paste0("resp_var ~ ",x," + (1|codevillage/pointdecapture2)")), data = df, family = truncated_nbinom2)
            return(pval)
          }
      }
      
      possible_a <- possibly(func, otherwise = NULL)
      pvals <- future_map(colnames(df[6:ncol(df)]), possible_a)
      
      names(pvals) <- colnames(df[6:ncol(df)])
      
      # pvals <- as.data.frame(pvals)
      # pvals$name <- expl_vars_to_test
      # colnames(pvals) <- c("pval","name")

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
    } else if (cross_validation_type == "spatiotemporal"){
      indices <- CreateSpacetimeFolds(df, timevar = "nummission", spacevar = "codevillage", k = ifelse(mod=="abundance",3,4), seed = 10) # set seed for reproducibility as folds of spatiotemporal cv can change 
    }
    
    if(mod == "abundance"){
      tr = trainControl(method="cv",
                      index = indices$index, 
                      indexOut = indices$indexOut)
      met = "RMSE"
    } else if (mod %in% c("physiological_resistance_kdrw","physiological_resistance_kdre","exophagy","early_late_biting","presence")){
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
    } else if(mod == "abundance_discrete"){
      tr = trainControl(method="cv",
                        index = indices$index, 
                        indexOut = indices$indexOut)
      met = "Accuracy"

    }
    
    if(type == "model_comparison"){
      
      mod_base <- caret::train(form = as.formula(paste0("resp_var ~ ",paste(expl_vars_to_keep,collapse="+"))), data = df, method="rf", tuneLength = tune_length, trControl=tr, metric = met)
      
      #predict(mod_base,type = "prob")
      #confusionMatrix(predict(mod_base), df$resp_var, positive = "Presence")
      
      if(mod == "abundance"){
        qual_basemod <- min(mod_base$results$RMSE)
        #qual_basemod <- max(mod_base$results$Rsquared)
      } else if (mod %in% c("physiological_resistance_kdrw","physiological_resistance_kdre","exophagy","early_late_biting","presence")){
        qual_basemod <- max(mod_base$results$AUC)
      } else if(mod == "abundance_discrete"){
        qual_basemod <- max(mod_base$results$Accuracy)
      }
    } else if (type=="univariate_selection"){
      qual_basemod <- 0
    }
      
      fun_univ_rf <- function(predictors){
        predictors <- c(predictors,expl_vars_to_keep)
        predictors <- paste(predictors,collapse="+")
        model <- caret::train(form = as.formula(paste0("resp_var ~ ",predictors)), data = df, method="ranger", tuneLength = tune_length, trControl=tr, metric = met)
        if(mod == "abundance"){
          ret <- min(model$results$RMSE)
          #ret <- max(model$results$Rsquared)
        } else if (mod %in% c("physiological_resistance_kdrw","physiological_resistance_kdre","exophagy","early_late_biting","presence")){
          ret <- max(model$results$AUC)
        } else if(mod == "abundance_discrete"){
          ret <- max(model$results$Accuracy)
        }
        return(ret)
      }
      
      results <- future_map_dbl(expl_vars_to_test, ~fun_univ_rf(.x))
      results <- data.frame(name = expl_vars_to_test, res = results)
      if(mod == "abundance"){
        results$diff_res_w_basemod <- qual_basemod - results$res 
      } else {
        results$diff_res_w_basemod <- results$res - qual_basemod
      }
      
      return(results)
    
    
    
  }
    
  
}





#### plots

# function to plot the CCM (simple plot : only the CCM)
fun_ccm_plot <- function(correlation_df, max_or_min, metric_name, var, filter_lags_ndays = 0, mod = "presence"){
  
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
    coord_fixed() +
    ylab(paste0("time lag 2 (",ifelse(grepl(1,correlation_df$name[1]),"days","weeks")," before the catch)")) +
    xlab(paste0("time lag 1 (",ifelse(grepl(1,correlation_df$name[1]),"days","weeks")," before the catch)"))
  
  if(metric_name=="Spearman\ncorrelation"){
    ccm_plot <- ccm_plot + 
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name = metric_name) 
  } else if(metric_name=="RMSE"){
    ccm_plot <- ccm_plot + 
     scale_fill_gradient(low = "red", high = "white", space = "Lab", name="RMSE") 
  } else if(metric_name=="estimate"){
    ccm_plot <- ccm_plot + 
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = ifelse(mod == "abundance", 0, 1), limit = c(min(correlation_df$correlation), max(correlation_df$correlation)), space = "Lab", name = ifelse(mod == "abundance","DDR","ODR")) 
  }
  
  return(ccm_plot)
  
}




# function to plot the time series of the response variable and the explanatory variable
fun_spatiotemporal_plots <- function(th_trmetrics_entomo_postedecapture, mod, expl_var, codepays){
  
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
    # scaleFactor_bf <- max(env_spatiotemporal$val, na.rm = T) / max(env_spatiotemporal$resp_var, na.rm = T)
    # 
    # th_timeplot_bf <-  ggplot(env_spatiotemporal) + 
    #   geom_line(aes(x = date, y = val), size = 0.5, show.legend = FALSE, color='steelblue') +
    #   geom_boxplot(aes(x = date, y = resp_var * scaleFactor_bf, group = date), show.legend = FALSE, outlier.shape=NA, size = 1.5) + 
    #   geom_jitter(aes(x = date, y = resp_var * scaleFactor_bf, group = date), position=position_jitter(3), cex=0.2) + 
    #   scale_y_continuous(name = paste0(th_pred_var$short_name," (",th_pred_var$unit,")"), sec.axis = sec_axis(~./scaleFactor_bf, name = "human biting rate (positive counts only)")) +
    #   scale_x_date(name = "date",date_labels = "%m/%Y", date_breaks = "20 months") +
    #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    #   ggtitle(th_pred_var$long_name) +
    #   facet_wrap(.~codevillage)
  
    # plot for whole area
    env_spatiotemporal2 <- env_spatiotemporal %>%
      filter(!is.na(resp_var))
    
    env_spatiotemporal3 <- env_spatiotemporal %>%
      dplyr::select(-resp_var) %>%
      unique() %>%
      group_by(date) %>%
      summarise(val = mean(val)) %>%
      filter(date > min(env_spatiotemporal2$date)-40, date < max(env_spatiotemporal2$date)+10 )
    
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
    
    # env_spatiotemporal <- env_spatiotemporal %>%
    #   mutate(bites = ifelse(resp_var == 1, "aa_presence", "absence")) %>%
    #   mutate(number = ifelse(is.na(resp_var), 0, 1))
    # 
    # env_spatiotemporal <- env_spatiotemporal[rep(seq_len(nrow(env_spatiotemporal)), 10), ]
    # 
    # scaleFactor_bf <- max(env_spatiotemporal$val, na.rm = T) / 8
    # 
    # th_timeplot_bf <- ggplot(env_spatiotemporal) + 
    #   geom_line(aes(x = date, y = val), size = 0.5, show.legend = FALSE, color='steelblue') +
    #   geom_histogram(aes(x = date, fill = bites), position = position_stack(reverse = TRUE), alpha = 0.8, na.rm = TRUE, size = 2) +
    #   scale_color_manual(values=c("#00BFC4", "#F8766D", "#00BFC4")) + 
    #   scale_fill_manual(values=c("#00BFC4", "#F8766D", "#00BFC4")) + 
    #   labs(fill = "Biting status") +
    #   scale_y_continuous(limits = c(0,max(env_spatiotemporal$val, na.rm = T)), name = paste0(th_pred_var$short_name," (",th_pred_var$unit,")"), sec.axis = sec_axis(~./scaleFactor_bf, name = "Number of sampling sites", labels = c("2","4","6","8",""))) +
    #   scale_x_date(name = "date",date_labels = "%m/%Y", date_breaks = "20 months") +
    #   #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    #   #theme_minimal(base_size = 10) + 
    #   ggtitle(th_pred_var$long_name) + 
    #   facet_wrap(.~codevillage)
    

    # plot for whole area
    th_timeseries_resp_bf <- th_timeseries_resp_bf %>%
           mutate(bites = ifelse(resp_var == 1, "presence", "absence")) %>%
           mutate(n = 1) %>%
           filter(bites == "presence")
    
    env_spatiotemporal3 <- env_spatiotemporal %>%
      dplyr::select(-resp_var) %>%
      unique() %>%
      group_by(date) %>%
      summarise(val = mean(val)) %>%
      filter(date > min(th_timeseries_resp_bf$date)-40, date < max(th_timeseries_resp_bf$date)+10 )
    
     # scale <- th_timeseries_resp_bf %>% 
     #     group_by(nummission, bites) %>%
     #   summarise(tot = sum(n))    
     # 
     # scaleFactor_bf <- max(env_spatiotemporal3$val, na.rm = T) / max(scale$tot, na.rm = T)
    
     scaleFactor_bf <- max(env_spatiotemporal3$val, na.rm = T) / 100
    

     th_timeseries_resp_bf <- th_timeseries_resp_bf %>%
      group_by(date, bites) %>%
      summarise(val = round(sum(n) / 216 * 100)) %>%  # 216 is for BF, to change for CI
       as_tibble()

     
     th_timeplot_wholearea <- ggplot() +
      geom_line(aes(x = env_spatiotemporal3$date, y = env_spatiotemporal3$val), size = 0.5, show.legend = FALSE, color='steelblue') +
      geom_point(aes(x = th_timeseries_resp_bf$date, y = th_timeseries_resp_bf$val), size = 4) +
      labs(fill = "Biting status") +
      scale_y_continuous(name = paste0(th_pred_var$short_name," (",th_pred_var$unit,")"), sec.axis = sec_axis(~./scaleFactor_bf, name = "% of sites sampled with >= 1 bite(s)")) +
      scale_x_date(name = "date",date_labels = "%m/%Y", date_breaks = "2 months") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme_minimal(base_size = 10) +
      ggtitle(th_pred_var$long_name)

      #   th_timeplot_bf <- ggplot() +
      #     geom_line(aes(x = env_spatiotemporal3$date, y = env_spatiotemporal3$val), size = 0.5, show.legend = FALSE, color='steelblue') +
      #     geom_bar(aes(x = th_timeseries_resp_bf$date, y = th_timeseries_resp_bf$val, fill = th_timeseries_resp_bf$bites), position = 'dodge', stat = 'identity', alpha = 0.8, na.rm = TRUE, size = 2) +
      #     labs(fill = "Biting status") +
      # scale_y_continuous(name = paste0(th_pred_var$short_name," (",th_pred_var$unit,")"), sec.axis = sec_axis(~./scaleFactor_bf, name = "Number of sites with presence of bites (>= 1 bite) ")) +
      # scale_x_date(name = "date",date_labels = "%m/%Y", date_breaks = "2 months") +
      # theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      # theme_minimal(base_size = 10) +
      # ggtitle(th_pred_var$long_name)

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
  
  #return(list(th_timeplot_village = th_timeplot_bf,th_timeplot_wholearea = th_timeplot_wholearea))
  return(th_timeplot_wholearea)
  
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
    filter(!is.na(buffer)) %>%
    dplyr::select(-c(lag_time, date)) %>%
    group_by(type_group1, var, codepays, buffer) %>%
    tidyr::nest(predictive_df = c(idpointdecapture, lag_n , val)) %>%
    mutate(fun_summarize_ccm = ifelse(var %in% c("RFD1F","RFD1L","RFD7_F","RFD7_L"), "sum", "mean")) %>%
    arrange(type_group1, var, codepays, as.numeric(buffer), fun_summarize_ccm)
  
  env_spatiotemporal <- env_spatiotemporal %>% 
    mutate(predictive_df = pmap(list(predictive_df, var, buffer, fun_summarize_ccm), ~fun_ccm_df(..1, ..2, ..3, function_to_apply = ..4))) %>%
    filter(codepays==code_pays)
  
  th_env_spatiotemporal <- env_spatiotemporal$predictive_df[[1]]
  
  for(i in 2:nrow(env_spatiotemporal)){
    th_env_spatiotemporal <- left_join(th_env_spatiotemporal,env_spatiotemporal$predictive_df[[i]])
  }
  
  
  
  
  return(th_env_spatiotemporal)
  
}




load_spatial_data <- function(code_pays, landcover_layers_to_keep, mod, landcover_metrics_to_keep = NULL, buffer_sizes){
  
  # extract human pop and number of animals
  POPANI <- dbReadTable(react_gpkg, 'recensement_villages_l1') %>% dplyr::select(-c(fid,geom)) %>% mutate(ANI=Bovin_stab+Mouton_stab+Chevre_stab+Porc_stab+Bov_extsed+Bov_trans) %>% dplyr::select(codevillage,population,ANI) %>% rename(POP=population) %>% mutate(POPANI = POP/ANI)
  
  # extract ligth from satellite data
  LIG <- dbReadTable(react_gpkg, 'env_spatiotemporal') %>% dplyr::select(-fid) %>% mutate(date = as.Date(date)) %>% dplyr::rename(idpointdecapture = id) %>% filter(var == "LIG30", lag_n == 3, buffer >= 500) %>% dplyr::select(idpointdecapture,buffer,val,var) %>% filter(buffer %in% buffer_sizes)
  
  # spatial-only explanatory variables
  env_spatial <- dbReadTable(react_gpkg,'env_spatial') %>% dplyr::select(-fid) %>% dplyr::rename(idpointdecapture = id) %>% filter( var!="POH") %>% filter(buffer %in% buffer_sizes)
  BCH_POP <- dbReadTable(react_gpkg,'env_spatial') %>% dplyr::select(-fid) %>% dplyr::rename(idpointdecapture = id) %>% filter(var %in% c("BCH","POP","POH"), buffer==500) %>% mutate(buffer=2000)
  env_spatial <- rbind(env_spatial,BCH_POP)
  
  # non-spatial explanatory variables
  env_static <- dbReadTable(react_gpkg, 'env_static') %>% dplyr::select(-fid) %>% dplyr::rename(idpointdecapture = id)
  
  # variables for the night of catch
  env_nightcatch <- dbReadTable(react_gpkg, 'env_nightcatch') %>% dplyr::select(-fid) %>% dplyr::rename(idpointdecapture = id) %>% dplyr::filter(var != "WDR")
  
  
  # from : https://physics.stackexchange.com/questions/4343/how-can-i-calculate-vapor-pressure-deficit-from-temperature-and-relative-humidit
  get.vpd <- function(rh, temp){
    ## calculate saturation vapor pressure
    es <- 6.11 * exp((2.5e6 / 461) * (1 / 273 - 1 / (273 + temp)))
    ## calculate vapor pressure deficit
    vpd <- ((100 - rh) / 100) * es
    return(vpd)
  }
  
  
  # variables for the night of catch at the postedecapture level
 if(mod %in% c("exophagy")){
    th_env_nightcatch_postedecapture <- read.csv("/home/ptaconet/Bureau/data_anglique.csv") %>%
      dplyr::select(idpostedecapture,date_time,temperature_hygro,humidity_hygro,pressure_baro,luminosite_hobo) %>%
      dplyr::rename(NMT = temperature_hygro, NMH = humidity_hygro, NMA = pressure_baro, NML = luminosite_hobo) %>%
      mutate(date = as.Date(date_time), heuredecapture = hour(date_time)) %>%
      dplyr::group_by(idpostedecapture,heuredecapture) %>%
      dplyr::summarise(NMT = mean(NMT,na.rm = T), NMH = mean(NMH,na.rm = T), NMA=mean(NMA,na.rm = T), NML=mean(NML,na.rm = T)) %>%
      mutate(VPD = get.vpd(NMH, NMT)) %>%
      as_tibble() %>%
      mutate(idpostedecapture=as.character(idpostedecapture))
      
    
    th_env_nightcatch_postedecapture2 <-th_env_nightcatch_postedecapture %>%
      mutate(idpointdecapture = substr(idpostedecapture,0,nchar(idpostedecapture)-1)) %>%
      mutate(postedecapture = substr(idpostedecapture,nchar(idpostedecapture),nchar(idpostedecapture))) %>%
      dplyr::select(-idpostedecapture) %>%
      pivot_longer(c(NMT,NMH , NML,NMA,VPD)) %>%
      pivot_wider(names_from = c(name,postedecapture), values_from = value) %>%
      mutate(DNMT = NMT_i - NMT_e,DNMH = NMH_i - NMH_e,DNML = NML_i - NML_e, DVPD = VPD_i - VPD_e) %>%
      dplyr::select( heuredecapture,idpointdecapture,NMT_i,NMH_i,VPD_i,NML_e,NMA_e,DNMT,DNMH,DNML,DVPD) %>%
      rename(NMTI = NMT_i, NMHI = NMH_i, NMLE = NML_e, NMA = NMA_e, VPDI = VPD_i) %>%
      mutate_all(funs(replace_na(.,0))) #%>%
      #mutate(idpostedecapture = paste0(pointdecapture,"e")) %>%
     # dplyr::select(-pointdecapture)
    
    # th_env_nightcatch_postedecapture3 <- th_env_nightcatch_postedecapture2 %>%
    #   mutate(DNMT = -DNMT,DNMH = -DNMH,DNML = -DNML) %>%
    #   mutate(idpostedecapture = gsub("e","i",idpostedecapture))
    
    # th_env_nightcatch_postedecapture2 <- rbind(th_env_nightcatch_postedecapture2,th_env_nightcatch_postedecapture3)
    
    
    
    th_env_nightcatch_postedecapture_wsp_rfh <-  read.csv("/home/ptaconet/phd/data/diebougou/wsp_rfh_hourly.csv")
    
    th_env_nightcatch_postedecapture <- th_env_nightcatch_postedecapture2 %>%
      left_join(th_env_nightcatch_postedecapture_wsp_rfh) %>%
      mutate(NMTI = ifelse(NMTI<10,NA,NMTI),NMA = ifelse(NMA<800,NA,NMA),NMLI = ifelse(NMLE>2000,2000,NMLE),DNML = ifelse(DNML< -2000,-2000,DNML), DNML = ifelse(DNML> 2000,2000,DNML))
    
   } else if (mod %in% c("physiological_resistance_kdrw_reg","physiological_resistance_kdre_reg","physiological_resistance_ace1_reg","exophagy_reg","early_biting_reg","late_biting_reg")){
     th_env_nightcatch_postedecapture <- read.csv("/home/ptaconet/Bureau/data_anglique.csv") %>%
       dplyr::select(idpostedecapture,date_time,temperature_hygro,humidity_hygro,pressure_baro,luminosite_hobo) %>%
       dplyr::rename(NMT = temperature_hygro, NMH = humidity_hygro, NMA = pressure_baro, NML = luminosite_hobo) %>%
       mutate(date = as.Date(date_time), heuredecapture = hour(date_time)) %>%
       dplyr::group_by(idpostedecapture) %>%
       dplyr::summarise(NMT = mean(NMT,na.rm = T), NMH = mean(NMH,na.rm = T), NMA=mean(NMA,na.rm = T), NML=mean(NML,na.rm = T)) %>%
       mutate(VPD = get.vpd(NMH, NMT)) %>%
       as_tibble() %>%
       mutate(idpostedecapture=as.character(idpostedecapture))
     
     
     th_env_nightcatch_postedecapture2 <-th_env_nightcatch_postedecapture %>%
       mutate(idpointdecapture = substr(idpostedecapture,0,nchar(idpostedecapture)-1)) %>%
       mutate(postedecapture = substr(idpostedecapture,nchar(idpostedecapture),nchar(idpostedecapture))) %>%
       dplyr::select(-idpostedecapture) %>%
       pivot_longer(c(NMT,NMH , NML,NMA,VPD)) %>%
       pivot_wider(names_from = c(name,postedecapture), values_from = value) %>%
       mutate(DNMT = NMT_i - NMT_e,DNMH = NMH_i - NMH_e,DNML = NML_i - NML_e, DVPD = VPD_i - VPD_e) %>%
       dplyr::select( idpointdecapture,NMT_i,NMH_i,VPD_i,NML_e,NMA_e,DNMT,DNMH,DNML,DVPD) %>%
       rename(NMT = NMT_i, NMH = NMH_i, NML = NML_e, NMA = NMA_e, VPD = VPD_i) %>%
       mutate_all(funs(replace_na(.,0)))
     
     th_env_nightcatch_postedecapture <- th_env_nightcatch_postedecapture2 %>%
       mutate(NMT = ifelse(NMT<10,NA,NMT),NMA = ifelse(NMA<800,NA,NMA),NML = ifelse(NML>2000,2000,NML),DNML = ifelse(DNML< -2000,-2000,DNML), DNML = ifelse(DNML> 2000,2000,DNML))
     
   } else if(mod %in% c("late_biting","early_biting")){
     th_env_nightcatch_postedecapture <- read.csv("/home/ptaconet/Bureau/data_anglique.csv") %>%
       dplyr::select(idpostedecapture,date_time,temperature_hygro,humidity_hygro,pressure_baro,luminosite_hobo) %>%
       dplyr::rename(NMT = temperature_hygro, NMH = humidity_hygro, NMA = pressure_baro, NML = luminosite_hobo) %>%
       mutate(date = as.Date(date_time), heuredecapture = hour(date_time)) %>%
       dplyr::group_by(idpostedecapture) %>%
       dplyr::summarise(NMT = mean(NMT,na.rm = T), NMH = mean(NMH,na.rm = T), NMA=mean(NMA,na.rm = T), NML=mean(NML,na.rm = T)) %>%
       mutate(VPD = get.vpd(NMH, NMT)) %>%
       as_tibble() %>%
       mutate(idpostedecapture=as.character(idpostedecapture)) %>%
       mutate(NMT = ifelse(NMT<10,NA,NMT),NMA = ifelse(NMA<800,NA,NMA),NML = ifelse(NML>2000,2000,NML))
   } else if(mod %in% c("physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1")){
     
     th_env_nightcatch_postedecapture <- read.csv("/home/ptaconet/Bureau/data_anglique.csv") %>%
       dplyr::select(idpostedecapture,date_time,temperature_hygro,humidity_hygro,pressure_baro,luminosite_hobo) %>%
       dplyr::rename(NMT = temperature_hygro, NMH = humidity_hygro, NMA = pressure_baro, NML = luminosite_hobo) %>%
       mutate(date = as.Date(date_time), heuredecapture = hour(date_time)) %>%
       dplyr::group_by(idpostedecapture,heuredecapture) %>%
       dplyr::summarise(NMT = mean(NMT,na.rm = T), NMH = mean(NMH,na.rm = T), NMA=mean(NMA,na.rm = T), NML=mean(NML,na.rm = T)) %>%
       mutate(VPD = get.vpd(NMH, NMT)) %>%
       as_tibble() %>%
       mutate(idpostedecapture=as.character(idpostedecapture))
     
     th_env_nightcatch_postedecapture <- th_env_nightcatch_postedecapture2 %>%
       mutate(NMT = ifelse(NMT<10,NA,NMT),NMA = ifelse(NMA<800,NA,NMA),NML = ifelse(NML>2000,2000,NML),DNML = ifelse(DNML< -2000,-2000,DNML), DNML = ifelse(DNML> 2000,2000,DNML))
   } else if( mod %in% c("presence","abundance")){
       
     th_env_nightcatch_postedecapture <- read.csv("/home/ptaconet/Bureau/data_anglique.csv") %>%
       dplyr::select(idpostedecapture,date_time,temperature_hygro,humidity_hygro,pressure_baro,luminosite_hobo) %>%
       dplyr::rename(NMT = temperature_hygro, NMH = humidity_hygro, NMA = pressure_baro, NML = luminosite_hobo) %>%
       mutate(date = as.Date(date_time), heuredecapture = hour(date_time)) %>%
       dplyr::group_by(idpostedecapture) %>%
       dplyr::summarise(NMT = mean(NMT,na.rm = T), NMH = mean(NMH,na.rm = T), NMA=mean(NMA,na.rm = T), NML=mean(NML,na.rm = T)) %>%
       mutate(VPD = get.vpd(NMH, NMT)) %>%
       as_tibble() %>%
       mutate(idpostedecapture=as.character(idpostedecapture)) %>%
       mutate(NMT = ifelse(NMT<10,NA,NMT),NMA = ifelse(NMA<800,NA,NMA),NML = ifelse(NML>2000,2000,NML))
     
     }
  
  # landcover variables
  env_landcover <-  dbReadTable(react_gpkg, 'env_landcover') %>% dplyr::select(-fid) %>% dplyr::rename(idpointdecapture = id) %>% filter(layer_id %in% landcover_layers_to_keep) %>% filter(buffer %in% buffer_sizes)
  
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
    pivot_wider(names_from = c(function_name,buffer,layer_id,pixval), values_from = val, names_sep = "_", values_fill = list(val = 0), values_fn = list(val = mean)) %>%
    mutate_all(funs(replace_na(.,0)))
  
  
  # all other data
  env_spatial <- env_spatial %>% 
    bind_rows(LIG) %>% 
    pivot_wider(names_from = c("var","buffer"), values_from = val) %>% 
    mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
  th_env_nightcatch  <- env_nightcatch %>% pivot_wider(names_from = var, values_from = val) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
  th_env_static <- env_static %>% pivot_wider(names_from = var, values_from = val) %>% mutate(VCT=as.numeric(VCT), WMD=as.numeric(WMD),BDE=as.numeric(BDE),VCP=ifelse(VCP=="TRUE",1,0)) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
  
  return(list(env_landcover, env_spatial, th_env_nightcatch_postedecapture, th_env_nightcatch, th_env_static, POPANI))
  
}
  

load_hmnbehav_data <- function(code_pays,entomo_csh_metadata_l1){  ## host availability
  
t = entomo_csh_metadata_l1 %>%
  filter(codepays == code_pays) %>%
    mutate(periode = ifelse(period_interv=="pre_intervention","preinterv","postinterv")) %>%
    mutate(date_capture = as.Date(date_capture)) %>%
    mutate(month = lubridate::month(date_capture)) %>%
    mutate(saison = ifelse(month <= 4 | month >=11 , "seche","pluies")) %>%
    dplyr::select(idpointdecapture,codevillage,periode,saison)
  
LUS = dbReadTable(react_gpkg, 'entomo_comportementhumain_l0') %>% 
  dplyr::select(-geom) %>%
  filter(codepays == code_pays) %>%
  group_by(codevillage,periode,saison,dormirssmoust) %>%
  dplyr::summarise(n=n()) %>%
  as_tibble() %>%
  pivot_wider(names_from = dormirssmoust, values_from = n, values_fill = list(n = 0)) %>%
  mutate(LUS = oui / (non + oui) * 100) %>%
  dplyr::select(codevillage,periode,saison,LUS)

if(code_pays=="CI"){
  t$periode=NULL
  LUS$periode=NULL
}

LUS <- t %>%
  left_join(LUS)

if(code_pays=="CI"){
  
  LUS_season <- LUS %>% group_by(saison) %>% summarise(LUS_mean = mean(LUS,na.rm = TRUE))
  LUS$LUS[which(is.na(LUS$LUS) & LUS$saison=="pluies")] <- LUS_season$LUS_mean[which(LUS_season$saison == "pluies")]
  LUS$LUS[which(is.na(LUS$LUS) & LUS$saison=="seche")] <- LUS_season$LUS_mean[which(LUS_season$saison == "seche")]
  
}


pop <- dbReadTable(react_gpkg, 'entomo_comportementhumain_l0') %>% 
  dplyr::select(-geom) %>%
  filter(codepays == code_pays) %>%
  dplyr::group_by(codevillage,periode,saison) %>%
  dplyr::summarise(pop = n()) %>%
  as_tibble()
  


HBI <- dbReadTable(react_gpkg, 'entomo_comportementhumain_l0') %>% 
  dplyr::select(-geom) %>%
     filter(codepays == code_pays) %>%
     mutate(hintmaison = as.numeric(substr(hintmaison,1,2)),hsortiemaison=as.numeric(substr(hsortiemaison,1,2))) %>%
     mutate(hintmaison = ifelse(hintmaison <=16, 19, hintmaison), hsortiemaison=ifelse(hsortiemaison>=11 | hsortiemaison<=3,6,hsortiemaison))

if(code_pays=="BF"){
hintmaison <- HBI %>%
  mutate(hintmaison = paste0("h_",hintmaison)) %>%
  group_by(codevillage, hintmaison, periode, saison) %>%
  dplyr::summarise(count = n()) %>%
  as_tibble() %>%
  pivot_wider(names_from = hintmaison, values_from = count, values_fill = list(count = 0)) %>%
  mutate(h_18=h_17+h_18) %>%
  mutate(h_19=h_18+h_19) %>%
  mutate(h_20=h_19+h_20) %>%
  mutate(h_21=h_20+h_21) %>%
  mutate(h_22=h_21+h_22) %>%
  mutate(h_23=h_22+h_23) %>%
  pivot_longer(cols = starts_with("h_"), names_to = "hour", values_to = "pop_indoor") %>%
  mutate(hour = as.numeric(gsub("h_","",hour)))

hsortiemaison <- HBI %>%
  mutate(hsortiemaison = paste0("h_",hsortiemaison)) %>%
  group_by(codevillage, hsortiemaison, periode, saison) %>%
  dplyr::summarise(count = n()) %>%
  as_tibble() %>%
  left_join(pop) %>%
  pivot_wider(names_from = hsortiemaison, values_from = count, values_fill = list(count = 0)) %>%
  mutate(h_h4=pop-(h_4)) %>%
  mutate(h_h5=pop-(h_4+h_5)) %>%
  mutate(h_h6=pop-(h_4+h_5+h_6)) %>%
  mutate(h_h7=pop-(h_4+h_5+h_6+h_7)) %>%
  mutate(h_h8=pop-(h_4+h_5+h_6+h_7+h_8)) %>%
  mutate(h_h9=pop-(h_4+h_5+h_6+h_7+h_8+h_9)) %>%
  dplyr::select(-c(h_4,h_5,h_6,h_7,h_8,h_9,pop)) %>%
  pivot_longer(cols = starts_with("h_h"), names_to = "hour", values_to = "pop_indoor") %>%
  mutate(hour = as.numeric(gsub("h_h","",hour)))
} else if(code_pays=="CI"){
  
  hintmaison <- HBI %>%
    mutate(hintmaison = paste0("h_",hintmaison)) %>%
    group_by(codevillage, hintmaison, periode, saison) %>%
    dplyr::summarise(count = n()) %>%
    as_tibble() %>%
    pivot_wider(names_from = hintmaison, values_from = count, values_fill = list(count = 0)) %>%
    mutate(h_19=h_18+h_19) %>%
    mutate(h_20=h_19+h_20) %>%
    mutate(h_21=h_20+h_21) %>%
    mutate(h_22=h_21+h_22) %>%
    mutate(h_23=h_22+h_23) %>%
    pivot_longer(cols = starts_with("h_"), names_to = "hour", values_to = "pop_indoor") %>%
    mutate(hour = as.numeric(gsub("h_","",hour)))
  
  hsortiemaison <- HBI %>%
    mutate(hsortiemaison = paste0("h_",hsortiemaison)) %>%
    group_by(codevillage, hsortiemaison, periode, saison) %>%
    dplyr::summarise(count = n()) %>%
    as_tibble() %>%
    left_join(pop) %>%
    pivot_wider(names_from = hsortiemaison, values_from = count, values_fill = list(count = 0)) %>%
    mutate(h_h4=pop-(h_4)) %>%
    mutate(h_h5=pop-(h_4+h_5)) %>%
    mutate(h_h6=pop-(h_4+h_5+h_6)) %>%
    mutate(h_h7=pop-(h_4+h_5+h_6+h_7)) %>%
    mutate(h_h8=pop-(h_4+h_5+h_6+h_7+h_8)) %>%
    dplyr::select(-c(h_4,h_5,h_6,h_7,h_8,h_10,pop)) %>%
    pivot_longer(cols = starts_with("h_h"), names_to = "hour", values_to = "pop_indoor") %>%
    mutate(hour = as.numeric(gsub("h_h","",hour)))
  
  
}

 HBB <- dbReadTable(react_gpkg, 'entomo_comportementhumain_l0') %>% 
   dplyr::select(-geom) %>%
   filter(codepays == code_pays) %>%
   mutate(hcoucher = as.numeric(substr(hcoucher,1,2)),hlever=as.numeric(substr(hlever,1,2))) %>%
   mutate(hcoucher = ifelse(hcoucher <=17, 20, hcoucher), hlever=ifelse(hlever>=11 | hlever<=3,6,hlever)) 

 hcoucher <- HBB %>%
   mutate(hcoucher = paste0("h_",hcoucher)) %>%
   group_by(codevillage, hcoucher, periode, saison) %>%
   dplyr::summarise(count = n()) %>%
   as_tibble() %>%
   pivot_wider(names_from = hcoucher, values_from = count, values_fill = list(count = 0)) %>%
   mutate(h_19=h_18+h_19) %>%
   mutate(h_20=h_19+h_20) %>%
   mutate(h_21=h_20+h_21) %>%
   mutate(h_22=h_21+h_22) %>%
   mutate(h_23=h_22+h_23) %>%
   pivot_longer(cols = starts_with("h_"), names_to = "hour", values_to = "pop_coucher") %>%
   mutate(hour = as.numeric(gsub("h_","",hour)))
 
 hlever <- HBB %>%
   mutate(hlever = paste0("h_",hlever)) %>%
   group_by(codevillage, hlever, periode, saison) %>%
   dplyr::summarise(count = n()) %>%
   as_tibble() %>%
   left_join(pop) %>%
   pivot_wider(names_from = hlever, values_from = count, values_fill = list(count = 0)) %>%
   mutate(h_h4=pop-(h_4)) %>%
   mutate(h_h5=pop-(h_4+h_5)) %>%
   mutate(h_h6=pop-(h_4+h_5+h_6)) %>%
   mutate(h_h7=pop-(h_4+h_5+h_6+h_7)) %>%
   mutate(h_h8=pop-(h_4+h_5+h_6+h_7+h_8)) %>%
   mutate(h_h9=pop-(h_4+h_5+h_6+h_7+h_8+h_9)) %>%
   dplyr::select(-c(h_4,h_5,h_6,h_7,h_8,h_9,pop)) %>%
   pivot_longer(cols = starts_with("h_h"), names_to = "hour", values_to = "pop_coucher") %>%
   mutate(hour = as.numeric(gsub("h_h","",hour)))

hours <- c(0:10,16:23)
hours2 <- rep(hours,nrow(pop))

pop <- pop %>% 
  dplyr::slice(rep(1:n(), each = length(hours))) %>%
  bind_cols(as.data.frame(hours2)) %>%
  dplyr::rename(hour = hours2)


HBI_HBB <- pop %>%
  left_join(rbind(hintmaison,hsortiemaison)) %>%
  left_join(rbind(hcoucher,hlever)) %>%
  mutate(pop_indoor = ifelse(hour %in% c(0,1,2,3),pop,pop_indoor)) %>%
  mutate(pop_indoor = ifelse(hour %in% c(10,16),0,pop_indoor)) %>%
  mutate(pop_coucher = ifelse(hour %in% c(0,1,2,3),pop,pop_coucher)) %>%
  mutate(pop_coucher = ifelse(hour %in% c(10,16,17),0,pop_coucher)) %>%
  mutate(HBI = pop_indoor / pop * 100) %>%
  mutate(HBB = pop_coucher / pop * 100) %>%
  dplyr::rename(heuredecapture = hour) %>%
  dplyr::select(codevillage, periode, saison, heuredecapture, HBI, HBB)

hum_behav_4_exophagy <- t %>%
  left_join(HBI_HBB) %>%
  dplyr::select(idpointdecapture,heuredecapture,HBI,HBB)

 HBI <- dbReadTable(react_gpkg, 'entomo_comportementhumain_l0') %>% 
   dplyr::select(-geom) %>%
   filter(codepays == code_pays) %>%
   mutate(hintmaison = as.numeric(substr(hintmaison,1,2)),hsortiemaison=as.numeric(substr(hsortiemaison,1,2))) %>%
   mutate(hintmaison = ifelse(hintmaison <=16, 19, hintmaison), hsortiemaison=ifelse(hsortiemaison>=11 | hsortiemaison<=3,6,hsortiemaison)) %>%
   mutate(dateenquete = as.Date(dateenquete)) %>%
   mutate(dateenquete2 = as.Date(dateenquete) + 1) %>%
   mutate(date_int = lubridate::ymd_h(paste0(dateenquete," ",hintmaison))) %>%
   mutate(date_sort = lubridate::ymd_h(paste0(dateenquete2," 0",hsortiemaison))) %>%
   mutate(HBI = as.numeric(date_sort - date_int)) %>%
   dplyr::group_by(codevillage,periode,saison) %>%
   summarise(HBI2 = mean(HBI))

 HBB <- dbReadTable(react_gpkg, 'entomo_comportementhumain_l0') %>% 
   dplyr::select(-geom) %>%
   filter(codepays == code_pays) %>%
   mutate(hcoucher = as.numeric(substr(hcoucher,1,2)),hlever=as.numeric(substr(hlever,1,2))) %>%
   mutate(hcoucher = ifelse(hcoucher <=17, 20, hcoucher), hlever=ifelse(hlever>=11 | hlever<=3,6,hlever)) %>%
   mutate(dateenquete = as.Date(dateenquete)) %>%
   mutate(dateenquete2 = as.Date(dateenquete) + 1) %>%
   mutate(date_int = lubridate::ymd_h(paste0(dateenquete," ",hcoucher))) %>%
   mutate(date_sort = lubridate::ymd_h(paste0(dateenquete2," 0",hlever))) %>%
   mutate(HBB = as.numeric(date_sort - date_int)) %>%
   mutate(HBB = ifelse(dormirssmoust == "non",0,HBB)) %>%
   dplyr::group_by(codevillage,periode,saison) %>%
   summarise(HBB2 = mean(HBB))

 hum_behav_4_earlylatebiting <- t %>%
   left_join(HBI) %>%
   left_join(HBB) %>%
   dplyr::select(idpointdecapture,HBI2,HBB2,saison)
 
   if(code_pays=="CI"){
     
     HBI_season <- HBI %>% group_by(saison) %>% summarise(HBI_mean = mean(HBI2,na.rm = TRUE))
     hum_behav_4_earlylatebiting$HBI2[which(is.na(hum_behav_4_earlylatebiting$HBI2) & hum_behav_4_earlylatebiting$saison=="pluies")] <- HBI_season$HBI_mean[which(HBI_season$saison == "pluies")]
     hum_behav_4_earlylatebiting$HBI2[which(is.na(hum_behav_4_earlylatebiting$HBI2) & hum_behav_4_earlylatebiting$saison=="seche")] <- HBI_season$HBI_mean[which(HBI_season$saison == "seche")]
     
     HBB_season <- HBB %>% group_by(saison) %>% summarise(HBB_mean = mean(HBB2,na.rm = TRUE))
     hum_behav_4_earlylatebiting$HBB2[which(is.na(hum_behav_4_earlylatebiting$HBB2) & hum_behav_4_earlylatebiting$saison=="pluies")] <- HBI_season$HBI_mean[which(HBB_season$saison == "pluies")]
     hum_behav_4_earlylatebiting$HBB2[which(is.na(hum_behav_4_earlylatebiting$HBB2) & hum_behav_4_earlylatebiting$saison=="seche")] <- HBI_season$HBI_mean[which(HBB_season$saison == "seche")]
     
   }
 hum_behav_4_earlylatebiting$saison=NULL
 
 hum_behav_4_earlylatebiting <- hum_behav_4_earlylatebiting %>% 
   group_by(idpointdecapture) %>%
   summarise(HBI2 = mean(HBI2), HBB2 = mean(HBB2)) %>%
   as_tibble()
 
 hum_behav_4_exophagy <- hum_behav_4_exophagy %>% 
   group_by(idpointdecapture,heuredecapture) %>%
   summarise(HBI = mean(HBI), HBB = mean(HBB)) %>%
   as_tibble()
 
 LUS <- LUS %>% 
   group_by(idpointdecapture) %>%
   summarise(LUS = mean(LUS)) %>%
   as_tibble()

return(list(LUS = LUS, hum_behav_4_exophagy = hum_behav_4_exophagy, hum_behav_4_earlylatebiting = hum_behav_4_earlylatebiting))
}

load_csh_sp_coord <- function(){
  
  # spatial coordinates
  mean_coords_points_4326 = sf::st_read(path_to_db, 'entomo_csh_metadata_l1', crs = 4326) %>%
    dplyr::group_by(codevillage,pointdecapture) %>%
    dplyr::summarise(X_4326=mean(X),Y_4326=mean(Y)) %>%
    sf::st_drop_geometry() %>%
    sf::st_as_sf(coords = c("X_4326", "Y_4326"), crs = 4326)
  
  mean_coords_points_4326$X_4326 = as.numeric(sf::st_coordinates(mean_coords_points_4326)[,1])
  mean_coords_points_4326$Y_4326 = as.numeric(sf::st_coordinates(mean_coords_points_4326)[,2])
  mean_coords_points_4326 = sf::st_drop_geometry(mean_coords_points_4326) %>% as_tibble() %>% mutate(codevillage=as.character(codevillage), pointdecapture=as.character(pointdecapture))
  
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


load_time_since_vc <- function(code_pays, entomo_csh_metadata_l1){
  
  time_since_vc <- entomo_csh_metadata_l1 %>%
    filter(codepays==code_pays) %>%
    left_join(dbReadTable(react_gpkg, 'recensement_villages_l1') %>% dplyr::select(codevillage,date_debut_interv)) %>%
    mutate(VCT = as.numeric(as.Date(date_capture)-as.Date("2016-07-15"))) %>%
    mutate(VCT2 = as.numeric(as.Date(date_capture)-as.Date(date_debut_interv))) %>%
    mutate(VCT2 = ifelse(VCT2<0 | VCT == VCT2 | is.na(VCT2),0,VCT2)) %>%
    mutate(VCT2 = VCT2/30) %>%
    mutate(VCT = VCT/30) %>%
    dplyr::select(idpointdecapture,VCT,VCT2)
    
  return(time_since_vc)
  
}

fun_ffs_tempvar <- function(df, model_type, mod, time_vars, cols_to_keep, spearman_factor = "codevillage", colnames_tempvar, tune_length = 5){
  
  modcomp <- data.frame(name = character(), res = numeric(), diff_res_w_basemod = numeric(), var = character())
  
  for(j in 1:length(time_vars)){
    cat('calculating column to keep for temporal variable ',time_vars[j],"\n")
      expl_vars_to_test <- intersect(colnames_tempvar,colnames(df[which(grepl(time_vars[j],colnames(df)))]))
    if(grepl("1", time_vars[j])){
      time_lag1 <- as.numeric(sub('.*\\_', '', expl_vars_to_test))
      time_lag2 <- as.numeric(stringr::str_match(expl_vars_to_test, '([^_]+)(?:_[^_]+){1}$')[,2])
      diff_lag <- time_lag1 - time_lag2
      ##########expl_vars_to_test <- expl_vars_to_test[which(diff_lag >= 4)]
    }
    
    # remove colinear variables
    ind_to_rm <- NULL
    for(i in 1:length(expl_vars_to_test)){
      for(k in 1:length(cols_to_keep)){
        m <- cor(df[,expl_vars_to_test[i]],df[,cols_to_keep[k]], method = "pearson", use = "na.or.complete")
        if(abs(m) > 0.7){
          ind_to_rm <- c(ind_to_rm, i)
          }
      }
    }
    if(!is.null(ind_to_rm)){
      expl_vars_to_test <- expl_vars_to_test[-unique(ind_to_rm)]
    }
  
    
    if(model_type == "rf" & mod == "abundance"){
      #logtransform_resp_var = TRUE
      logtransform_resp_var = FALSE
    } else {
      logtransform_resp_var = FALSE
    }
    
    if(model_type == "rf"){
      cols_to_keep <- unique(c(cols_to_keep,"X_32630", "Y_32630"))
      cross_validation_type <- "temporal"
    } else {
      cross_validation_type <- NULL
    }
    
    cols_to_keep1 <- cols_to_keep
    if(model_type == "spearman"){
      cols_to_keep1 = NULL
    }
    
    if(length(expl_vars_to_test) > 0){
      th_modcomp <- fun_feature_forward_selection(df = df, 
                                                stat_method = model_type,
                                                mod = mod, 
                                                spearman_factor = spearman_factor,
                                                type = "model_comparison",
                                                expl_vars_to_test = expl_vars_to_test, 
                                                expl_vars_to_keep = cols_to_keep1, 
                                                logtransform_resp_var = logtransform_resp_var,
                                                cross_validation_type = cross_validation_type, 
                                                tune_length = tune_length
                                                )
    th_modcomp$var <- time_vars[j]
    
    modcomp <- rbind(modcomp, th_modcomp)
    }
  }
  
  return(modcomp)
}


fun_glmm_cross_validation <- function(indices_cv, th_mod, mod, df, ind_vars_to_center){

  #df <- df %>% dplyr::select(c("resp_var","codevillage","pointdecapture2","int_ext","VCM",ind_vars_to_center))
    
  #cv <- fun_glmm_cross_validation(indices_temporal, th_mod, mod, df, predictors_to_scale)
  
  df$pred <- NA
  metric <- NULL
  for (i in 1:length(indices_cv$index)){
    df_train <- df[indices_cv$index[[i]],]
    df_test <- df[indices_cv$indexOut[[i]],]
    
    # here, scale the numerical explanatory variables for train and test datasets

    df_train <- df_train %>% mutate_at(ind_vars_to_center, ~scale(., center = TRUE, scale = FALSE))
    df_test <- df_test %>% mutate_at(ind_vars_to_center, ~scale(., center = TRUE, scale = FALSE))
    
    if(mod == "abundance"){
      th_mod_th_it <- glmmTMB(formula(th_mod), data = df_train, family = truncated_nbinom2)
      preds_th_it <- predict(th_mod_th_it, newdata = df_test, type = "response", allow.new.levels=TRUE)
      metric <- c(metric,Metrics::rmse(df_test$resp_var,preds_th_it))
    } else if (mod %in% c("physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1","exophagy","early_late_biting","presence","early_biting","late_biting","physiological_resistance_kdrw_reg","physiological_resistance_kdre_reg","exophagy_reg","early_late_biting_reg","presence_reg","early_biting_reg","late_biting_reg")){
      th_mod_th_it <- glmmTMB(formula(th_mod), data = df_train, family = binomial(link = "logit"))
      preds_th_it <- predict(th_mod_th_it, newdata = df_test, type = "response", allow.new.levels=TRUE)
          metric <- c(metric,mltools::auc_roc(preds_th_it, df_test$resp_var))
    }
    
    df$pred[indices_cv$indexOut[[i]]] <- preds_th_it # fill in df with predictions out of CV
    
  }
  
  mean_metric <- mean(metric,na.rm = TRUE)
  
  df <- df %>% dplyr::select(resp_var,pred,codevillage,nummission,pointdecapture2,int_ext) %>% dplyr::rename(obs = resp_var)
  
  #return(list(mean_metric = mean_metric, df_cv = df))
  return(df)
}


fun_multicollinearity_lsm <- function(df, vars_to_test){
  
  lco_metadata <- dbReadTable(react_gpkg, 'lco_metadata') # table containing pixel value and label for each land cover map
  metrics_defs <- landscapemetrics::list_lsm() # list of landscape metrics
  
  var_to_remove <- NULL
  vars_to_keep <- vars_to_test
  p <- NULL
  m <- df[,vars_to_test] %>%
    cor(.,method = "pearson", use = "na.or.complete")
  index <- which(abs(m) > .8 & abs(m) < 1,arr.ind = T) 
  p <- cbind.data.frame(stock1 = rownames(m)[index[,1]], stock2 = colnames(m)[index[,2]])
  
  vars_to_test_df <- data.frame(vars_to_test)
  colnames(vars_to_test_df) = "name"
  
    p <- p %>%
      mutate(name1 = gsub("_"," ",stock1)) %>%
      mutate(function_name1 = paste(word(name1,1),word(name1,2),word(name1,3),sep="_")) %>%
      mutate(buffer1 = as.numeric(word(name1,4))) %>%
      mutate(layer_id = as.numeric(word(name1,5))) %>%
      mutate(pixval = as.numeric(word(name1,6))) %>%
      left_join(vars_to_test_df %>% dplyr::select(name), by = c("stock1"="name")) %>%
      left_join(lco_metadata %>% dplyr::select(pixval,pixlabel,layer_id,priority2)) %>%
      dplyr::rename(priority = priority2) %>%
      dplyr::rename(layer_id1 = layer_id, pixval1 = pixval, pixlabel1 = pixlabel, priority1 = priority) %>%
      mutate(name2 = gsub("_"," ",stock2)) %>%
      mutate(function_name2 = paste(word(name2,1),word(name2,2),word(name2,3),sep="_")) %>%
      mutate(buffer2 = as.numeric(word(name2,4))) %>%
      mutate(layer_id = as.numeric(word(name2,5))) %>%
      mutate(pixval = as.numeric(word(name2,6))) %>%
      left_join(vars_to_test_df %>% dplyr::select(name), by = c("stock2"="name")) %>%
      left_join(lco_metadata %>% dplyr::select(pixval,pixlabel,layer_id,priority2)) %>%
      dplyr::rename(priority = priority2) %>%
      dplyr::rename(layer_id2 = layer_id, pixval2 = pixval, pixlabel2 = pixlabel, priority2 = priority) %>%
      mutate(priority1 = ifelse(is.na(priority1),100,priority1)) %>%
      mutate(priority2 = ifelse(is.na(priority2),100,priority2)) %>%
      mutate(stock1 = as.character(stock1)) %>%
      mutate(stock2 = as.character(stock2))
    
    if(nrow(p)>0){
      
      for(i in 1:nrow(p)){
        if(p$priority1[i] < p$priority2[i]){
          var_to_remove <- c(var_to_remove,p$stock2[i])
        } else if (p$priority1[i] > p$priority2[i]) {
          var_to_remove <- c(var_to_remove,p$stock1[i])
        } else if (p$priority1[i] == p$priority2[i]) {  # case the lc class and the lc layer are the same
          #if(p$correlation1[i] < p$correlation2[i]){
          if(grepl("prd|shdi",p$name1[i])){
            var_to_remove <- c(var_to_remove,p$stock2[i])
          } else if (p$buffer1 > p$buffer2) {
            var_to_remove <- c(var_to_remove,p$stock2[i])
          }  else if (p$buffer1 < p$buffer2) {
            var_to_remove <- c(var_to_remove,p$stock1[i])
          } else {
            var_to_remove <- c(var_to_remove,p$stock1[i])
          }
        }
      }
    
      
      var_to_remove <- unique(var_to_remove)
      vars_to_keep <- setdiff(vars_to_test,var_to_remove)
      
    }
  
  
  return(vars_to_keep)
  
  
}
  


fun_multicollinearity <- function(df, vars_to_test){
  
  var_to_remove <- NULL
  vars_to_keep <- vars_to_test
  p <- NULL
    m <- df[,vars_to_test] %>%
      cor(.,method = "pearson", use = "na.or.complete")
    index <- which(abs(m) > .8 & abs(m) < 1,arr.ind = T) 
    p <- cbind.data.frame(stock1 = rownames(m)[index[,1]], stock2 = colnames(m)[index[,2]])
    
    if(nrow(p)>0){
      
      p <- p %>%
        mutate(name1 = gsub("_"," ",stock1)) %>%
        mutate(name1 = word(name1,1)) %>%
        mutate(name2 = gsub("_"," ",stock2)) %>%
        mutate(name2 = word(name2,1)) %>%
        mutate(name1 = ifelse(name1 == "RFD1", "RFD1F", name1)) %>%
        mutate(name2 = ifelse(name2 == "RFD1", "RFD1F", name2)) %>%
        mutate(name1 = ifelse(name1 == "TMAX7", "TMAX1", name1)) %>%
        mutate(name2 = ifelse(name2 == "TMAX7", "TMAX1", name2)) %>%
        mutate(name1 = ifelse(name1 == "TMIN7", "TMIN1", name1)) %>%
        mutate(name2 = ifelse(name2 == "TMIN7", "TMIN1", name2)) %>%
        mutate(name1 = ifelse(name1 == "TAMP7", "TAMP1", name1)) %>%
        mutate(name2 = ifelse(name2 == "TAMP7", "TAMP1", name2)) %>%
        mutate(name1 = ifelse(name1 == "SMO7", "SMO1", name1)) %>%
        mutate(name2 = ifelse(name2 == "SMO7", "SMO1", name2)) %>%
        left_join(prediction_vars %>% dplyr::select(code,priority), by = c("name1"="code")) %>%
        dplyr::rename(priority1 = priority) %>%
        left_join(prediction_vars %>% dplyr::select(code,priority), by = c("name2"="code")) %>%
        dplyr::rename(priority2 = priority) %>%
        mutate(priority1 = ifelse(name1 == "lsm", 16, priority1)) %>%
        mutate(priority2 = ifelse(name2 == "lsm", 16, priority2)) %>%
        mutate(var_to_remove="")
      
      for(i in 1:nrow(p)){
        if(as.numeric(p$priority1[i]) < as.numeric(p$priority2[i])){
          var_to_remove <- c(var_to_remove,as.character(p$stock2[i]))
        } else if (as.numeric(p$priority1[i]) > as.numeric(p$priority2[i])) {
          var_to_remove <- c(var_to_remove,as.character(p$stock1[i]))
        } else if (as.numeric(p$priority1[i]) == as.numeric(p$priority2[i])) {
          if (as.numeric(word(p$stock1[i],2,sep="_")) > as.numeric(word(p$stock2[i],2,sep="_"))){
            var_to_remove <- c(var_to_remove,as.character(p$stock2[i]))
          } else if (as.numeric(word(p$stock1[i],2,sep="_")) < as.numeric(word(p$stock2[i],2,sep="_"))){
            var_to_remove <- c(var_to_remove,as.character(p$stock1[i]))
          } else {
          var_to_remove <- NA
          }
        }
        p$var_to_remove[i] <- NA
      }
      
      var_to_remove <- unique(var_to_remove)
      vars_to_keep <- setdiff(vars_to_test,var_to_remove)
      
    }
    
  
    return(vars_to_keep)
  
}


fun_compute_glmm <- function(df, predictors, predictors_forced = NULL, mod, cv_type = NULL, predictors_interaction = NULL){
  
  ###### create indices for cross-validation
  indices_spatial <- CAST::CreateSpacetimeFolds(df, spacevar = "codevillage", k = length(unique(df$codevillage)))
  indices_temporal <- CAST::CreateSpacetimeFolds(df, timevar = "nummission", k = length(unique(df$nummission)))
  indices_spatiotemporal <- CAST::CreateSpacetimeFolds(df, timevar = "nummission", spacevar = "codevillage", k = ifelse(mod=="abundance",3,4), seed = 10) # set seed for reproducibility as folds of spatiotemporal cv can change 
  #indices_spatiotemporal2 <- CAST::CreateSpacetimeFolds(df, spacevar = "col_folds", k =  length(unique(df$col_folds))) 
  
  df <- df %>% mutate(int_ext = fct_relevel(int_ext,c("i","e")))
  df <- df %>% dplyr::select(resp_var,codevillage,pointdecapture,int_ext,nummission,predictors,predictors_forced) %>% mutate_if(is.character, as.factor)
  df$pointdecapture2 <- as.factor(paste0(df$codevillage,df$pointdecapture))

  if(!is.null(predictors_interaction)){
    predictors <- setdiff(predictors,predictors_interaction)
    form <- as.formula(paste("resp_var ~ ",paste(predictors, collapse = "+"),"+",paste(predictors_interaction, collapse = "*"))) # collapse = "*" to take into account the interactions
  } else {
   form <- as.formula(paste("resp_var ~ ",paste(predictors, collapse = "+"))) # collapse = "*" to take into account the interactions
  }
  
  
  
  if(!is.null(predictors_forced)){
    form_forc <- as.formula(paste(" ~", paste(predictors_forced, collapse = "+"), "+ (1|codevillage/pointdecapture2)")) 
  } else {
    form_forc <- as.formula("~ (1|codevillage/pointdecapture2)")
  }
  
  predictors_to_scale <- setdiff(c(predictors, predictors_forced, predictors_interaction), c("VCM","IEH","int_ext","kdre","kdrw","ace1"))
  df_mod <- df %>% mutate_at(predictors_to_scale, ~scale(., center = TRUE, scale = FALSE)) %>% dplyr::select(c("resp_var","codevillage","pointdecapture2",predictors,predictors_forced,predictors_interaction))
  
  if(mod == "abundance"){
    th_mod <- buildglmmTMB(form, include = form_forc, data = df_mod, family = truncated_nbinom2)
    #df_mod_results <- broom.mixed::tidy(th_mod@model, conf.int = TRUE)
  } else if (mod %in% c("presence","abundance", "early_late_biting","late_biting","early_biting", "exophagy", "physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1","late_biting_reg","early_biting_reg", "exophagy_reg", "physiological_resistance_kdrw_reg","physiological_resistance_kdre_reg","physiological_resistance_ace1_reg")){
    th_mod <- buildglmmTMB(form, include = form_forc, data = df_mod, family = binomial(link = "logit"))
    #df_mod_results <- broom.mixed::tidy(th_mod@model, conf.int = TRUE, exponentiate = TRUE)
  }
  
  df_cv_llo <- df_cv_lto <- df_cv_llto <- df_cv_llto2 <- NULL
  
  if(is.null(cv_type)){
    cv <- NULL
  } else {
   #leave-one-village-out : predict on a new village but on known missions
   if("llo" %in% cv_type){
     df_cv_llo <- fun_glmm_cross_validation(indices_spatial, th_mod, mod, df, predictors_to_scale)
    }
    if ("lto" %in% cv_type){
      # leave-one-mission-out : predict on known villages but on unknown missions
      df_cv_lto <- fun_glmm_cross_validation(indices_temporal, th_mod, mod, df, predictors_to_scale)
    }
    if ("llto" %in% cv_type){
     #  leave-one-village-and-mission-out : predict on unknown villages and on unknown missions
      df_cv_llto <- fun_glmm_cross_validation(indices_spatiotemporal, th_mod, mod, df, predictors_to_scale)
    }
    if ("llto2" %in% cv_type){
        df_cv_llto2 <- fun_glmm_cross_validation(indices_spatiotemporal2, th_mod, mod, df, predictors_to_scale)
    }
  }
  
  return(list(mod = th_mod, 
              df_cv_llo = df_cv_llo,
              df_cv_lto = df_cv_lto,
              df_cv_llto = df_cv_llto,
              df_cv_llto2 = df_cv_llto2
              
              ))
  
}


fun_compute_rf_old <- function(df, predictors, cv_type, mod, featureselect){

  ###### create indices for cross-validation
  indices_spatial <- CAST::CreateSpacetimeFolds(df, spacevar = "codevillage", k = length(unique(df$codevillage)))
  indices_temporal <- CAST::CreateSpacetimeFolds(df, timevar = "nummission", k = length(unique(df$nummission)))
  indices_spatiotemporal <- CAST::CreateSpacetimeFolds(df, timevar = "nummission", spacevar = "codevillage", k = ifelse(mod=="abundance",3,4), seed = 10) # set seed for reproducibility as folds of spatiotemporal cv can change 
  indices_spatiotemporal2 <- CAST::CreateSpacetimeFolds(df, spacevar = "col_folds", k =  length(unique(df$col_folds)))

  
  df <- df %>% dplyr::select(resp_var,predictors,codevillage,nummission,pointdecapture,int_ext) %>% mutate_if(is.character, as.factor)

    if(mod %in% c("abundance","abundance_discrete")){
    tr = trainControl(method="cv")
    tr_spatial = trainControl(method="cv",
                              index = indices_spatial$index, 
                              indexOut = indices_spatial$indexOut,
                              savePredictions = 'final')
    tr_temporal = trainControl(method="cv",
                               index = indices_temporal$index, 
                               indexOut = indices_temporal$indexOut,
                               savePredictions = 'final')
    tr_spatiotemporal = trainControl(method="cv",
                                     index = indices_spatiotemporal$index, 
                                     indexOut = indices_spatiotemporal$indexOut,
                                     savePredictions = 'final')
    tr_spatiotemporal2 = trainControl(method="cv",
                                     index = indices_spatiotemporal2$index, 
                                     indexOut = indices_spatiotemporal2$indexOut,
                                     savePredictions = 'final')
    
   
    if(mod == "abundance"){
      met = "MAE"   # RMSE
      df$resp_var <- log(df$resp_var)
    } else {
      met = "Accuracy"
      df$resp_var <- as.factor(df$resp_var)
    }
    
    
  } else if (mod %in% c("presence","exophagy", "early_late_biting","early_biting","late_biting","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1")){
    
    df$resp_var <- ifelse(df$resp_var==0,"Absence","Presence")
    df$resp_var <- as.factor(df$resp_var)
    met = "AUC"
    
   d <- as.data.frame(table(df$resp_var))
   if(d$Freq[which(d$Var1=="Presence")] > d$Freq[which(d$Var1=="Absence")]){
     df$resp_var <- forcats::fct_relevel(df$resp_var,c("Presence","Absence"))
   }
  
   ratio_obs <- d$Freq[1]/d$Freq[2]
   
   if(ratio_obs < 0.66 | ratio_obs > 1.5){
    
    tr = trainControl(method="cv",
                              sampling = "up",
                              summaryFunction = prSummary,
                              classProbs = TRUE)
    tr_spatial = trainControl(method="cv",
                              index = indices_spatial$index, 
                              indexOut = indices_spatial$indexOut,
                              sampling = "up",
                              summaryFunction = prSummary,
                              classProbs = TRUE,
                              savePredictions = 'final')
    tr_temporal = trainControl(method="cv",
                               index = indices_temporal$index, 
                               indexOut = indices_temporal$indexOut,
                               sampling = "up",
                               summaryFunction = prSummary,
                               classProbs = TRUE,
                               savePredictions = 'final')
    tr_spatiotemporal = trainControl(method="cv",
                                     index = indices_spatiotemporal$index, 
                                     indexOut = indices_spatiotemporal$indexOut,
                                     sampling = "up",
                                     summaryFunction = prSummary,
                                     classProbs = TRUE,
                                     savePredictions = 'final')
    tr_spatiotemporal2 = trainControl(method="cv",
                                     index = indices_spatiotemporal2$index, 
                                     indexOut = indices_spatiotemporal2$indexOut,
      #method = "LOOCV", number = 1,
                                     sampling = "up",
                                     summaryFunction = prSummary,
                                     classProbs = TRUE,
                                     savePredictions = 'final')
    
    
   } else {
     
    tr = trainControl(method="cv",
                      summaryFunction = prSummary,
                      classProbs = TRUE)
    tr_spatial = trainControl(method="cv",
                              index = indices_spatial$index, 
                              indexOut = indices_spatial$indexOut,
                              summaryFunction = prSummary,
                              classProbs = TRUE,
                              savePredictions = 'final')
    tr_temporal = trainControl(method="cv",
                               index = indices_temporal$index, 
                               indexOut = indices_temporal$indexOut,
                               summaryFunction = prSummary,
                               classProbs = TRUE,
                               savePredictions = 'final')
    tr_spatiotemporal = trainControl(method="cv",
                                     index = indices_spatiotemporal$index, 
                                     indexOut = indices_spatiotemporal$indexOut,
                                     summaryFunction = prSummary,
                                     classProbs = TRUE,
                                     savePredictions = 'final')
    tr_spatiotemporal2 = trainControl(method="cv",
                                      index = indices_spatiotemporal2$index, 
                                      indexOut = indices_spatiotemporal2$indexOut,
                                      summaryFunction = prSummary,
                                      classProbs = TRUE,
                                      savePredictions = 'final')
   }

    # if(cv_type == "random"){
    #   rfFuncs$summary <- prSummary
    #   df <- DMwR::SMOTE(resp_var ~ ., df)
    # }
  }
  
  
  if(cv_type == "random"){
    th_mod <- caret::train(x = df[,predictors], y = df$resp_var, method = "rf", tuneLength = 10, trControl = tr, metric = met, importance = T, keep.forest=TRUE, keep.inbag=TRUE)
    
  # rfFuncs$summary <- prSummary
  # control <- rfeControl(functions=rfFuncs, method="cv", number=10, )
  # th_mod <- rfe(df[,predictors], df$resp_var, rfeControl=control, sizes = c(5,10,15,20,25,30,35,40), metric = met, keep.forest=TRUE, keep.inbag=TRUE)
  } else if(cv_type == "llo"){
    
    if(featureselect == TRUE){
       th_mod <- CAST::ffs(predictors = df[,predictors], response = df$resp_var, method = "rf", tuneLength = 5, trControl = tr_spatial, metric = met, importance = T)
     } else {
       th_mod <- caret::train(x = df[,predictors], y = df$resp_var, method = "rf", tuneLength = 10, trControl = tr_spatial, metric = met, importance = T, keep.forest=TRUE, keep.inbag=TRUE)
     }
    
  } else if (cv_type == "lto"){
    
    if(featureselect == TRUE){
      th_mod <- CAST::ffs(predictors = df[,predictors], response = df$resp_var, method = "rf", tuneLength = 5, trControl = tr_temporal, metric = met, importance = T)
    } else {
      th_mod <- caret::train(x = df[,predictors], y = df$resp_var, method = "rf", tuneLength = 10, trControl = tr_temporal, metric = met, importance = T, keep.forest=TRUE, keep.inbag=TRUE)
    }
    
  } else if (cv_type == "llto"){
    
    if(featureselect == TRUE){
     th_mod <- CAST::ffs(predictors = df[,predictors], response = df$resp_var, method = "rf", tuneLength = 5, trControl = tr_spatiotemporal, metric = met, importance = T)
    } else {
      th_mod <- caret::train(x = df[,predictors], y = df$resp_var, method = "rf", tuneLength = 10, trControl = tr_spatiotemporal, metric = met, importance = T, keep.forest=TRUE, keep.inbag=TRUE)
    }
    
  } else if (cv_type == "llto2"){
    
    if(featureselect == TRUE){
      th_mod <- CAST::ffs(predictors = df[,predictors], response = df$resp_var, method = "rf", tuneLength = 5, trControl = tr_spatiotemporal2, metric = met, importance = T)
    } else {
      th_mod <- caret::train(x = df[,predictors], y = df$resp_var, method = "rf", tuneLength = 10, trControl = tr_spatiotemporal2, metric = met, importance = T, keep.forest=TRUE, keep.inbag=TRUE)
    }
    
  }
  
  df$rowIndex <- seq(1,nrow(df),1)
  
   if (mod %in% c("presence", "early_late_biting","early_biting","late_biting", "exophagy","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1")){
     df_cv <- th_mod$pred %>%
       left_join(df) %>%
       dplyr::select(pred,Presence,obs,codevillage,nummission,pointdecapture,int_ext) %>%
       mutate(obs = ifelse(obs == "Absence",0,1)) %>%
       dplyr::rename(pred_final = pred, pred = Presence)
   } else {
     df_cv <- th_mod$pred %>%
       left_join(df) %>%
       dplyr::select(pred,obs,codevillage,nummission,pointdecapture,int_ext)
   }
   

  return(list(mod = th_mod, df_mod = df, df_cv = df_cv))
  
}








fun_compute_rf <- function(df, predictors, cv_col, mod, featureselect){
  
  ###### create indices for cross-validation
  #indices_cv <- CAST::CreateSpacetimeFolds(df, spacevar = "codevillage",timevar = "nummission", k = 10, seed = 29)
  
   indices_cv <- CAST::CreateSpacetimeFolds(df, spacevar = cv_col,k = length(unique(unlist(df[,cv_col]))))  # k = length(unique(df[,cv_col])
# also works with : indices_cv <- groupKFold(df[,cv_col], k =  length(unique(df[,cv_col])))
  
  if("int_ext" %in% colnames(df)){
    df <- df %>% mutate(int_ext = ifelse(int_ext %in% c("int.","i"),"i","e"))
  }
   df <- df %>% dplyr::select(resp_var,predictors,codevillage,nummission,pointdecapture,int_ext) %>% mutate_if(is.character, as.factor)
   
  
  if(mod %in% c("presence","abundance")){
    rownames(df)<- paste0(df$nummission,df$codevillage,df$pointdecapture,df$int_ext)
  }
  
  if(mod %in% c("abundance","abundance_discrete")){
    
    tr = trainControl(method="cv",
                              index = indices_cv$index, 
                              indexOut = indices_cv$indexOut,
                              savePredictions = 'final')

    
    if(mod == "abundance"){
      met = "RMSE"   # RMSE
      df$resp_var <- log(df$resp_var)
    } else {
      met = "Accuracy"
      df$resp_var <- as.factor(df$resp_var)
    }
    
    
  } else if (mod %in% c("presence","exophagy","early_biting","late_biting","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1")){
    
    
    
    comboSummary <- function(data, lev = NULL, model = NULL) {
      out <- c(twoClassSummary(data, lev, model), prSummary(data, lev, model))
      
      # add brier (see https://machinelearningmastery.com/tour-of-evaluation-metrics-for-imbalanced-classification/ and https://www.machinelearningplus.com/statistics/brier-score/)
      brier <- DescTools::BrierScore(data[, lev[1]], ifelse(data$obs == lev[1], 1, 0))
      out <- c(out,brier)
      names(out)[8] <- "brier"
      # special case missing value for F
      #out$F <- ifelse(is.na(out$F), 0, out$F)  
      names(out) <- gsub("AUC", "PR_AUC", names(out))
      names(out) <- gsub("ROC", "ROC_AUC", names(out))
      
      return(out)
    }
    
    
    if(mod != "physiological_resistance_kdrw"){
    df$resp_var <- ifelse(df$resp_var==0,"Absence","Presence")
    } else {
      df$resp_var <- ifelse(df$resp_var==0,"Presence","Absence")
    }
    df$resp_var <- as.factor(df$resp_var)
    df$resp_var <- forcats::fct_relevel(df$resp_var,c("Presence","Absence"))
    

    if(mod %in% c("presence","early_biting","late_biting","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1")){
    met = "PR_AUC"
    } else if(mod %in% c("exophagy")){
    met =  "ROC_AUC"
    }
    # met="brier"

    d <- as.data.frame(table(df$resp_var))
    ratio_obs <- d$Freq[1]/d$Freq[2]
    if(ratio_obs < 0.66 | ratio_obs > 1.5){
      samp <- "up"
    } else {
      samp <- NULL
    }
    
      tr = trainControl(method="cv",
                        sampling = samp,
                        index = indices_cv$index, 
                        indexOut = indices_cv$indexOut,
                        summaryFunction = comboSummary,
                        classProbs = TRUE,
                        savePredictions = 'final',
                        verboseIter = TRUE)
    
    # if(cv_type == "random"){
    #   rfFuncs$summary <- prSummary
    #   df <- DMwR::SMOTE(resp_var ~ ., df)
    # }
  } else if(mod == "early_late_biting"){
    
    tr = trainControl(method="cv",
                      sampling = "smote",
                      index = indices_cv$index, 
                      indexOut = indices_cv$indexOut,
                      classProbs = TRUE,
                      savePredictions = 'final')
    
    met = "Accuracy"
    
  } else if (mod %in% c("exophagy_reg","early_biting_reg","late_biting_reg","physiological_resistance_kdrw_reg","physiological_resistance_kdre_reg","physiological_resistance_ace1_reg")){
    
    
    tr = trainControl(method="cv",
                      index = indices_cv$index, 
                      indexOut = indices_cv$indexOut,
                      savePredictions = 'final')
    
    met = "RMSE" 
    
    if(mod %in% c("early_biting_reg","late_biting_reg","physiological_resistance_kdre_reg","physiological_resistance_ace1_reg")){
    df$resp_var <- log(df$resp_var)
    }

  }
  

  if(cv_col == "random"){
    th_mod <- caret::train(x = df[,predictors], y = df$resp_var, method = "rf", tuneLength = 10, trControl = tr, metric = met, importance = T, keep.forest=TRUE, keep.inbag=TRUE)
    
    # rfFuncs$summary <- prSummary
    # control <- rfeControl(functions=rfFuncs, method="cv", number=10, )
    # th_mod <- rfe(df[,predictors], df$resp_var, rfeControl=control, sizes = c(5,10,15,20,25,30,35,40), metric = met, keep.forest=TRUE, keep.inbag=TRUE)
  } else {
    
    if(featureselect == TRUE){
      th_mod <- CAST::ffs(predictors = df[,predictors], response = df$resp_var, method = "ranger", tuneLength = 5, trControl = tr, metric = met, maximize = ifelse(met %in% c("RMSE", "logLoss", "MAE", "brier","Dist"), FALSE,TRUE),  preProcess = c("center","scale"),importance = "permutation", local.importance = TRUE)
    } else {
      #th_mod <- caret::train(x = df[,predictors], y = df$resp_var, method = "rf", tuneLength = 10, trControl = tr, metric = met, maximize = ifelse(met %in% c("RMSE", "logLoss", "MAE", "brier","Dist"), FALSE,TRUE), importance = T, keep.forest=TRUE, keep.inbag=TRUE)
      th_mod <- caret::train(x = df[,predictors], y = df$resp_var, method = "ranger", tuneLength = 5, trControl = tr, metric = met, maximize = ifelse(met %in% c("RMSE", "logLoss", "MAE", "brier","Dist"), FALSE,TRUE),  preProcess = c("center","scale"),importance = "permutation", local.importance = TRUE) #importance = "impurity_corrected" 
      
    }
    
  }

  df$rowIndex <- seq(1,nrow(df),1)
  
  if (mod %in% c("presence","early_biting","late_biting", "exophagy","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1")){
    df_cv <- th_mod$pred %>%
      left_join(df) %>%
      dplyr::select(pred,Presence,obs,codevillage,nummission,pointdecapture,int_ext) %>%
      mutate(obs = ifelse(obs == "Absence",0,1)) %>%
      dplyr::rename(pred_final = pred, pred = Presence)
    
  } else {
    df_cv <- th_mod$pred %>%
      left_join(df) %>%
      dplyr::select(pred,obs,codevillage,nummission,pointdecapture,int_ext)
  }
  
  
  
  return(list(mod = th_mod, df_mod = df, df_cv = df_cv))
  
}
















## multicollinearity among predictors
fun_multicol <- function(th_trmetrics_entomo_postedecapture, preds){
  
  predictors_numeric <- th_trmetrics_entomo_postedecapture %>%
    dplyr::select(preds) %>%
    dplyr::select_if(is.numeric)
  predictors_numeric <- colnames(predictors_numeric)
  predictors_character <- th_trmetrics_entomo_postedecapture %>%
    dplyr::select(preds) %>%
    dplyr::select_if(is.character)
  predictors_character <- colnames(predictors_character)
  
  ## multicollinearity among predictors
  lsm_vars <- predictors_numeric[grepl("lsm", predictors_numeric)]
  other_sp_vars_num <- predictors_numeric[!grepl("lsm", predictors_numeric)]
  if(length(lsm_vars) > 1){
    lsm_vars <- fun_multicollinearity_lsm(th_trmetrics_entomo_postedecapture, lsm_vars)
  } else if(length(lsm_vars) == 0){
    lsm_vars = NULL
 }
  vars_multiv_num <- c(other_sp_vars_num, lsm_vars)
  vars_multiv_num <- fun_multicollinearity(th_trmetrics_entomo_postedecapture, vars_multiv_num)
  vars_multiv <- c(vars_multiv_num, predictors_character)
  
  return(vars_multiv)
  
}





fun_get_temporal_preds_columns <- function(lag_time_start, lag_time_end, time_vars, temporal_agg_number_days, th_trmetrics_entomo_postedecapture, buffer_sizes){
  
 # temporal_agg_number_days = 7
  
  colnames_tot <- NULL
  for(k in 1:length(time_vars)){
    temp_res <-  regmatches(time_vars[k], gregexpr("[[:digit:]]+", time_vars[k]))
    temp_res <-  as.numeric(unlist(temp_res))
    step <- floor(temporal_agg_number_days/temp_res)
    week <- seq(ceiling(lag_time_start/temp_res),ceiling(lag_time_end/temp_res), ifelse(step==0,1,step))
    
    lags_temporalpreds_tempres1 <- NULL
    for(i in 1:length(week)){
      for (j in (i+1):length(week)){
        for(l in 1:length(buffer_sizes)){
        lags_temporalpreds_tempres1 <- c(lags_temporalpreds_tempres1, paste0("_",buffer_sizes[l],"_",week[i],"_",week[j]))
      }
      }
    }
    lags_temporalpreds_tempres1 <- lags_temporalpreds_tempres1[1:(length(lags_temporalpreds_tempres1)-2)]
    colnames_tempvar <- NULL
    colnames_tempvar <- c(colnames_tempvar, paste0(time_vars[k],lags_temporalpreds_tempres1))
    
    colnames_tot <- c(colnames_tot,colnames_tempvar)
  }
  
  return( colnames_tot )
}



fun_get_time_preds_rf <- function(df,colnames_tempvar,time_vars_rf,tune_length, mod){
  
  df_temporal <- NULL
  
  ### select the first temporal variable
  for(i in 1:length(time_vars_rf)){
    
    cat("Calculating CCM for variable ",time_vars_rf[i],"\n")
    
    expl_vars_to_test <- intersect(colnames_tempvar, colnames(df[which(grepl(time_vars_rf[i],colnames(df)))]))
    
    
    corr <- fun_feature_forward_selection(
      df = df,
      stat_method = "rf",
      mod = mod,
      type = "model_comparison",
      expl_vars_to_keep = c("X_32630","Y_32630"),
      expl_vars_to_test = expl_vars_to_test,
      cross_validation_type = "temporal",
      tune_length = tune_length)
    
    df_temporal <- rbind(df_temporal, corr)
    
  }
  
  cols_to_keep_timevar <- as.character(df_temporal$name[which.max(df_temporal$diff_res_w_basemod)])
  
  ### forward selection to select the next 2 temporal variables
  # 2nd time var
  
  #ffs_temp_it2 <- fun_ffs_tempvar(df = df, model_type = "rf", mod, time_vars_rf, cols_to_keep_timevar, colnames_tempvar = colnames_tempvar, tune_length = tune_length)
  
  
  time_vars_it2 <- setdiff(time_vars_rf, word(gsub("_"," ", cols_to_keep_timevar,1)))
  ffs_temp_it2 <- fun_ffs_tempvar(df = df, model_type = "rf", mod, time_vars_it2, cols_to_keep_timevar, colnames_tempvar = colnames_tempvar, tune_length = tune_length)
  
  
  if(max(ffs_temp_it2$diff_res_w_basemod) > 0){
    cols_to_keep_timevar <- c(cols_to_keep_timevar, as.character(ffs_temp_it2$name[which.max(ffs_temp_it2$diff_res_w_basemod)]))
  }
  
  # 3d time var
   # if(length(cols_to_keep_timevar) > 1){
   #   ffs_temp_it3 <- fun_ffs_tempvar(df = df, model_type = "rf", mod, time_vars_rf, cols_to_keep_timevar, colnames_tempvar = colnames_tempvar, tune_length = tune_length)
   #   if(max(ffs_temp_it3$diff_res_w_basemod) > 0){
   #    cols_to_keep_timevar <- c(cols_to_keep_timevar, as.character(ffs_temp_it3$name[which.max(ffs_temp_it3$diff_res_w_basemod)]))
   #   }
   # 
   # }
  
  return(cols_to_keep_timevar)
}



annotation_compass <- function(label,
                               position = c('N','NE','E','SE','S','SW','W','NW'),
                               padding = grid::unit(c(0.5,0.5),"line"), ...){
  position <- match.arg(position)
  x <- switch (position,
               N = 0.5,
               NE = 1,
               E = 1,
               SE = 1,
               S = 0.5, 
               SW = 0,
               W = 0, 
               NW = 0
  )
  y <- switch (position,
               N = 1,
               NE = 1,
               E = 0.5,
               SE = 0,
               S = 0, 
               SW = 0,
               W = 0.5, 
               NW = 1
  )
  hjust <- switch (position,
                   N = 0.5,
                   NE = 1,
                   E = 1,
                   SE = 1,
                   S = 0.5, 
                   SW = 0,
                   W = 0, 
                   NW = 0
  )
  vjust <- switch (position,
                   N = 1,
                   NE = 1,
                   E = 0.5,
                   SE = 0,
                   S = 0, 
                   SW = 0,
                   W = 0.5, 
                   NW = 1
  )
  f1 <- switch (position,
                N = 0,
                NE = -1,
                E = -1,
                SE = -1,
                S = 0, 
                SW = 1,
                W = 1, 
                NW = 1
  )
  f2 <- switch (position,
                N = -1,
                NE = -1,
                E = 0,
                SE = 1,
                S = 1, 
                SW = 1,
                W = 0, 
                NW = -1
  )
  annotation_custom(grid::textGrob(label, 
                                   x=grid::unit(x,"npc") + f1*padding[1] , 
                                   y=grid::unit(y,"npc") + f2*padding[2],
                                   hjust=hjust,vjust=vjust, ...))
}



create_folds <- function(df, number_bites){
  
   a = df %>%
     group_by(nummission,codevillage) %>%
     summarise(n=n()) %>%
     mutate(col_folds = ifelse(n>=number_bites,paste0(codevillage,nummission),"all_other")) %>%
     dplyr::select(nummission,codevillage,col_folds,n)
   
   
  
  b <- a %>% filter(col_folds!="all_other") %>% rename(n2=n)# b OK
  num_groups = median(b$n2)
  
  c <- a %>% filter(col_folds=="all_other") %>% 
    group_by(codevillage) %>% 
    mutate(n2=sum(n)) %>%
    mutate(col_folds = ifelse(n2>=number_bites,paste0(codevillage),"all_other"))
  
  #set.seed(42) 
  d <- c %>% filter(col_folds!="all_other") %>% dplyr::select(-n) # d OK
  
  e <- c %>% 
    filter(col_folds=="all_other") %>%
    dplyr::select(-n2)
  
  e <- e[sample(nrow(e)), ]
  e$cumsum <- cumsum(e$n)
  e$new_mission_village <- trunc(e$cumsum/num_groups)
  e$col_folds <- paste0("all_other_",e$new_mission_village) # e OK
  
  e$n <- e$new_mission_village <- e$cumsum <- NULL

  b$n2 <- NULL
  d$n2 <- NULL
  
  folds <- rbind(b,d,e)
  
  
  
  return(folds)
  
}

create_folds2 <- function(df){
  
   a = df %>% 
     group_by(codevillage) %>% 
     summarise(n=n()) 
   
   mean_n <- round(mean(a$n))
   
   b <- a %>% filter(n>=mean_n) %>% mutate(col_folds = codevillage) %>% dplyr::select(-n)
   
   c <-  a %>% filter(n<mean_n) 
   
   e <- c[sample(nrow(c)), ]
   e$cumsum <- cumsum(e$n)
   e$new_mission_village <- trunc(e$cumsum/mean_n)
   e$col_folds <- paste0("all_other_",e$new_mission_village) # e OK
   e$n <- e$new_mission_village <- e$cumsum <- NULL
   
   b$n2 <- NULL
   e$n2 <- NULL
   
   folds <- rbind(b,e)
   
   return(folds)
   
}

   
