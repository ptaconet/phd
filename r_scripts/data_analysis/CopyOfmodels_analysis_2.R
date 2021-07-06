library(tidyverse)
library(patchwork)
library(DBI)
library(sjPlot) # see : https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html
library(stringr)

### connect to the database
path_to_db <- "data/react_db/react_db.gpkg" 
react_gpkg <- DBI::dbConnect(RSQLite::SQLite(),dbname = path_to_db) 

# open model results
#model_results <- readRDS("/home/ptaconet/Bureau/data_analysis/model_results_newanalysis.rds")

## table of exhaustive definitions of the explanatory variables
googlesheets4::sheets_deauth()
prediction_vars <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1dIeSOa2WinXvOQGLmIjA0gFdsHnb6zMMsDME-G5pyMc/edit?usp=sharing", sheet = "var_explication", col_types="c")

lco_metadata <- dbReadTable(react_gpkg, 'lco_metadata') %>%
  dplyr::select(pixval,pixlabel_english,layer_id)# table containing pixel value and label for each land cover map


fun_get_predictors_labels <- function(table = NULL, vector_predictors_name = NULL, vector_predictors = NULL){
  
  vector_predictors_lsm <- vector_predictors_vcm <- vector_predictors_other <- NULL
  
  if(!is.null(table)){
    vector_predictors <- table[[vector_predictors_name]]
  }
  vector_predictors_lsm <- vector_predictors[grep("lsm",vector_predictors)]
  vector_predictors_vcm <- vector_predictors[grep("VCM",vector_predictors)]
  vector_predictors_other <-setdiff(vector_predictors, c(vector_predictors_lsm,vector_predictors_vcm))
  
  if(length(vector_predictors_lsm)>0){
  vector_predictors_lsm <- as.data.frame(vector_predictors_lsm)
  names(vector_predictors_lsm) <- "code"
  vector_predictors_lsm$code <- as.character(vector_predictors_lsm$code)
  vector_predictors_lsm <- vector_predictors_lsm %>%
      dplyr::mutate(name1 = gsub("_"," ",code)) %>%
    dplyr::mutate(function_name1 = paste(word(name1,1),word(name1,2),word(name1,3),sep="_")) %>%
    dplyr::mutate(buffer1 = as.numeric(word(name1,4))) %>%
    dplyr::mutate(layer_id = as.numeric(word(name1,5))) %>%
    dplyr::mutate(pixval = as.numeric(word(name1,6))) %>%
    dplyr::left_join(lco_metadata) %>%
    dplyr::rename(pixlabel = pixlabel_english) %>%
    dplyr::mutate(pixlabel = ifelse(grepl("prd",name1),"patch richness density",pixlabel)) %>%
    dplyr::mutate(pixlabel = ifelse(grepl("shdi",name1),"shannon diversity index",pixlabel)) %>%
    dplyr::select(code, pixlabel) %>%
    dplyr::rename(short_name = pixlabel) %>%
    dplyr::arrange(code) %>%
    dplyr::mutate(group = "Land cover") %>%
    dplyr::mutate(unit = "% of landscape")
  }
  
  if(length(vector_predictors_vcm)>0){
  vector_predictors_vcm <- as.data.frame(vector_predictors_vcm)
  names(vector_predictors_vcm) <- "code"
  vector_predictors_vcm$code <- as.character(vector_predictors_vcm$code)
  vector_predictors_vcm <- vector_predictors_vcm %>% left_join(prediction_vars %>% dplyr::select(code, short_name, group, unit))
  }
  
  if(length(vector_predictors_other)>0){
  vector_predictors_other <- as.data.frame(vector_predictors_other)
  names(vector_predictors_other) <- "code_df"
  vector_predictors_other$code_df <- as.character(vector_predictors_other$code_df)
  vector_predictors_other$code <- word(vector_predictors_other$code_df,1,sep = "\\_")
  vector_predictors_other <- vector_predictors_other %>% left_join(prediction_vars %>% dplyr::select(code, short_name, group, unit)) %>% dplyr::select(-code) %>% dplyr::rename(code = code_df)
  }

  table_labels <- rbind(vector_predictors_lsm, vector_predictors_other, vector_predictors_vcm)
  
  if(!is.null(table)){
    colnames(table_labels) <- c(vector_predictors_name,"label","label_group","unit")
    table_labels <- table_labels %>%
      dplyr::right_join(table)
  }
  
  return(table_labels)
  
}


fun_plot_tile <- function(df, type, xvar = "species", yvar = "label", fillvar, metric_name, indicator, model_type){
  
  df <- df %>%
    mutate(label_group = forcats::fct_relevel(label_group, c("Model perf.", unique(prediction_vars$group))))
  
  p <- ggplot(df, aes_string(xvar, yvar)) + 
    geom_tile(aes_string(fill = fillvar), color = "white") + facet_grid(label_group~., scales="free_y", space="free_y") +
    theme_bw() + 
    ggtitle(indicator, subtitle = model_type) + 
    ylab("variable")
    
  if(type == "spearman"){
   p <- p + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name = metric_name) + geom_text(aes(label = ifelse(is.na(correlation), "",round(correlation,2))), size = 3) +
     labs(caption = paste0("Values displayed inside the cells : spearman correlation coefficient \n Only significant results (correlation >= 0.2 and pval <= 0.2 analysis) are displayed. Vector control measures and place (int.ext) variables where one-hot encoded."))
  }
  if(type == "glmm"){
    p <- p + geom_text(aes(label = ifelse(is.na(estimate), "",paste0(round(estimate,2)," [",round(conf.low,2),"; ",round(conf.high,2),"] / p",ifelse(p.value < 0.01,"<0.01",paste0("=",round(p.value,2)))))), size = 3) +
      labs(caption = paste0("Format of the values displayed inside the cells : ",ifelse(indicator == "abundance","DDR","ODR")," [95% CI] / p-value \n Only significant results (pval <= 0.2 for univariate and pval <= 0.05 for multivariate analysis) are displayed, except for vector control measures where all results are displayed."))
  }
  if(type == "rf"){
    p <- p + scale_fill_gradient(na.value = "white", low = "blue", high = "red", limit = c(0,max(df$importance[which(df$label_group!="Model perf.")])), space = "Lab", name = metric_name) + geom_text(aes(label = ifelse(label_group == "Model perf.", round(importance,2),"")), size = 3) +
    labs(caption = paste0("The overall quality of the model is displayed in the first row."))
    
  }
  
  
  return(p)
}


fun_plot_pdp <- function(model, df, indicator, species, quality){

  library(iml)
  #model=model_results$multiv_rf_dayscience[[1]]$mod$fit
  #df = model_results$multiv_rf_dayscience[[1]]$df_mod
  
  if(indicator != "abundance"){
  mod <- Predictor$new(model, data = df, class = "Presence")
  metric <- "AUC"
  } else {
  mod <- Predictor$new(model, data = df)
  metric <- "RMSE"
  }
  
  # pdp for top 5 important variables
  imp <- randomForest::importance(model, scale = F, type = 1)
  imp <- as.data.frame(imp)
  imp$var <- rownames(imp)
  imp <- fun_get_predictors_labels(imp,"var")
  colnames(imp)[ncol(imp)] <- "importance"
  imp <- imp %>%
    mutate(label = forcats::fct_reorder(label, importance)) %>%
    arrange(-importance)
  
  # variable importance plot
  plot_imp <- ggplot(imp, aes(x = importance , y = label)) +
    geom_bar(stat="identity") + 
    theme_bw() + 
    #theme(axis.text.x = element_text(angle=90, hjust=1)) +
    ylab("") + 
    ggtitle("Variable importance plot")
    
  # interactions for the most important variable
  interact = Interaction$new(mod, grid.size = 50, feature = as.character(imp$var[1]))
  
  interact$results$feat <- word(gsub(":"," ",interact$results$.feature),1)
  interact$results <- fun_get_predictors_labels(interact$results,"feat")
  interact$results$.feature <- paste(interact$results$label,imp$label[1], sep = " : ")
  interact$results <- interact$results %>% filter(!is.na(.feature))
  plot_interactions <- plot(interact) +
    scale_y_discrete("") + 
    theme_bw() + 
    ggtitle("Interaction strengths")
  
  # pdp 2 vars for the main interaction
  pd = FeatureEffect$new(mod, c(as.character(imp$var[1]), as.character(imp$var[2])), method = "pdp") 
  pdp_2vars <- plot(pd) + 
    xlab(as.character(imp$label[1])) + 
    ylab(as.character(imp$label[2]))  + 
    theme_bw() + 
    ggtitle("partial dependance plots (2 vars)")
  
  # partial dependance plot
  pdps <- list()

  for(i in 1:5){
    pdp = FeatureEffect$new(mod, imp$var[i], method = "pdp")
    if(indicator != "abundance"){
    pdps[[i]] = pdp$plot() +
      scale_y_continuous('', limits = c(0, 1)) +
      xlab(paste0(imp$label[i], " (", imp$unit[i], ")")) +
      theme_bw()
    } else {
      pdps[[i]] = pdp$plot() +
        scale_y_continuous('', limits = c(0, max(df$resp_var))) +
        xlab(paste0(imp$label[i], " (", imp$unit[i], ")")) +
        theme_bw()
    }
  }
  
  pdps <- wrap_plots(pdps)

  p_final <- (plot_imp  + pdps ) / (plot_interactions + pdp_2vars) + plot_annotation(title = paste0(species, " - ", indicator, " (",metric," = ",round(quality,2),")")) + plot_layout(guides = 'auto')

  return(p_final)

}


model_results <- readRDS("/home/ptaconet/Bureau/data_analysis/model_results_newanalysis.rds") %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus ss.",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(univ_spearmancorr = map(univ_spearmancorr, ~fun_get_predictors_labels(table = ., vector_predictors_name = "name"))) %>%
  mutate(univ_glmm = map(univ_glmm, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(glmm_spearmanfilt_dayscience_tabversion = map2(glmm_spearmanfilt_dayscience, mod, ~broom.mixed::tidy(.x$model@model, conf.int = TRUE, exponentiate = ifelse(.y == "abundance", FALSE, TRUE))))  %>%
  mutate(glmm_spearmanfilt_nightscience_tabversion = map2(glmm_spearmanfilt_nightscience, mod, ~broom.mixed::tidy(.x$model@model, conf.int = TRUE, exponentiate = ifelse(.y == "abundance", FALSE, TRUE)))) %>%
  mutate(glmm_spearmanfilt_dayscience_tabversion = map(glmm_spearmanfilt_dayscience_tabversion, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(glmm_spearmanfilt_nightscience_tabversion = map(glmm_spearmanfilt_nightscience_tabversion, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(glmm_pvalfilt_dayscience_tabversion = map2(glmm_pvalfilt_dayscience, mod, ~broom.mixed::tidy(.x$model@model, conf.int = TRUE, exponentiate = ifelse(.y == "abundance", FALSE, TRUE))))  %>%
  mutate(glmm_pvalfilt_nightscience_tabversion = map2(glmm_pvalfilt_nightscience, mod, ~broom.mixed::tidy(.x$model@model, conf.int = TRUE, exponentiate = ifelse(.y == "abundance", FALSE, TRUE)))) %>%
  mutate(glmm_pvalfilt_dayscience_tabversion = map(glmm_pvalfilt_dayscience_tabversion, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(glmm_pvalfilt_nightscience_tabversion = map(glmm_pvalfilt_nightscience_tabversion, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(rf_dayscience_varimp = map(multiv_rf_dayscience, ~randomForest::importance(.$mod$fit, type = 1, scale = F))) %>%
  mutate(rf_nightscience_varimp = map(multiv_rf_nightscience, ~randomForest::importance(.$mod$fit, type = 1, scale = F))) %>%
  mutate(rf_dayscience_varimp = map(rf_dayscience_varimp, ~as.data.frame(.))) %>%
  mutate(rf_nightscience_varimp = map(rf_nightscience_varimp, ~as.data.frame(.))) %>%
  mutate(rf_dayscience_varimp = map(rf_dayscience_varimp, ~dplyr::rename(., importance = 1))) %>%
  mutate(rf_nightscience_varimp = map(rf_nightscience_varimp, ~rename(.,importance = 1))) %>%
  mutate(rf_dayscience_varimp = map(rf_dayscience_varimp, ~mutate(.,name=rownames(.)))) %>%
  mutate(rf_nightscience_varimp = map(rf_nightscience_varimp, ~mutate(.,name=rownames(.)))) %>%
  mutate(rf_dayscience_varimp = map(rf_dayscience_varimp, ~fun_get_predictors_labels(table = ., vector_predictors_name = "name"))) %>%
  mutate(rf_nightscience_varimp = map(rf_nightscience_varimp, ~fun_get_predictors_labels(table = ., vector_predictors_name = "name"))) %>%
  mutate(rf_dayscience_perf = map2(multiv_rf_dayscience, mod, ~ifelse( .y == "abundance", min(.x$mod$results$RMSE), max(.x$mod$results$AUC)))) %>%
  mutate(rf_nightscience_perf = map2(multiv_rf_nightscience, mod, ~ifelse( .y == "abundance", min(.x$mod$results$RMSE), max(.x$mod$results$AUC)))) %>%
  mutate(rf_dayscience_varimp = pmap(list(rf_dayscience_varimp, rf_dayscience_perf, mod), ~bind_rows(..1, data.frame(name=NA, label=ifelse(..3 == "abundance","RMSE","AUC"), label_group="Model perf.", importance = ..2)))) %>%
  mutate(rf_nightscience_varimp = pmap(list(rf_nightscience_varimp, rf_nightscience_perf, mod), ~bind_rows(..1, data.frame(name=NA, label=ifelse(..3 == "abundance","RMSE","AUC"), label_group="Model perf.", importance = ..2)))) %>%
  mutate(univ_spearmancorr = pmap(list(univ_spearmancorr, response_var, code_pays, mod), ~mutate(..1, species = ..2, country = ..3, indicator = ..4))) %>%
  mutate(univ_glmm = pmap(list(univ_glmm, response_var, code_pays, mod), ~mutate(..1, species = ..2, country = ..3, indicator = ..4))) %>%
  mutate(glmm_spearmanfilt_dayscience_tabversion =  pmap(list(glmm_spearmanfilt_dayscience_tabversion, response_var, code_pays, mod), ~mutate(..1, species = ..2, country = ..3, indicator = ..4))) %>%
  mutate(glmm_spearmanfilt_nightscience_tabversion =  pmap(list(glmm_spearmanfilt_nightscience_tabversion, response_var, code_pays, mod), ~mutate(..1, species = ..2, country = ..3, indicator = ..4))) %>%
  mutate(glmm_pvalfilt_dayscience_tabversion =  pmap(list(glmm_pvalfilt_dayscience_tabversion, response_var, code_pays, mod), ~mutate(..1, species = ..2, country = ..3, indicator = ..4))) %>%
  mutate(glmm_pvalfilt_nightscience_tabversion =  pmap(list(glmm_pvalfilt_nightscience_tabversion, response_var, code_pays, mod), ~mutate(..1, species = ..2, country = ..3, indicator = ..4))) %>%
  mutate(rf_dayscience_varimp =  pmap(list(rf_dayscience_varimp, response_var, code_pays, mod), ~mutate(..1, species = ..2, country = ..3, indicator = ..4))) %>%
  mutate(rf_nightscience_varimp =  pmap(list(rf_nightscience_varimp, response_var, code_pays, mod), ~mutate(..1, species = ..2, country = ..3, indicator = ..4)))
  

univ_glmm <- do.call(rbind.data.frame, model_results$univ_glmm)
univ_spearmancorr <- do.call(rbind.data.frame, model_results$univ_spearmancorr)
glmm_pvalfilt_dayscience <-  do.call(rbind.data.frame, model_results$glmm_pvalfilt_dayscience_tabversion)
glmm_pvalfilt_nightscience <-  do.call(rbind.data.frame, model_results$glmm_pvalfilt_dayscience_tabversion)
glmm_spearmanfilt_dayscience <-  do.call(rbind.data.frame, model_results$glmm_spearmanfilt_dayscience_tabversion)
glmm_spearmanfilt_nightscience <-  do.call(rbind.data.frame, model_results$glmm_spearmanfilt_dayscience_tabversion)
rf_dayscience <-  do.call(rbind.data.frame, model_results$rf_dayscience_varimp)
rf_nightscience <-  do.call(rbind.data.frame, model_results$rf_nightscience_varimp)


  
univ_spearmancorr <- univ_spearmancorr %>%
  mutate(correlation = ifelse(pval >= 0.2 | abs_corr <= 0.2, NA, correlation)) %>%
  mutate(correlation = round(correlation,2)) %>%
  nest(-indicator)


univ_glmm <- univ_glmm %>%
  mutate(effect = case_when(estimate < 1 & indicator != "abundance" ~ "negative",
                         estimate >= 1 & indicator != "abundance" ~ "positive",
                         estimate >= 0 & indicator == "abundance" ~ "positive",
                         estimate < 0  & indicator == "abundance" ~ "negative")) %>%
  mutate(effect = ifelse(p.value >= 0.2, NA, effect)) %>%
  mutate(estimate = ifelse(p.value >= 0.2, NA, estimate)) %>%
  nest(-indicator)


glmm_pvalfilt_dayscience <- glmm_pvalfilt_dayscience %>%
  filter(!is.na(label)) %>%
  mutate(label = ifelse(grepl("Vector control measure|interior",label), paste0(label, " (comp. to ",unit,")"),paste0(label, " (by add. ",unit,")"))) %>%
  mutate(effect = case_when(estimate < 1 & indicator != "abundance" ~ "negative",
                                                   estimate >= 1 & indicator != "abundance" ~ "positive",
                                                   estimate >= 0 & indicator == "abundance" ~ "positive",
                                                   estimate < 0  & indicator == "abundance" ~ "negative")) %>%
  mutate(estimate = ifelse(p.value >= 0.05 & !grepl("Vector control measure",label), NA, estimate)) %>%
  nest(-indicator)

glmm_pvalfilt_nightscience <- glmm_pvalfilt_nightscience %>%
  filter(!is.na(label)) %>%
  mutate(label = paste0(label, " (by add. ",unit,")")) %>%
  mutate(effect = case_when(estimate < 1 & indicator != "abundance" ~ "negative",
                            estimate >= 1 & indicator != "abundance" ~ "positive",
                            estimate >= 0 & indicator == "abundance" ~ "positive",
                            estimate < 0  & indicator == "abundance" ~ "negative")) %>%
  mutate(estimate = ifelse(p.value >= 0.05 & !grepl("Vector control measure",label), NA, estimate)) %>%
  nest(-indicator)

glmm_spearmanfilt_dayscience <- glmm_spearmanfilt_dayscience %>%
  filter(!is.na(label)) %>%
  mutate(label = paste0(label, " (by add. ",unit,")")) %>%
  mutate(effect = case_when(estimate < 1 & indicator != "abundance" ~ "negative",
                            estimate >= 1 & indicator != "abundance" ~ "positive",
                            estimate >= 0 & indicator == "abundance" ~ "positive",
                            estimate < 0  & indicator == "abundance" ~ "negative")) %>%
  mutate(estimate = ifelse(p.value >= 0.05 & !grepl("Vector control measure",label), NA, estimate)) %>%
  nest(-indicator)

glmm_spearmanfilt_nightscience <- glmm_spearmanfilt_nightscience %>%
  filter(!is.na(label)) %>%
  mutate(label = paste0(label, " (by add. ",unit,")")) %>%
  mutate(effect = case_when(estimate < 1 & indicator != "abundance" ~ "negative",
                            estimate >= 1 & indicator != "abundance" ~ "positive",
                            estimate >= 0 & indicator == "abundance" ~ "positive",
                            estimate < 0  & indicator == "abundance" ~ "negative")) %>%
  mutate(estimate = ifelse(p.value >= 0.05 & !grepl("Vector control measure",label), NA, estimate)) %>%
  nest(-indicator)


rf_dayscience <- rf_dayscience %>%
  mutate(importance = ifelse(importance < 0, 0, importance)) %>%
  nest(-indicator)

rf_nightscience <- rf_nightscience %>%
  mutate(importance = ifelse(importance < 0, 0, importance)) %>%
  nest(-indicator)



univ_spearmancorr <- univ_spearmancorr %>%
  mutate(plot_univ_spearmancorr = map2(data, indicator, ~fun_plot_tile(df = .x, type = "spearman", fillvar = "correlation", metric_name = "spearman corr.", indicator = .y, model_type = "Spearman correlation (univariate)"))) %>%
  dplyr::select(-data)

univ_glmm <- univ_glmm %>%
  mutate(plot_univ_glmm = map2(data, indicator, ~fun_plot_tile(df = .x, type = "glmm", fillvar = "effect", metric_name = "effect sign", indicator = .y, model_type = "GLMM (univariate)"))) %>%
  dplyr::select(-data)

glmm_pvalfilt_dayscience <- glmm_pvalfilt_dayscience %>%
  mutate(plot_glmm_pvalfilt_dayscience = map2(data, indicator, ~fun_plot_tile(df = .x, type = "glmm", fillvar = "effect", metric_name = "effect sign", indicator = .y, model_type = "GLMM (multivariate) / univ. filter : pvalue of univ. GLMM"))) %>%
  dplyr::select(-data)

glmm_pvalfilt_nightscience <- glmm_pvalfilt_nightscience %>%
  mutate(plot_glmm_pvalfilt_nightscience = map2(data, indicator, ~fun_plot_tile(df = .x, type = "glmm", fillvar = "effect", metric_name = "effect sign", indicator = .y, model_type = "GLMM (multivariate) / univ. filter : pvalue of univ. GLMM"))) %>%
  dplyr::select(-data)

glmm_spearmanfilt_dayscience <- glmm_spearmanfilt_dayscience %>%
  mutate(plot_glmm_spearmanfilt_dayscience = map2(data, indicator, ~fun_plot_tile(df = .x, type = "glmm", fillvar = "effect", metric_name = "effect sign", indicator = .y, model_type = "GLMM (multivariate) / univ. filter : spearman corr."))) %>%
  dplyr::select(-data)

glmm_spearmanfilt_nightscience <- glmm_spearmanfilt_nightscience %>%
  mutate(plot_glmm_spearmanfilt_nightscience = map2(data, indicator, ~fun_plot_tile(df = .x, type = "glmm", fillvar = "effect", metric_name = "effect sign", indicator = .y, model_type = "GLMM (multivariate) / univ. filter : spearman corr."))) %>%
  dplyr::select(-data)

rf_dayscience <- rf_dayscience %>%
  mutate(plot_rf_dayscience = map2(data, indicator, ~fun_plot_tile(df = .x, type = "rf", fillvar = "importance", metric_name = "importance", indicator = .y, model_type = "Random forest - feature importance (permutation)"))) %>%
  dplyr::select(-data)

rf_nightscience <- rf_nightscience %>%
  mutate(plot_rf_nightscience = map2(data, indicator, ~fun_plot_tile(df = .x, type = "rf", fillvar = "importance", metric_name = "importance", indicator = .y, model_type = "Random forest - feature importance (permutation)"))) %>%
  dplyr::select(-data)

plots <- univ_spearmancorr %>%
  left_join(univ_glmm) %>%
  left_join(glmm_pvalfilt_dayscience) %>%
  left_join(glmm_pvalfilt_nightscience) %>%
  left_join(glmm_spearmanfilt_dayscience) %>%
  left_join(glmm_spearmanfilt_nightscience) %>%
  left_join(rf_dayscience) %>%
  left_join(rf_nightscience)


# save plots

plots <- plots %>%
  mutate(plot_multiv = purrr::pmap(list(plot_univ_glmm, plot_glmm_pvalfilt_dayscience), ~patchwork::wrap_plots(..1,..2) ))

# pdps
plots_interpret_rf <- model_results[6,] %>%
  filter(mod %in% c("presence","abundance")) %>%
  mutate(plots_rf_dayscience = furrr::future_pmap(list(multiv_rf_dayscience, mod, response_var, rf_dayscience_perf) , ~fun_plot_pdp(..1$mod$fit, ..1$df_mod, ..2, ..3, ..4))) %>%
  dplyr::select(response_var,code_pays,mod,plots_rf_dayscience)
  #mutate(plots_rf_nightscience = furrr::future_pmap(list(multiv_rf_nightscience, mod, response_var, rf_nightscience_perf) , ~fun_plot_pdp(..1$mod$fit,..1$df_mod,..2,..3, ..4))) %>%
  #dplyr::select(response_var,code_pays,mod,plots_rf_dayscience,plots_rf_nightscience)





