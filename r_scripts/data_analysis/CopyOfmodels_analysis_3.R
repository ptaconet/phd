library(tidyverse)
library(patchwork)
library(DBI)
library(sjPlot) # see : https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html
library(stringr)
library(sf)
library(ggmap)

### connect to the database
path_to_db <- "data/react_db/react_db.gpkg" 
react_gpkg <- DBI::dbConnect(RSQLite::SQLite(),dbname = path_to_db) 

# open model results
#model_results <- readRDS("/home/ptaconet/Bureau/data_analysis/model_results_newanalysis.rds")

### source home-made functions 
source("r_scripts/data_analysis_tests/functions_script_data_analysis.R")

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
      #dplyr::mutate(pixlabel = paste0(pixlabel, " - ",buffer1," m buff.")) %>%
      dplyr::mutate(pixlabel = ifelse(grepl("prd",name1),"patch richness density",pixlabel)) %>%
      dplyr::mutate(pixlabel = ifelse(grepl("shdi",name1),"shannon diversity index",pixlabel)) %>%
      dplyr::select(code, pixlabel) %>%
      dplyr::rename(short_name = pixlabel) %>%
      dplyr::arrange(code) %>%
      dplyr::mutate(group = ifelse(short_name %in% c("Marsh","Riparian forests","Still waters"), "Landscape - wetlands","Landscape - other")) %>%
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
    
    vector_predictors_past_climatic_conditions <- vector_predictors_other %>%
      dplyr::filter(group == "Past climatic conditions") %>%
      mutate(day_start = as.numeric(word(code,3,sep = "\\_"))) %>%
      mutate(day_end = as.numeric(word(code,4,sep = "\\_"))) %>%
      mutate(day_start = ifelse(grepl("TMIN1|TMAX1|TAMP1|RFD1|SMO1",code),day_start/7,day_start)) %>%
      mutate(day_end = ifelse(grepl("TMIN1|TMAX1|TAMP1|RFD1|SMO1",code),day_end/7,day_end)) %>%
      mutate(short_name = paste0(short_name, " b/w ",day_start," and ",day_end," weeks")) %>%
      dplyr::select(-c(day_start,day_end))
    
    vector_predictors_other <- vector_predictors_other %>%
      dplyr::filter(group != "Past climatic conditions") %>%
      bind_rows(vector_predictors_past_climatic_conditions)
    
  }
  
  table_labels <- rbind(vector_predictors_lsm, vector_predictors_other, vector_predictors_vcm)
  
  if(!is.null(table)){
    colnames(table_labels) <- c(vector_predictors_name,"label","label_group","unit")
    table_labels <- table_labels %>%
      dplyr::right_join(table)
  }
  
  return(table_labels)
  
}


fun_glmm_dotpoint <- function(glmm_model_tabversion, mod, quality){
  
  if(mod == "presence"){
    metric <- "AUC"
  } else if (mod == "abundance_discrete"){
    metric <- "Accuracy"
  } else if (mod == "abundance"){
    metric <- "RMSE"
  }
  
  glmm_model_tabversion <- glmm_model_tabversion %>%
    filter(!is.na(label)) %>%
    mutate(label = ifelse(grepl("Vector control measure|Household outdoor|still water",label), paste0(label, " (comp. to ",unit,")"),paste0(label, " (by add. ",unit,")"))) %>%
    # mutate(p.value2 = case_when(
    #   p.value <= 0.01 ~ "< 0.01 ***",
    #   p.value >= 0.01 & p.value <= 0.03 ~ paste0("= ", round(p.value,2), " **"),
    #   p.value > 0.03 & p.value <= 0.05 ~ paste0("= ",round(p.value,2), " *"),
    #   p.value > 0.05 ~ paste0("= ",round(p.value,2))
    # )) %>%
    mutate(p.value2 = case_when(
      p.value <= 0.01 ~ "***",
      p.value >= 0.01 & p.value <= 0.03 ~  "**",
      p.value > 0.03 & p.value <= 0.05 ~  "*",
      p.value > 0.05 ~ ""
    )) %>%
    mutate(p.value2 = ifelse(label_group == "Vector control",paste0(" (p=",round(p.value,2),p.value2,")"),p.value2)) %>%
    mutate(p.value2 = paste(round(estimate,2), p.value2)) %>%
    #mutate(p.value2 = paste0(label," : ", p.value2)) %>%
    mutate(col = case_when(
      mod == "abundance" & estimate <= 0 ~ "negative", 
      mod == "abundance" & estimate > 0 ~ "positive", 
      mod != "abundance" & estimate <= 1 ~ "negative", 
      mod != "abundance" & estimate > 1 ~ "positive"
    )) %>%
    mutate(label = forcats::fct_reorder(label,estimate)) %>%
    mutate(label_group = forcats::fct_relevel(label_group,c("Past climatic conditions",
                                                            "Micro-climatic conditions on the night of catch",
                                                            "Landscape - wetlands",
                                                            "Landscape - other",
                                                            "Water pres.",
                                                            "Topography",
                                                            "Human hosts availability",
                                                            "Other",
                                                            "Place",
                                                            "Hum. behav.",
                                                            "Vector control tools"
    ))) %>%
    mutate(label_group = case_when(label_group == "Past climatic conditions" ~ "A",
                                   label_group == "Landscape - wetlands" ~ "B",
                                   label_group == "Landscape - other" ~ "C",
                                   label_group == "Micro-climatic conditions on the night of catch" ~ "D",
                                   label_group == "Water pres." ~ "-",
                                   label_group == "Topography" ~ "--",
                                   label_group == "Human hosts availability" ~ "E",
                                   label_group == "Other" ~ "---",
                                   label_group == "Place" ~ "F",
                                   label_group == "Hum. behav." ~ "----",
                                   label_group == "Vector control tools" ~ "G"))
    
  plot <- ggplot(glmm_model_tabversion, aes(x = estimate, y = label, label = p.value2)) +
    theme_bw() + 
    facet_grid(label_group~., scales="free_y", space="free_y") + 
    geom_point() + 
    geom_errorbar(aes(xmin=conf.low, xmax=conf.high), width=.2, position=position_dodge(.9)) +
    geom_text(size = 3, vjust = -.7, hjust = 0.08) + 
    theme(axis.text.y = element_text(size = 8, colour = "black"), 
          legend.position = "none", strip.text.y = element_text(size = 8), 
          plot.title = element_text(size = 10),
          axis.title.x = element_text(size = 8)) + #, axis.text.y = element_text(vjust = -0.7, margin = margin(l = 20, r = -100))) + axis.text.y=element_blank(), 
    xlab(ifelse(mod=="abundance","Incidence rate-ratio","Odd ratio"))  +
    ylab("") +
    scale_color_manual(values=c("darkorchid2", "darkorange", "green")) +
    geom_vline(aes(xintercept = ifelse(mod=="abundance", 0, 1)),linetype = "dashed") +
    #ggtitle("Multivariate analysis (GLMM)")#+ 
    ggtitle(paste0(metric," = ",round(quality,2)))
  
  return(plot)
}


fun_plot_tile <- function(df, type, xvar = "model", yvar = "label", fillvar, metric_name, indicator, model_type, species, country){
  
  df <- df %>%
    mutate(label_group = forcats::fct_relevel(label_group, c("Model perf.", unique(prediction_vars$group)))) %>%
    mutate(model = forcats::fct_relevel(model,c("glmm univariate",
                                                "glmm multivariate \n 'day science' \n univ. filter = pval",
                                                "glmm multivariate \n 'night science' \n univ. filter = pval",
                                                "glmm multivariate \n 'day science' \n univ. filter = spearman coeff.",
                                                "glmm multivariate \n 'night science' \n univ. filter = spearman coeff."
    )))
  
  p <- ggplot(df, aes_string(xvar, yvar)) + 
    geom_tile(aes_string(fill = fillvar), color = "white") + facet_grid(label_group~., scales="free_y", space="free_y") +
    theme_bw() + 
    ggtitle(paste(species, country, indicator, sep = " - "), subtitle = paste0("Model family : ", model_type)) + 
    ylab("variable")
  
  if(type == "spearman"){
    p <- p + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name = metric_name, na.value="transparent") + geom_text(aes(label = ifelse(is.na(correlation), "",round(correlation,2))), size = 3) +
      labs(caption = paste0("Values displayed inside the cells : spearman correlation coefficient \n Only significant results (correlation >= 0.2 and pval <= 0.2 analysis) are displayed. Vector control measures and place (int.ext) variables where one-hot encoded."))
  }
  if(type == "glmm"){
    p <- p +  scale_fill_discrete(na.value="transparent")  + 
      geom_text(aes(label = ifelse(is.na(estimate), "",paste0(round(estimate,2)," [",round(conf.low,2),"; ",round(conf.high,2),"] / p",ifelse(p.value < 0.01,"<0.01",paste0("=",round(p.value,2)))))), size = 3) +
      labs(caption = paste0("Format of the values displayed inside the cells : ",ifelse(indicator == "abundance","DDR","ODR")," [95% CI] / p-value \n Only significant results (pval <= 0.05) are displayed, except for vector control measures where all results are displayed."))
  }
  if(type == "rf"){
    p <- p + scale_fill_gradient(na.value = "transparent", low = "blue", high = "red", limit = c(0,max(df$importance[which(df$label_group!="Model perf.")])), space = "Lab", name = metric_name) + geom_text(aes(label = ifelse(label_group == "Model perf.", round(importance,2),"")), size = 3) +
      labs(caption = paste0("The overall quality of the model is displayed in the first row."))
    
  }
  
  return(p)
}

fun_plot_tile_univ_spatial <- function(df, type, xvar = "buffer", yvar = "label", fillvar, metric_name, indicator, 
                                       model_type, species, country){
  
  #df$label <- reorder(as.factor(df$label), df$label_group)
  df <- df %>%
    mutate(buffer = forcats::fct_relevel(buffer, c("50","100","250","500","1000","2000"))) %>%
    mutate(indicator = forcats::fct_relevel(indicator, c("presence","abundance")))
  df$label <- factor(df$label, levels = unique(df$label[order(df$correlation)]))
  df <- df %>% filter(!is.na(correlation),pval<=0.2)
  
  
  p <- ggplot(df, aes_string(xvar, yvar)) + 
    geom_tile(aes_string(fill = fillvar), color = "white") + #facet_grid(.~indicator, scales="free_y", space="free_y") +
    #ggtitle(paste(species, country, indicator, sep = " - "), subtitle = paste0("Model family : ", model_type)) + 
    #ggtitle("Bivariate analysis - spatial variables") +
    #ylab("variable") +
    xlab("buffer size around the catch point") + 
    ylab("") +
    theme_bw() +
   # theme(axis.ticks.x = element_blank(),
          #axis.text.x = element_blank(),
          #axis.ticks.y = element_blank(),
          #axis.text.y = element_blank()) +
    theme(legend.position = "none")
      
  
  if(type == "spearman"){
    p <- p + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name = metric_name, na.value="grey") + geom_text(aes(label = ifelse(is.na(correlation), "",paste0(round(correlation,2)," / p",ifelse(pval < 0.01,"<0.01",paste0("=",round(pval,2)))))), size = 3) #+
      #labs(caption = paste0("Values displayed inside the cells : spearman correlation coefficient \n Only significant results (pval <= 0.2) are displayed"))
  }
  if(type == "glmm"){
    p <- p + geom_text(aes(label = ifelse(is.na(estimate), "",paste0(round(estimate,2)," [",round(conf.low,2),"; ",round(conf.high,2),"] / p",ifelse(p.value < 0.01,"<0.01",paste0("=",round(p.value,2)))))), size = 3) +
      labs(caption = paste0("Format of the values displayed inside the cells : ",ifelse(indicator == "abundance","DDR","ODR")," [95% CI] / p-value \n Only significant results (pval <= 0.2) are displayed.")) + 
      scale_fill_discrete(na.value="transparent") 
  }
  
  return(p)
}

fun_plot_dotpoint_univ_spatial <- function(df){
  
  df$label <- factor(df$label, levels = unique(df$label[order(df$correlation)]))
  df <- df %>% filter(!is.na(correlation))
 plot =  ggplot(df, aes(correlation, label)) +
  geom_point(size=1.5) + 
  geom_text(aes(label=paste0(round(correlation, 2)," (p",ifelse(pval < 0.01,"<0.01",paste0("=",round(pval,2))),")")),hjust=-0.3, vjust=0.3, size = 3) +
  xlim(-1,1) +
  ylab("variable") + 
  xlab("Spearman correlation") + 
  theme_bw() +
  geom_vline(xintercept=0, linetype="dashed", color = "red") + 
  guides(shape=FALSE) +
  facet_grid(label_group~., scales = "free", space = "free") + 
  labs(x = NULL, y = "spearman correlation")
 
 return(plot)
}


# function to plot the CCM (simple plot : only the CCM)
fun_ccm_plot2 <- function(correlation_df, max_or_min, metric_name, var, filter_lags_ndays = 0, mod = "presence"){
  
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
          axis.title = element_text(size = 8),
          legend.key.size = unit(0.8, "cm"),
          legend.title=element_text(size=10),
          legend.position = "none"
    ) +
    #ggtitle(paste0("CCM for : area = ",country," ; variable = ",var," ; buffer = ",buffer," m")) +
    #ggtitle("Bivariate analysis - temporal variables") +
    ggtitle(var) +
    annotate("text", size = 3,x = min(correlation_df$time_lag_1), y = max(correlation_df$time_lag_2), vjust = "inward", hjust = "inward", label = paste0("r(0,0) = ", round(correlation_df$correlation[1],3),"\nr(",abs_corr$time_lag_1,",",abs_corr$time_lag_2,") = ",round(abs_corr$correlation,3))) +
    coord_fixed() +
    ylab(paste0("time lag 2")) +
    xlab(paste0("time lag 1"))
  
  if(metric_name=="Spearman\ncorrelation"){
    ccm_plot <- ccm_plot + 
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name = metric_name) 
  } else if(metric_name=="RMSE"){
    ccm_plot <- ccm_plot + 
      scale_fill_gradient(low = "red", high = "white", space = "Lab", name="RMSE") 
  } else if(metric_name=="estimate"){
    ccm_plot <- ccm_plot + 
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = ifelse(mod == "abundance", 0, 1), limit = c(correlation_df$min[1], correlation_df$max[1]), space = "Lab", name = ifelse(mod == "abundance","DDR","ODR")) 
  }
  
  return(ccm_plot)
  
}



fun_plot_pdp <- function(modell, indicator, species){
  
  library(iml)
  library("future")
  library("future.callr")
  model = modell$mod$finalModel
  df = modell$df_mod %>% dplyr::select(resp_var, model$xNames)
  
  if(indicator == "presence"){
    mod <- Predictor$new(model, data = df, class = "Presence")
    metric <- "AUC"
    quality = max(modell$mod$results$AUC)
  } else if (indicator == "abundance_discrete"){
    mod <- Predictor$new(model, data = df)
    metric <- "Accuracy"
    quality = max(modell$mod$results$Accuracy)
  } else if (indicator == "abundance"){
    mod <- Predictor$new(model, data = df)
    metric <- "RMSE"
    quality = min(modell$mod$results$RMSE)
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
  # plan("callr", workers = 7)
  # future::plan(multisession, workers = 7)
  # interact = Interaction$new(mod, grid.size = 30)
  # 
  # interact$results$feat <- word(gsub(":"," ",interact$results$.feature),1)
  # interact$results <- fun_get_predictors_labels(interact$results,"feat")
  # interact$results$.feature <- interact$results$label
  # interact$results <- interact$results %>% filter(!is.na(.feature))
  # plot_interactions <- plot(interact) +
  #   scale_y_discrete("") +
  #   theme_bw() +
  #   ggtitle("Interaction strengths")
  
  # get most important interaction
  #interaction_most_interact_var = Interaction$new(mod, feature = interact$results$feat[which.max(interact$results$.interaction)]) 
  #interaction_most_interact_var$results = interaction_most_interact_var$results %>% dplyr::arrange(.interaction)
  
  # pdp 2 vars for the main interaction
  # pd = FeatureEffect$new(mod, c(as.character(imp$var[1]), as.character(imp$var[2])), method = "pdp") 
  # pdp_2vars <- plot(pd) + 
  #   xlab(as.character(imp$label[1])) + 
  #   ylab(as.character(imp$label[2]))  + 
  #   theme_bw() + 
  #   ggtitle("partial dependance plots (2 vars)")
  
  
   pred_wrapper_classif <- function(object, newdata) {
     p <- predict(object, newdata = newdata, type ="prob")[,"Presence"]
     c("avg" = mean(p), "avg-1sd" = mean(p) - sd(p), "avg+1sd" = mean(p) + sd(p))
   }
   
   pred_wrapper_reg <- function(object, newdata) {
     p <- predict(object, newdata = newdata)
     c("avg" = mean(p), "avg-1sd" = mean(p) - sd(p), "avg+1sd" = mean(p) + sd(p))
   }
  
  # partial dependance plot
  pdps <- list()
  

  imp <- imp %>%
    mutate(label_group = forcats::fct_relevel(label_group,c("Past climate",
                                                          "Micro-climate",
                                                          "Land cover",
                                                          "Land cover and related",
                                                          "Water pres.",
                                                          "Topography",
                                                          "Human hosts availability",
                                                          "Other",
                                                          "Place",
                                                          "Hum. behav.",
                                                          "Vector control"
  ))) %>%
    mutate(label_group = case_when(label_group == "Past climatic conditions" ~ "A",
                                   label_group == "Landscape - wetlands" ~ "B",
                                   label_group == "Landscape - other" ~ "C",
                                   label_group == "Micro-climatic conditions on the night of catch" ~ "D",
                                   label_group == "Water pres." ~ "-",
                                   label_group == "Topography" ~ "--",
                                   label_group == "Human hosts availability" ~ "E",
                                   label_group == "Other" ~ "---",
                                   label_group == "Place" ~ "F",
                                   label_group == "Hum. behav." ~ "----",
                                   label_group == "Vector control tools" ~ "G")) %>%
    arrange(label_group)
  
  
  
  # if(species == "An. funestus ss." & indicator == "abundance_discrete"){
  #   target <- c("TMAX1_2000_7_35","lsm_c_pland_2000_3_9","lsm_c_pland_2000_3_4","RFD1F_2000_7_14","RFH","LMN")
  #   imp <- imp %>% arrange(factor(var, levels = target))
  # }
  # if(species == "An. gambiae ss." & indicator == "abundance_discrete"){
  #   imp <- imp %>% dplyr::filter(var!="int_ext")
  # }
  # if(species == "An. coluzzii" & indicator == "abundance_discrete"){
  #   imp <- imp %>% dplyr::filter(var!="VCM")
  # }
  # if(species == "An. coluzzii" & indicator == "presence"){
  #   target <- c("RFD1F_2000_21_35","TMAX1_2000_21_35","lsm_c_pland_2000_3_9","WMD","lsm_c_pland_2000_3_12","WAC_2000")
  #   imp <- imp %>% dplyr::filter(var %in% target) %>% arrange(factor(var, levels = target))
  # }
  # if(species == "An. gambiae ss." & indicator == "presence"){
  #   target <- c("RFD1F_2000_14_21","TMAX1_2000_0_42","lsm_c_pland_2000_3_9","lsm_c_pland_2000_3_12","WLS_2000","VCM")
  #   imp <- imp %>% dplyr::filter(var %in% target) %>% arrange(factor(var, levels = target))
  #     
  # }
  # if(species == "An. funestus ss." & indicator == "presence"){
  #   target <- c("lsm_c_pland_2000_3_9","lsm_c_pland_2000_3_12","WAC_2000","RFD1F_2000_14_21","RFH","LMN")
  #   imp <- imp %>% dplyr::filter(var %in% target) %>% arrange(factor(var, levels = target))
  #   
  # }

  
  for(i in 1:nrow(imp)){
    
    pdp = FeatureEffect$new(mod, imp$var[i], method = "pdp")
    
    if(indicator == "presence"){
      pdps[[i]] = pdp$plot() +
        scale_y_continuous('', limits = c(0, 1)) +
        #xlab(paste0(imp$label_group[i]," : \n",imp$label[i], " (", imp$unit[i], ")")) +
        #xlab(imp$unit[i]) +
        xlab("") +
        labs(subtitle = paste0(imp$label[i], " (", imp$unit[i], ")")) + 
        theme_bw() + 
        theme(axis.title.x = element_text(size = 8),
              axis.text.x = element_text(size = 6),
              axis.text.y = element_text(size = 6),
              plot.subtitle = element_text(size = 8))
      
    } else if (indicator == "abundance_discrete"){
      
      pdp$results <- pdp$results %>%
        dplyr::mutate(.class = case_when(.class == "high....4.bites." ~ "high (> 4 bites)",
                                         .class == "low..1.bite." ~ "low (1 bite)",
                                         .class == "medium..between.1.and.4.bites." ~ "medium (b/w 1 and 4 bites)")) %>%
        mutate(.class = fct_relevel(.class,c("low (1 bite)","medium (b/w 1 and 4 bites)","high (> 4 bites)"))) %>%
        dplyr::rename(biting_rate = .class)
      
      pdps[[i]] = ggplot(pdp$results,aes_string(x = imp$var[i], y = ".value")) +
        geom_line(aes(linetype =biting_rate)) +
        scale_y_continuous('', limits = c(0, 1)) +
        #xlab(paste0(imp$label_group[i]," : \n",imp$label[i], " (", imp$unit[i], ")")) +
        #xlab(imp$unit[i]) +
        xlab("") +
        labs(subtitle = paste0(imp$label[i], " (", imp$unit[i], ")")) + 
        theme_bw() + 
        theme(axis.title.x = element_text(size = 8),
              axis.text.x = element_text(size = 6),
              axis.text.y = element_text(size = 6),
              legend.title = element_text(size = 9), 
              legend.text = element_text(size = 8),
              plot.subtitle = element_text(size = 8))
      
       

      
        # pdps[[i]] =  pdp$plot() +
        # scale_y_continuous('', limits = c(0, 1)) +
        # xlab(paste0(imp$label[i], " (", imp$unit[i], ")")) +
        # theme_bw()
      
      
      }
    else if (indicator == "abundance"){
      pdps[[i]] = pdp$plot() +
        scale_y_continuous('', limits = c(0, max(df$resp_var))) +
        #xlab(paste0(imp$label_group[i]," : \n",imp$label[i], " (", imp$unit[i], ")")) +
        #xlab(imp$unit[i]) +
        xlab("") +
        labs(subtitle = paste0(imp$label[i], " (", imp$unit[i], ")")) + 
        theme_bw() + 
        theme(axis.title.x = element_text(size = 8),
              axis.text.x = element_text(size = 6),
              axis.text.y = element_text(size = 6),
              plot.subtitle = element_text(size = 8))
    }
    
    if(indicator != "abundance_discrete" & !is.factor(df[,imp$var[i]])){
    pdp$results$.value <- pdp$results$.type <- NULL

    if(indicator != "abundance"){
     p = pdp::partial(model, pred.var = imp$var[i], pred.fun = pred_wrapper_classif, train = df, pred.grid = pdp$results)
    } else {
      p = pdp::partial(model, pred.var = imp$var[i], pred.fun = pred_wrapper_reg, train = df, pred.grid = pdp$results)
    }

    p<-p  %>% mutate(yhat = ifelse(yhat<0,0,yhat))

    p$yhat.id <- as.factor(p$yhat.id)

    pdps[[i]]$layers[[1]] <- NULL
    pdps[[i]] <-  pdps[[i]] +
      geom_line(data = p %>% filter(yhat.id == "avg"), aes_string(x = imp$var[i], y = "yhat"), size = 0.5) +
      geom_line(data = p %>% filter(yhat.id != "avg"), aes_string(x = imp$var[i], y = "yhat", group = "yhat.id"), linetype = "dashed", size = 0.15)
    }
    
  }
  
  pdps <- wrap_plots(pdps) +  plot_layout(guides = 'collect') + plot_annotation(title = paste0(metric," = ",round(quality,2)), theme = theme(plot.title = element_text(size = 10)))
  
  #p_final <- (plot_imp  + pdps ) / (plot_interactions ) + plot_annotation(title = paste0(species, " - ", indicator, " (",metric," = ",round(quality,2),")")) + plot_layout(guides = 'auto')

  return(list(plot_imp = plot_imp, pdps = pdps, quality = quality, metric = metric))
  
}




### define function ti shift legend (from https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2)
library(gtable)
library(lemon)
library(ggplotify)
shift_legend2 <- function(p) {
  # check if p is a valid object
  if(!(inherits(p, "gtable"))){
    if(inherits(p, "ggplot")){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]), 
                               USE.NAMES = F)
  empty.facet.panels <- facet.panels[empty.facet.panels]
  
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }
  
  # establish name of empty panels
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  names <- empty.facet.panels$name
  
  # return repositioned legend
  return(as.ggplot(reposition_legend(p, 'center', panel=names)))
}

#### plot general data viz

fun_map <- function(mod,code_pays,response_var){

  ## map
  roi <- sf::st_read(path_to_db,"contexte_frontieresreact") %>% 
  filter(codepays == code_pays) %>%
  sf::st_transform(4326) %>%
  sf::st_bbox() %>%
  as.numeric()
  
myLocation <- c(roi[1], roi[2], roi[3], roi[4])
myMap <- get_map(location=myLocation, source="osm",crop=FALSE)

entomo_csh_metadata_l1 <- dbReadTable(react_gpkg, 'entomo_csh_metadata_l1') %>% filter(!(nummission %in% c("11","12","13","15")))

mean_date_mission <- entomo_csh_metadata_l1 %>% mutate(date_capture = as.Date(date_capture)) %>% dplyr::group_by(codepays,nummission) %>% dplyr::summarise(mean_date_mission=mean(date_capture)) %>% as_tibble() %>% filter(codepays==code_pays) %>% dplyr::select(-codepays) %>% mutate(date_mission_char = paste0(lubridate::year(mean_date_mission)," - ",lubridate::month(mean_date_mission,label=TRUE, abbr=FALSE, locale = "en_US.utf8")))  %>%   mutate(date_mission_char = forcats::fct_reorder(date_mission_char,mean_date_mission))


# load coordinates
spatial_coordinates <- load_csh_sp_coord()
mean_coords_points_4326 <- spatial_coordinates[[1]]

df <- dbReadTable(react_gpkg, 'trmetrics_entomo_postedecapture') %>% 
  dplyr::select(-fid) %>% 
  left_join(entomo_csh_metadata_l1 %>% dplyr::select(idpointdecapture, codevillage, pointdecapture, codepays, nummission, period_interv)) %>% 
  filter(!is.na(codevillage)) %>%
  filter(codepays == code_pays) %>%
  mutate(heuredecapture = NA) %>%
  left_join(mean_date_mission) %>%
  left_join(mean_coords_points_4326)

df <- df %>%
  dplyr::select(codevillage,nummission,date_mission_char,X_4326,Y_4326,ma_funestus_ss,ma_gambiae_ss,ma_coluzzi) %>%
  pivot_longer(c(ma_funestus_ss,ma_gambiae_ss,ma_coluzzi)) %>%
  mutate(name = case_when(
    name == "ma_funestus_ss" ~ "An. funestus ss.",
    name == "ma_gambiae_ss" ~ "An. gambiae ss.",
    name == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(species = fct_relevel(name,c("An. coluzzii","An. gambiae ss.","An. funestus ss.")))


 df_map <- df %>%
      group_by(codevillage,nummission, date_mission_char,species) %>%
       summarise(n_bites = sum(value), X = mean(X_4326), Y = mean(Y_4326)) %>%
   mutate(n_bites = ifelse(n_bites==0,NA,n_bites))

 villages <-  dbReadTable(react_gpkg, 'recensement_villages_l1') %>%
   filter(!is.na(intervention),codepays == code_pays)
 

  ggmap(myMap) + geom_point(aes(x = X, y = Y), data = villages, size = 0.7, color = "seagreen4") + 
      geom_point(aes(x = X, y = Y, size = n_bites), data = df_map, colour = "darkred") + 
      facet_grid(species~date_mission_char) + 
    theme(strip.text.y = element_text(size = 11, face = "bold"))
    
 
  
  
  
df$resp_var <- df[,response_var]

response_var = case_when(
  response_var == "ma_funestus_ss" ~ "An. funestus ss.",
  response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
  response_var == "ma_coluzzi" ~ "An. coluzzii")

if(mod == "presence"){
  df_map <- df %>%
    group_by(codevillage, nummission, date_mission_char) %>%
    summarise(resp_var = sum(resp_var), X = mean(X_4326), Y = mean(Y_4326)) %>%
    mutate(biting_status = ifelse(resp_var > 0, "presence", "absence"))

  map <- ggmap(myMap) + 
    geom_point(aes(x = X, y = Y, color = biting_status), data = df_map) + 
    facet_grid(.~date_mission_char) + 
    ggtitle(response_var)
  
} else if(mod == "abundance"){
  df_map <- df %>%
    group_by(codevillage,nummission, date_mission_char) %>%
    summarise(resp_var = sum(resp_var), X = mean(X_4326), Y = mean(Y_4326)) %>%
    rename(n_bites = resp_var)
  
  map <- ggmap(myMap) + 
    geom_point(aes(x = X, y = Y, size = n_bites), data = df_map, color="darkred", alpha = .5) + 
    facet_grid(.~date_mission_char) + 
    ggtitle(response_var)
  
}
#map <- shift_legend2(map)

return(map)

}


######################
###### maps and univariate analysis######
######################



model_univanalysis_results <- readRDS("/home/ptaconet/Bureau/data_analysis/model_results_univanalysis_glmm.rds") %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus ss.",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  #mutate(spatial_corrs_spearman = map(spatial_corrs_spearman, ~fun_get_predictors_labels(table = ., vector_predictors_name = "name"))) %>%
  #mutate(spatial_corrs_spearman = map(spatial_corrs_spearman, ~mutate(., buffer = word(gsub("_"," ",name), ifelse(label_group %in% c("Landscape - other","Landscape - wetlands"), 4, 2))))) %>%
  #mutate(spatial_corrs_spearman = map(spatial_corrs_spearman, ~mutate(., buffer = ifelse(is.na(buffer), word(gsub("_"," ",name), 2), buffer)))) %>%
  mutate(spatial_corrs_glmm = map(spatial_corrs_glmm, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(spatial_corrs_glmm = map(spatial_corrs_glmm, ~mutate(., buffer = word(gsub("_"," ",term), ifelse(label_group == "Land cover", 4, 2))))) %>%
  mutate(spatial_corrs_glmm = map(spatial_corrs_glmm, ~mutate(., buffer = ifelse(is.na(buffer), word(gsub("_"," ",term), 2), buffer)))) %>%
  #mutate(spatial_corrs_spearman =  pmap(list(spatial_corrs_spearman, response_var, code_pays, mod), ~mutate(..1, species = ..2, country = ..3, indicator = ..4))) %>%
  mutate(spatial_corrs_glmm =  pmap(list(spatial_corrs_glmm, response_var, code_pays, mod), ~mutate(..1, species = ..2, country = ..3, indicator = ..4))) %>%
  #mutate(temporal_corrs_spearman =  map(temporal_corrs_spearman, ~do.call(rbind.data.frame,.))) %>%
  mutate(temporal_corrs_glmm =  map(temporal_corrs_glmm, ~do.call(rbind.data.frame,.))) %>%
  #mutate(temporal_corrs_spearman = map(temporal_corrs_spearman, ~fun_get_predictors_labels(table = ., vector_predictors_name = "var"))) %>%
  mutate(temporal_corrs_glmm = map(temporal_corrs_glmm, ~fun_get_predictors_labels(table = ., vector_predictors_name = "var"))) %>%
  #mutate(temporal_corrs_spearman =  pmap(list(temporal_corrs_spearman, response_var, code_pays, mod), ~mutate(..1, species = ..2, country = ..3, indicator = ..4)))
  mutate(temporal_corrs_glmm =  pmap(list(temporal_corrs_glmm, response_var, code_pays, mod), ~mutate(..1, species = ..2, country = ..3, indicator = ..4)))
  

# univ_glmm_spatial <-  do.call(rbind.data.frame, model_univanalysis_results$spatial_corrs_glmm) %>% 
#   mutate(model = "glmm univariate") %>%
#   mutate(effect = case_when(estimate < 1 & indicator != "abundance" ~ "negative",
#                             estimate >= 1 & indicator != "abundance" ~ "positive",
#                             estimate >= 0 & indicator == "abundance" ~ "positive",
#                             estimate < 0  & indicator == "abundance" ~ "negative")) %>%
#   mutate(effect = ifelse(p.value >= 0.2, NA, effect)) %>%
#   mutate(estimate = ifelse(p.value >= 0.2, NA, estimate)) %>%
#   filter(!is.na(estimate)) %>%
#   nest(-c(indicator,species,country))
  
model_univanalysis_results_spatial_wetseason <- readRDS("/home/ptaconet/Bureau/data_analysis/model_results_univanalysis_spatial_presenceabundance_wet.rds") %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus ss.",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(spatial_corrs_spearman = map(spatial_corrs_spearman, ~fun_get_predictors_labels(table = ., vector_predictors_name = "name"))) %>%
  mutate(spatial_corrs_spearman = map(spatial_corrs_spearman, ~mutate(., buffer = word(gsub("_"," ",name), ifelse(label_group %in% c("Landscape - other","Landscape - wetlands"), 4, 2))))) %>%
  mutate(spatial_corrs_spearman = map(spatial_corrs_spearman, ~mutate(., buffer = ifelse(is.na(buffer), word(gsub("_"," ",name), 2), buffer)))) %>%
  mutate(spatial_corrs_spearman =  pmap(list(spatial_corrs_spearman, response_var, code_pays, mod), ~mutate(..1, species = ..2, country = ..3, indicator = ..4, season = "wet", indicator_season = paste(..4,"wet"))))

model_univanalysis_results_spatial_dryseason <- readRDS("/home/ptaconet/Bureau/data_analysis/model_results_univanalysis_spatial_presenceabundance_dry.rds") %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus ss.",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(spatial_corrs_spearman = map(spatial_corrs_spearman, ~fun_get_predictors_labels(table = ., vector_predictors_name = "name"))) %>%
  mutate(spatial_corrs_spearman = map(spatial_corrs_spearman, ~mutate(., buffer = word(gsub("_"," ",name), ifelse(label_group %in% c("Landscape - other","Landscape - wetlands"), 4, 2))))) %>%
  mutate(spatial_corrs_spearman = map(spatial_corrs_spearman, ~mutate(., buffer = ifelse(is.na(buffer), word(gsub("_"," ",name), 2), buffer)))) %>%
  mutate(spatial_corrs_spearman =  pmap(list(spatial_corrs_spearman, response_var, code_pays, mod), ~mutate(..1, species = ..2, country = ..3, indicator = ..4, season = "dry", indicator_season = paste(..4,"dry"))))


univ_spearman_spatial <- do.call(rbind.data.frame, model_univanalysis_results_spatial_wetseason$spatial_corrs_spearman) %>% 
  bind_rows(do.call(rbind.data.frame, model_univanalysis_results_spatial_dryseason$spatial_corrs_spearman)) %>% 
  filter(buffer %in% c(100,1000,2000), name %in% c("WAL_2000","WAC_2000","WLS_2000","POP_2000","BCH_2000","lsm_c_pland_2000_3_12","lsm_c_pland_2000_3_5","lsm_c_pland_2000_3_3","lsm_c_pland_2000_3_2","lsm_c_pland_2000_3_4","lsm_c_pland_2000_3_9","lsm_c_pland_2000_3_1","lsm_c_pland_2000_3_7","lsm_c_pland_2000_3_8","lsm_c_pland_2000_3_11",
                                                           "WAL_1000","WAC_1000","WLS_1000","POP_1000","BCH_1000","lsm_c_pland_1000_3_12","lsm_c_pland_1000_3_5","lsm_c_pland_1000_3_3","lsm_c_pland_1000_3_2","lsm_c_pland_1000_3_4","lsm_c_pland_1000_3_9","lsm_c_pland_1000_3_1","lsm_c_pland_1000_3_7","lsm_c_pland_1000_3_8","lsm_c_pland_1000_3_11",
                                                           "WAL_500","WAC_500","WLS_500","POP_500","BCH_500","lsm_c_pland_500_3_12","lsm_c_pland_500_3_5","lsm_c_pland_500_3_3","lsm_c_pland_500_3_2","lsm_c_pland_500_3_4","lsm_c_pland_500_3_9","lsm_c_pland_500_3_1","lsm_c_pland_500_3_7","lsm_c_pland_500_3_8","lsm_c_pland_500_3_11",
                                                           "WAL_250","WAC_250","WLS_250","POP_250","BCH_250","lsm_c_pland_250_3_12","lsm_c_pland_250_3_5","lsm_c_pland_250_3_3","lsm_c_pland_250_3_2","lsm_c_pland_250_3_4","lsm_c_pland_250_3_9","lsm_c_pland_250_3_1","lsm_c_pland_250_3_7","lsm_c_pland_250_3_8","lsm_c_pland_250_3_11",
                                                           "WAL_100","WAC_100","WLS_100","POP_100","BCH_100","lsm_c_pland_100_3_12","lsm_c_pland_100_3_5","lsm_c_pland_100_3_3","lsm_c_pland_100_3_2","lsm_c_pland_100_3_4","lsm_c_pland_100_3_9","lsm_c_pland_100_3_1","lsm_c_pland_100_3_7","lsm_c_pland_100_3_8","lsm_c_pland_100_3_11")) %>%
  mutate(model = "spearman univariate") %>%
  dplyr::rename(pval = p) %>%
  mutate(correlation = ifelse(pval >= 0.2 | abs(correlation)<=0.1, NA, correlation)) %>%
  #filter(!is.na(correlation)) %>%
  nest(-c(season,species,country))
  
# univ_glmm_temporal <- do.call(rbind.data.frame, model_univanalysis_results$temporal_corrs_glmm) %>% 
#   group_by(indicator,var,country) %>%
#   group_modify(~mutate(.,min = min(.$correlation, na.rm = T))) %>%
#   group_modify(~mutate(.,max = max(.$correlation, na.rm = T))) %>%
#   mutate(model = "glmm univariate") %>%
#   nest(-c(indicator,species,country,var))

colnames_tempvar <- fun_get_temporal_preds_columns(0,42,c("RFD1F","TMAX1"),7)

univ_spearman_temporal <-  do.call(rbind.data.frame, model_univanalysis_results$temporal_corrs_spearman) %>% 
  filter(name %in% colnames_tempvar) %>%
  mutate(time_lag_1=ifelse(grepl("1",name),time_lag_1/7,time_lag_1),time_lag_2=ifelse(grepl("1",name),time_lag_2/7,time_lag_2),diff_lag=ifelse(grepl("1",name),diff_lag/7,diff_lag)) %>%
  mutate(label = gsub("between.*","",label)) %>%
  mutate(model = "spearman univariate") %>%
  nest(-c(indicator,species,country,var))
  


#### spatial univariate

# plots_univ_glmm_spatial <- univ_glmm_spatial %>%
#   mutate(plots_glmm = pmap(list(data, indicator, species, country), ~fun_plot_tile_univ_spatial(df = ..1, type = "glmm", fillvar = "effect", metric_name = "effect sign", indicator = ..2, model_type = "GLMM", species = ..3, country = ..4))) %>%
#   dplyr::select(-data)

plots_univ_spearman_spatial <- univ_spearman_spatial %>%
  mutate(univ_spatial = pmap(list(data, species, country), ~fun_plot_tile_univ_spatial(df = ..1, type = "spearman", fillvar = "correlation", metric_name = "spearman correlation", model_type = "Spearman correlation", species = ..3, country = ..4))) %>%
  #mutate(univ_spatial = pmap(list(data), ~fun_plot_dotpoint_univ_spatial(..1))) %>%
  dplyr::select(-data)

#purrr::pmap(list(plots_univ_glmm_spatial$plots_glmm, plots_univ_glmm_spatial$species, plots_univ_glmm_spatial$country, plots_univ_glmm_spatial$indicator), ~ggsave(filename = paste(gsub("\\.","",..2),..3,..4,"spatial","univ","glmm",sep="_"), plot = ..1, device = "png", path = "plots", width = 12, height = 8))
# purrr::pmap(list(plots_univ_spearman_spatial$plots_spearman, plots_univ_spearman_spatial$species, plots_univ_spearman_spatial$country, plots_univ_spearman_spatial$indicator), ~ggsave(filename = paste(gsub("\\.","",..2),..3,..4,"spatial","univ","spearman",sep="_"), plot = ..1, device = "png", path = "plots", width = 12, height = 8))

###### temporal univariate

plots_univ_spearman_temporal <- univ_spearman_temporal %>%
  arrange(indicator,species,var) %>%
  mutate(univ_temporal = pmap(list(data, species, country, indicator), ~fun_ccm_plot2(..1, "max", "Spearman\ncorrelation", paste0(..1$label[1]," (",..1$unit,")"), 0, ..4))) %>%
  nest(-c(species,country,indicator)) %>%
  mutate(univ_temporal = map(data, ~wrap_plots(.$univ_temporal, ncol = 1, nrow = 2))) %>%
  #mutate(univ_temporal = pmap(list(univ_temporal,species,country,indicator), ~..1 + plot_annotation(title = paste(..2,..3,..4, sep = " - "), subtitle = paste("CCM using spearman coefficient"), caption = "Only significant results (pval <= 0.05) are displayed (non-signficant results are greyed out)"))) %>%
  dplyr::select(-data)


# plots_univ_glmm_temporal <- univ_glmm_temporal %>%
#   arrange(indicator,species,var) %>%
#   mutate(plot_glmm = pmap(list(data, species, country, indicator), ~fun_ccm_plot2(..1, "max", "estimate", paste0(..1$label[1]," (",..1$unit,")"), 0, ..4))) %>%
#   nest(-c(species,country,indicator)) %>%
#   mutate(plot_glmm = map(data, ~wrap_plots(.$plot_glmm))) %>% 
#   mutate(plot_glmm = pmap(list(plot_glmm,species,country,indicator), ~..1 + plot_annotation(title = paste(..2,..3,..4, sep = " - "), subtitle = paste("CCM using glmm"), caption = "Only significant results (pval <= 0.05) are displayed (non-signficant results are greyed out)"))) %>%
#   dplyr::select(-data)

#purrr::pmap(list(plots_univ_spearman_temporal$plot_spearman, plots_univ_spearman_temporal$species, plots_univ_spearman_temporal$country, plots_univ_spearman_temporal$indicator), ~ggsave(filename = paste(gsub("\\.","",..2),..3,..4,"ccm","univ","spearman",sep="_"), plot = ..1, device = "png", path = "plots"))
#purrr::pmap(list(plots_univ_glmm_temporal$plot_glmm, plots_univ_glmm_temporal$species, plots_univ_glmm_temporal$country, plots_univ_glmm_temporal$indicator), ~ggsave(filename = paste(gsub("\\.","",..2),..3,..4,"ccm","univ","glmm",sep="_"), plot = ..1, device = "png", path = "plots"))




# plots_maps <- readRDS("/home/ptaconet/Bureau/data_analysis/model_results_univanalysis.rds") %>%
#   mutate(map = pmap(list(mod,code_pays,response_var), ~fun_map(..1,..2,..3))) %>%
#   mutate(response_var = case_when(
#     response_var == "ma_funestus_ss" ~ "An. funestus ss.",
#     response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
#     response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
#   dplyr::select(response_var, code_pays, mod, map) %>%
#   dplyr::rename(species = response_var, country = code_pays, indicator = mod )
#   


######################
###### multivariate
######################
  

fun_get_metrics <- function(df,mod){
  
  if(mod == "abundance"){
    met = Metrics::rmse(df$obs,df$pred)
    #met = cor(df$obs,df$pred, method = "spearman")
  } else if (mod == "abundance_discrete"){
    met = Metrics::accuracy(df$obs,df$pred)
  } else {
    met = Metrics::auc(df$obs,df$pred)
  }
  
  return(met)
  
}


multivariate_results9 <- readRDS("/home/ptaconet/Bureau/data_analysis/model_results_newnewanalysis_9.rds") %>%
  filter(mod != "abundance_discrete") %>%
  filter(!( response_var == "ma_gambiae_ss" & mod=="presence")) %>%
  filter(!( response_var == "ma_coluzzi" & mod=="presence")) %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus ss.",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(glmm_model_tabversion = map2(glmm_model, mod, ~broom.mixed::tidy(.x$mod@model, conf.int = TRUE, exponentiate = ifelse(.y == "abundance", FALSE, TRUE)))) %>%
  mutate(glmm_model_tabversion = map(glmm_model_tabversion, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(glmm_df_cv_lto = map(glmm_model, ~.$df_cv_lto)) %>%
  mutate(rf_df_cv_lto = map(rf_model_lto, ~.$df_cv)) %>%
  mutate(quality_lto_glmm = map2_dbl(glmm_df_cv_lto,mod, ~fun_get_metrics(.x,.y))) %>%
  mutate(quality_lto_rf = map2_dbl(rf_df_cv_lto,mod, ~fun_get_metrics(.x,.y))) %>%
  mutate(multiv_glmm = pmap(list(glmm_model_tabversion,mod,quality_lto_glmm), ~fun_glmm_dotpoint(..1,..2,..3))) %>%
  mutate(rf_plots = pmap(list(rf_model_lto,mod,response_var), ~fun_plot_pdp(..1,..2,..3))) %>%
  dplyr::select(response_var, code_pays, mod, multiv_glmm, rf_plots) %>%
  dplyr::rename(species = response_var, country = code_pays, indicator = mod) #%>%
  #mutate(final_plots = pmap(list(multiv_glmm, rf_plots, indicator, species), ~wrap_plots(list(..1,..2$plot_imp,..2$pdps)) + plot_annotation(title = paste(..3,..4), subtitle = paste0("RF = ",..2$quality))))

multivariate_results_abundance_discrete9 <- readRDS("/home/ptaconet/Bureau/data_analysis/model_results_newnewanalysis_9.rds") %>%
  filter(mod == "abundance_discrete") %>%
  filter(!(response_var == "ma_coluzzi")) %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus ss.",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(rf_df_cv_lto = map(rf_model_lto, ~.$df_cv)) %>%
  mutate(rf_plots = pmap(list(rf_model_lto,mod,response_var), ~fun_plot_pdp(..1,..2,..3))) #%>%
# mutate(final_plots = pmap(list(rf_plots, mod, response_var), ~wrap_plots(list(..1$plot_imp,..1$pdps), nrow = 2, ncol = 1) + plot_annotation(title = paste(..2,..3), subtitle = paste0("RF = ",..1$quality))))




multivariate_results12 <- readRDS("/home/ptaconet/Bureau/data_analysis/model_results_newnewanalysis_12.rds") %>%
  filter(mod != "abundance_discrete", response_var == "ma_coluzzi",mod=="presence") %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus ss.",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(glmm_model_tabversion = map2(glmm_model, mod, ~broom.mixed::tidy(.x$mod@model, conf.int = TRUE, exponentiate = ifelse(.y == "abundance", FALSE, TRUE)))) %>%
  mutate(glmm_model_tabversion = map(glmm_model_tabversion, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(glmm_df_cv_lto = map(glmm_model, ~.$df_cv_lto)) %>%
  mutate(rf_df_cv_lto = map(rf_model_lto, ~.$df_cv)) %>%
  mutate(quality_lto_glmm = map2_dbl(glmm_df_cv_lto,mod, ~fun_get_metrics(.x,.y))) %>%
  mutate(quality_lto_rf = map2_dbl(rf_df_cv_lto,mod, ~fun_get_metrics(.x,.y))) %>%
  mutate(multiv_glmm = pmap(list(glmm_model_tabversion,mod,quality_lto_glmm), ~fun_glmm_dotpoint(..1,..2,..3))) %>%
  mutate(rf_plots = pmap(list(rf_model_lto,mod,response_var), ~fun_plot_pdp(..1,..2,..3))) %>%
  dplyr::select(response_var, code_pays, mod, multiv_glmm, rf_plots) %>%
  dplyr::rename(species = response_var, country = code_pays, indicator = mod) #%>%
#mutate(final_plots = pmap(list(multiv_glmm, rf_plots, indicator, species), ~wrap_plots(list(..1,..2$plot_imp,..2$pdps)) + plot_annotation(title = paste(..3,..4), subtitle = paste0("RF = ",..2$quality))))



multivariate_results13 <- readRDS("/home/ptaconet/Bureau/data_analysis/model_results_newnewanalysis_13.rds") %>%
  filter(mod != "abundance_discrete", response_var == "ma_gambiae_ss",mod=="presence") %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus ss.",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(glmm_model_tabversion = map2(glmm_model, mod, ~broom.mixed::tidy(.x$mod@model, conf.int = TRUE, exponentiate = ifelse(.y == "abundance", FALSE, TRUE)))) %>%
  mutate(glmm_model_tabversion = map(glmm_model_tabversion, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(glmm_df_cv_lto = map(glmm_model, ~.$df_cv_lto)) %>%
  mutate(rf_df_cv_lto = map(rf_model_lto, ~.$df_cv)) %>%
  mutate(quality_lto_glmm = map2_dbl(glmm_df_cv_lto,mod, ~fun_get_metrics(.x,.y))) %>%
  mutate(quality_lto_rf = map2_dbl(rf_df_cv_lto,mod, ~fun_get_metrics(.x,.y))) %>%
  mutate(multiv_glmm = pmap(list(glmm_model_tabversion,mod,quality_lto_glmm), ~fun_glmm_dotpoint(..1,..2,..3))) %>%
  mutate(rf_plots = pmap(list(rf_model_lto,mod,response_var), ~fun_plot_pdp(..1,..2,..3))) %>%
  dplyr::select(response_var, code_pays, mod, multiv_glmm, rf_plots) %>%
  dplyr::rename(species = response_var, country = code_pays, indicator = mod) #%>%
#mutate(final_plots = pmap(list(multiv_glmm, rf_plots, indicator, species), ~wrap_plots(list(..1,..2$plot_imp,..2$pdps)) + plot_annotation(title = paste(..3,..4), subtitle = paste0("RF = ",..2$quality))))

multivariate_results_abundance_discrete13 <- readRDS("/home/ptaconet/Bureau/data_analysis/model_results_newnewanalysis_13.rds") %>%
  filter(mod == "abundance_discrete", response_var == "ma_coluzzi") %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus ss.",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(rf_df_cv_lto = map(rf_model_lto, ~.$df_cv)) %>%
  mutate(rf_plots = pmap(list(rf_model_lto,mod,response_var), ~fun_plot_pdp(..1,..2,..3))) #%>%
# mutate(final_plots = pmap(list(rf_plots, mod, response_var), ~wrap_plots(list(..1$plot_imp,..1$pdps), nrow = 2, ncol = 1) + plot_annotation(title = paste(..2,..3), subtitle = paste0("RF = ",..1$quality))))




multivariate_results <- bind_rows(multivariate_results9,multivariate_results12,multivariate_results13)
multivariate_results_abundance_discrete <-  bind_rows(multivariate_results_abundance_discrete9,multivariate_results_abundance_discrete13) %>%
  dplyr::select(response_var, code_pays, mod,rf_plots) %>%
  dplyr::rename(species = response_var, country = code_pays, indicator = mod)

multivariate_results$rf_plots[[1]] <- multivariate_results_abundance_discrete$rf_plots[[1]]
multivariate_results$rf_plots[[2]] <- multivariate_results_abundance_discrete$rf_plots[[2]]
multivariate_results$rf_plots[[3]] <- multivariate_results_abundance_discrete$rf_plots[[3]]


multivariate_results <- multivariate_results %>%
  left_join(plots_univ_spearman_temporal) %>%
  left_join(plots_univ_spearman_spatial)
  

wrap_plots(list(multivariate_results$multiv_glmm[[6]], multivariate_results$rf_plots[[6]]$pdps, multivariate_results$multiv_glmm[[2]], multivariate_results$rf_plots[[2]]$pdps), nrow = 2, ncol = 2)

  
purrr::pmap(list(multivariate_results9$species, multivariate_results9$indicator, multivariate_results9$final_plots), ~ggsave(filename = paste(gsub("\\.","",..1,),..2,sep="_"), plot = ..3, device = "png", path = "plots", width = 29, height = 21, units = "cm"))
purrr::pmap(list(multivariate_results_abundance_discrete9$response_var, multivariate_results_abundance_discrete9$mod, multivariate_results_abundance_discrete9$final_plots), ~ggsave(filename = paste(..1,..2,"abundancediscrete",sep="_"), plot = ..3, device = "png", path = "plots", width = 29, height = 21, units = "cm"))

######## plot results
# res <- plots_maps %>%
#   left_join(multivariate_results) %>%
#   left_join(plots_univ_spearman_spatial) %>%
#   left_join(plots_univ_spearman_temporal) %>%
#   mutate(plot_final = pmap(list(map,univ_temporal,univ_spatial,multiv_glmm,species, indicator, country), ~wrap_plots(..1,..2,..3,..4) + plot_annotation(title = paste(..5,..6,..7, sep = " - "), subtitle = paste("CCM using glmm"), caption = "Only significant results (pval <= 0.05) are displayed (non-signficant results are greyed out)")))


 
res <- multivariate_results %>%
   left_join(plots_univ_spearman_spatial) %>%
   left_join(plots_univ_spearman_temporal) #%>%
   #mutate(plot_final = pmap(list(map,univ_temporal,univ_spatial,multiv_glmm,species, indicator, country), ~wrap_plots(..1,..2,..3,..4) + plot_annotation(title = paste(..5,..6,..7, sep = " - "), subtitle = paste("CCM using glmm"), caption = "Only significant results (pval <= 0.05) are displayed (non-signficant results are greyed out)")))

# funestus
wrap_plots(res$univ_temporal[[2]], res$univ_spatial[[2]], res$multiv_glmm[[2]]) / wrap_plots(res$univ_temporal[[1]], res$univ_spatial[[1]], res$multiv_glmm[[1]]) + plot_annotation(title = 'Models for Anopheles funestus ss.')

# gambiae
wrap_plots(res$univ_temporal[[6]], res$univ_spatial[[6]], res$multiv_glmm[[6]]) / wrap_plots(res$univ_temporal[[4]], res$univ_spatial[[4]], res$multiv_glmm[[4]]) + plot_annotation(title = 'Models for Anopheles gambiae ss.')

# coluzzi
wrap_plots(res$univ_temporal[[3]], res$univ_spatial[[3]], res$multiv_glmm[[3]]) / wrap_plots(res$univ_temporal[[5]], res$univ_spatial[[5]], res$multiv_glmm[[5]]) + plot_annotation(title = 'Models for Anopheles coluzzii')

  

############################################
############### predictions######################
############################################

fun_get_metrics <- function(df,mod){
  
  if(mod == "abundance"){
    #met = Metrics::rmse(df$obs,df$pred)
    met = cor(df$obs,df$pred, method = "spearman")
  } else if (mod == "abundance_discrete"){
    met = Metrics::accuracy(df$obs,df$pred)
  } else {
    met = Metrics::auc(df$obs,df$pred)
  }
  
  return(met)
  
}

predictions_explicative_model <- readRDS("/home/ptaconet/Bureau/data_analysis/model_results_newnewanalysis_3.rds") %>%
  filter(purpose == "explicative") %>%
  mutate(glmm_df_cv_llo = map(glmm_model, ~.$df_cv_llo)) %>%
  mutate(glmm_df_cv_lto = map(glmm_model, ~.$df_cv_lto)) %>%
  mutate(glmm_df_cv_llto = map(glmm_model, ~.$df_cv_llto)) %>%
  mutate(glmm_df_cv_llo = pmap(list(glmm_df_cv_llo, response_var, code_pays, mod, purpose), ~mutate(..1, species = ..2, country = ..3, indicator = ..4))) %>%
  mutate(glmm_df_cv_lto = pmap(list(glmm_df_cv_lto, response_var, code_pays, mod, purpose), ~mutate(..1, species = ..2, country = ..3, indicator = ..4))) %>%
  mutate(glmm_df_cv_llto = pmap(list(glmm_df_cv_llto, response_var, code_pays, mod, purpose), ~mutate(..1, species = ..2, country = ..3, indicator = ..4))) %>%
  mutate(quality_llo_glmm = map2_dbl(glmm_df_cv_llo,mod, ~fun_get_metrics(.x,.y))) %>%
  mutate(quality_lto_glmm = map2_dbl(glmm_df_cv_lto,mod, ~fun_get_metrics(.x,.y)))
  
  
glmm_df_cv_llo <- do.call(rbind.data.frame, predictions_explicative_model$glmm_df_cv_llo) %>% mutate(model = "preds. leave-village-out") %>% dplyr::rename(pointdecapture = pointdecapture2)
glmm_df_cv_lto <- do.call(rbind.data.frame, predictions_explicative_model$glmm_df_cv_lto) %>% mutate(model = "preds. leave-mission-out") %>% dplyr::rename(pointdecapture = pointdecapture2)
glmm_df_cv_llto <- do.call(rbind.data.frame, predictions_explicative_model$glmm_df_cv_llto) %>% mutate(model = "preds. leave-village-and-mission-out") %>% dplyr::rename(pointdecapture = pointdecapture2)


obs = glmm_df_cv_llo %>%
  dplyr::select(-pred) %>%
  dplyr::rename(pred = obs) %>%
  mutate(model = "observed values")


df_cv <- rbind(glmm_df_cv_llo,glmm_df_cv_lto) %>%
  dplyr::select(-obs) %>%
  bind_rows(obs) %>%
  group_by(codevillage,nummission,species,country,indicator,model) %>%
  summarise(pred = sum(pred)) %>%
  mutate(model = fct_relevel(model,c("observed values","preds. leave-village-out","preds. leave-mission-out","preds. leave-village-and-mission-out")))

#### abundance
abundance_coluzzi <- ggplot(df_cv %>% filter(indicator == "abundance", species == "ma_coluzzi"), aes(x = nummission, y = pred, color = model, group = model, size=model)) + 
  facet_wrap(.~codevillage) + 
  geom_line() + 
  scale_colour_manual(breaks=c("observed values","preds. leave-village-out","preds. leave-mission-out","preds. leave-village-and-mission-out"), values=c("red","orange","blue","pink")) +
  scale_size_manual(breaks=c("observed values","preds. leave-village-out","preds. leave-mission-out","preds. leave-village-and-mission-out"), values=c(1,0.5,0.5,0.5)) + 
  scale_y_continuous(trans = "log10") + 
  ggtitle("An. coluzzi") + theme_bw()


abundance_funestus <- ggplot(df_cv %>% filter(indicator == "abundance", species == "ma_funestus_ss"), aes(x = nummission, y = pred, color = model, group = model, size=model)) + 
  facet_wrap(.~codevillage) + 
  geom_line() + 
  scale_colour_manual(breaks=c("observed values","preds. leave-village-out","preds. leave-mission-out","preds. leave-village-and-mission-out"), values=c("red","orange","blue","pink")) +
  scale_size_manual(breaks=c("observed values","preds. leave-village-out","preds. leave-mission-out","preds. leave-village-and-mission-out"), values=c(1,0.5,0.5,0.5)) + 
  scale_y_continuous(trans = "log10") + 
  ggtitle("An. funestus ss.") + theme_bw()
  

abundance_gambiae <- ggplot(df_cv %>% filter(indicator == "abundance", species == "ma_gambiae_ss"), aes(x = nummission, y = pred, color = model, group = model, size=model)) + 
  facet_wrap(.~codevillage) + 
  geom_line() + 
  scale_colour_manual(breaks=c("observed values","preds. leave-village-out","preds. leave-mission-out","preds. leave-village-and-mission-out"), values=c("red","orange","blue","pink")) +
  scale_size_manual(breaks=c("observed values","preds. leave-village-out","preds. leave-mission-out","preds. leave-village-and-mission-out"), values=c(1,0.5,0.5,0.5)) + 
  scale_y_continuous(trans = "log10") + 
  ggtitle("An. gambiae ss.") + theme_bw()

abundance_funestus + abundance_gambiae + abundance_coluzzi + plot_layout(guides = 'collect') + plot_annotation(title="Preds. vs. observed values for the 'abundance' models")

##### presence
library(precrec)

df_cv <- rbind(glmm_df_cv_llo,glmm_df_cv_lto) %>%
  dplyr::select(-obs) %>%
  bind_rows(obs) %>%
  filter(indicator == "presence") %>%
  pivot_wider(names_from = model,values_from = pred)

df_cv2 <- df_cv %>% filter(indicator == "presence", species == "ma_coluzzi")
msmdat1 <- mmdata(scores = join_scores(df_cv2$`preds. leave-village-out`,df_cv2$`preds. leave-mission-out`),labels = df_cv2$`observed values`, modnames = c("preds. leave-village-out","preds. leave-mission-out"))
#msmdat1 <- mmdata(scores = join_scores(df_cv2$`preds. leave-mission-out`),labels = df_cv2$`observed values`, modnames = c("preds. leave-mission-out"))
precrec_coluzzi <- evalmod(msmdat1)

df_cv2 <- df_cv %>% filter(indicator == "presence", species == "ma_gambiae_ss")
msmdat1 <- mmdata(scores = join_scores(df_cv2$`preds. leave-village-out`,df_cv2$`preds. leave-mission-out`),labels = df_cv2$`observed values`, modnames = c("preds. leave-village-out","preds. leave-mission-out"))
#msmdat1 <- mmdata(scores = join_scores(df_cv2$`preds. leave-mission-out`),labels = df_cv2$`observed values`, modnames = c("preds. leave-mission-out"))
precrec_gambiae <- evalmod(msmdat1)

df_cv2 <- df_cv %>% filter(indicator == "presence", species == "ma_funestus_ss")
msmdat1 <- mmdata(scores = join_scores(df_cv2$`preds. leave-village-out`,df_cv2$`preds. leave-mission-out`),labels = df_cv2$`observed values`, modnames = c("preds. leave-village-out","preds. leave-mission-out"))
precrec_funestus <- evalmod(msmdat1)

p_coluzzi <- autoplot(precrec_coluzzi, curvetype = c("ROC")) + ggtitle("An. coluzzii")
p_gambiae <- autoplot(precrec_gambiae, curvetype = c("ROC")) + ggtitle("An. gambiae ss.")
p_funestus <- autoplot(precrec_funestus, curvetype = c("ROC")) + ggtitle("An. funestus ss.")

p_funestus + p_gambiae + p_coluzzi + plot_layout(guides = 'collect') + plot_annotation(title="ROC curves for the 'presence' models")






###### pure predictive models

predictions <- readRDS("/home/ptaconet/Bureau/data_analysis/model_results_newnewanalysis_3.rds") %>%
  filter(mod %in% c("presence","abundance"),purpose == "predictive") %>%
  mutate(glmm_df_cv_llo = map(glmm_model, ~.$df_cv_llo)) %>%
  mutate(glmm_df_cv_lto = map(glmm_model, ~.$df_cv_lto)) %>%
  mutate(glmm_df_cv_llto = map(glmm_model, ~.$df_cv_llto)) %>%
  mutate(rf_df_cv_llo = map(rf_model_llo, ~.$df_cv)) %>%
  mutate(rf_df_cv_lto = map(rf_model_lto, ~.$df_cv)) %>%
  mutate(rf_df_cv_llto = map(rf_model_llto, ~.$df_mod)) %>%
  mutate(glmm_df_cv_llo = pmap(list(glmm_df_cv_llo, response_var, code_pays, mod, purpose), ~mutate(..1, species = ..2, country = ..3, indicator = ..4))) %>%
  mutate(glmm_df_cv_lto = pmap(list(glmm_df_cv_lto, response_var, code_pays, mod, purpose), ~mutate(..1, species = ..2, country = ..3, indicator = ..4))) %>%
  mutate(rf_df_cv_llo = pmap(list(rf_df_cv_llo, response_var, code_pays, mod, purpose), ~mutate(..1, species = ..2, country = ..3, indicator = ..4))) %>%
  mutate(rf_df_cv_lto = pmap(list(rf_df_cv_lto, response_var, code_pays, mod, purpose), ~mutate(..1, species = ..2, country = ..3, indicator = ..4))) %>%
  mutate(quality_llo_glmm = map2_dbl(glmm_df_cv_llo,mod, ~fun_get_metrics(.x,.y))) %>%
  mutate(quality_lto_glmm = map2_dbl(glmm_df_cv_lto,mod, ~fun_get_metrics(.x,.y))) %>%
  mutate(quality_llo_rf = map2_dbl(rf_df_cv_llo,mod, ~fun_get_metrics(.x,.y))) %>%
  mutate(quality_lto_rf = map2_dbl(rf_df_cv_lto,mod, ~fun_get_metrics(.x,.y)))


glmm_df_cv_llo <- do.call(rbind.data.frame, predictions$glmm_df_cv_llo) %>% mutate(model = "GLMM - leave-village-out") %>% dplyr::rename(pointdecapture = pointdecapture2)
glmm_df_cv_lto <- do.call(rbind.data.frame, predictions$glmm_df_cv_lto) %>% mutate(model = "GLMM - leave-mission-out") %>% dplyr::rename(pointdecapture = pointdecapture2)
rf_df_cv_llo <- do.call(rbind.data.frame, predictions$rf_df_cv_llo) %>% mutate(model = "RF - leave-village-out")
rf_df_cv_lto <- do.call(rbind.data.frame, predictions$rf_df_cv_lto) %>% mutate(model = "RF - leave-mission-out")


obs = glmm_df_cv_llo %>%
  dplyr::select(-pred) %>%
  dplyr::rename(pred = obs) %>%
  mutate( model = "observed values")

df_cv <- rbind(glmm_df_cv_llo,glmm_df_cv_lto,rf_df_cv_llo,rf_df_cv_lto) %>%
  dplyr::select(-obs) %>%
  bind_rows(obs) %>%
  group_by(codevillage,nummission,species,country,indicator,model) %>%
  summarise(pred = sum(pred)) %>%
  mutate(model = fct_relevel(model,c("observed values","GLMM - leave-village-out","GLMM - leave-mission-out","RF - leave-village-out","RF - leave-mission-out")))


abundance_coluzzi_llo <- ggplot(df_cv %>% filter(indicator == "abundance", species == "ma_coluzzi", model %in% c("observed values","GLMM - leave-village-out","RF - leave-village-out")), aes(x = nummission, y = pred, color = model, group = model, size=model)) + 
  facet_wrap(.~codevillage) + 
  geom_line() + 
  scale_colour_manual(breaks=c("observed values","GLMM - leave-village-out","RF - leave-village-out"), values=c("red","orange","blue")) +
  scale_size_manual(breaks=c("observed values","GLMM - leave-village-out","RF - leave-village-out"), values=c(1,0.5,0.5)) + 
  scale_y_continuous(trans = "log10") + 
  ggtitle("An. coluzzi") + theme_bw()

abundance_coluzzi_lto <- ggplot(df_cv %>% filter(indicator == "abundance", species == "ma_coluzzi", model %in% c("observed values","GLMM - leave-mission-out","RF - leave-mission-out")), aes(x = nummission, y = pred, color = model, group = model, size=model)) + 
  facet_wrap(.~codevillage) + 
  geom_line() + 
  scale_colour_manual(breaks=c("observed values","GLMM - leave-mission-out","RF - leave-mission-out"), values=c("red","orange","blue")) +
  scale_size_manual(breaks=c("observed values","GLMM - leave-mission-out","RF - leave-mission-out"), values=c(1,0.5,0.5)) + 
  scale_y_continuous(trans = "log10") + 
  ggtitle("An. coluzzi") + theme_bw()





##### quality indices


quality_df <- predictions %>%
  dplyr::select(response_var,  code_pays, mod,     purpose,quality_llo_glmm,quality_lto_glmm,quality_llo_rf,quality_lto_rf)



predictions_explicative_model <- predictions_explicative_model %>%
  dplyr::select(response_var, code_pays, mod, purpose, quality_llo_glmm,quality_lto_glmm) %>%
  mutate(quality_llo_rf = NA, quality_lto_rf = NA)


quals <- rbind(quality_df,predictions_explicative_model) %>%
  pivot_longer(c(quality_llo_glmm ,quality_lto_glmm ,quality_llo_rf, quality_lto_rf)) %>%
  mutate(cv_type = word(name,2,sep="_")) %>%
  mutate(cv_type = ifelse(cv_type == "llo","leave-location-out","leave-time-out")) %>%
  mutate(model_type = word(name,3,sep="_")) %>%
  mutate(name = paste0(name,"_",purpose)) %>%
  mutate(name = gsub("_lto|_llo|_llto","",name)) %>%
  mutate(name = gsub("quality_","",name)) %>%
  filter(!is.na(value)) %>%
  mutate(name = as.factor(fct_relevel(name,c("rf_predictive","glmm_predictive","glmm_explicative")))) %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus ss.",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(response_var = as.factor(fct_relevel(response_var,c("An. funestus ss.","An. gambiae ss.","An. coluzzii"))))
  

plot_presence =  ggplot(quals %>% filter(mod=="presence"), aes(value, name)) +
  geom_point(size=1.5) + 
  geom_text(aes(label=paste0(round(value, 2))), size = 3, vjust = -1) +
  xlim(0.5,1) +
  theme_bw() +
  geom_vline(xintercept=0.5, linetype="dashed", color = "red") + 
  #guides(shape=FALSE) +
  facet_grid(response_var~cv_type, scales = "free", space = "free") + 
  labs(y = "model", x = "AUC")


plot_abundance =  ggplot(quals %>% filter(mod=="abundance"), aes(value, name)) +
  geom_point(size=1.5) + 
  geom_text(aes(label=paste0(round(value, 2))), size = 3, vjust = -1) +
 xlim(-0.2,1) +
  theme_bw() +
  facet_grid(response_var~cv_type, scales = "free", space = "free") + 
  labs(y = "model", x = "Spearman coeff. between obs and preds")








ggplot(df_cv_abundance %>% filter(species == "ma_funestus_ss"), aes(x = nummission, y = pred, group = model, color = model)) + geom_line() + facet_wrap(.~codevillage) + scale_y_continuous(trans = "log10")
ggplot(df_cv_abundance %>% filter(species == "ma_gambiae_ss"), aes(x = nummission, y = pred, group = model, color = model)) + geom_line() + facet_wrap(.~codevillage) + scale_y_continuous(trans = "log10")
ggplot(df_cv_abundance %>% filter(species == "ma_gambiae_ss", model %in% c("Observed values","GLMM - leave-village-out","RF - leave-village-out")), aes(x = nummission, y = pred, group = model, color = model)) + geom_line() + facet_wrap(.~codevillage) + scale_y_continuous(trans = "log10")



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



univ_glmm <- do.call(rbind.data.frame, model_results$univ_glmm) %>% mutate(model = "glmm univariate")
univ_spearmancorr <- do.call(rbind.data.frame, model_results$univ_spearmancorr) %>% mutate(model = "spearman univariate")
glmm_pvalfilt_dayscience <-  do.call(rbind.data.frame, model_results$glmm_pvalfilt_dayscience_tabversion) %>% mutate(model = "glmm multivariate \n 'day science' \n univ. filter = pval")
glmm_pvalfilt_nightscience <-  do.call(rbind.data.frame, model_results$glmm_pvalfilt_dayscience_tabversion) %>% mutate(model = "univariate filter = \n pval <= 0.2")
glmm_spearmanfilt_dayscience <-  do.call(rbind.data.frame, model_results$glmm_spearmanfilt_dayscience_tabversion) %>% mutate(model = "glmm multivariate \n 'day science' \n univ. filter = spearman coeff.")
glmm_spearmanfilt_nightscience <-  do.call(rbind.data.frame, model_results$glmm_spearmanfilt_dayscience_tabversion) %>% mutate(model = "univariate filter = \n spearman coeff. >= 0.2")
rf_dayscience <-  do.call(rbind.data.frame, model_results$rf_dayscience_varimp) %>% mutate(model = "'day science'")
rf_nightscience <-  do.call(rbind.data.frame, model_results$rf_nightscience_varimp) %>% mutate(model = "random forest")


glmm <- rbind(glmm_pvalfilt_nightscience,glmm_spearmanfilt_nightscience) %>%  #rbind(univ_glmm,glmm_pvalfilt_dayscience,glmm_pvalfilt_nightscience,glmm_spearmanfilt_dayscience,glmm_spearmanfilt_nightscience) %>%
  mutate(label = ifelse(grepl("Vector control tools|exterior",label), paste0(label, " (comp. to ",unit,")"),paste0(label, " (by add. ",unit,")"))) %>%
  mutate(effect = case_when(estimate < 1 & indicator != "abundance" ~ "negative",
                            estimate >= 1 & indicator != "abundance" ~ "positive",
                            estimate >= 0 & indicator == "abundance" ~ "positive",
                            estimate < 0  & indicator == "abundance" ~ "negative")) %>%
  mutate(estimate = ifelse(p.value >= 0.05 & !grepl("Vector control measure",label) & grepl("multivariate",model), NA, estimate)) %>%
  mutate(estimate = ifelse(p.value >= 0.2 & !grepl("Vector control measure",label) & grepl("univariate",model), NA, estimate)) %>%
  mutate(effect = ifelse(p.value >= 0.2 & grepl("univariate",model), NA, effect)) %>%
  mutate(label = ifelse(term == "(Intercept)","(Model Intercept)",label)) %>%
  filter(term!='sd__(Intercept)') %>%
  filter(!is.na(estimate)) %>%
  nest(-c(indicator,species,country))

plots_glmm <- glmm %>%
  mutate(plots_glmm = pmap(list(data, indicator, species, country), ~fun_plot_tile(df = ..1, type = "glmm", fillvar = "effect", metric_name = "effect sign", indicator = ..2, model_type = "GLMM", species = ..3, country = ..4))) %>%
  dplyr::select(-data)


rf <- rf_nightscience %>% #rbind(rf_dayscience,rf_nightscience) %>%
  mutate(importance = ifelse(importance < 0, 0, importance)) %>%
  nest(-c(indicator,species,country))

plots_rf <- rf %>%
  mutate(plots_rf = pmap(list(data, indicator, species, country), ~fun_plot_tile(df = ..1, type = "rf", fillvar = "importance", metric_name = "Feature importance", indicator = ..2, model_type = "Random forest", species = ..3, country = ..4))) %>%
  dplyr::select(-data)
  


plots <- plots_glmm %>%
  left_join(plots_rf)
  
purrr::pmap(list(plots$plots_glmm, plots$species, plots$country, plots$indicator), ~ggsave(filename = paste(gsub("\\.","",..2),..3,..4,"glmm",sep="_"), plot = ..1, device = "png", path = "plots"))
purrr::pmap(list(plots$plots_rf, plots$species, plots$country, plots$indicator), ~ggsave(filename = paste(gsub("\\.","",..2),..3,..4,"rf",sep="_"), plot = ..1, device = "png", path = "plots"))


# save plots

# pdps
plots_interpret_rf <- model_results %>%
  filter(mod %in% c("presence","abundance")) %>%
  mutate(plots_rf_nightscience = furrr::future_pmap(list(multiv_rf_nightscience, mod, response_var, rf_nightscience_perf) , ~fun_plot_pdp(..1$mod$fit, ..1$df_mod, ..2, ..3, ..4))) %>%
  dplyr::select(response_var,code_pays,mod,plots_rf_nightscience)
#mutate(plots_rf_nightscience = furrr::future_pmap(list(multiv_rf_nightscience, mod, response_var, rf_nightscience_perf) , ~fun_plot_pdp(..1$mod$fit,..1$df_mod,..2,..3, ..4))) %>%
#dplyr::select(response_var,code_pays,mod,plots_rf_dayscience,plots_rf_nightscience)





