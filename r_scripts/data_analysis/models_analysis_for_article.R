library(tidyverse)
library(patchwork)
library(DBI)
library(sjPlot) # see : https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html
library(stringr)
library(sf)
library(ggmap)
library(precrec)

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
      dplyr::mutate(unit = "% land.")
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
      #mutate(short_name = paste0(short_name, " b/w ",day_start," and ",day_end," weeks")) %>%
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

fun_plot_tile_univ_spatial <- function(df, metric_name = "Spearman correlation", country){
  
  df <- df %>%
    mutate(buffer = forcats::fct_relevel(buffer, c("250","500","1000","2000"))) %>%
    mutate(indicator = forcats::fct_relevel(indicator, c("Presence","Abundance"))) %>%
    mutate(species = forcats::fct_relevel(species, c("An. funestus ss.","An. gambiae ss.","An. coluzzii")))
  
  df$label <- factor(df$label, levels = unique(df$label[order(df$correlation)]))
  df <- df %>% filter(!is.na(correlation),pval<=0.2) %>%
     mutate(p.value2 = case_when(
       pval <= 0.01 ~ "***",
       pval >= 0.01 & pval <= 0.03 ~  " **",
       pval > 0.03 & pval <= 0.05 ~ " *",
       pval > 0.05 ~ ""
     ))
    

  p <- ggplot(df, aes(buffer, label)) + 
    geom_tile(aes(fill = correlation), color = "white") + facet_grid(species~indicator, scales="free_y", space="free_y") +
    xlab("buffer radius around the collection site (meters)") + 
    ylab("") +
    theme_bw() +
    theme(legend.position = "bottom") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-0.6,0.6), space = "Lab", name = metric_name, na.value="grey") + 
    geom_text(aes(label = ifelse(is.na(correlation), "",paste(round(correlation,2), p.value2))), size = 3)

  
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
  labs(x = NULL, y = "Spearman correlation")
 
 return(plot)
}


# function to plot the CCM (simple plot : only the CCM)
fun_ccm_plot2 <- function(correlation_df, var){
  
    abs_corr <- correlation_df %>% filter(abs_corr == max(abs_corr, na.rm = T))

    abs_corr2 <- correlation_df %>% arrange(desc(abs_corr)) %>% head(round(0.03*nrow(.))) # 3 % top correlations will be blacked borders

  
  ccm_plot <- ggplot(data = correlation_df, aes(time_lag_1, time_lag_2, fill = correlation)) +
    geom_tile(color = "white", show.legend = TRUE, size = 0.05) +
    geom_tile(data = abs_corr2 , color = "black", size = 0.3, show.legend = FALSE) +
    geom_tile(data = abs_corr , color = "deeppink3", size = 0.6, show.legend = FALSE) +
    theme_minimal() + 
    theme(plot.title = element_text(size = 10, hjust = 0.5),
          axis.title = element_text(size = 8),
          legend.key.size = unit(0.8, "cm"),
          legend.title=element_text(size=10),
          legend.position = "none"
    ) +
    ggtitle(var) +
    annotate("text", size = 3,x = min(correlation_df$time_lag_1), y = max(correlation_df$time_lag_2), vjust = "inward", hjust = "inward", label = paste0("r(",abs_corr$time_lag_2,",",abs_corr$time_lag_1,") = ",round(abs_corr$correlation,3))) +
    coord_fixed() +
    ylab("time lag 1") +
    xlab("time lag 2") + 
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-.6,.6), space = "Lab", name = "Spearman correlation", na.value = "grey") 

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
    metric <- "Rsquared"
    quality = max(modell$mod$results$Rsquared)
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
  plot_imp <- ggplot(imp, aes(x = importance , y = label, label = label)) +
    geom_bar(stat="identity") + 
    theme_bw() + 
      geom_label(size=2, aes(fontface=2), label.padding = unit(0.15, "lines"), x = 0.05) +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_text(size = 8),
          plot.subtitle = element_text(size = 9, face="bold")
          ) +
    ylab("") + 
    xlab("") +
    labs(subtitle = "Variable importance")
    #ggtitle("Variable importance plot")
  
  # interactions for the most important variable
  # plan("callr", workers = 7)
  # future::plan(multisession, workers = 7)
   #interact = Interaction$new(mod, grid.size = 30)
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
  # interaction_most_interact_var = Interaction$new(mod, feature = interact$results$feat[which.max(interact$results$.interaction)]) 
  # interaction_most_interact_var$results = interaction_most_interact_var$results %>% dplyr::arrange(.interaction)
  
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
  

  # imp <- imp %>%
  #   mutate(label_group = forcats::fct_relevel(label_group,c("Past climate",
  #                                                         "Micro-climate",
  #                                                         "Land cover",
  #                                                         "Land cover and related",
  #                                                         "Water pres.",
  #                                                         "Topography",
  #                                                         "Human hosts availability",
  #                                                         "Other",
  #                                                         "Place",
  #                                                         "Hum. behav.",
  #                                                         "Vector control"
  # ))) %>%
  #   mutate(label_group = case_when(label_group == "Past climatic conditions" ~ "A",
  #                                  label_group == "Landscape - wetlands" ~ "B",
  #                                  label_group == "Landscape - other" ~ "C",
  #                                  label_group == "Micro-climatic conditions on the night of catch" ~ "D",
  #                                  label_group == "Water pres." ~ "-",
  #                                  label_group == "Topography" ~ "--",
  #                                  label_group == "Human hosts availability" ~ "E",
  #                                  label_group == "Other" ~ "---",
  #                                  label_group == "Place" ~ "F",
  #                                  label_group == "Hum. behav." ~ "----",
  #                                  label_group == "Vector control tools" ~ "G")) %>%
  #   arrange(label_group)
  # 
  # 
  
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

  
  imp <- imp %>%
    mutate(var = forcats::fct_relevel(var, c("RFD1F_2000_14_21","TMIN1_2000_0_14","TMAX1_2000_0_14","lsm_c_pland_2000_3_9","lsm_c_pland_2000_3_12","WLS_2000","WMD","lsm_c_pland_2000_3_4","lsm_c_pland_2000_3_1","lsm_c_pland_2000_3_2","VCM","int_ext"))) %>%
    arrange(var) %>%
    mutate(var = as.character(var))
    
  pdps[[1]] <- plot_imp
  
  for(i in 1:nrow(imp)){
    
    pdp = FeatureEffect$new(mod, imp$var[i], method = "pdp")
    
    subt <- imp$label[i]
    if(!is.na(imp$unit[i])){
      subt <- paste0(subt," (", imp$unit[i], ")")
    }

    if(indicator == "presence"){
      
      pdps[[i+1]] = pdp$plot() +
        scale_y_continuous('', limits = c(0, 1)) +
        #xlab(paste0(imp$label_group[i]," : \n",imp$label[i], " (", imp$unit[i], ")")) +
        #xlab(imp$unit[i]) +
        xlab("") +
        labs(subtitle = subt) + 
        theme_bw() + 
        theme(axis.title.x = element_text(size = 8),
              axis.text.x = element_text(size = 6),
              axis.text.y = element_text(size = 6),
              plot.subtitle = element_text(size = 9, face="bold"))
      
    } else if (indicator == "abundance_discrete"){
      
      pdp$results <- pdp$results %>%
        dplyr::mutate(.class = case_when(.class == "high....5.bites." ~ "high (> 5 bites)",
                                         .class == "low..1.bite." ~ "low (1 bite)",
                                         .class == "medium..between.1.and.5.bites." ~ "medium (b/w 1 and 5 bites)")) %>%
        mutate(.class = fct_relevel(.class,c("low (1 bite)","medium (b/w 1 and 5 bites)","high (> 5 bites)"))) %>%
        dplyr::rename(biting_rate = .class)
      
      pdps[[i+1]] = ggplot(pdp$results,aes_string(x = imp$var[i], y = ".value")) +
        geom_line(aes(linetype =biting_rate)) +
        scale_y_continuous('', limits = c(0, 1)) +
        #xlab(paste0(imp$label_group[i]," : \n",imp$label[i], " (", imp$unit[i], ")")) +
        #xlab(imp$unit[i]) +
        xlab("") +
        labs(subtitle = subt) + 
        theme_bw() + 
        theme(axis.title.x = element_text(size = 8),
              axis.text.x = element_text(size = 6),
              axis.text.y = element_text(size = 6),
              legend.title = element_text(size = 9), 
              legend.text = element_text(size = 8),
              plot.subtitle = element_text(size = 9, face = "bold"))
      
       

      
        # pdps[[i]] =  pdp$plot() +
        # scale_y_continuous('', limits = c(0, 1)) +
        # xlab(paste0(imp$label[i], " (", imp$unit[i], ")")) +
        # theme_bw()
      
      
      } else if (indicator == "abundance"){
        
        if(species %in% c("An. funestus ss.","An. coluzzii")){
          y_max = 2
        } else if(species == "An. gambiae ss."){
          y_max = 1
        }
        
      pdps[[i+1]] = pdp$plot() +
        #scale_y_continuous('', limits = c(0, max(df$resp_var))) +
        scale_y_continuous('', limits = c(0, y_max)) +
        #xlab(paste0(imp$label_group[i]," : \n",imp$label[i], " (", imp$unit[i], ")")) +
        #xlab(imp$unit[i]) +
        xlab("") +
        labs(subtitle = subt) + 
        theme_bw() + 
        theme(axis.title.x = element_text(size = 8),
              axis.text.x = element_text(size = 6),
              axis.text.y = element_text(size = 6),
              plot.subtitle = element_text(size = 9, face = "bold"))
    }
    
    if(indicator != "abundance_discrete" & !is.factor(df[,imp$var[i]])){
    pdp$results$.value <- pdp$results$.type <- NULL

    if(indicator != "abundance"){
     p = pdp::partial(model, pred.var = imp$var[i], pred.fun = pred_wrapper_classif, train = df, pred.grid = pdp$results)
    } else {
      p = pdp::partial(model, pred.var = imp$var[i], pred.fun = pred_wrapper_reg, train = df, pred.grid = pdp$results)
    }

    p$yhat[which(p$yhat<0)] <-0 

    p$yhat.id <- as.factor(p$yhat.id)

    pdps[[i+1]]$layers[[1]] <- NULL
    pdps[[i+1]] <-  pdps[[i+1]] +
      geom_line(data = p[p$yhat.id == "avg", ], aes_string(x = imp$var[i], y = "yhat"), size = 0.5) +
      geom_line(data = p[p$yhat.id != "avg", ], aes_string(x = imp$var[i], y = "yhat", group = "yhat.id"), linetype = "dashed", size = 0.15)
    }
    
  }
  
  
  pdps <- wrap_plots(pdps)
  
  #p_final <- (plot_imp  + pdps ) / (plot_interactions ) + plot_annotation(title = paste0(species, " - ", indicator, " (",metric," = ",round(quality,2),")")) + plot_layout(guides = 'auto')

  return(list(pdps = pdps, quality = quality, metric = metric))
  
}

fun_plot_validation_abundance <- function(df, spec){
  
  plot_validation_abundance <- ggplot(data = df, aes(x = nummission, y = value, group = name, col = name)) + 
    geom_line() + 
    geom_point(size = 1) +
    facet_wrap(species~codevillage) + 
    ylab("log (n bites)") + 
    xlab("entomological mission") +
    ggtitle(spec) + 
    theme_bw()
  
  return(plot_validation_abundance)
  
}


fun_plot_validation_presence <- function(df, spec){
  
  precrec_obj <- evalmod(scores = df$pred, labels = df$obs)
  plot_validation_presence <- autoplot(precrec_obj,curvetype = c("ROC","PRC"))# + ggtitle(spec)
  
  return(plot_validation_presence)
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
  mutate(species = fct_relevel(name,c("An. funestus ss.","An. gambiae ss.", "An. coluzzii")))


 df_map <- df %>%
      group_by(codevillage,nummission, date_mission_char,species) %>%
       summarise(Biting.rate = sum(value), X = mean(X_4326), Y = mean(Y_4326)) %>%
   mutate(Biting.rate = ifelse(Biting.rate==0,NA,Biting.rate))

 villages <- dbReadTable(react_gpkg, 'recensement_villages_l1') %>%
   dplyr::select(-geom) %>%
   dplyr::filter(!is.na(intervention),codepays == code_pays)
 

  m <- ggmap(myMap) + geom_point(aes(x = X, y = Y), data = villages, size = 0.7, color = "seagreen4") + 
      geom_point(aes(x = X, y = Y, size = Biting.rate), data = df_map, colour = "darkred") + 
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


res <-  readRDS("/home/ptaconet/Bureau/data_analysis/model_results_univanalysis4.rds")


model_univanalysis_results <- res %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus ss.",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(spatial_corrs_spearman = map(spatial_corrs_spearman, ~fun_get_predictors_labels(table = ., vector_predictors_name = "name"))) %>%
  mutate(spatial_corrs_spearman = map(spatial_corrs_spearman, ~mutate(., buffer = word(gsub("_"," ",name), ifelse(label_group %in% c("Landscape - other","Landscape - wetlands"), 4, 2))))) %>%
  mutate(spatial_corrs_spearman = map(spatial_corrs_spearman, ~mutate(., buffer = ifelse(is.na(buffer), word(gsub("_"," ",name), 2), buffer)))) %>%
  mutate(spatial_corrs_spearman =  pmap(list(spatial_corrs_spearman, response_var, code_pays, mod), ~mutate(..1, species = ..2, country = ..3, indicator = ..4))) %>%
  mutate(temporal_corrs_spearman =  map(temporal_corrs_spearman, ~do.call(rbind.data.frame,.))) %>%
  mutate(temporal_corrs_spearman = map(temporal_corrs_spearman, ~fun_get_predictors_labels(table = ., vector_predictors_name = "var"))) %>%
  mutate(temporal_corrs_spearman =  pmap(list(temporal_corrs_spearman, response_var, code_pays, mod), ~mutate(..1, species = ..2, country = ..3, indicator = ..4)))


univ_spearman_spatial <- do.call(rbind.data.frame, model_univanalysis_results$spatial_corrs_spearman) %>% 
  #filter(buffer %in% c(250,1000,2000), name %in% c("WAL_2000","WAC_2000","WLS_2000","POP_2000","BCH_2000","lsm_c_pland_2000_3_12","lsm_c_pland_2000_3_5","lsm_c_pland_2000_3_3","lsm_c_pland_2000_3_2","lsm_c_pland_2000_3_4","lsm_c_pland_2000_3_9","lsm_c_pland_2000_3_1","lsm_c_pland_2000_3_7","lsm_c_pland_2000_3_8","lsm_c_pland_2000_3_11",
  #                                                         "WAL_1000","WAC_1000","WLS_1000","POP_1000","BCH_1000","lsm_c_pland_1000_3_12","lsm_c_pland_1000_3_5","lsm_c_pland_1000_3_3","lsm_c_pland_1000_3_2","lsm_c_pland_1000_3_4","lsm_c_pland_1000_3_9","lsm_c_pland_1000_3_1","lsm_c_pland_1000_3_7","lsm_c_pland_1000_3_8","lsm_c_pland_1000_3_11",
  #                                                         "WAL_500","WAC_500","WLS_500","POP_500","BCH_500","lsm_c_pland_500_3_12","lsm_c_pland_500_3_5","lsm_c_pland_500_3_3","lsm_c_pland_500_3_2","lsm_c_pland_500_3_4","lsm_c_pland_500_3_9","lsm_c_pland_500_3_1","lsm_c_pland_500_3_7","lsm_c_pland_500_3_8","lsm_c_pland_500_3_11",
  #                                                         "WAL_250","WAC_250","WLS_250","POP_250","BCH_250","lsm_c_pland_250_3_12","lsm_c_pland_250_3_5","lsm_c_pland_250_3_3","lsm_c_pland_250_3_2","lsm_c_pland_250_3_4","lsm_c_pland_250_3_9","lsm_c_pland_250_3_1","lsm_c_pland_250_3_7","lsm_c_pland_250_3_8","lsm_c_pland_250_3_11",
  #                                                         "WAL_100","WAC_100","WLS_100","POP_100","BCH_100","lsm_c_pland_100_3_12","lsm_c_pland_100_3_5","lsm_c_pland_100_3_3","lsm_c_pland_100_3_2","lsm_c_pland_100_3_4","lsm_c_pland_100_3_9","lsm_c_pland_100_3_1","lsm_c_pland_100_3_7","lsm_c_pland_100_3_8","lsm_c_pland_100_3_11")) %>%
      filter(buffer %in% c(NA,250,500,1000,2000), name %in% c("POP_2000","WMD","BDE","BCH","WLS_2000","lsm_c_pland_2000_3_12","lsm_c_pland_2000_3_3","lsm_c_pland_2000_3_2","lsm_c_pland_2000_3_4","lsm_c_pland_2000_3_9","lsm_c_pland_2000_3_1","lsm_c_pland_2000_3_7","lsm_c_pland_2000_3_5","lsm_c_pland_2000_3_11",
                                                          "POP_1000","WLS_1000","lsm_c_pland_1000_3_12","lsm_c_pland_1000_3_3","lsm_c_pland_1000_3_2","lsm_c_pland_1000_3_4","lsm_c_pland_1000_3_9","lsm_c_pland_1000_3_1","lsm_c_pland_1000_3_7","lsm_c_pland_1000_3_5","lsm_c_pland_1000_3_11",
                                                          "POP_500","WLS_500","lsm_c_pland_500_3_12","lsm_c_pland_500_3_3","lsm_c_pland_500_3_2","lsm_c_pland_500_3_4","lsm_c_pland_500_3_9","lsm_c_pland_500_3_1","lsm_c_pland_500_3_7","lsm_c_pland_500_3_5","lsm_c_pland_500_3_11",
                                                          "POP_250","WLS_250","lsm_c_pland_250_3_12","lsm_c_pland_250_3_3","lsm_c_pland_250_3_2","lsm_c_pland_250_3_4","lsm_c_pland_250_3_9","lsm_c_pland_250_3_1","lsm_c_pland_250_3_7","lsm_c_pland_250_3_5","lsm_c_pland_250_3_11")) %>%
  mutate(model = "spearman univariate") %>%
  mutate(indicator = ifelse(indicator == "presence","Presence","Abundance")) %>%
  mutate(buffer = ifelse(is.na(buffer),"2000",buffer)) %>%
  dplyr::rename(pval = p) %>%
  mutate(correlation = ifelse(pval >= 0.2 | abs(correlation)<=0.1, NA, correlation)) %>%
  nest(-c(country))
  


univ_spearman_temporal <-  do.call(rbind.data.frame, model_univanalysis_results$temporal_corrs_spearman) %>% 
  filter(var %in% c("RFD1F", "TMIN1", "TMAX1")) %>%
  mutate(correlation = ifelse(p<=0.05,correlation,NA) ) %>%
  mutate(model = "spearman univariate") %>%
  nest(-c(indicator,country,species,var))
  


#### spatial univariate


plots_univ_spearman_spatial <- univ_spearman_spatial %>%
  mutate(univ_spatial = pmap(list(data, country), ~fun_plot_tile_univ_spatial(df = ..1,country = ..4))) %>%
  dplyr::select(-data)

###### temporal univariate

plots_univ_spearman_temporal <- univ_spearman_temporal %>%
  arrange(rev(indicator),species,var) %>%
  mutate(univ_temporal = pmap(list(data), ~fun_ccm_plot2(..1, paste0(..1$label[1]," (",..1$unit,")")))) %>%
  mutate(species = fct_relevel(species,c("An. funestus ss.","An. gambiae ss.","An. coluzzii"))) %>%
  arrange(species) %>%
  nest(-c(indicator,country)) %>%
  mutate(univ_temporal = map(data, ~wrap_plots(.$univ_temporal, nrow = 3, ncol = 3))) %>%
  #mutate(univ_temporal = pmap(list(univ_temporal,species,country,indicator), ~..1 + plot_annotation(title = paste(..2,..3,..4, sep = " - "), subtitle = paste("CCM using spearman coefficient"), caption = "Only significant results (pval <= 0.05) are displayed (non-signficant results are greyed out)"))) %>%
  dplyr::select(-data)

wrap_plots(plots_univ_spearman_temporal$univ_temporal, ncol = 2, nrow = 1) 

######################
###### multivariate
######################
  
## pdps
pdps <- res %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus ss.",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(rf_plots_lto = pmap(list(rf_lto,mod,response_var), ~fun_plot_pdp(..1,..2,..3)))

wrap_plots(pdps$rf_plots_lto[[1]]$pdps, pdps$rf_plots_lto[[4]]$pdps) # funestus
wrap_plots(pdps$rf_plots_lto[[2]]$pdps, pdps$rf_plots_lto[[5]]$pdps) # gambiae ss.
wrap_plots(pdps$rf_plots_lto[[3]]$pdps, pdps$rf_plots_lto[[6]]$pdps) # coluzzii


# plots for models validation
model_validation <- res %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus ss.",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(df_cv = map(rf_lto, ~pluck(.,"df_cv"))) %>%
  dplyr::select(response_var, code_pays, mod, df_cv) %>%
  mutate(df_cv = map2(df_cv,response_var, ~mutate(.x, species = .y))) %>%
  nest(-c(mod,code_pays))

model_validation <- model_validation %>%
  mutate(df_cv2 = map(data, ~do.call(rbind.data.frame, .$df_cv)))
  
# abundance
plots_validation_abundance <- model_validation$df_cv2[[2]] %>%
  group_by(codevillage,nummission,species) %>%
  summarise(pred = sum(pred), obs = sum(obs)) %>%
  pivot_longer(c("pred","obs")) %>%
  nest(-species) %>%
  mutate(plots_validation = map2(data, species, ~fun_plot_validation_abundance(..1,..2)))

# presence
plots_validation_presence <- model_validation$df_cv2[[1]] %>%
  nest(-c(species)) %>%
  mutate(plots_validation = map2(data, species, ~fun_plot_validation_presence(..1,..2)))


