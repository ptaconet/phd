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
      dplyr::mutate(pixlabel_detail = paste0(pixlabel, "\n",buffer1," m buffer")) %>%
      dplyr::mutate(pixlabel = ifelse(grepl("prd",name1),"patch richness density",pixlabel)) %>%
      dplyr::mutate(pixlabel = ifelse(grepl("shdi",name1),"shannon diversity index",pixlabel)) %>%
      dplyr::select(code, pixlabel,pixlabel_detail) %>%
      dplyr::rename(short_name = pixlabel) %>%
      dplyr::rename(short_name_detail = pixlabel_detail) %>%
      dplyr::arrange(code) %>%
      dplyr::mutate(group = ifelse(short_name %in% c("Marsh","Riparian forests","Still waters"), "Landscape - wetlands","Landscape - other")) %>%
      dplyr::mutate(unit = "% land.") %>%
      dplyr::select(code,short_name,short_name_detail,group,unit)
  }
  
  if(length(vector_predictors_vcm)>0){
    vector_predictors_vcm <- as.data.frame(vector_predictors_vcm)
    names(vector_predictors_vcm) <- "code"
    vector_predictors_vcm$code <- as.character(vector_predictors_vcm$code)
    vector_predictors_vcm <- vector_predictors_vcm %>% left_join(prediction_vars %>% dplyr::select(code, short_name, group, unit))  %>% mutate(short_name_detail="") %>%
      dplyr::select(code,short_name,short_name_detail,group,unit)
  }
  
  if(length(vector_predictors_other)>0){
    vector_predictors_other <- as.data.frame(vector_predictors_other)
    names(vector_predictors_other) <- "code_df"
    vector_predictors_other$code_df <- as.character(vector_predictors_other$code_df)
    vector_predictors_other$code <- word(vector_predictors_other$code_df,1,sep = "\\_")
    vector_predictors_other <- vector_predictors_other %>% left_join(prediction_vars %>% dplyr::select(code, short_name, group, unit)) %>% dplyr::select(-code) %>% dplyr::rename(code = code_df) %>% mutate(short_name_detail="")
    
    vector_predictors_past_climatic_conditions <- vector_predictors_other %>%
      dplyr::filter(group == "Past climatic conditions")
    
    if(nrow(vector_predictors_past_climatic_conditions)>0){
      vector_predictors_past_climatic_conditions <- vector_predictors_past_climatic_conditions %>%
        mutate(day_start = as.numeric(word(code,3,sep = "\\_"))) %>%
      mutate(day_end = as.numeric(word(code,4,sep = "\\_"))) %>%
      mutate(day_start = ifelse(grepl("TMIN1|TMAX1|TAMP1|RFD1|SMO1",code),day_start/7,day_start)) %>%
      mutate(day_end = ifelse(grepl("TMIN1|TMAX1|TAMP1|RFD1|SMO1",code),day_end/7,day_end)) %>%
      mutate(short_name_detail = paste0(short_name, "\nb/w ",day_start," and ",day_end," weeks")) %>%
      dplyr::select(-c(day_start,day_end))
    } else {
      vector_predictors_past_climatic_conditions = NULL
    }
    
    vector_predictors_other <- vector_predictors_other %>%
      dplyr::filter(group != "Past climatic conditions") %>%
      bind_rows(vector_predictors_past_climatic_conditions) %>%
      dplyr::select(code,short_name,short_name_detail,group,unit)
    
  }
  
  table_labels <- rbind(vector_predictors_lsm, vector_predictors_other, vector_predictors_vcm)
  
  if(!is.null(table)){
    colnames(table_labels) <- c(vector_predictors_name,"label","label_detail","label_group","unit")
    table_labels <- table_labels %>%
      dplyr::right_join(table) %>%
      mutate(label_detail = ifelse(label_detail=="",label,label_detail))
  }
  
  
  return(table_labels)
  
}


fun_glmm_dotpoint <- function(glmm_model_tabversion, mod, species, period_interv){
  
  
  glmm_model_tabversion <- glmm_model_tabversion %>%
    filter(!is.na(label)) %>%
    mutate(label = ifelse(grepl("VC|Household outdoor|still water",label), paste0(label, " (comp. to ",unit,")"),paste0(label, " (by add. ",unit,")"))) %>%
    mutate(p.value2 = case_when(
      p.value <= 0.001 ~ "***",
      p.value > 0.001 & p.value <= 0.01  ~  "**",
      p.value > 0.01 & p.value <= 0.05 ~ " *",
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
    )))
    
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
    ggtitle(paste0(mod," - ",species," - ",period_interv))#+ 
    #ggtitle(paste0(metric," = ",round(quality,2)))
  
  return(plot)
}


fun_plot_tile_univ_spatial <- function(df, metric_name = "Spearman correlation", country){
  
  df <- df %>%
    mutate(buffer = forcats::fct_relevel(buffer, c("250","500","1000","2000"))) %>%
    mutate(indicator = forcats::fct_relevel(indicator, c("Presence","Abundance"))) %>%
    mutate(species = forcats::fct_relevel(species, c("An. funestus","An. gambiae ss.","An. coluzzii")))
  
  df$label <- factor(df$label, levels = unique(df$label[order(df$correlation)]))
  df <- df %>% filter(!is.na(correlation),pval<=0.2) %>%
     mutate(p.value2 = case_when(
       pval <= 0.001 ~ "***",
       pval > 0.001 & pval <= 0.01  ~  "**",
       pval > 0.01 & pval <= 0.05 ~ " *",
       pval > 0.05 ~ ""
     ))
    

  p <- ggplot(df, aes(buffer, label)) + 
    geom_tile(aes(fill = correlation), color = "white") + facet_grid(species~indicator, scales="free_y", space="free_y") +
      xlab("buffer radius around the collection site (meters)") + 
    ylab("") +
    theme_bw() +
    theme(legend.position = "bottom",
          strip.text.x = element_text(size = 11),
          strip.text.y = element_text(size = 11)
    ) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-0.6,0.6), space = "Lab", name = metric_name, na.value="grey") + 
    geom_text(aes(label = ifelse(is.na(correlation), "",paste(round(correlation,2), p.value2))), size = 3)

  
  return(p)
}



# function to plot the CCM (simple plot : only the CCM)
fun_ccm_plot2 <- function(correlation_df, var, time_frame){
  
    most_corr <- correlation_df %>% filter(abs_corr == max(abs_corr, na.rm = T))

    #most_corr2 <- correlation_df %>% arrange(desc(abs_corr)) %>% head(round(0.1*nrow(.))) # 3 % top correlations will be blacked borders
    #most_corr2 <- correlation_df %>% arrange(desc(abs_corr)) %>% filter(abs_corr >= most_corr$abs_corr - 0.05)
    most_corr2 <- correlation_df %>% arrange(desc(abs_corr)) %>% filter(abs_corr >= most_corr$abs_corr * 0.9)
    
    
    ccm_plot <- ggplot(data = correlation_df, aes(time_lag_1, time_lag_2, fill = correlation)) +
    geom_tile(color = "white", show.legend = TRUE, size = 0.05,aes(width=1, height=1)) + 
      geom_tile(data = most_corr2 , color = "black", size = 0.2, show.legend = FALSE,aes(width=1, height=1)) +  # ,aes(width=1, height=1)
    geom_tile(data = most_corr , color = "deeppink3", size = 0.6, show.legend = FALSE,aes(width=1, height=1)) +  # ,aes(width=1, height=1)
    theme_minimal() + 
    theme(plot.title = element_text(size = 10, hjust = 0.5),
          axis.title = element_text(size = 8),
          legend.key.size = unit(0.8, "cm"),
          legend.title=element_text(size=10),
          legend.position = "none"
    ) +
    ggtitle(var) +
    annotate("text", size = 3,x = min(correlation_df$time_lag_1), y = max(correlation_df$time_lag_2), vjust = "inward", hjust = "inward", label = paste0("r(",most_corr$time_lag_2*time_frame,",",most_corr$time_lag_1*time_frame,") = ",round(most_corr$correlation,2))) +
    coord_fixed() +
    ylab("time lag 1") +
    xlab("time lag 2") + 
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-.8,.8), space = "Lab", name = "Spearman correlation", na.value = "grey") #+
    #scale_x_continuous(breaks = seq(0,8,2), labels = seq(0,40,10)) +
    #scale_y_continuous(breaks = seq(0,8,2), labels = seq(0,40,10))

  return(ccm_plot)
  
}



fun_plot_pdp <- function(modell, indicator, species, period_interv = "all", dayscience_nightscience = "dayscience"){
  
  library(iml)
  library("future")
  library("future.callr")
  model = modell$mod$finalModel
  df = modell$df_mod %>% dplyr::select(resp_var, model$xNames)
  
  if(indicator %in% c("presence","exophagy","early_biting","late_biting","early_late_biting","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1")){
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
  
  
  if(indicator != "abundance"){
  # boxplots of AUCs
  data_boxplot_auc <-  modell$mod$resample %>% left_join( modell$mod$pred %>% group_by(Resample) %>% summarise(n=n()) ) %>% mutate(n = ifelse(is.na(AUC),0,n))
  # predictive quality (weighted mean)
  mean_w_AUC <- weighted.mean(data_boxplot_auc$AUC, sqrt(data_boxplot_auc$n))
  
  bp <- ggplot(data = data_boxplot_auc, aes(x = "NULL", y = AUC)) + 
    geom_boxplot() + 
    theme_bw() + 
    ylim(0,1) + 
    geom_jitter(shape=16, position=position_jitter(0.2), aes(size = n)) + 
    stat_summary(fun=mean, geom="point", shape=20, size = 5, color="red", fill="red") +
    labs(subtitle = paste0("Boxplot of AUCs. n. folds = ",nrow(data_boxplot_auc)))
  
  # confusion matrix
  cm <- caret::confusionMatrix(data = modell$mod$pred$pred, reference = modell$mod$pred$obs)$table
  table <- data.frame(cm)
  plotTable <- table %>%
    mutate(goodbad = ifelse(table$Prediction == table$Reference, "good", "bad")) %>%
    group_by(Reference) %>%
    mutate(prop = Freq/sum(Freq))
  # fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups (see dplyr code above as well as original confusion matrix for comparison)
  cmplot <- ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
    geom_tile() +
    geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
    scale_fill_manual(values = c(good = "green", bad = "red")) +
    theme_bw() +
    xlim(rev(levels(table$Reference)))
  }

  
  # pdp for top 5 important variables
  imp <- randomForest::importance(model, scale = F, type = 1)
  imp <- as.data.frame(imp)
  imp$var <- rownames(imp)
  imp <- fun_get_predictors_labels(imp,"var")
  colnames(imp)[ncol(imp)] <- "importance"
  imp <- imp %>%
    mutate(label = forcats::fct_reorder(label, importance)) %>%
    arrange(-importance) #%>%
    #filter(!(label %in% c("Place","Vector control measure")))
  
  # variable importance plot
  plot_imp <- ggplot(imp, aes(x = importance , y = label, label = label)) +
    geom_bar(position = 'dodge', stat="identity") + 
    theme_bw() + 
    geom_label(size=2.5,position = position_dodge(0.9),aes(fontface=2),hjust=-0.1,label.padding = unit(0.2, "lines")) +
   #   geom_label(size=2, aes(fontface=2), label.padding = unit(0.15, "lines"), x = 0.05, alpha = 0.5) +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_text(size = 8),
          plot.subtitle = element_text(size = 9, face="bold")
          ) +
    ylab("") + 
    xlab("") +
    xlim(NA,max(imp$importance, na.rm = T) + max(imp$importance, na.rm = T)*2.5) +
    labs(subtitle = paste0(indicator,"\n",species,"\ntimeframe = ",period_interv,"\n",dayscience_nightscience,"\nAUC = ",round(quality,2),"\nVariable importance"))
    #ggtitle("Variable importance plot")
  
  # interactions 
  plot_interactions <- NULL
  plot_interactions_strengths <- NULL
   #plan("callr", workers = 7)
   #future::plan(multisession, workers = 7)
  #  interactions_strenght = Interaction$new(mod, grid.size = 30)
  #   
  #  interactions_strenght$results$feat <- word(gsub(":"," ",interactions_strenght$results$.feature),1)
  #  interactions_strenght$results <- fun_get_predictors_labels(interact3ions_strenght$results,"feat")
  #  interactions_strenght$results$.feature <- interactions_strenght$results$label
  #  interactions_strenght$results <- interactions_strenght$results %>% filter(!is.na(.feature))
  #   plot_interactions_strengths <- plot(interactions_strenght) +
  #     scale_y_discrete("") +
  #     theme_bw() +
  #     ggtitle("Interaction strengths")
  # 
  #  # interact <- furrr::future_map_dfr(imp$var,~iml::Interaction$new(mod, feature = .)$results)
  # interactions_to_study <- interactions_strenght$results %>% filter(.interaction>0.2) %>% arrange(desc(.interaction))
  # 
  
  
  


    nmax <- 7
    if(length(imp$var) < nmax){
      nmax = length(imp$var)
    }

    # library(furrr)
    # interact <- future_map_dfr(imp$var[1:nmax],~iml::Interaction$new(mod, feature = .)$results)
    # #interact <- map_dfr(imp$var[1:nmax],~iml::Interaction$new(mod, feature = .)$results)
    # 
    # interact <- interact %>%
    #      as.data.frame() %>%
    #      mutate(var1 = word(.feature,1,sep = ":")) %>%
    #      mutate(var2 = word(.feature,2,sep = ":")) %>%
    #      group_by(grp = paste(pmax(var1, var2), pmin(var1, var2), sep = "_")) %>%
    #      dplyr::slice(1) %>%
    #      ungroup() %>%
    #      dplyr::select(.interaction, var1, var2) %>%
    #      left_join(imp %>% dplyr::select(var,label), by = c("var1" = "var")) %>%
    #      dplyr::rename(label1 = label) %>%
    #      left_join(imp %>% dplyr::select(var,label), by = c("var2" = "var")) %>%
    #      dplyr::rename(label2 = label) %>%
    #      filter(!is.na(label1),!is.na(label2)) %>%
    #      filter(var1 %in% imp$var[1:nmax],var2 %in% imp$var[1:nmax]) %>%
    #      #filter(!(label1 %in% c("Place","Vector control measure")), !(label2 %in% c("Place","Vector control measure"))) %>%
    #      dplyr::select(.interaction ,label1,label2)
    # 
    #    plot_interactions <- ggplot(data = interact, aes(label2 , label1 , fill = .interaction)) +
    #    geom_tile(color = "white", show.legend = TRUE, size = 0.05) +
    #    theme_bw() +
    #    theme(axis.title.x = element_blank(),
    #          axis.title.y = element_blank(),
    #          axis.text.x = element_text(angle = 45, hjust=1),
    #          legend.title = element_text("interaction\nstrength")) +
    #    scale_fill_gradient(low = "white", high = "red", limits = c(0,1))


  
  
  
  # interaction_most_interact_var = Interaction$new(mod, feature = interact$results$feat[which.max(interact$results$.interaction)]) 
  # interaction_most_interact_var$results = interaction_most_interact_var$results %>% dplyr::arrange(.interaction)
  
  # pdp 2 vars for the main interaction
   # pd = FeatureEffect$new(mod, c("RFD1F_2000_14_21","TMAX1_2000_0_14" ), method = "pdp") 
   # pdp_2vars1 <- plot(pd) + 
   #   xlab("Rainfall") + 
   #   ylab("Diurnal temperature")  + 
   #   theme_bw() + 
   #   theme(axis.title.x = element_text(size = 8),
   #         axis.title.y = element_text(size = 8),
   #         axis.text.x = element_text(size = 6),
   #         axis.text.y = element_text(size = 6),
   #         plot.subtitle = element_text(size = 9, face="bold"),
   #         legend.position = "bottom") +
   #   labs(subtitle = "PDP 2 vars")
   # 
   # pd = FeatureEffect$new(mod, c("TMIN1_2000_0_14","TMAX1_2000_0_14" ), method = "pdp") 
   # pdp_2vars2 <- plot(pd) + 
   #   xlab("Nocturnal temperature") + 
   #   ylab("Diurnal temperature")  + 
   #   theme_bw() + 
   #   theme(axis.title.x = element_text(size = 8),
   #         axis.title.y = element_text(size = 8),
   #         axis.text.x = element_text(size = 6),
   #         axis.text.y = element_text(size = 6),
   #         plot.subtitle = element_text(size = 9, face="bold"),
   #         legend.position = "bottom") +
   #   labs(subtitle = "PDP 2 vars")
   # 
  
   pred_wrapper_classif <- function(object, newdata) {
     p <- predict(object, newdata = newdata, type ="prob")[,"Presence"]
     c("avg" = mean(p), "avg-1sd" = mean(p) - sd(p), "avg+1sd" = mean(p) + sd(p))
   }
   
     pred_wrapper_reg <- function(object, newdata) {
     p <- predict(object, newdata = newdata)
     c("avg" = mean(p), "avg-1sd" = mean(p) - sd(p), "avg+1sd" = mean(p) + sd(p))
   }
  
  # partial dependance plot
  

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
  
  # if(species == "An. funestus" & indicator == "abundance_discrete"){
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
  # if(species == "An. funestus" & indicator == "presence"){
  #   target <- c("lsm_c_pland_2000_3_9","lsm_c_pland_2000_3_12","WAC_2000","RFD1F_2000_14_21","RFH","LMN")
  #   imp <- imp %>% dplyr::filter(var %in% target) %>% arrange(factor(var, levels = target))
  #   
  # }

  pdps <- list()

    
  pdps[[1]] <- plot_imp
  
  if(indicator %in% c("presence","abundance")){
    imp <- imp %>%
      mutate(var = forcats::fct_relevel(var, c(imp$var[grep("RFD",imp$var)],
                                               imp$var[grep("TMIN",imp$var)],
                                               imp$var[grep("TMAX",imp$var)],
                                               imp$var[grep("3_9",imp$var)],
                                               imp$var[grep("2_5",imp$var)],
                                               "WLS_2000","WMD",
                                               imp$var[grep("3_4",imp$var)],
                                               imp$var[grep("3_1",imp$var)],
                                               imp$var[grep("3_2",imp$var)],
                                               imp$var[grep("3_3",imp$var)]))) %>%
      dplyr::filter(!(var %in% c("VCM","int_ext"))) %>%
      arrange(var) %>%
      mutate(var = as.character(var))
    
   nmax_vars_pdp <- nrow(imp)
   
   n_row=3
   n_col=4
  } else if (indicator %in% c("exophagy","early_late_biting","early_biting","late_biting","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1")){
    #nmax_vars_pdp <- 8
    nmax_vars_pdp <- length(imp$var)
    
    if(length(imp$var) < nmax_vars_pdp){
      nmax_vars_pdp <- length(imp$var)
    }
    
    n_row=1
    n_col=nmax_vars_pdp+1
    
    n_row=5
    n_col=5
    
  }
  
  for(i in 1:nmax_vars_pdp){
    
    pdp = FeatureEffect$new(mod, imp$var[i], method = "pdp")
    
    subt <- imp$label_detail[i]
    if(subt =="Grassland\n2000 m buffer" & species == "An. funestus"){
      subt <- gsub("2000","250",subt)
    }
    # if(!is.na(imp$unit[i])){
    #   subt <- paste0(subt," (", imp$unit[i], ")")
    # }

    if(indicator %in% c("presence","exophagy","early_late_biting","early_biting","late_biting","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1")){
      
      pdps[[i+1]] = pdp$plot() +
        scale_y_continuous('', limits = c(0, 1)) +
        #xlab(paste0(imp$label_group[i]," : \n",imp$label[i], " (", imp$unit[i], ")")) +
        xlab(imp$unit[i]) +
        ylab("Probability of >= 1 bite") +
      #  xlab("") +
        labs(subtitle = subt) + 
        theme_bw() + 
        theme(axis.title.x = element_text(size = 9),
              axis.title.y = element_text(size = 9),
              axis.text.x = element_text(size = 8),
              axis.text.y = element_text(size = 7),
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
        xlab(imp$unit[i]) +
        #xlab("") +
        labs(subtitle = subt) + 
        theme_bw() + 
        theme(axis.title.x = element_text(size = 8),
              axis.text.x = element_text(size = 7),
              axis.text.y = element_text(size = 6),
              legend.title = element_text(size = 9), 
              legend.text = element_text(size = 8),
              plot.subtitle = element_text(size = 9, face = "bold"))
      
       

      
        # pdps[[i]] =  pdp$plot() +
        # scale_y_continuous('', limits = c(0, 1)) +
        # xlab(paste0(imp$label[i], " (", imp$unit[i], ")")) +
        # theme_bw()
      
      
      } else if (indicator == "abundance"){
        
        if(species %in% c("An. funestus","An. coluzzii")){
          y_max = 2
        } else if(species == "An. gambiae ss."){
          y_max = 1
        }
        
      pdps[[i+1]] = pdp$plot() +
        #scale_y_continuous('', limits = c(0, max(df$resp_var))) +
        scale_y_continuous('', limits = c(0, y_max)) +
        #xlab(paste0(imp$label_group[i]," : \n",imp$label[i], " (", imp$unit[i], ")")) +
        xlab(imp$unit[i]) +
        ylab(ifelse(i==1,"log10 (num. bites/human/night)","")) +
        #xlab("") +
        labs(subtitle = subt) + 
        theme_bw() + 
        theme(axis.title.x = element_text(size = 9),
              axis.title.y = element_text(size = 9),
              axis.text.x = element_text(size = 8),
              axis.text.y = element_text(size = 7),
              plot.subtitle = element_text(size = 9, face="bold"))
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
  
  
  # pdps[[length(pdps)+1]] <- pdp_2vars1
  # pdps[[length(pdps)+1]] <- pdp_2vars2
  
  
 # pdps[[length(pdps)+1]] <- bp
  #pdps[[length(pdps)+1]] <- cmplot
  
  pdps <- wrap_plots(pdps) + plot_layout(nrow=n_row,ncol=n_col)


  #p_final <- (plot_imp  + pdps ) / (plot_interactions ) + plot_annotation(title = paste0(species, " - ", indicator, " (",metric," = ",round(quality,2),")")) + plot_layout(guides = 'auto')

  return(list(pdps = pdps, plot_interactions = plot_interactions, plot_interactions_strengths = plot_interactions_strengths, quality = quality, metric = metric))
  
}

fun_plot_validation <- function(df, spec, df_perf_metric, mod){
  
  data_text <- df_perf_metric %>% mutate(name = seq(1,nrow(.),1))  # to add a label within each facet
  
  if(mod=="abundance"){
    data_text <- data_text %>% rename(label=mae)
  } else {
    data_text <- data_text %>% rename(label=f1score)
  }
  
  # data_text <- data.frame(
  #   label = as.character(seq(1,length(unique(df$codevillage)),1)/100),
  #   codevillage   = unique(df$codevillage),
  #   name = unique(df$codevillage),
  #   stringsAsFactors = F
  # )
  
  if(mod == "presence"){
    y_lab = "∑ Presence probability"
  } else if (mod == "abundance"){
    y_lab = "Abundance"
  }
  
  df$codevillage <- factor(df$codevillage, levels = unique(df$codevillage))
  df$name <- ifelse(df$name=="pred","Predicted","Observed")
  
  plot_validation_abundance <- ggplot() + 
    geom_line(data = df, aes(x = nummission, y = value, group = name, colour = name)) + 
    geom_point(data = df, aes(x = nummission, y = value, group = name, colour = name),size = 1) +
    facet_wrap(.~codevillage) + 
    ylab(y_lab) + 
    xlab("entomological survey") +
  #  ggtitle(spec) + 
    theme_bw() +
    labs(color = "") +
    # geom_label(
    #   data = data_text,
    #   mapping = aes(x = 1, y = max(df$value,na.rm = T), label = label),
    #   vjust = 1,
    #   hjust = -0.001,
    #   alpha = 0.5,
    #   size = 3
    # ) + 
    scale_colour_manual(values=c("#009E73","#E69F00"),na.translate = F) +
    #scale_color_brewer(palette = "Dark2")
    theme(axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8))
  
  if(mod=='abundance'){
    plot_validation_abundance <- plot_validation_abundance  + scale_y_continuous(trans='log10')
  }
  
  return(plot_validation_abundance)
  
}



# fonction pour préparer le fichier qui permettra de calculer les indices de performance au niveau du village ou du point de capture
fun_prepare_df_perf <- function(df,mod){
  
  if(mod == "abundance"){
    df$obs <- exp(df$obs)
    df$pred <- exp(df$pred)
  }
  
  df <- df %>%
#    mutate(codevillage = paste0(codevillage,pointdecapture)) %>%  # pour avoir à l'échelle du point de capture
    group_by(codevillage,nummission,species) %>%   
    summarise(pred = sum(pred), obs = sum(obs)) %>%
    as_tibble() %>%
    pivot_longer(c("pred","obs")) %>%
    complete(species, nesting(codevillage, nummission)) %>%
    nest(-species)
  
  return(df)
}


# fonction pour calculer les indicateurs de performance au niveau du village (toutes missions confondues) pour chaque espèce 
fun_compute_perf_metric <- function(df_cv,mod){
  
  sens <- function(obs,pred){
    tryCatch({
      ret = round(MLmetrics::Sensitivity(y_true = obs ,y_pred = pred, positive = 1),2)
      return(ret)
    },error=function(error_message) {
      return(NaN)
    })}
  spec <- function(obs,pred){
    tryCatch({
      ret = round(MLmetrics::Specificity(y_true = obs ,y_pred = pred, positive = 1),2)
      return(ret)
    },error=function(error_message) {
      return(NaN)
    })}
  prec <- function(obs,pred){
    tryCatch({
      ret = round(MLmetrics::Precision(y_true = obs ,y_pred = pred ,positive = 1),2)
      return(ret)
    },error=function(error_message) {
      return(NaN)
    })}
  reca <- function(obs,pred){
    tryCatch({
      ret = round(MLmetrics::Recall(y_true = obs ,y_pred = pred ,positive = 1),2)
      return(ret)
    },error=function(error_message) {
      return(NaN)
    })}
  rocauc <- function(obs,pred){
    tryCatch({
      ret = round(MLmetrics::AUC(y_true = obs ,y_pred = pred),2)
      return(ret)
    },error=function(error_message) {
      return(NaN)
    })}  
  prauc <- function(obs,pred){
    tryCatch({
      ret = round(MLmetrics::PRAUC(y_true = obs ,y_pred = pred),2)
      return(ret)
    },error=function(error_message) {
      return(NaN)
    })}
  f1 <- function(obs,pred){
    tryCatch({
      ret = round(MLmetrics::F1_Score(y_true = obs ,y_pred = pred,positive = 1),2)
      return(ret)
    },error=function(error_message) {
      return(NaN)
    })}
  
  
  specs <- unique(df_cv$species)
  df<-NULL
  for(j in 1:length(specs)){
  
    th_df_cv <- df_cv %>% filter(species == specs[j])
    
  ## find threshold
  if(mod !="abundance"){
    thresholds = seq(0, 1, 0.001)
    aucs_thresholds <- NULL
    for(i in 1:length(thresholds)){
      
      pred_class <- ifelse(th_df_cv$pred < thresholds[i],0,1)
      if(length(unique(pred_class))==1){
        th_auc <- NA
      } else {
        # th_auc <- MLmetrics::F1_Score(y_true = th_df_cv$obs,y_pred = pred_class, positive = "1")
        th_auc <- MLmetrics::AUC(y_true = th_df_cv$obs,y_pred = pred_class)
        #th_auc <- MLmetrics::PRAUC(y_true = th_df_cv$obs,y_pred = pred_class)
        #th_auc <- MLmetrics::Specificity(y_true = th_df_cv$obs,y_pred = pred_class)
      }
      
      aucs_thresholds <-c(aucs_thresholds, th_auc)

    }
    
    max_metric <- max(aucs_thresholds, na.rm = T)
    threshold <- thresholds[which.max(aucs_thresholds)]
    
    th_df_cv$pred2 <- ifelse(th_df_cv$pred < threshold, 0, 1)

  }
  

  
  if(mod!="abundance"){ 
    th_df_cv <- th_df_cv %>%
    #mutate(codevillage = paste0(codevillage,pointdecapture)) %>%  # pour avoir à l'échelle du point de capture
    group_by(codevillage,species) %>%   
    summarise(sensitivity = sens(obs,pred2),
              specificity =  spec(obs,pred2),
              precision =  prec(obs,pred2),
              recall =  reca(obs,pred2),
              f1score = f1(obs,pred2),
              ROC_AUC =  rocauc(obs,pred2),
              PR_AUC =  prauc(obs,pred2)) %>%
    as_tibble() %>%
      mutate(max_auc = max_metric) %>%
      mutate(threshold = threshold)
    
  } else {
    th_df_cv <- th_df_cv %>%
      #mutate(codevillage = paste0(codevillage,pointdecapture)) %>%  # pour avoir à l'échelle du point de capture
      group_by(codevillage,species) %>%   
      summarise(mae = round(MLmetrics::MAE(y_true = obs ,y_pred = pred),2),
                mse =  round(MLmetrics::MSE(y_true = obs ,y_pred = pred),2),
                rmse =  round(MLmetrics::RMSE(y_true = obs ,y_pred = pred),2),
                rsq =  round(MLmetrics::R2_Score(y_true = obs ,y_pred = pred),2)) %>%
      as_tibble()
    }
    
    df <- rbind(df,th_df_cv) 
    
  }
  
  df <- df %>%
    complete(species, nesting( codevillage)) %>%
    nest(-species)
  
    
  return(df)
  
}


fun_plot_validation_presence <- function(df, spec){
  
  precrec_obj <- evalmod(scores = df$pred, labels = df$obs)
  plot_validation_presence <- autoplot(precrec_obj,curvetype = c("PRC")) + ggtitle(spec) +     
    theme(axis.title.x = element_text(size = 8),
         axis.title.y = element_text(size = 8))
  # urvetype = c("ROC") for ROC curves
  
  return(plot_validation_presence)
}

fun_plot_validation_abundance <- function(df, spec){
  
  df <- df %>%
    mutate(obs = exp(obs)) %>%
    mutate(pred = exp(pred)) %>%
    mutate(rep = abs(obs - pred)/obs) %>%
    mutate(residuals = obs - pred) %>%
    mutate(groups = case_when(obs==1 ~ "1",
                              obs>1 & obs<=3 ~ "2-3",
                              obs>3 & obs<=10 ~ "4-10",
                              obs>10 ~ ">10"
    )) %>%
    mutate(groups = fct_relevel(groups, c("1","2-3","4-10",">10")))
  
  df_metrics_perf <- df %>%
    group_by(groups) %>%
    summarise(mae = round(MLmetrics::MAE(y_true = obs ,y_pred = pred),2),
              mse =  round(MLmetrics::MSE(y_true = obs ,y_pred = pred),2),
              rmse =  round(MLmetrics::RMSE(y_true = obs ,y_pred = pred),2),
              mape =  round(MLmetrics::MAPE(y_true = obs ,y_pred = pred),2),
              n=n()) %>%
    as_tibble()
  
  
  plot_validation_abundance <- ggplot() + 
    geom_violin(data = df, aes(x=groups , y=residuals)) + 
    #geom_jitter(data = df, aes(x=groups , y=residuals), position = position_jitter(width = .15), size = 0.3) + 
    stat_summary(data = df, aes(x=groups , y=residuals), fun=median, geom="point", size=2, color="black") +
    theme_bw() + 
    xlab("Observed counts") + 
    ylab("Residuals (obs - pred)") + 
    geom_label(data = df_metrics_perf,
               size = 2.5,
               mapping = aes(x = groups, y = max(df$residuals,na.rm = T), label = paste0('MAE = ',mae,'\nn = ',n),
                vjust = 1)) +
    ggtitle(spec) + 
    geom_hline(yintercept=0, linetype="dashed") + 
    theme(axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8))
          
  
  return(plot_validation_abundance)
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

entomo_csh_metadata_l1 <- dbReadTable(react_gpkg, 'entomo_csh_metadata_l1') %>% 
  dplyr::select(-geom) %>%
  filter(!(nummission %in% c("11","12","13","15")))

mean_date_mission <- entomo_csh_metadata_l1 %>% 
  mutate(date_capture = as.Date(date_capture)) %>% 
  dplyr::group_by(codepays,nummission) %>% 
  dplyr::summarise(mean_date_mission=mean(date_capture)) %>% 
  as_tibble() %>% filter(codepays==code_pays) %>% 
  dplyr::select(-codepays) %>% 
  mutate(date_mission_char = paste0(nummission, " - ", lubridate::month(mean_date_mission,label=TRUE, abbr=FALSE, locale = "en_US.utf8"), " ",lubridate::year(mean_date_mission)))  %>%  
  mutate(date_mission_char = forcats::fct_reorder(date_mission_char,mean_date_mission))


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


exo_byvillage <- dbReadTable(react_gpkg, 'entomo_idmoustiques_l0') %>%
  filter(codepays == "BF") %>%
  filter(pcr_espece %in% c( "An.funestus_ss", "An.coluzzii", "An.gambiae_ss"), nummission <10) %>%
  group_by(nummission, codepays, codevillage, postedecapture,pcr_espece) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = postedecapture, values_from = n) %>%
  mutate_all(funs(replace_na(., 0))) %>%
  mutate(tot = e+i) %>%
  mutate(exophagy = e / tot * 100) %>%
  mutate(exophagy = ifelse(is.na(exophagy),0,exophagy)) %>%
  mutate(nummission = as.character(nummission)) %>%
  left_join(mean_date_mission) %>%
  left_join(mean_coords_points_4326 %>% group_by(codevillage) %>% summarise(X_4326 = mean(X_4326),Y_4326 = mean(Y_4326))) %>%
  filter(tot>5)

if(code_pays=="BF"){
df <- df %>%
  dplyr::select(codevillage,nummission,date_mission_char,X_4326,Y_4326,ma_funestus_ss,ma_gambiae_ss,ma_coluzzi) %>%
  pivot_longer(c(ma_funestus_ss,ma_gambiae_ss,ma_coluzzi)) %>%
  mutate(name = case_when(
    name == "ma_funestus_ss" ~ "An. funestus",
    name == "ma_gambiae_ss" ~ "An. gambiae ss.",
    name == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(species = fct_relevel(name,c("An. funestus","An. gambiae ss.", "An. coluzzii")))
} else if (code_pays=="CI"){
  df <- df %>%
    dplyr::select(codevillage,nummission,date_mission_char,X_4326,Y_4326,ma_funestus_ss,ma_gambiae_sl) %>%
    pivot_longer(c(ma_funestus_ss,ma_gambiae_sl)) %>%
    mutate(name = case_when(
      name == "ma_funestus_ss" ~ "An. funestus",
      name == "ma_gambiae_sl" ~ "An. gambiae sl.")) %>%
    mutate(species = fct_relevel(name,c("An. funestus","An. gambiae sl."))) 
}


 df_map <- df %>%
      group_by(codevillage,nummission, date_mission_char,species) %>%
       summarise(Biting.rate = mean(value), X = mean(X_4326), Y = mean(Y_4326)) %>%
   mutate(Biting.rate = ifelse(Biting.rate==0,NA,Biting.rate))

 villages <- dbReadTable(react_gpkg, 'recensement_villages_l1') %>%
   dplyr::select(-geom) %>%
   dplyr::filter(!is.na(intervention),codepays == code_pays)
 

  m <- ggmap(myMap) + geom_point(aes(x = X, y = Y), data = villages, size = 0.8, color = "seagreen4") + 
      geom_point(aes(x = X, y = Y, size = Biting.rate), data = df_map, colour = "darkred") + 
      facet_grid(species~date_mission_char) + 
    theme(strip.text.y = element_text(size = 13, face = "bold")) + 
    theme(strip.text.x = element_text(size = 13))
    
 
  
  
  
df$resp_var <- df[,response_var]

response_var = case_when(
  response_var == "ma_funestus_ss" ~ "An. funestus",
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











fun_plot_pdp2 <- function(modell, indicator, species, period_interv = "all", dayscience_nightscience = "dayscience"){

  library(iml)
  library("future")
  library("future.callr")
  model = modell$mod
  df = modell$df_mod %>% dplyr::select(resp_var, model$finalMode$xNames)
  df_cv <-  modell$df_cv
  
  
  ## find threshold
  if(indicator %in% c("presence","exophagy","early_biting","late_biting","early_late_biting","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1")){
    thresholds = seq(0, 1, 0.001)
  aucs_thresholds <- NULL
  for(i in 1:length(thresholds)){
    
    pred_class <- ifelse(df_cv$pred < thresholds[i],0,1)
    if(length(unique(pred_class))==1){
      th_auc <- NA
    } else {
      # th_auc <- MLmetrics::F1_Score(y_true = df_cv$obs,y_pred = pred_class, positive = "1")
      th_auc <- MLmetrics::AUC(y_true = df_cv$obs,y_pred = pred_class)
      # th_auc <- MLmetrics::PRAUC(y_true = df_cv$obs,y_pred = pred_class)
      
    }
    
    aucs_thresholds <-c(aucs_thresholds, th_auc)
    
  }
  
  max_metric <- max(aucs_thresholds, na.rm = T)
  threshold <- thresholds[which.max(aucs_thresholds)]
  
  }
  
  #### local importance
  ## first re-run model with parameters from the cv
 #  r <- ranger::ranger(data = df, 
 #                      formula = resp_var ~ . ,
 #                      num.trees = model$finalModel$num.trees,
 #                      mtry = model$finalModel$mtry,
 #                      min.node.size = model$finalModel$min.node.size,
 #                      splitrule = model$finalModel$splitrule,
 #                      num.random.splits = ifelse(!is.null(model$finalModel$num.random.splits), model$finalModel$num.random.splits,1),
 #                      importance = "permutation",
 #                      local.importance = TRUE,
 #                      probability = ifelse(indicator=="abundance",F,T))
 #  
 #  
 #  a <- as.data.frame(r$variable.importance.local)
 #  a$names <- rownames(a)
 #  a$nummission <- substr(a$names,1,1)
 #  a$codevillage <- substr(a$names,2,4)
 #  a$pointdecapture <- substr(a$names,5,5)
 #  a$int_ext2 <-  substr(a$names,6,nchar(a$names))
 #  df_cv <- df_cv %>% rename(int_ext2 = int_ext)
 #  localvarimp <- left_join(a,df_cv)
 #  rownames(localvarimp) <- a$names
 #  
 #  localvarimp <- localvarimp %>%
 #   mutate(saison = case_when(nummission %in% c("2","7") ~ "chaude & sèche",
 #                              nummission %in% c("1","5","6") ~ "froide & sèche",
 #                              nummission %in% c("3","4") ~ "pluvieuse")) %>%
 #   #filter(saison == "pluvieuse") %>%
 # # filter(pred >= threshold) %>%
 #    group_by(codevillage) %>%  #group_by(nummission)
 #    summarise_if(is.numeric, mean, na.rm = TRUE) %>%
 #    as_tibble()
 #  
 #  #n <- paste0(localvarimp$nummission,localvarimp$codevillage)
 #  #n  = localvarimp$nummission
 #  n  = localvarimp$codevillage
 #  
 #  localvarimp <- localvarimp %>% dplyr::select(colnames(r$variable.importance.local))
 #  
 #  localvarimp_mat <- as.matrix(sapply(localvarimp, as.numeric))
 #  #rownames(localvarimp_mat) <- rownames(localvarimp)
 #  rownames(localvarimp_mat) <- n
  
  #    pheatmap::pheatmap(t(localvarimp_mat), cluster_cols = FALSE, cluster_rows = FALSE)
  
  
  
  if(indicator %in% c("presence","exophagy","early_biting","late_biting","early_late_biting","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1")){
    mod <- Predictor$new(model, data = df, class = "Presence")
    metric <- "ROC_AUC"
    quality = max(modell$mod$results$ROC_AUC)
  } else if (indicator == "abundance_discrete"){
    mod <- Predictor$new(model, data = df)
    metric <- "Accuracy"
    quality = max(modell$mod$results$Accuracy)
  } else if (indicator == "abundance"){
    mod <- Predictor$new(model, data = df)
    metric <- "Rsquared"
    quality = max(modell$mod$results$Rsquared)
  }
  
  
  if(indicator != "abundance"){
    # boxplots of AUCs
    data_boxplot_auc <-  modell$mod$resample %>% left_join( modell$mod$pred %>% group_by(Resample) %>% summarise(n=n()) ) %>% mutate(n = ifelse(is.na(ROC_AUC),0,n))
    # predictive quality (weighted mean)
    mean_w_AUC <- weighted.mean(data_boxplot_auc$ROC_AUC, sqrt(data_boxplot_auc$n))
    
    bp <- ggplot(data = data_boxplot_auc, aes(x = "NULL", y = ROC_AUC)) + 
      geom_boxplot() + 
      theme_bw() + 
      ylim(0,1) + 
      geom_jitter(shape=16, position=position_jitter(0.2), aes(size = n)) + 
      stat_summary(fun=mean, geom="point", shape=20, size = 5, color="red", fill="red") +
      labs(subtitle = paste0("Boxplot of AUCs. n. folds = ",nrow(data_boxplot_auc)))
    
    # confusion matrix
    
    df_cv_cm <- df_cv %>% mutate(obs = ifelse(obs == 0, "Absence","Presence")) %>% mutate(pred = ifelse(pred < threshold,"Absence","Presence")) %>% mutate_if(is.character, as.factor)
    
    cm <- caret::confusionMatrix(data = df_cv_cm$pred, reference = df_cv_cm$obs)$table
    table <- data.frame(cm)
    plotTable <- table %>%
      mutate(goodbad = ifelse(table$Prediction == table$Reference, "good", "bad")) %>%
      group_by(Reference) %>%
      mutate(prop = Freq/sum(Freq))
    # fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups (see dplyr code above as well as original confusion matrix for comparison)
    cmplot <- ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
      geom_tile() +
      geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
      scale_fill_manual(values = c(good = "green", bad = "red")) +
      theme_bw() +
      xlim(rev(levels(table$Reference))) + 
      theme(legend.position = "none")
  }
  
  
  # pdp for top 5 important variables
  imp <- model$finalModel$variable.importance
  #imp <- randomForest::importance(model$finalModel, scale = F, type = 1)
  
  imp <- as.data.frame(imp)
  imp$var <- rownames(imp)
  imp <- fun_get_predictors_labels(imp,"var")
  colnames(imp)[ncol(imp)] <- "importance"
  imp <- imp %>%
    mutate(label = forcats::fct_reorder(label, importance)) %>%
    arrange(-importance)
  
  # variable importance plot
  plot_imp <- ggplot(imp, aes(x = importance , y = label, label = label)) +
    geom_bar(position = 'dodge', stat="identity") + 
    theme_bw() + 
    geom_label(size=2.5,position = position_dodge(0.9),aes(fontface=2),hjust=-0.1,label.padding = unit(0.2, "lines")) +
    #   geom_label(size=2, aes(fontface=2), label.padding = unit(0.15, "lines"), x = 0.05, alpha = 0.5) +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_text(size = 8),
          plot.subtitle = element_text(size = 9, face="bold")
    ) +
    ylab("") + 
    xlab("") +
    xlim(NA,max(imp$importance, na.rm = T) + max(imp$importance, na.rm = T)*2.5) +
    labs(subtitle = paste0(indicator,"\n",species,"\ntimeframe = ",period_interv,"\n",dayscience_nightscience,"\nAUC = ",round(max_metric,2),"\nVariable importance"))

  # interactions 
  plot_interactions <- NULL
  plot_interactions_strengths <- NULL
  #plan("callr", workers = 7)
  # future::plan(multisession, workers = 7)
  #   interactions_strenght = Interaction$new(mod, grid.size = 30)
  #   
  #  interactions_strenght$results$feat <- word(gsub(":"," ",interactions_strenght$results$.feature),1)
  #  interactions_strenght$results <- fun_get_predictors_labels(interactions_strenght$results,"feat")
  #  interactions_strenght$results$.feature <- interactions_strenght$results$label
  #  interactions_strenght$results <- interactions_strenght$results %>% filter(!is.na(.feature))
  #   plot_interactions_strengths <- plot(interactions_strenght) +
  #     scale_y_discrete("") +
  #     theme_bw() +
  #     ggtitle("Interaction strengths")
  # 
    # interact <- furrr::future_map_dfr(imp$var,~iml::Interaction$new(mod, feature = .)$results)
  # interactions_to_study <- interactions_strenght$results %>% filter(.interaction>0.2) %>% arrange(desc(.interaction))
  # 
  
  
  
  
  
  nmax <- 7
  if(length(imp$var) < nmax){
    nmax = length(imp$var)
  }
  
  # library(furrr)
  # interact <- future_map_dfr(imp$var[1:nmax],~iml::Interaction$new(mod, feature = .)$results)
  # #interact <- map_dfr(imp$var[1:nmax],~iml::Interaction$new(mod, feature = .)$results)
  # 
  # interact <- interact %>%
  #      as.data.frame() %>%
  #      mutate(var1 = word(.feature,1,sep = ":")) %>%
  #      mutate(var2 = word(.feature,2,sep = ":")) %>%
  #      group_by(grp = paste(pmax(var1, var2), pmin(var1, var2), sep = "_")) %>%
  #      dplyr::slice(1) %>%
  #      ungroup() %>%
  #      dplyr::select(.interaction, var1, var2) %>%
  #      left_join(imp %>% dplyr::select(var,label), by = c("var1" = "var")) %>%
  #      dplyr::rename(label1 = label) %>%
  #      left_join(imp %>% dplyr::select(var,label), by = c("var2" = "var")) %>%
  #      dplyr::rename(label2 = label) %>%
  #      filter(!is.na(label1),!is.na(label2)) %>%
  #      filter(var1 %in% imp$var[1:nmax],var2 %in% imp$var[1:nmax]) %>%
  #      #filter(!(label1 %in% c("Place","Vector control measure")), !(label2 %in% c("Place","Vector control measure"))) %>%
  #      dplyr::select(.interaction ,label1,label2)
  # 
  #    plot_interactions <- ggplot(data = interact, aes(label2 , label1 , fill = .interaction)) +
  #    geom_tile(color = "white", show.legend = TRUE, size = 0.05) +
  #    theme_bw() +
  #    theme(axis.title.x = element_blank(),
  #          axis.title.y = element_blank(),
  #          axis.text.x = element_text(angle = 45, hjust=1),
  #          legend.title = element_text("interaction\nstrength")) +
  #    scale_fill_gradient(low = "white", high = "red", limits = c(0,1))
  
  
  
  
  
  # interaction_most_interact_var = Interaction$new(mod, feature = interact$results$feat[which.max(interact$results$.interaction)]) 
  # interaction_most_interact_var$results = interaction_most_interact_var$results %>% dplyr::arrange(.interaction)
  
  # pdp 2 vars for the main interaction
  # pd = FeatureEffect$new(mod, c("RFD1F_2000_14_21","TMAX1_2000_0_14" ), method = "pdp") 
  # pdp_2vars1 <- plot(pd) + 
  #   xlab("Rainfall") + 
  #   ylab("Diurnal temperature")  + 
  #   theme_bw() + 
  #   theme(axis.title.x = element_text(size = 8),
  #         axis.title.y = element_text(size = 8),
  #         axis.text.x = element_text(size = 6),
  #         axis.text.y = element_text(size = 6),
  #         plot.subtitle = element_text(size = 9, face="bold"),
  #         legend.position = "bottom") +
  #   labs(subtitle = "PDP 2 vars")
  # 
  # pd = FeatureEffect$new(mod, c("TMIN1_2000_0_14","TMAX1_2000_0_14" ), method = "pdp") 
  # pdp_2vars2 <- plot(pd) + 
  #   xlab("Nocturnal temperature") + 
  #   ylab("Diurnal temperature")  + 
  #   theme_bw() + 
  #   theme(axis.title.x = element_text(size = 8),
  #         axis.title.y = element_text(size = 8),
  #         axis.text.x = element_text(size = 6),
  #         axis.text.y = element_text(size = 6),
  #         plot.subtitle = element_text(size = 9, face="bold"),
  #         legend.position = "bottom") +
  #   labs(subtitle = "PDP 2 vars")
  # 
  
  pred_wrapper_classif <- function(object, newdata) {
    p <- predict(object, newdata = newdata, type ="prob")[,"Presence"]
    c("avg" = mean(p), "avg-1sd" = mean(p) - sd(p), "avg+1sd" = mean(p) + sd(p))
  }
  
  pred_wrapper_reg <- function(object, newdata) {
    p <- predict(object, newdata = newdata)
    c("avg" = mean(p), "avg-1sd" = mean(p) - sd(p), "avg+1sd" = mean(p) + sd(p))
  }
  
  # partial dependance plot
  
  
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
  
  # if(species == "An. funestus" & indicator == "abundance_discrete"){
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
  # if(species == "An. funestus" & indicator == "presence"){
  #   target <- c("lsm_c_pland_2000_3_9","lsm_c_pland_2000_3_12","WAC_2000","RFD1F_2000_14_21","RFH","LMN")
  #   imp <- imp %>% dplyr::filter(var %in% target) %>% arrange(factor(var, levels = target))
  #   
  # }
  
  pdps <- list()
  
  
  pdps[[1]] <- plot_imp
  
  if(indicator %in% c("presence","abundance")){
    imp <- imp %>%
      mutate(var = forcats::fct_relevel(var, c(imp$var[grep("RFD",imp$var)],
                                               imp$var[grep("TMIN",imp$var)],
                                               imp$var[grep("TMAX",imp$var)],
                                               imp$var[grep("3_9",imp$var)],
                                               imp$var[grep("2_5",imp$var)],
                                               "WLS_2000","WMD",
                                               imp$var[grep("3_4",imp$var)],
                                               imp$var[grep("3_1",imp$var)],
                                               imp$var[grep("3_2",imp$var)],
                                               imp$var[grep("3_3",imp$var)]))) %>%
      dplyr::filter(!(var %in% c("VCM","int_ext","codevillage"))) %>%
      arrange(var) %>%
      mutate(var = as.character(var))
    
    nmax_vars_pdp <- nrow(imp)
    
    n_row=3
    n_col=4
  } else if (indicator %in% c("exophagy","early_late_biting","early_biting","late_biting","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1")){
    #nmax_vars_pdp <- 8
    nmax_vars_pdp <- length(imp$var)
    
    if(length(imp$var) < nmax_vars_pdp){
      nmax_vars_pdp <- length(imp$var)
    }
    
    n_row=1
    n_col=nmax_vars_pdp+1
    
    n_row=5
    n_col=5
    
  }
  
  for(i in 1:nmax_vars_pdp){
    
    pdp = FeatureEffect$new(mod, imp$var[i], method = "pdp")
    
    subt <- imp$label_detail[i]
    if(subt =="Grassland\n2000 m buffer" & species == "An. funestus"){
      subt <- gsub("2000","250",subt)
    }
    # if(!is.na(imp$unit[i])){
    #   subt <- paste0(subt," (", imp$unit[i], ")")
    # }
    
    if(indicator %in% c("presence","exophagy","early_late_biting","early_biting","late_biting","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1")){
      
      pdps[[i+1]] = pdp$plot() +
        scale_y_continuous('', limits = c(0, 1)) +
        #xlab(paste0(imp$label_group[i]," : \n",imp$label[i], " (", imp$unit[i], ")")) +
        xlab(imp$unit[i]) +
        ylab("Probability of >= 1 bite") +
        #  xlab("") +
        labs(subtitle = subt) + 
        theme_bw() + 
        theme(axis.title.x = element_text(size = 9),
              axis.title.y = element_text(size = 9),
              axis.text.x = element_text(size = 8),
              axis.text.y = element_text(size = 7),
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
        xlab(imp$unit[i]) +
        #xlab("") +
        labs(subtitle = subt) + 
        theme_bw() + 
        theme(axis.title.x = element_text(size = 8),
              axis.text.x = element_text(size = 7),
              axis.text.y = element_text(size = 6),
              legend.title = element_text(size = 9), 
              legend.text = element_text(size = 8),
              plot.subtitle = element_text(size = 9, face = "bold"))
      
      
      
      
      # pdps[[i]] =  pdp$plot() +
      # scale_y_continuous('', limits = c(0, 1)) +
      # xlab(paste0(imp$label[i], " (", imp$unit[i], ")")) +
      # theme_bw()
      
      
    } else if (indicator == "abundance"){
      
      if(species %in% c("An. funestus","An. coluzzii")){
        y_max = 2
      } else if(species == "An. gambiae ss."){
        y_max = 1
      }
      
      pdps[[i+1]] = pdp$plot() +
        #scale_y_continuous('', limits = c(0, max(df$resp_var))) +
        scale_y_continuous('', limits = c(0, y_max)) +
        #xlab(paste0(imp$label_group[i]," : \n",imp$label[i], " (", imp$unit[i], ")")) +
        xlab(imp$unit[i]) +
        ylab(ifelse(i==1,"log10 (num. bites/human/night)","")) +
        #xlab("") +
        labs(subtitle = subt) + 
        theme_bw() + 
        theme(axis.title.x = element_text(size = 9),
              axis.title.y = element_text(size = 9),
              axis.text.x = element_text(size = 8),
              axis.text.y = element_text(size = 7),
              plot.subtitle = element_text(size = 9, face="bold"))
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
  
  
  # pdps[[length(pdps)+1]] <- pdp_2vars1
  # pdps[[length(pdps)+1]] <- pdp_2vars2
  
  
  precrec_obj <- precrec::evalmod(scores = df_cv$pred, labels = df_cv$obs)
  pcr_curves <- autoplot(precrec_obj, curvetype = "PRC") + ggtitle("")
  roc_curves <- autoplot(precrec_obj, curvetype = "ROC") + ggtitle("")
  evaluation_curves <- pcr_curves + roc_curves
  
  pdps[[length(pdps)+1]] <- bp
  pdps[[length(pdps)+1]] <- cmplot

  pdps <- wrap_plots(pdps) + plot_layout(nrow=n_row,ncol=n_col)
  
  
  #p_final <- (plot_imp  + pdps ) / (plot_interactions ) + plot_annotation(title = paste0(species, " - ", indicator, " (",metric," = ",round(quality,2),")")) + plot_layout(guides = 'auto')
  
  return(list(pdps = pdps, plot_interactions = plot_interactions, plot_interactions_strengths = plot_interactions_strengths, quality = quality, metric = metric, evaluation_curves = evaluation_curves))
  
}



######################
###### maps and univariate analysis######
######################


res <-  readRDS("/home/ptaconet/Bureau/data_analysis/model_results_univanalysis9.rds")


model_univanalysis_results <- res %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus",
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
      filter(buffer %in% c(NA,250,500,1000,2000), name %in% c("WMD","BDE","BCH","WLS_2000","lsm_c_pland_2000_2_5","lsm_c_pland_2000_3_3","lsm_c_pland_2000_3_2","lsm_c_pland_2000_3_4","lsm_c_pland_2000_3_9","lsm_c_pland_2000_3_1","lsm_c_pland_2000_3_5",
                                                          "WLS_1000","lsm_c_pland_1000_2_5","lsm_c_pland_1000_3_3","lsm_c_pland_1000_3_2","lsm_c_pland_1000_3_4","lsm_c_pland_1000_3_9","lsm_c_pland_1000_3_1","lsm_c_pland_1000_3_5",
                                                          "WLS_500","lsm_c_pland_500_2_5","lsm_c_pland_500_3_3","lsm_c_pland_500_3_2","lsm_c_pland_500_3_4","lsm_c_pland_500_3_9","lsm_c_pland_500_3_1","lsm_c_pland_500_3_5",
                                                          "WLS_250","lsm_c_pland_250_2_5","lsm_c_pland_250_3_3","lsm_c_pland_250_3_2","lsm_c_pland_250_3_4","lsm_c_pland_250_3_9","lsm_c_pland_250_3_1","lsm_c_pland_250_3_5")) %>%
  mutate(model = "spearman univariate") %>%
  mutate(indicator = ifelse(indicator == "presence","Presence","Abundance")) %>%
  mutate(buffer = ifelse(is.na(buffer),"2000",buffer)) %>%
  dplyr::rename(pval = p) %>%
  mutate(correlation = ifelse(pval >= 0.2 | abs(correlation)<=0.1, NA, correlation)) %>%
  nest(-c(country))
  


univ_spearman_temporal <-  do.call(rbind.data.frame, model_univanalysis_results$temporal_corrs_spearman) %>% 
  filter(var %in% c("RFD1F", "TMIN1", "TMAX1")) %>%
  mutate(correlation = ifelse(p<=0.2 | correlation >= 0.1,correlation,NA) ) %>%
  mutate(time_lag_1=ifelse(grepl("1",name),time_lag_1/7,time_lag_1),time_lag_2=ifelse(grepl("1",name),time_lag_2/7,time_lag_2),diff_lag=ifelse(grepl("1",name),diff_lag/7,diff_lag)) %>%
  mutate(model = "spearman univariate") %>%
  nest(-c(indicator,country,species,var))


#### spatial univariate


plots_univ_spearman_spatial <- univ_spearman_spatial %>%
  mutate(univ_spatial = pmap(list(data, country), ~fun_plot_tile_univ_spatial(df = ..1,country = ..4))) %>%
  dplyr::select(-data)

###### temporal univariate

plots_univ_spearman_temporal <- univ_spearman_temporal %>%
  arrange(rev(indicator),species,var) %>%
  mutate(univ_temporal = pmap(list(data), ~fun_ccm_plot2(..1, ..1$label[1], time_frame = 1))) %>%
  mutate(species = fct_relevel(species,c("An. funestus","An. gambiae ss.","An. coluzzii"))) %>%
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
    response_var == "ma_funestus_ss" ~ "An. funestus",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(rf_plots = pmap(list(rf_llo,mod,response_var), ~fun_plot_pdp2(..1,..2,..3)))

wrap_plots(pdps$rf_plots[[1]]$pdps, pdps$rf_plots[[4]]$pdps) # funestus
wrap_plots(pdps$rf_plots[[2]]$pdps, pdps$rf_plots[[5]]$pdps) # gambiae ss.
wrap_plots(pdps$rf_plots[[3]]$pdps, pdps$rf_plots[[6]]$pdps) # coluzzii

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


res <-  readRDS("/home/ptaconet/Bureau/data_analysis/model_results_resistances4.rds")

## pdps
pdps_resistances <- res[1,] %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(rf_dayscience_plots = pmap(list(rf_dayscience ,mod,response_var,period_interv), ~fun_plot_pdp2(..1,..2,..3,..4,"dayscience"))) %>%
  mutate(rf_nightscience_plots = pmap(list(rf_nightscience,mod,response_var,period_interv), ~fun_plot_pdp2(..1,..2,..3,..4,"nightscience")))


pmap(list(pdps_resistances$rf_dayscience_plots,pdps_resistances$response_var,pdps_resistances$code_pays,pdps_resistances$mod,pdps_resistances$period_interv),~ggsave(paste(..2,..3,..4,..5,"dayscience",sep="_"),..1$pdps,"png","plots_resistance"))
pmap(list(pdps_resistances$rf_nightscience_plots,pdps_resistances$response_var,pdps_resistances$code_pays,pdps_resistances$mod,pdps_resistances$period_interv),~ggsave(paste(..2,..3,..4,..5,"nightscience",sep="_"),..1$pdps,"png","plots_resistance"))


pmap(list(pdps_resistances3$rf_dayscience_plots,pdps_resistances3$rf_nightscience_plots,pdps_resistances3$response_var,pdps_resistances3$code_pays,pdps_resistances3$mod,pdps_resistances3$period_interv),~ggsave(paste(..3,..4,..5,..6,sep="_"),wrap_plots(..1$pdps,..2$pdps,ncol=2,nrow=1),"png","plots_resistance"))


wrap_plots(pdps_resistances$rf_dayscience_plots[[1]]$pdps, pdps_resistances$rf_nightscience[[2]]$pdps, nrow = 1, ncol = 2)


# plots for models validation
model_validation <- res %>%
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(df_cv = map(rf_nightscience, ~pluck(.,"df_cv"))) %>%
  dplyr::select(response_var, code_pays, mod, df_cv) %>%
  mutate(df_cv = map2(df_cv,response_var, ~mutate(.x, species = .y))) %>%
  nest(-c(mod,code_pays))

model_validation <- model_validation %>%
  mutate(df_cv2 = map(data, ~do.call(rbind.data.frame, .$df_cv)))

plots_validation_exophagy <- model_validation$df_cv2 %>%
  nest(-c(species)) %>%
  mutate(plots_validation = map2(data, species, ~fun_plot_validation_presence(..1,..2)))







## glmm

res = res[-12,]
glmm_plots <- res %>% 
  mutate(response_var = case_when(
    response_var == "ma_funestus_ss" ~ "An. funestus",
    response_var == "ma_gambiae_ss" ~ "An. gambiae ss.",
    response_var == "ma_coluzzi" ~ "An. coluzzii")) %>%
  mutate(glmm_multiv = map2(glmm, mod, ~broom.mixed::tidy(.x$mod@model, conf.int = TRUE, exponentiate = ifelse(.y == "abundance", FALSE, TRUE)))) %>%
  mutate(glmm_multiv = map(glmm_multiv, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(glmm_univ = map(glmms_univs, ~fun_get_predictors_labels(table = ., vector_predictors_name = "term"))) %>%
  mutate(glmm_multiv_plot = pmap(list(glmm_multiv,mod,response_var,period_interv), ~fun_glmm_dotpoint(..1,..2,..3,..4))) %>%
  mutate(glmm_univ_plot = pmap(list(glmm_univ,mod,response_var,period_interv), ~fun_glmm_dotpoint(..1,..2,..3,..4)))
 
pmap(list(glmm_plots$glmm_univ_plot,glmm_plots$glmm_multiv_plot,glmm_plots$response_var,glmm_plots$code_pays,glmm_plots$mod,glmm_plots$period_interv),~ggsave(paste(..3,..4,..5,..6,"glmm",sep="_"),wrap_plots(..1, ..2, nrow = 1, ncol = 2) ,"png","plots_resistance"))

wrap_plots(glmm_plots$glmm_univ_plot[[1]], glmm_plots$glmm_multiv_plot[[1]], nrow = 1, ncol = 2) 

# get rsquared from the multivariate models
df_mod=res$glmm[[1]]$mod@model$frame
r2 = MuMIn::r.squaredGLMM(res$glmm[[1]]$mod@model)
