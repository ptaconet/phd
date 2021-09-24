


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


fun_glmm_dotpoint <- function(glmm_model_tabversion, mod, species, period_interv, auc = NA, univ_multiv){
  
  
  glmm_model_tabversion <- glmm_model_tabversion %>%
    mutate(label = ifelse(grepl(":",term),term,label)) %>%
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
    ))
  
  if(nrow(glmm_model_tabversion)>0){
    glmm_model_tabversion <- glmm_model_tabversion %>%
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
      geom_point(size = 1) + 
      geom_errorbar(aes(xmin=conf.low, xmax=conf.high), width=.2, position=position_dodge(.9)) +
      geom_text(size = 2, vjust = -.7, hjust = 0.08) + 
      theme(axis.text.y = element_text(size = 8, colour = "black"), 
            legend.position = "none", strip.text.y = element_text(size = 8), 
            plot.title = element_text(size = 10),
            axis.title.x = element_text(size = 8)) + #, axis.text.y = element_text(vjust = -0.7, margin = margin(l = 20, r = -100))) + axis.text.y=element_blank(), 
      xlab(ifelse(mod=="abundance","Incidence rate-ratio","Odd ratio"))  +
      ylab("") +
      scale_color_manual(values=c("darkorchid2", "darkorange", "green")) +
      geom_vline(aes(xintercept = ifelse(mod=="abundance", 0, 1)),linetype = "dashed")
    
  } else {
    
    plot <- ggplot() +
      theme_void() +
      geom_text(aes(0,0,label='no variable improving the NULL model'))
  }
  
  if(univ_multiv == "multivariate"){
    plot <- plot +  labs(subtitle = paste0(mod,"\n",species,"\n",period_interv,"\n",univ_multiv,"\nAUC = ",round(auc,2)))
  } else {
    plot <- plot +  labs(subtitle = paste0(mod,"\n",species,"\n",period_interv,"\n",univ_multiv))
  }
  
  plot <- plot +
    theme(axis.title.x = element_text(size = 6, colour = "grey30"),
          axis.title.y = element_text(size = 8),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 5),
          plot.subtitle = element_text(size = 7, face="bold"))
  
  return(plot)
}


fun_plot_tile_univ_spatial <- function(df, metric_name = "Spearman correlation", country, indicator2 = NULL){
  
  df <- df %>%
    mutate(buffer = forcats::fct_relevel(buffer, c("250","500","1000","2000"))) %>%
    mutate(indicator = forcats::fct_relevel(indicator, c("Presence","Abundance"))) %>%
    mutate(species = forcats::fct_relevel(species, c("An. funestus","An. gambiae s.s.","An. gambiae s.l.","An. coluzzii")))
  
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
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10, face = "italic")
    ) +
    geom_text(aes(label = ifelse(is.na(correlation), "",paste(round(correlation,2), p.value2))), size = 3)
  
  
  if(metric_name == "glmm"){
    if(indicator2 == "Presence"){
      p <- p + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 1, space = "Lab", name = metric_name, na.value="grey")
    } else if (indicator2 == "Abundance"){
      p <- p + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, space = "Lab", name = metric_name, na.value="grey")
    }
  } else if (metric_name == "Spearman correlation"){
    p <- p + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-0.7,0.7), space = "Lab", name = metric_name, na.value="grey")
  }
  
  return(p)
}



# function to plot the CCM (simple plot : only the CCM)
fun_ccm_plot2 <- function(correlation_df, var, time_frame,   metric_name, indicator){
  
  if(metric_name == "glmm"){
    if(indicator=="presence"){
    correlation_df$abs_corr <- abs(correlation_df$correlation - 1 )
    } else if (indicator=="abundance"){
      correlation_df$abs_corr <- abs(correlation_df$correlation)
    }
  }
  
  if(!is.na(unique(correlation_df$abs_corr)[1])){ # to deal with case all correlation values are NAs
  most_corr <- correlation_df %>% filter(abs_corr == max(abs_corr, na.rm = T))
  
  #most_corr2 <- correlation_df %>% arrange(desc(abs_corr)) %>% head(round(0.1*nrow(.))) # 3 % top correlations will be blacked borders
  #most_corr2 <- correlation_df %>% arrange(desc(abs_corr)) %>% filter(abs_corr >= most_corr$abs_corr - 0.05)
  
  most_corr2 <- correlation_df %>% arrange(desc(abs_corr)) %>% filter(abs_corr >= most_corr$abs_corr * 0.9)
  } else {
    most_corr <- most_corr2 <- correlation_df[1,]
  }
  
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
    xlab("time lag 2")
  #scale_x_continuous(breaks = seq(0,8,2), labels = seq(0,40,10)) +
  #scale_y_continuous(breaks = seq(0,8,2), labels = seq(0,40,10))
  
  
  if(metric_name=="Spearman"){
    ccm_plot <- ccm_plot + 
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-.71,.71), space = "Lab", name = "Spearman correlation", na.value = "grey") #+
  } else if(metric_name=="glmm"){
    ccm_plot <- ccm_plot + 
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = ifelse(indicator == "abundance", 0, 1), space = "Lab", name = ifelse(indicator == "abundance","DDR","ODR"),
                           # limit = c(correlation_df$min[1], correlation_df$max[1])) 
                           #limit = c(-1.5, 1.5)
                           ) 
  }
  
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
  
  if(mod != "abundance"){
    y_lab = "∑ Presence probability"
  } else {
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
          # th_auc <- MLmetrics::AUC(y_true = th_df_cv$obs,y_pred = pred_class) 
          th_auc <- MLmetrics::PRAUC(y_true = th_df_cv$obs,y_pred = pred_class) 
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


fun_compute_perf_metric_predictive <- function(df_cv){
  
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
  
  

    ## find threshold
  
      thresholds = seq(0, 1, 0.001)
      aucs_thresholds <- NULL
      for(i in 1:length(thresholds)){
        
        pred_class <- ifelse(df_cv$pred < thresholds[i],0,1)
        if(length(unique(pred_class))==1){
          th_auc <- NA
        } else {
          # th_auc <- MLmetrics::F1_Score(y_true = df_cv$obs,y_pred = pred_class, positive = "1")
          th_auc <- MLmetrics::AUC(y_true = df_cv$obs,y_pred = pred_class)  #  =====> pour les modèles prédictifs
          #th_auc <- MLmetrics::PRAUC(y_true = df_cv$obs,y_pred = pred_class)      =====> pour les modèles explicatifs
          #th_auc <- MLmetrics::Specificity(y_true = df_cv$obs,y_pred = pred_class)
        }
        
        aucs_thresholds <-c(aucs_thresholds, th_auc)
        
      }
      
      max_metric <- max(aucs_thresholds, na.rm = T)
      threshold <- thresholds[which.max(aucs_thresholds)]
      
      df_cv$pred2 <- ifelse(df_cv$pred < threshold, 0, 1)
      
      df_cv$threshold <- threshold
      df_cv$max_metric <- max_metric
      
    
      sensitivity = sens(df_cv$obs,df_cv$pred2)
      specificity = spec(df_cv$obs,df_cv$pred2)
      precision = prec(df_cv$obs,df_cv$pred2)
      recall = reca(df_cv$obs,df_cv$pred2)
      f1score = f1(df_cv$obs,df_cv$pred2)
      ROC_AUC = rocauc(df_cv$obs,df_cv$pred2)
      PR_AUC = prauc(df_cv$obs,df_cv$pred2)
  
  # df <- df %>%
  #   complete(species, nesting( code_pays)) %>%  
  #   nest(-species)
  
  
  return(list(df_cv = df_cv,
              sensitivity=sensitivity,
              specificity=specificity,
              precision=precision,
              recall=recall,
              f1score=f1score,
              ROC_AUC=ROC_AUC,
              PR_AUC=PR_AUC))
  
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
  
  
  fun_map <- function(code_pays,tit){
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
    filter(codepays == code_pays) %>%
   # filter(pcr_espece %in% c( "An.funestus_ss", "An.coluzzii", "An.gambiae_ss"), nummission <10) %>%
  #  group_by(nummission, codepays, codevillage, postedecapture,pcr_espece) %>%
    filter(especeanoph %in% c("An.funestus", "An.gambiae s.l.")) %>%
    group_by(nummission, codepays, codevillage, postedecapture,especeanoph) %>%
    summarise(n=n()) %>%
    pivot_wider(names_from = postedecapture, values_from = n) %>%
    mutate_all(funs(replace_na(., 0))) %>%
    mutate(tot = e+i) %>%
    mutate(exophagy = e / tot * 100) %>%
    mutate(exophagy = ifelse(is.na(exophagy),0,exophagy)) %>%
    mutate(nummission = as.character(nummission)) %>%
    left_join(mean_date_mission) %>%
    left_join(mean_coords_points_4326 %>% group_by(codevillage) %>% summarise(X_4326 = mean(X4326),Y_4326 = mean(Y4326))) %>%
    filter(tot>5)
  
  if(code_pays=="BF"){
    df <- df %>%
      dplyr::select(codevillage,nummission,date_mission_char,X4326,Y4326,ma_funestus_ss,ma_gambiae_ss,ma_coluzzi) %>%
      pivot_longer(c(ma_funestus_ss,ma_gambiae_ss,ma_coluzzi)) %>%
      mutate(name = case_when(
        name == "ma_funestus_ss" ~ "An. funestus",
        name == "ma_gambiae_ss" ~ "An. gambiae s.s.",
        name == "ma_coluzzi" ~ "An. coluzzii")) %>%
      mutate(species = forcats::fct_relevel(name,c("An. gambiae s.s.","An. coluzzii","An. funestus")))
  } else if (code_pays=="CI"){
    df <- df %>%
      dplyr::select(codevillage,nummission,date_mission_char,X4326,Y4326,ma_funestus_ss,ma_gambiae_sl) %>%
      pivot_longer(c(ma_funestus_ss,ma_gambiae_sl)) %>%
      mutate(name = case_when(
        name == "ma_funestus_ss" ~ "An. funestus",
        name == "ma_gambiae_sl" ~ "An. gambiae s.l.")) %>%
      mutate(species = forcats::fct_relevel(name,c("An. gambiae s.l.","An. funestus"))) 
  }
  
  
  df_map <- df %>%
    group_by(codevillage,nummission, date_mission_char,species) %>%
    summarise(Biting.rate = mean(value), X = mean(X4326), Y = mean(Y4326)) %>%
    mutate(Biting.rate = ifelse(Biting.rate==0,NA,Biting.rate))
  
  villages <- dbReadTable(react_gpkg, 'recensement_villages_l1') %>%
    dplyr::select(-geom) %>%
    dplyr::filter(!is.na(intervention),codepays == code_pays)
  
  
  m <- ggmap(myMap) + geom_point(aes(x = X, y = Y), data = villages, size = 0.6, color = "seagreen4") + 
    geom_point(aes(x = X, y = Y, size = Biting.rate), data = df_map, colour = "darkred") + 
    facet_grid(species~date_mission_char) + 
    theme(strip.text.y = element_text(size = 8, face = "bold"),
          strip.text.x = element_text(size = 7),
          axis.title.x = element_text(size = 6),
          axis.title.y = element_text(size = 8),
          axis.text.x = element_text(size = 4),
          axis.text.y = element_text(size = 4),
          legend.title = element_text(size = 7),
          legend.text = element_text(size = 6)) + 
    ggtitle(ifelse(code_pays=="BF","","Biting rate of the main vectors in each village and entomological survey")) + 
    labs(subtitle = tit, caption = ifelse(code_pays=="CI","","Unit: average number of bites / human / night. Blue dots indicate absence of bite in the village for the considered survey. Background layer: OpenStreetMap"))
  # ggsn::scalebar(dist = 5, dist_unit = "km",
  #                transform = TRUE, model = "WGS84", 
  #                height = 0.01, st.size	=2.5,st.dist = 0.03,
  #                x.min = myLocation[1], x.max = myLocation[2] , y.min = myLocation[3],y.max = myLocation[4],
  #                border.size = 0.5
  # )
  
  return(m)
  }
  
  m_ci <- fun_map("CI","Korhogo (IC)")
  m_bf <- fun_map("BF","Diébougou (BF)")
  

  ggsave(filename="add_bitingrates_ci.png",plot=m_ci,path="/home/ptaconet/phd/articles/article2", width = 170,unit="mm",dpi = 600)
  ggsave(filename="add_bitingrates_bf.png",plot=m_bf,path="/home/ptaconet/phd/articles/article2", width = 170,unit="mm",dpi = 600)
  
  
  
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





fun_glmm_model_check <- function(glmm){
  
  if(!is.null(glmm)){
    library(DHARMa)
    glmm_mod = glmm$mod@model
    df_mod = glmm$mod@model$frame
      simulationOutput <- simulateResiduals(fittedModel = glmm_mod, n = 1000)
      plot(simulationOutput)
    p <- recordPlot()
    plot.new()
  } else {
    p <- ggplot() +theme_void() +geom_text(aes(0,0,label='N/A')) 
  }
  
  return(p)
  
}






fun_plot_pdp2 <- function(modell, indicator, species, period_interv, glmm_univ_plot = NULL, glmm_multiv_plot = NULL){
  
  library(iml)
  library("future")
  library("future.callr")
  model = modell$mod
  df = modell$df_mod %>% dplyr::select(resp_var, model$finalMode$xNames)
  df_cv <-  modell$df_cv
  
  max_metric = NULL
  ## find threshold
  if(indicator %in% c("presence","exophagy","early_biting","late_biting","early_late_biting","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1")){
    thresholds = seq(0, 1, 0.001)
    aucs_thresholds <- NULL
    auc <- MLmetrics::AUC(y_true = df_cv$obs,y_pred = df_cv$pred)
    
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
    mod <- Predictor$new(model, data = df, class = ifelse(indicator!="physiological_resistance_kdrw","Presence","Absence"))
    metric <- "ROC_AUC"
    quality = max(modell$mod$results$ROC_AUC)
  } else if (indicator == "abundance_discrete"){
    mod <- Predictor$new(model, data = df)
    metric <- "Accuracy"
    quality = max(modell$mod$results$Accuracy)
  } else if(indicator %in% c("exophagy_reg","early_biting_reg","late_biting_reg","early_late_biting_reg","physiological_resistance_kdrw_reg","physiological_resistance_kdre_reg","physiological_resistance_ace1_reg")){
    mod <- Predictor$new(model, data = df)
    metric <- "Rsquared"
    quality = max(modell$mod$results$Rsquared)
  }
  
  
  if(indicator %in% c("presence","exophagy","early_biting","late_biting","early_late_biting","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1")){
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
      labs(subtitle = paste0("Boxplot of AUCs.\n n. folds = ",nrow(data_boxplot_auc))) + 
      theme(legend.position = "none")
    
    # confusion matrix
    
    df_cv_cm <- df_cv %>% mutate(obs = ifelse(obs == 0, "Absence","Presence")) %>% mutate(pred = ifelse(pred < threshold,"Absence","Presence")) %>% mutate_if(is.character, as.factor)
    
    cm <- caret::confusionMatrix(data = df_cv_cm$pred, reference = df_cv_cm$obs)$table
    table <- data.frame(cm)
    
    if(indicator == "physiological_resistance_kdrw"){
      table <- table %>% mutate(Prediction=ifelse(Prediction=="Absence","Presence","Absence")) %>% mutate(Reference=ifelse(Reference=="Absence","Presence","Absence")) %>% mutate_if(is.character,as.factor)
    }
    
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
      theme(legend.position = "none") +
      labs(subtitle = paste0("CM thresh. = ",round(threshold,2))) 
    
    
    
    
    
    # confusion matrix
    cm <- caret::confusionMatrix(data = model$pred$pred, reference = model$pred$obs)$table
    table <- data.frame(cm)
    if(indicator == "physiological_resistance_kdrw"){
      table <- table %>% mutate(Prediction=ifelse(Prediction=="Absence","Presence","Absence")) %>% mutate(Reference=ifelse(Reference=="Absence","Presence","Absence")) %>% mutate_if(is.character,as.factor)
    }
    table <- table %>% arrange(-row_number())
    plotTable <- table %>%
      mutate(goodbad = ifelse(table$Prediction == table$Reference, "good", "bad")) %>%
      group_by(Reference) %>%
      mutate(prop = Freq/sum(Freq))
    # fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups (see dplyr code above as well as original confusion matrix for comparison)
    cmplot2 <- ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
      geom_tile() +
      geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
      scale_fill_manual(values = c(good = "green", bad = "red")) +
      theme_bw() +
      ylim(rev(levels(table$Reference))) +
      theme(legend.position = "none") +
      labs(subtitle = "CM thresh. = 0.5") 
    
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
    geom_bar(position = 'dodge', stat="identity", width = 0.6) + 
    theme_bw() + 
    geom_text(size=1.6,position = position_dodge(0.9),hjust=-0.1,label.padding = unit(0.2, "lines")) + #,aes(fontface=2)
    #   geom_label(size=2, aes(fontface=2), label.padding = unit(0.15, "lines"), x = 0.05, alpha = 0.5) +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_text(size = 7),
          plot.subtitle = element_text(size = 7, face="bold")
    ) +
    ylab("") + 
    xlab("") +
    xlim(NA,max(imp$importance, na.rm = T) + max(imp$importance, na.rm = T)*2.5) +
    labs(subtitle = paste0(gsub("physiological_resistance_","",indicator),"\n",species,"\ntimeframe = ",period_interv,"\nAUC = ",ifelse(indicator %in% c("presence","exophagy","early_biting","late_biting","early_late_biting","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1"),round(max_metric,2),""),"AUC2 = ",ifelse(indicator %in% c("presence","exophagy","early_biting","late_biting","early_late_biting","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1"),round(auc,2)),"\nVariable importance"))
  # labs(subtitle = "Variable importance")
  
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
    p <- predict(object, newdata = newdata, type ="prob")[,ifelse(indicator!="physiological_resistance_kdrw","Presence","Absence")]
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
  
  if(!is.null(glmm_multiv_plot)){
    pdps[[1]] <- glmm_univ_plot
    pdps[[2]] <- glmm_multiv_plot
    l_pdp <- 3
  } else {
    l_pdp <- 1
  }
  
  pdps[[length(pdps)+1]] <- plot_imp
  
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
                                               imp$var[grep("3_3",imp$var)],
                                               imp$var["NMT"],
                                               imp$var["NMH"],
                                               imp$var["NMA"],
                                               imp$var["RFH"],
                                               imp$var["WSP"],
                                               imp$var["LMN"]))) %>%
      #dplyr::filter(!(var %in% c("VCM","int_ext"))) %>%
      arrange(var) %>%
      mutate(var = as.character(var))
    
    nmax_vars_pdp <- nrow(imp)
    
    n_row=6#5
    n_col=5#4
  } else if (indicator %in% c("exophagy","early_late_biting","early_biting","late_biting","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1",
                              "exophagy_reg","early_late_biting_reg","early_biting_reg","late_biting_reg","physiological_resistance_kdrw_reg","physiological_resistance_kdre_reg","physiological_resistance_ace1_reg")){
    #nmax_vars_pdp <- 8
    nmax_vars_pdp <- length(imp$var)
    
    if(length(imp$var) < nmax_vars_pdp){
      nmax_vars_pdp <- length(imp$var)
    }
    
    n_row=1
    n_col=nmax_vars_pdp+1
    
    n_row=6
    n_col=5
    
  }
  
  plan("callr", workers = 7)
  future::plan(multisession, workers = 7)
  
  for(i in 1:nmax_vars_pdp){
    
    pdp = FeatureEffect$new(mod, imp$var[i], method = "pdp")
    
    subt <- imp$label_detail[i]
    if(subt =="Grassland\n2000 m buffer" & species == "An. funestus"){
      subt <- gsub("2000","250",subt)
    }
    if(grepl("b/w 0 and 4.28571428571429 weeks",subt)){
      subt <- gsub("b/w 0 and 4.28571428571429 weeks","(month preceding)",subt)
    }
    if(grepl("b/w 0 and 0 weeks",subt)){
      subt <- gsub("b/w 0 and 0 weeks","(day of catch)",subt)
    }
    # if(!is.na(imp$unit[i])){
    #   subt <- paste0(subt," (", imp$unit[i], ")")
    # }
    
    if(indicator %in% c("presence","exophagy","early_late_biting","early_biting","late_biting","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1")){
      
      pdps[[i+l_pdp]] = pdp$plot() +
        scale_y_continuous('', limits = c(0, 1)) +
        #xlab(paste0(imp$label_group[i]," : \n",imp$label[i], " (", imp$unit[i], ")")) +
        xlab(imp$unit[i]) +
        ylab("Probability of >= 1 bite") +
        #  xlab("") +
        labs(subtitle = subt) + 
        theme_bw() + 
        theme(axis.title.x = element_text(size = 6, colour = "grey30"),
              axis.title.y = element_text(size = 8),
              axis.text.x = element_text(size = 6),
              axis.text.y = element_text(size = 5),
              plot.subtitle = element_text(size = 7, face="bold"))
      
    } else if (indicator == "abundance_discrete"){
      
      pdp$results <- pdp$results %>%
        dplyr::mutate(.class = case_when(.class == "high....5.bites." ~ "high (> 5 bites)",
                                         .class == "low..1.bite." ~ "low (1 bite)",
                                         .class == "medium..between.1.and.5.bites." ~ "medium (b/w 1 and 5 bites)")) %>%
        mutate(.class = fct_relevel(.class,c("low (1 bite)","medium (b/w 1 and 5 bites)","high (> 5 bites)"))) %>%
        dplyr::rename(biting_rate = .class)
      
      pdps[[i+l_pdp]] = ggplot(pdp$results,aes_string(x = imp$var[i], y = ".value")) +
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
      
      
    } else if (indicator %in% c("abundance","exophagy_reg","early_late_biting_reg","early_biting_reg","late_biting_reg","physiological_resistance_kdrw_reg","physiological_resistance_kdre_reg","physiological_resistance_ace1_reg")){
      
      if(species %in% c("An. funestus","An. coluzzii")){
        y_max = 2
      } else if(species == "An. gambiae ss."){
        y_max = 1
      }
      
      pdps[[i+l_pdp]] = pdp$plot() +
        #scale_y_continuous('', limits = c(0, max(df$resp_var))) +
        scale_y_continuous('', limits = c(0, y_max)) +
        #xlab(paste0(imp$label_group[i]," : \n",imp$label[i], " (", imp$unit[i], ")")) +
        xlab(imp$unit[i]) +
        ylab(ifelse(i==1,"log10 (num. bites/human/night)","")) +
        #xlab("") +
        labs(subtitle = subt) + 
        theme_bw() + 
        theme(axis.title.x = element_text(size = 6, colour = "grey30"),
              axis.title.y = element_text(size = 8),
              axis.text.x = element_text(size = 6),
              axis.text.y = element_text(size = 5),
              plot.subtitle = element_text(size = 7, face="bold"))
    }
    
    if(indicator %in% c("presence","exophagy","early_late_biting","early_biting","late_biting","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1") & !is.factor(df[,imp$var[i]])){
      pdp$results$.value <- pdp$results$.type <- NULL
      
      if(indicator %in% c("presence","exophagy","early_late_biting","early_biting","late_biting","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1")){
        p = pdp::partial(model, pred.var = imp$var[i], pred.fun = pred_wrapper_classif, train = df, pred.grid = pdp$results)
      } else {
        p = pdp::partial(model, pred.var = imp$var[i], pred.fun = pred_wrapper_reg, train = df, pred.grid = pdp$results)
      }
      
      p$yhat[which(p$yhat<0)] <-0 
      
      p$yhat.id <- as.factor(p$yhat.id)
      
      pdps[[i+l_pdp]]$layers[[1]] <- NULL
      pdps[[i+l_pdp]] <-  pdps[[i+l_pdp]] +
        geom_line(data = p[p$yhat.id == "avg", ], aes_string(x = imp$var[i], y = "yhat"), size = 0.5, colour = "black") +
        geom_line(data = p[p$yhat.id != "avg", ], aes_string(x = imp$var[i], y = "yhat", group = "yhat.id"), linetype = "dashed", size = 0.25)
    }
    
  }
  
  # var = "NMA"
  # library(jtools)
  # p = jtools::effect_plot(model@model, pred = !!var, interval = TRUE)
  # dat = ggplot_build(p)$data
  # dat[[1]]$x = dat[[1]]$x + attr(model@model$frame[,var],'scaled:center')
  # dat[[2]]$x = dat[[2]]$x + attr(model@model$frame[,var],'scaled:center')
  # 
  # p1=ggplot() +  geom_line(data = dat[[1]], aes(x = x, y =y)) + 
  #   geom_ribbon(data = dat[[2]], aes(x = x, y =y, ymin = ymin, ymax = ymax), alpha = 0.2, fill = "grey20") + ylim(0,1)
  # 
  # 
  # pdp = FeatureEffect$new(mod, var, method = "pdp")$results
  # pdp[,var] = pdp[,var] + attr(model@model$frame[,var],'scaled:center')
  # pdp$.value = arm::invlogit(pdp$.value)
  # p2=ggplot() +  geom_line(data = pdp, aes_string(x = var, y =".value")) + ylim(0,1)
  #   
  #precrec_obj <- precrec::evalmod(scores = df_cv$pred, labels = df_cv$obs)
  #pcr_curves <- autoplot(precrec_obj, curvetype = "PRC") + theme_bw()+ theme(legend.position = "none",plot.title = element_blank())
  #roc_curves <- autoplot(precrec_obj, curvetype = "ROC") + theme_bw()+ theme(legend.position = "none",plot.title = element_blank()) 
  
  rocplot <- ggplot(df_cv, aes(m = pred, d = obs))+ geom_roc(n.cuts=20,labels=FALSE) + style_roc(theme = theme_grey, xlab = "", ylab = "") + geom_rocci(fill="pink")
  
  
   pdps[[length(pdps)+1]] <- bp
   pdps[[length(pdps)+1]] <- cmplot
   pdps[[length(pdps)+1]] <- cmplot2
   pdps[[length(pdps)+1]] <- rocplot
  
  
  
  pdps =  wrap_plots(pdps,nrow=n_row,ncol=n_col)
  
  #p_final <- (plot_imp  + pdps ) / (plot_interactions ) + plot_annotation(title = paste0(species, " - ", indicator, " (",metric," = ",round(quality,2),")")) + plot_layout(guides = 'auto')
  
  return(list(pdps = pdps, plot_interactions = plot_interactions, plot_interactions_strengths = plot_interactions_strengths, quality = quality, metric = metric, auc = max_metric))
  
}







  # rf=res$rf[[3]]
  # rf_selectvar=res$rf_selectvar[[3]]
  # rf_allpredictors=res$rf_allpredictors[[3]]
  # glmm=res$glmm[[3]]
  # glmm_aic=res$glmm_aic[[1]]

fun_plot_pdp3 <- function(rf,rf_selectvar,rf_allpredictors, glmm, glmm_aic, indicator, species, code_pays){
  
  library(iml)
  library(vip)
  library(jtools)
  library(MLmetrics)
  library(precrec)
  library(pdp)
  library(caret)
  library(ranger)
  library(glmmTMB)
  library(broom.mixed)
  library(gridExtra)
  library(cowplot)
  
  auc_glmm <- NA
  auc_glmm_aic <- NA
  auc_rf <- NA
  auc_rf_selectvar <- NA
  auc_rf_allpredictors <- NA
  
  prauc_glmm <- NA
  prauc_glmm_aic <- NA
  prauc_rf <- NA
  prauc_rf_selectvar <- NA
  prauc_rf_allpredictors <- NA

  rf_allpredictors_mod = rf_allpredictors$mod
  df_rf_allpredictors = rf_allpredictors$df_mod %>% dplyr::select(resp_var, rf_allpredictors_mod$finalMode$xNames)
  df_cv_rf_allpredictors <-  rf_allpredictors$df_cv
  
  auc_rf_allpredictors <- AUC(y_true = df_cv_rf_allpredictors$obs,y_pred = df_cv_rf_allpredictors$pred)
  prauc_rf_allpredictors <- PRAUC(y_true = df_cv_rf_allpredictors$obs,y_pred = df_cv_rf_allpredictors$pred)
  
  
  if(!is.null(rf)){
  
    rf_mod = rf$mod
    df_rf = rf$df_mod %>% dplyr::select(resp_var, rf_mod$finalMode$xNames)
    df_cv_rf <-  rf$df_cv
    
    auc_rf <- AUC(y_true = df_cv_rf$obs,y_pred = df_cv_rf$pred)
    prauc_rf <- PRAUC(y_true = df_cv_rf$obs,y_pred = df_cv_rf$pred)
    
    if(!is.null(glmm)){
    glmm_mod = glmm$mod
    df_glmm = glmm$mod@model$frame
    df_cv_glmm <-  glmm$df_cv
    
    auc_glmm <- AUC(y_true = df_cv_glmm$obs,y_pred = df_cv_glmm$pred)
    prauc_glmm <- PRAUC(y_true = df_cv_glmm$obs,y_pred = df_cv_glmm$pred)
    }
    
    if(!is.null(glmm_aic)){
    glmm_aic_mod = glmm_aic$mod
    df_glmm_aic = glmm_aic$mod@model$frame
    df_cv_glmm_aic <-  glmm_aic$df_cv
    
    auc_glmm_aic <- AUC(y_true = df_cv_glmm_aic$obs,y_pred = df_cv_glmm_aic$pred)
    prauc_glmm_aic <- PRAUC(y_true = df_cv_glmm_aic$obs,y_pred = df_cv_glmm_aic$pred)
    }
    
    if(!is.null(rf_selectvar)){
    rf_selectvar_mod = rf_selectvar$mod
    df_rf_selectvar = rf_selectvar$df_mod %>% dplyr::select(resp_var, rf_selectvar_mod$finalMode$xNames)
    df_cv_rf_selectvar <-  rf_selectvar$df_cv
    
    auc_rf_selectvar <- AUC(y_true = df_cv_rf_selectvar$obs,y_pred = df_cv_rf_selectvar$pred)
    prauc_rf_selectvar <- PRAUC(y_true = df_cv_rf_selectvar$obs,y_pred = df_cv_rf_selectvar$pred)
    }
    
    # get predictors labels
    df_vars_labs <- data.frame(var = rf_allpredictors_mod$finalMode$xNames)
    df_vars_labs <- fun_get_predictors_labels(df_vars_labs,"var") %>% distinct()
    df_vars_labs <- df_vars_labs %>%
      mutate(label_detail = gsub("b/w 0 and 4.28571428571429 weeks","month preceding collection",label_detail)) %>%
      mutate(label_detail = gsub("b/w 0 and 0 weeks","day of collection",label_detail))
    
    if(indicator == "late_biting"){
      df_vars_labs <- df_vars_labs %>%
        mutate(label_detail = gsub("day of collection","day preceding collection",label_detail)) %>%
        mutate(label_detail = gsub("hour of collection","night of collection",label_detail))
    }

    # get glmm in tidy form
    if(!is.null(glmm)){
    glmm_tidy <- tidy(glmm_mod@model, conf.int = TRUE, exponentiate = ifelse(indicator == "abundance", FALSE, TRUE))
    glmm_tidy <- glmm_tidy %>%
      mutate(p.value2 = case_when(
        p.value <= 0.001 ~ "***",
        p.value > 0.001 & p.value <= 0.01  ~  "**",
        p.value > 0.01 & p.value <= 0.05 ~ "*",
        p.value > 0.05 ~ ""
      ))
    }

    
    baseline_prauc <- df_cv_rf %>% group_by(obs) %>% summarise(n=n())
    baseline_prauc <- baseline_prauc$n[which(baseline_prauc$obs==1)]/sum(baseline_prauc$n)
    # open iml models
    #rf_mod_iml <- Predictor$new(rf_mod, data = df_rf, class = ifelse(indicator!="physiological_resistance_kdrw","Presence","Absence"))
    #rf_mod_allvars_iml <- Predictor$new(rf_allvars_mod, data = df_rf_allvars, class = ifelse(indicator!="physiological_resistance_kdrw","Presence","Absence"))
    
    # draw ROC curves            
    if(!is.null(glmm)){
      scores =  list(df_cv_rf$pred, df_cv_rf_allpredictors$pred, df_cv_rf_selectvar$pred,df_cv_glmm$pred,df_cv_glmm_aic$pred)
      modnames = c("RF","RF all preds","RF feat. select." ,"GLMM LRT","GLMM AIC")
      cols = c("#009E73", "#E69F00","black","red","grey")
    } else {
      scores =  list(df_cv_rf$pred, df_cv_rf_allpredictors$pred)
      modnames = c("RF","RF all preds")
      cols = c("#009E73", "#E69F00")
      
    }
    
    
    samps <- list(scores = scores, labels = df_cv_rf$obs,  modnames = modnames)
    mdat <- mmdata(samps[["scores"]], samps[["labels"]],modnames = samps[["modnames"]])
    m <- evalmod(mdat)
    p = autoplot(m, curvetype = "ROC") +
      theme(plot.title = element_blank(),
            axis.title.x = element_text(size = 7),
            axis.title.y = element_text(size = 7),
            axis.text.y = element_text(size = 7),
            axis.text.x = element_text(size = 7),
            legend.position=c(0.7, 0.2),
            legend.text = element_text(size = 7),
            legend.background=element_rect(fill = alpha("white", 0)),
            legend.key.size = unit(0.2, "cm")
      ) +
      scale_color_manual(values=cols) +
      annotate("text", x = 0.35, y = 0.95, label = paste0("AUC GLMM LRT= ",round(auc_glmm,2)), size = 2.5) +
      annotate("text", x = 0.35, y = 0.85, label = paste0("AUC GLMM AIC= ",round(auc_glmm_aic,2)), size = 2.5) +
      annotate("text", x = 0.35, y = 0.75, label = paste0("AUC RF = ",round(auc_rf,2)), size = 2.5) + 
      annotate("text", x = 0.35, y = 0.65, label = paste0("AUC RF feat. select. = ",round(auc_rf_selectvar,2)), size = 2.5) +
      annotate("text", x = 0.35, y = 0.55, label = paste0("AUC RF all preds = ",round(auc_rf_allpredictors,2)), size = 2.5)
    
    
    p2 = autoplot(m, curvetype = "PRC") +
      theme(plot.title = element_blank(),
            axis.title.x = element_text(size = 7),
            axis.title.y = element_text(size = 7),
            axis.text.y = element_text(size = 7),
            axis.text.x = element_text(size = 7),
            legend.position=c(0.7, 0.2),
            legend.text = element_text(size = 7),
            legend.background=element_rect(fill = alpha("white", 0)),
            legend.key.size = unit(0.2, "cm")
      ) +
      scale_color_manual(values=cols) +
      annotate("text", x = 0.35, y = 0.95, label = paste0("PRC baseline = ",round(baseline_prauc,2)), size = 2.5) +
      annotate("text", x = 0.35, y = 0.85, label = paste0("PRC GLMM LRT= ",round(prauc_glmm,2)), size = 2.5) +
      annotate("text", x = 0.35, y = 0.75, label = paste0("PRC GLMM AIC= ",round(prauc_glmm_aic,2)), size = 2.5) +
      annotate("text", x = 0.35, y = 0.65, label = paste0("PRC RF = ",round(prauc_rf,2)), size = 2.5) + 
      annotate("text", x = 0.35, y = 0.55, label = paste0("PRC RF feat. select. = ",round(prauc_rf_selectvar,2)), size = 2.5) +
      annotate("text", x = 0.35, y = 0.45, label = paste0("PRC RF all preds = ",round(prauc_rf_allpredictors,2)), size = 2.5)
      
    
    #+ ggtitle(paste0(indicator,'-',species,"-",period_interv))
    
    # draw VIP
    # vi_rf <- vi(rf_mod$finalModel,nsim = 20 ,method = "permute", target = "resp_var",reference_class = "Presence", train = df_rf, metric = "auc")
    # vi_glmm <- vi(glmm_mod@model,nsim = 20 ,method = "permute", target = "resp_var", train = df_glmm, metric = "auc",reference_class = "1",allow.new.levels=TRUE)
    # 
    # vi_rf$model <- "RF"
    # vi_glmm$model <- "GLMM"
    # 
    # vi <- bind_rows(vi_rf,vi_glmm) %>%
    #   left_join(df_vars_labs, by = c("Variable" = "var")) %>%
    #   filter(!is.na(label)) %>%
    #   complete(label, nesting(model),fill = (list( Importance = 0, StDev = 0)))
    # 
    # vi_plot <- ggplot(vi, aes(x = Importance , y = label, label = label, group = model, fill = model)) +
    #   geom_bar(position = 'dodge', stat="identity", width = 0.6) + 
    #   geom_errorbar(aes(ymin=Importance-StDev, ymax=Importance+StDev)) +
    #   theme_bw() + 
    #   geom_text(size=1.6,position = position_dodge(0.9),hjust=-0.1,label.padding = unit(0.2, "lines")) + #,aes(fontface=2)
    #   theme(axis.text.y = element_blank(),
    #         axis.title.x = element_text(size = 7),
    #         plot.subtitle = element_text(size = 7, face="bold"),
    #         legend.position = "none"
    #   ) +
    #   ylab("") + 
    #   xlab("") +
    #   xlim(NA,max(vi$Importance, na.rm = T) + max(vi$Importance, na.rm = T)*2.5) +
    #   labs(subtitle = "Variable importance")
    
    
    # get rsquared from the multivariate models
    # df_mod=df_glmm
    # r2 = MuMIn::r.squaredGLMM(glmm_mod@model)
    # 
    
    # draw PDP
    features_rf <- setdiff(colnames(df_rf_allpredictors),"resp_var")
    features <- features_rf
    if(!is.null(glmm)){
    features_glmm <- setdiff(colnames(df_glmm),"resp_var")
    features <- c(intersect(features_rf,features_glmm),setdiff(features_rf,features_glmm))
    #order vars for plots
    g <- glmm_tidy %>% arrange(p.value)
    features <- features[order(match(features, g$term))]
    }
    # pred_wrapper_classif <- function(object, newdata) {
    #   p <- predict(object, newdata = newdata, type ="prob")[,ifelse(indicator!="physiological_resistance_kdrw","Presence","Absence")]
    #   c("avg" = mean(p), "avg-1sd" = mean(p) - sd(p), "avg+1sd" = mean(p) + sd(p))
    # }
    
    pred_wrapper_classif <- function(object, newdata) {
      p <- predict(object, newdata = newdata, type ="prob")[,ifelse(indicator!="physiological_resistance_kdrw","Presence","Absence")]
      c("avg" = mean(p))
    }
    
    pdps <- lapply(features, FUN = function(feature) {
      
      pd <- partial(rf_allpredictors_mod, pred.var = feature, pred.fun = pred_wrapper_classif, train = df_rf_allpredictors)
      pd$yhat[which(pd$yhat<0)] <-0 
      
      cat(feature)
      # write label
      lab_df <- df_vars_labs %>% filter(var==feature) %>% distinct()
      lab_x <- lab_df$label_detail
      lab_x <- gsub("\\(","\n",lab_x)
      if(!is.na(lab_df$unit)){
        if(!grepl("collection",lab_x)){
         lab_x <- paste0(lab_x,"\n",lab_df$unit)
        } else {
          lab_x <- paste0(lab_x,", ",lab_df$unit)
        }
      }
      
      
      if(!is.null(glmm)){
      if(feature %in% glmm_tidy$term){
        lab_x <- paste0(glmm_tidy$p.value2[which(glmm_tidy$term==feature)]," ",lab_x)
      }
      if(!grepl("\n",lab_x)){
        lab_x <- paste0(lab_x,"\n")
      }
      
      t <- grepl(feature,glmm_tidy$term)
      
      if(TRUE %in% t & is.factor(df_rf_allpredictors[,feature])){
        t <- glmm_tidy[t,]
        t$term <- gsub(feature,"",t$term)
        t <- t %>% dplyr::select(term,p.value2)
        colnames(t)[which(colnames(t)=='term')]=feature
        pd <- pd %>% left_join(t) 
        pd$p.value2[which(is.na(pd$p.value2))] <- ""
        pd$feature_rep <- pd[,feature]
        pd[,feature] = paste0(pd[,feature],"",pd$p.value2)
        
        pd_4_rep <- unique(pd[,c(feature,"feature_rep")])
        colnames(pd_4_rep) <- rev(colnames(pd_4_rep))
        df_rf <- left_join(df_rf,pd_4_rep)
        colnames(df_rf)[which(colnames(df_rf)==feature)]="useless"
        colnames(df_rf)[which(colnames(df_rf)=="feature_rep")]=feature
        df_rf[,feature] <- as.factor(df_rf[,feature] )
      }
      }
      
      # get smoothed version of RF pdp
      if(is.numeric(df_rf_allpredictors[,feature][[1]])){
        p <- autoplot(pd, smooth = T)  
        dat <- ggplot_build(p)$data[[2]]
      } else {
        dat <- pd
        colnames(dat)[which(colnames(dat)==feature)]="x"
        colnames(dat)[which(colnames(dat)=="yhat")]="y"
      }
      
      #modnames = c("RF","RF all preds","RF feat. select." ,"GLMM LRT","GLMM AIC"
      #scale_color_manual(values=c("#009E73", "#E69F00","black","red","grey")) +
        
      p <- ggplot() + 
        geom_line(data = dat, aes(x = x, y = y), size = 0.5, colour = "#E69F00") +
        #geom_line(data = pd[pd$yhat.id == "avg", ], aes_string(x = feature, y = "yhat", group = "yhat.id"), size = 0.5, colour = "black") +
        #geom_line(data = pd[pd$yhat.id != "avg", ], aes_string(x = feature, y = "yhat", group = "yhat.id"), linetype = "dashed", size = 0.25) +
        ylim(0,1) +
        theme_light() + 
        xlab(lab_x) + 
        geom_rug(data = df_rf_allpredictors,aes_string(x = feature),sides="b",colour ="grey20") +
        theme(axis.title.y = element_blank(),
              axis.text.x = element_text(size = 7),
              axis.text.y = element_text(size = 7),
              axis.title.x = element_text(size = 8))
      
      if(is.factor(df_rf_allpredictors[,feature][[1]])){
        p <- p +  
          geom_col(data = dat, aes(x = x, y = y))
        #geom_point(data = pd[pd$yhat.id == "avg", ], aes_string(x = feature, y = "yhat", group = "yhat.id"), size = 1, colour = "black") + 
        #geom_point(data = pd[pd$yhat.id != "avg", ], aes_string(x = feature, y = "yhat", group = "yhat.id"), size = 1, colour = "black") 
      }
      
      if(!is.null(rf_selectvar)){
        
      if(feature %in% c(colnames(df_rf_selectvar))){
        pd_selectvar <- partial(rf_selectvar_mod, pred.var = feature, pred.fun = pred_wrapper_classif, train = df_rf_selectvar)
        pd_selectvar$yhat[which(pd_selectvar$yhat<0)] <-0 
        
        if(is.numeric(df_rf_selectvar[,feature][[1]])){
          p_1 <- autoplot(pd_selectvar, smooth = T)  
          dat_1 <- ggplot_build(p_1)$data[[2]]
        } else {
          dat_1 <- pd_selectvar
          colnames(dat_1)[which(colnames(dat_1)==feature)]="x"
          colnames(dat_1)[which(colnames(dat_1)=="yhat")]="y"
        }
        p <- p +  geom_line(data = dat_1, aes(x = x, y = y), size = 0.5, colour = "black")
        if(is.factor(df_rf_selectvar[,feature][[1]])){
          p <- p +  geom_col(data = dat_1, aes(x = x, y = y))
        }
      }
      }
      
      if(feature %in% c(colnames(df_rf))){
        pd_allpredictors <- partial(rf_mod, pred.var = feature, pred.fun = pred_wrapper_classif, train = df_rf)
        pd_allpredictors$yhat[which(pd_allpredictors$yhat<0)] <-0 
        
        if(is.numeric(df_rf[,feature][[1]])){
          p_2 <- autoplot(pd_allpredictors, smooth = T)  
          dat_2 <- ggplot_build(p_2)$data[[2]]
        } else {
          dat_2 <- pd_allpredictors
          colnames(dat_2)[which(colnames(dat_2)==feature)]="x"
          colnames(dat_2)[which(colnames(dat_2)=="yhat")]="y"
        }
        p <- p +  geom_line(data = dat_2, aes(x = x, y = y), size = 0.5, colour = "#009E73")
        if(is.factor(df_rf[,feature][[1]])){
          p <- p +  geom_col(data = dat_2, aes(x = x, y = y))
        }
      }
      
      

      if(!is.null(glmm)){
      if(feature %in% colnames(df_glmm)){
        pglmm = jtools::effect_plot(glmm_mod@model, pred = !!feature, interval = TRUE)
        dat = ggplot_build(pglmm)$data
        if(is.numeric(df_rf[,feature][[1]])){
          dat[[1]]$x = dat[[1]]$x + attr(df_glmm[,feature],'scaled:center')
          dat[[2]]$x = dat[[2]]$x + attr(df_glmm[,feature],'scaled:center')
        }
        
        if(is.numeric(df_rf[,feature][[1]])){
          p <- p + 
            geom_line(data = dat[[1]], aes(x = x, y =y), colour = "red", size = 0.5) + 
            geom_ribbon(data = dat[[2]], aes(x = x, y =y, ymin = ymin, ymax = ymax), alpha = 0.2, fill = "grey20")
        } else if(is.factor(df_rf[,feature][[1]])){
          p <- p +  
            geom_errorbar(data = dat[[2]], aes(x = x, y =y, ymin = ymin, ymax = ymax), width=.1, colour ="grey20") + 
            geom_point(data = dat[[1]], aes(x = x, y = y), size = 1.5, colour = "red")
        }
      }
      }
      return(p)
    })
    
    
    p1 <- list(p,p2)
    p_final <- grid.arrange(grobs = c(p1,pdps), ncol = 5,top =  textGrob(paste0(code_pays," - ",indicator,' - ',species,' - occurence = ',round(nrow(df_rf_allpredictors[which(df_rf_allpredictors$resp_var=="Presence"),])/nrow(df_rf_allpredictors),2)*100," % (n = ",nrow(df_rf_allpredictors[which(df_rf_allpredictors$resp_var=="Presence"),]),")"),gp=gpar(fontsize=12)))
    p_final <- cowplot::ggdraw(p_final) + theme(plot.background = element_rect(fill="white", color = NA))
    
  } else {
    p_final <- ggplot() +theme_void() +geom_text(aes(0,0,label='N/A')) + ggtitle(paste0(code_pays," - ",indicator,' - ',species,' n. presence =',round(nrow(df_rf_allpredictors[which(df_rf_allpredictors$resp_var=="Presence"),])/nrow(df_rf_allpredictors)),2)*100," % (n = ",nrow(df_rf_allpredictors[which(df_rf_allpredictors$resp_var=="Presence"),]),")")
  }
  
  return(p_final)
}




# rf=res[1,]$rf[[1]]
# glmm=res[1,]$glmm_aic[[1]]
# indicator='exophagy'
# species="An. funestus"
# code_pays="BF"
# get_all_plots = TRUE


fun_plot_pdp4 <- function(rf, glmm, indicator, species, code_pays, get_all_plots = TRUE){
  
  library(iml)
  library(vip)
  library(jtools)
  library(MLmetrics)
  library(precrec)
  library(pdp)
  library(caret)
  library(ranger)
  library(glmmTMB)
  library(broom.mixed)
  library(gridExtra)
  library(cowplot)
  #library(egg)
  
  auc_glmm <- NA
  prauc_glmm <- NA
  
  auc_rf <- NA
  prauc_rf <- NA
  
  if(!is.null(rf[[1]])){
    
    rf_mod = rf$mod
    df_rf = rf$df_mod %>% dplyr::select(resp_var, rf_mod$finalMode$xNames)
    df_cv_rf <-  rf$df_cv
    
    auc_rf <- AUC(y_true = df_cv_rf$obs,y_pred = df_cv_rf$pred)
    prauc_rf <- PRAUC(y_true = df_cv_rf$obs,y_pred = df_cv_rf$pred)
    
  }
  glmm_mod = glmm$mod
  df_glmm = glmm$mod@model$frame
  df_cv_glmm <-  glmm$df_cv
  
  df_mod=df_glmm
  r2 = MuMIn::r.squaredGLMM(glmm_mod@model)[1,1]
  
  auc_glmm <- AUC(y_true = df_cv_glmm$obs,y_pred = df_cv_glmm$pred)
  prauc_glmm <- PRAUC(y_true = df_cv_glmm$obs,y_pred = df_cv_glmm$pred)
  
  
  # get predictors labels
  df_vars_labs <- data.frame(var = setdiff(colnames(glmm_mod@model$frame), c("resp_var","codevillage","pointdecapture2")))
  
  
  if(nrow(df_vars_labs)>0){
    
    if(!is.null(rf[[1]])){
      df_vars_labs_rf <- data.frame(var = rf_mod$finalMode$xNames)
      df_vars_labs <- unique(rbind(df_vars_labs,df_vars_labs_rf))
    }
    
    df_vars_labs <- fun_get_predictors_labels(df_vars_labs,"var") %>% distinct()
    
    if(indicator == "late_biting"){
      df_vars_labs <- df_vars_labs %>%
        mutate(label_detail = gsub("day of collection","day preceding collection",label_detail)) %>%
        mutate(label_detail = gsub("hour of collection","night of collection",label_detail))
    }
    
    if(!(indicator %in% c("presence","abundance"))){
      df_vars_labs <- df_vars_labs %>%
        mutate(label_detail = gsub("b/w 0 and 4.28571428571429 weeks","month preceding collection",label_detail)) %>%
        mutate(label_detail = gsub("b/w 0 and 0 weeks","day of collection",label_detail)) %>%
        mutate(label_detail = gsub("\n2000 m buffer","",label_detail))
    }
    
    if(indicator %in% c("presence","abundance")){
      df_vars_labs <- df_vars_labs %>%
        mutate(label_detail = gsub("hour of collection","night of collection",label_detail))
    }
    
    df_vars_labs <- df_vars_labs %>%
      left_join(prediction_vars %>% dplyr::select(var=code,group_behaviour_resistance)) %>%
      mutate(group_behaviour_resistance = ifelse(is.na(group_behaviour_resistance),"Past weather",group_behaviour_resistance))
    
    # get glmm in tidy form
    if(!is.null(glmm[[1]])){
      glmm_tidy <- tidy(glmm_mod@model, conf.int = TRUE, exponentiate = ifelse(indicator == "abundance", FALSE, TRUE))
      glmm_tidy <- glmm_tidy %>%
        mutate(p.value2 = case_when(
          p.value <= 0.001 ~ "***",
          p.value > 0.001 & p.value <= 0.01  ~  "**",
          p.value > 0.01 & p.value <= 0.05 ~ "*",
          p.value > 0.05 ~ ""
        ))
    }
    
    
    baseline_prauc <- df_cv_glmm %>% group_by(obs) %>% summarise(n=n())
    baseline_prauc <- baseline_prauc$n[which(baseline_prauc$obs==1)]/sum(baseline_prauc$n)
    # open iml models
    #rf_mod_iml <- Predictor$new(rf_mod, data = df_rf, class = ifelse(indicator!="physiological_resistance_kdrw","Presence","Absence"))
    #rf_mod_allvars_iml <- Predictor$new(rf_allvars_mod, data = df_rf_allvars, class = ifelse(indicator!="physiological_resistance_kdrw","Presence","Absence"))
    
    # draw ROC curves            
    if(!is.null(rf[[1]])){
      scores =  list(df_cv_rf$pred,df_cv_glmm$pred)
      modnames = c("GLMM","RF")
      cols = c("#E69F00","#009E73")
    } else {
      scores =  list(df_cv_glmm$pred)
      modnames = c("GLMM")
      cols = c("#E69F00")
    }
    
    
    samps <- list(scores = scores, labels = df_cv_glmm$obs,  modnames = modnames)
    mdat <- mmdata(samps[["scores"]], samps[["labels"]],modnames = samps[["modnames"]])
    m <- evalmod(mdat)
    p = autoplot(m, curvetype = "ROC") +
      theme(plot.title = element_blank(),
            axis.title.x = element_text(size = 7),
            axis.title.y = element_text(size = 7),
            axis.text.y = element_text(size = 7),
            axis.text.x = element_text(size = 7),
            legend.position=c(0.7, 0.2),
            legend.text = element_text(size = 7),
            legend.background=element_rect(fill = alpha("white", 0)),
            legend.key.size = unit(0.2, "cm")
      ) +
      scale_color_manual(values=cols) +
      annotate("text", x = 0.35, y = 0.95, label = paste0("AUC GLMM = ",round(auc_glmm,2)), size = 2.5) +
      annotate("text", x = 0.35, y = 0.85, label = paste0("AUC RF = ",round(auc_rf,2)), size = 2.5)
    
    
    p2 = autoplot(m, curvetype = "PRC") +
      theme(plot.title = element_blank(),
            axis.title.x = element_text(size = 7),
            axis.title.y = element_text(size = 7),
            axis.text.y = element_text(size = 7),
            axis.text.x = element_text(size = 7),
            legend.position=c(0.7, 0.2),
            legend.text = element_text(size = 7),
            legend.background=element_rect(fill = alpha("white", 0)),
            legend.key.size = unit(0.2, "cm")
      ) +
      scale_color_manual(values=cols) +
      annotate("text", x = 0.35, y = 0.95, label = paste0("PRC baseline = ",round(baseline_prauc,2)), size = 2.5) +
      annotate("text", x = 0.35, y = 0.85, label = paste0("PRC GLMM = ",round(prauc_glmm,2)), size = 2.5) +
      annotate("text", x = 0.35, y = 0.75, label = paste0("PRC RF = ",round(prauc_rf,2)), size = 2.5)
    
    
    #+ ggtitle(paste0(indicator,'-',species,"-",period_interv))
    
    # draw VIP
    # vi_rf <- vi(rf_mod$finalModel,nsim = 20 ,method = "permute", target = "resp_var",reference_class = "Presence", train = df_rf, metric = "auc")
    # vi_glmm <- vi(glmm_mod@model,nsim = 20 ,method = "permute", target = "resp_var", train = df_glmm, metric = "auc",reference_class = "1",allow.new.levels=TRUE)
    # 
    # vi_rf$model <- "RF"
    # vi_glmm$model <- "GLMM"
    # 
    # vi <- bind_rows(vi_rf,vi_glmm) %>%
    #   left_join(df_vars_labs, by = c("Variable" = "var")) %>%
    #   filter(!is.na(label)) %>%
    #   complete(label, nesting(model),fill = (list( Importance = 0, StDev = 0)))
    # 
    # vi_plot <- ggplot(vi, aes(x = Importance , y = label, label = label, group = model, fill = model)) +
    #   geom_bar(position = 'dodge', stat="identity", width = 0.6) + 
    #   geom_errorbar(aes(ymin=Importance-StDev, ymax=Importance+StDev)) +
    #   theme_bw() + 
    #   geom_text(size=1.6,position = position_dodge(0.9),hjust=-0.1,label.padding = unit(0.2, "lines")) + #,aes(fontface=2)
    #   theme(axis.text.y = element_blank(),
    #         axis.title.x = element_text(size = 7),
    #         plot.subtitle = element_text(size = 7, face="bold"),
    #         legend.position = "none"
    #   ) +
    #   ylab("") + 
    #   xlab("") +
    #   xlim(NA,max(vi$Importance, na.rm = T) + max(vi$Importance, na.rm = T)*2.5) +
    #   labs(subtitle = "Variable importance")
    
    
    # get rsquared from the multivariate models
    # df_mod=df_glmm
    # r2 = MuMIn::r.squaredGLMM(glmm_mod@model)
    # 
    
    # draw PDP
    # features = df_vars_labs$var
    # g <- glmm_tidy %>% arrange(p.value)
    # features <- features[order(match(features, g$term))]
    
    features = df_vars_labs %>%
      mutate(group_behaviour_resistance = fct_relevel(group_behaviour_resistance,c("Vector control","Human behaviour","Vector resistance","Micro-climate","Past weather","Landscape","Other")))
    features = features[order(features$group_behaviour_resistance),]
    features =  features$var
    
    
    # pred_wrapper_classif_werror <- function(object, newdata) {
    #   p <- predict(object, newdata = newdata, type ="prob")[,ifelse(indicator!="physiological_resistance_kdrw","Presence","Absence")]
    #   c("avg" = mean(p), "avg-1sd" = mean(p) - sd(p), "avg+1sd" = mean(p) + sd(p))
    # }
    
    var_to_plot <- "Presence"
    if(indicator=="physiological_resistance_kdrw" & !is.null(rf[[1]])){
      df_rf <- df_rf %>% mutate(resp_var=ifelse(resp_var == "Presence","Absence","Presence"))
      var_to_plot <- "Absence"
    }
    if(indicator=="presence" & species =="An. gambiae s.l." & !is.null(rf[[1]])){
      df_rf <- df_rf %>% mutate(resp_var=ifelse(resp_var == "Presence","Absence","Presence"))
      var_to_plot <- "Absence"
    }
    
    pred_wrapper_classif <- function(object, newdata) {
      p <- predict(object, newdata = newdata, type ="prob")[,var_to_plot]
      c("avg" = mean(p))
    }
    
    pdps <- lapply(features, FUN = function(feature) {
      
      rf_var_imp=NA
      glmm_var_imp=NA
      
      cat(feature)
      # write label
      lab_df <- df_vars_labs %>% filter(var==feature) %>% distinct()
      lab_x <- lab_df$label_detail
      lab_x <- gsub("\\(","\n",lab_x)
      lab_x <- gsub("\n","\n(",lab_x)
      lab_x <- paste0(lab_x,")")
      
      if(!grepl('\\(',lab_x)){
        lab_x <- gsub(")","",lab_x)
      }
      
      if(is.na(lab_df$unit)){
        lab_df$unit <- ""
      }
      # if(!is.na(lab_df$unit)){
      #   if(!grepl("collection",lab_x)){
      #     lab_x <- paste0(lab_x,"\n",lab_df$unit)
      #   } else {
      #     lab_x <- paste0(lab_x,", ",lab_df$unit)
      #   }
      # }
      
      
      if(!is.null(rf[[1]])){
        if(feature %in% rf_mod$finalMode$xNames){
          pd <- partial(rf_mod, pred.var = feature, pred.fun = pred_wrapper_classif, train = df_rf)
          pd$yhat[which(pd$yhat<0)] <-0 
          # get smoothed version of RF pdp
          if(is.numeric(df_rf[,feature][[1]])){
            p <- autoplot(pd, smooth = T)  
            dat <- ggplot_build(p)$data[[2]]
          } else {
            dat <- pd
            colnames(dat)[which(colnames(dat)==feature)]="x"
            colnames(dat)[which(colnames(dat)=="yhat")]="y"
          }
        }
      }
      
      if(!is.null(glmm[[1]])){
        if(feature %in% glmm_tidy$term){
          lab_x <- paste0(glmm_tidy$p.value2[which(glmm_tidy$term==feature)]," ",lab_x)
        }
        if(!grepl("\n",lab_x)){
          lab_x <- paste0(lab_x,"\n")
        }
        
        
        t <- grepl(feature,glmm_tidy$term)
        
        if(TRUE %in% t & is.factor(df_rf[,feature])){
          t <- glmm_tidy[t,]
          t$term <- gsub(feature,"",t$term)
          t <- t %>% dplyr::select(term,p.value2)
          colnames(t)[which(colnames(t)=='term')]=feature
          pd <- pd %>% left_join(t) 
          pd$p.value2[which(is.na(pd$p.value2))] <- ""
          pd$feature_rep <- pd[,feature]
          pd[,feature] = paste0(pd[,feature],"",pd$p.value2)
          
          pd_4_rep <- unique(pd[,c(feature,"feature_rep")])
          colnames(pd_4_rep) <- rev(colnames(pd_4_rep))
          df_rf <- left_join(df_rf,pd_4_rep)
          colnames(df_rf)[which(colnames(df_rf)==feature)]="useless"
          colnames(df_rf)[which(colnames(df_rf)=="feature_rep")]=feature
          df_rf[,feature] <- as.factor(df_rf[,feature] )
          
          dat$x <- pd[,feature]
          df_rf$useless <- NULL
        }
      }
      
      
      
      group = df_vars_labs$group_behaviour_resistance[which(df_vars_labs$var == feature)]
      
      # p <- ggplot() + 
      #   ylim(-0.08,1.05) +
      #   scale_y_continuous(breaks=c(0,0.5,1)) +
      #   theme_light() + 
      #   xlab(lab_df$unit) + 
      #   ggtitle(lab_x, subtitle = paste0('Group = ',group)) + 
      #   theme(plot.title = element_text(size = 7.5,colour = "grey10"),
      #         plot.subtitle = element_text(size = 6,colour = "grey20",face="italic"),
      #         axis.title.y = element_blank(),
      #         axis.text.x = element_text(size = 6),
      #         axis.text.y = element_text(size = 6),
      #         axis.title.x = element_text(size = 6,colour = "grey30"),
      #         legend.position = "none")
      
      p <- ggplot() + 
        ylim(-0.08,1.05) +
        scale_y_continuous(breaks=c(0,0.5,1)) +
        theme_light() + 
        xlab(lab_df$unit) + 
        ggtitle(group, subtitle = lab_x) + 
        theme(plot.subtitle = element_text(size = 7.5,colour = "grey10"),
              plot.title = element_text(size = 6,colour = "grey20",face="italic"),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size = 6),
              axis.text.y = element_text(size = 6),
              axis.title.x = element_text(size = 6,colour = "grey30"),
              legend.position = "none")
      
      #+ 
      #annotate("label", x=-Inf,hjust = -0.1, y = 0.84, label = paste0 ('Group = ',group), size = 2.1, colour = "grey20")#+ 
      # geom_rug(data = df_rf %>% filter(resp_var=="Absence"),aes_string(x = feature),sides="b",colour ="grey20") + 
      # geom_rug(data = df_rf %>% filter(resp_var=="Presence"),aes_string(x = feature),sides="t",colour ="grey20")
      #geom_bin2d(data = df_rf, aes_string(x = feature, y = "resp_var") ,  binwidth = c(max(df_rf[,feature])/15,.05)) +  scale_fill_continuous(type = "viridis")
      
      #modnames = c("RF","RF all preds","RF feat. select." ,"GLMM LRT","GLMM AIC"
      #scale_color_manual(values=c("#009E73", "#E69F00","black","red","grey")) +
      
      if(!is.null(rf[[1]])){
        if(feature %in% rf_mod$finalMode$xNames){
          if((abs(max(pd$yhat)-min(pd$yhat))>=0.1 | get_all_plots == TRUE) | (!is.null(glmm[[1]]) & feature %in% colnames(df_glmm))){
            if(is.numeric(df_rf[,feature][[1]])){
              p <- p + 
                geom_line(data = dat, aes(x = x, y = y), size = 0.5, colour = "#009E73")
              #geom_line(data = pd[pd$yhat.id == "avg", ], aes_string(x = feature, y = "yhat", group = "yhat.id"), size = 0.5, colour = "black") +
              #geom_line(data = pd[pd$yhat.id != "avg", ], aes_string(x = feature, y = "yhat", group = "yhat.id"), linetype = "dashed", size = 0.25) +
              
              #if(feature %in% c("NMA","LUS")){
              p <- p + geom_bin2d(data = df_rf %>% mutate(resp_var = ifelse(resp_var=="Presence",1,0)), aes_string(x = feature, y = "resp_var"), colour = "black") +  scale_fill_gradient(low = "gray90", high = "black")# scale_fill_continuous(type = "viridis")
              #} else {
              #  p <- p + geom_bin2d(data = df_rf %>% mutate(resp_var = ifelse(resp_var=="Presence",1,0)), aes_string(x = feature, y = "resp_var"), colour = "black",  binwidth = c(max(df_rf[,feature])/15,.05)) +  scale_fill_gradient(low = "gray90", high = "black")# scale_fill_continuous(type = "viridis")
              #}
              
              rf_var_imp <- sd(dat$y)
              
            } else if(is.factor(df_rf[,feature][[1]])){
              
              df_rf <- as.data.frame(df_rf)
              df_rf[,feature] <- gsub("\\+","\n+",df_rf[,feature])
              dat$x <- gsub("\\+","\n+",dat$x)
              
              p <- p +  
                geom_col(data = dat, aes(x = x, y = y), fill = "#009E73") + 
                geom_bin2d(data = df_rf %>% mutate(resp_var = ifelse(resp_var=="Presence",1,0)), aes_string(x = feature, y = "resp_var"), colour = "black") +   scale_fill_gradient(low = "gray90", high = "black") #scale_fill_continuous(type = "viridis")
              #geom_point(data = pd[pd$yhat.id == "avg", ], aes_string(x = feature, y = "yhat", group = "yhat.id"), size = 1, colour = "black") + 
              #geom_point(data = pd[pd$yhat.id != "avg", ], aes_string(x = feature, y = "yhat", group = "yhat.id"), size = 1, colour = "black") 
              
              rf_var_imp <- (max(dat$y)-min(dat$y))/4
              
            }
          }
          
        }
      }
      
      
      
      if(!is.null(glmm[[1]])){
        if(feature %in% colnames(df_glmm)){
          pglmm = jtools::effect_plot(glmm_mod@model, pred = !!feature, interval = TRUE)
          dat = ggplot_build(pglmm)$data
          if(is.numeric(df_glmm[,feature][[1]])){
            dat[[1]]$x = dat[[1]]$x + attr(df_glmm[,feature],'scaled:center')
            dat[[2]]$x = dat[[2]]$x + attr(df_glmm[,feature],'scaled:center')
          }
          
          if(is.numeric(df_glmm[,feature][[1]])){
            p <- p + 
              geom_ribbon(data = dat[[2]], aes(x = x, y =y, ymin = ymin, ymax = ymax), alpha = 0.2, fill = "grey20") +
              geom_line(data = dat[[1]], aes(x = x, y =y), colour = "#E69F00", size = 0.5)
            
            glmm_var_imp <- sd(dat[[1]]$y)
            
          } else if(is.factor(df_glmm[,feature][[1]])){
            p <- p +  
              geom_errorbar(data = dat[[2]], aes(x = x, y =y, ymin = ymin, ymax = ymax), width=.1, colour ="grey20") + 
              geom_point(data = dat[[1]], aes(x = x, y = y), size = 1.5, colour = "#E69F00")
            
            glmm_var_imp <- (max(dat[[1]]$y)-min(dat[[1]]$y))/4
            
          }
        }
      }
      
      cols <- unlist(purrr::map(ggplot_build(p)$data, ~colnames(.)))
      if("y" %in% cols | get_all_plots == TRUE){
        return(list(plot = p,rf_var_imp = rf_var_imp,glmm_var_imp = glmm_var_imp))
      } else {
        return(NULL)
      }
      
    })
    
    
    names(pdps) <- features
    # variable importance
    df_varimp <- data.frame(var = character(), RF = numeric(), GLMM = numeric(), stringsAsFactors = F)
    for(i in 1:length(pdps)){
      df_varimp[nrow(df_varimp) + 1,] = c(names(pdps)[[i]],pdps[[i]]$rf_var_imp,pdps[[i]]$glmm_var_imp)
    }
    df_varimp <- df_varimp %>% 
      pivot_longer(c( RF,GLMM)) %>% 
      mutate(value = as.numeric(value)) %>% 
      mutate(value = ifelse(is.na(value), 0 , value)) %>%
      left_join(df_vars_labs) %>%
      mutate(country = code_pays, indicator = indicator, species = species)
    
    
    varimplot <- ggplot(df_varimp, aes(y=reorder(label, value), x = value, fill = name,group = name, label = label)) + 
      geom_bar(position="dodge", stat="identity") + 
      scale_fill_manual("model", values = c("GLMM" = "#E69F00", "RF" = "#009E73")) + 
      theme_light() + 
      #geom_text(size=1.6,position = position_dodge(0.9),hjust=-0.1,label.padding = unit(0.2, "lines"))
      theme(axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            legend.position = "bottom") + 
      facet_grid(label_group~name)
    
    #pdps_plots = list(varimplot)
    pdps_plots = list()
    
    # ord <- df_varimp %>% arrange(desc(value)) 
    # ord <- unique(ord$var)
    # pdps <- pdps[ord]
    
    for(i in 1:length(pdps)){
      pdps_plots[[i+1]] <- pdps[[i]]$plot
    }
    
    # create plots for model quality
    df_qual <- data.frame(model = c("GLMM","RF"), quality = c(round(r2,2),round(auc_rf,2)), quality_lab = c(paste0("R² = ",round(r2,2)),paste0("AUC = ",round(auc_rf,2))), line = c(NA,.5))
    qual_plot <- ggplot(df_qual, aes(x=model,y=quality, label = quality_lab, fill = model)) + 
      geom_col() + 
      scale_fill_manual(breaks = c("GLMM", "RF"), values=c("#E69F00", "#009E73")) +
      geom_text(vjust = -0.5, size= 2) +
      ylim(c(0,1)) +
      facet_grid(.~model, scales = "free") +
      geom_hline(aes(yintercept = line),linetype = "dashed", color = "darkred", size = 0.5) + 
      theme_light() + 
      xlab("") +
      ggtitle('', subtitle = 'Power of the models\n') + 
      labs(caption = '') +
      theme(plot.subtitle = element_text(size = 7.5,colour = "grey0"),
            plot.title = element_text(size = 6,colour = "grey20",face="italic"),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 6),
            axis.ticks.x=element_blank(),
            legend.position = "none",
            strip.text.x = element_text(size = 7, colour = "black"))
    
    pdps_plots[[1]] <- qual_plot 
    
    pdps_plots <- pdps_plots[which(!sapply(pdps_plots, is.null))]
    
    #p1 <- list(p,p2)               #c(p1,pdps)
    if(code_pays=='BF'){
      code_pays="Diébougou (BF)"
    } else {
      code_pays="Korhogo (IC)"
    }
    p_final <- grid.arrange(grobs = pdps_plots, nrow = 5,top =  textGrob(paste0(code_pays," - ",species),x=0.028,hjust = 0,gp=gpar(fontsize=10)))#,' - occurence = ',round(nrow(df_glmm[which(df_glmm$resp_var==1),])/nrow(df_glmm),2)*100," % (n = ",nrow(df_glmm[which(df_glmm$resp_var==1),]),", n tot = ",nrow(df_glmm),")\nAUC GLMM = ",round(auc_glmm,2)," ; AUC RF = ",round(auc_rf,2)),gp=gpar(fontsize=10)))
    # library(egg)
    #p_final <- grid.arrange(grobs = lapply(pdps_plots,set_panel_size, width = unit(2.5, "cm"),  height = unit(2, "cm")), ncol = 5,top =  textGrob(paste0(code_pays," - ",indicator,' - ',species,' - occurence = ',round(nrow(df_glmm[which(df_glmm$resp_var==1),])/nrow(df_glmm),2)*100," % (n = ",nrow(df_glmm[which(df_glmm$resp_var==1),]),", n tot = ",nrow(df_glmm),")\nAUC GLMM = ",round(auc_glmm,2)," ; AUC RF = ",round(auc_rf,2)),gp=gpar(fontsize=12)))
    
    p_final <- cowplot::ggdraw(p_final) + theme(plot.background = element_rect(fill="white", color = NA))
    
  } else {
    p_final <- ggplot() +theme_void() +geom_text(aes(0,0,label='N/A')) + ggtitle(paste0(code_pays," - ",indicator,' - ',species,' - occurence = ',round(nrow(df_glmm[which(df_glmm$resp_var==1),])/nrow(df_glmm),2)*100," % (n = ",nrow(df_glmm[which(df_glmm$resp_var==1),]),", n tot = ",nrow(df_glmm),")\nAUC GLMM = ",round(auc_glmm,2)," ; AUC RF = ",round(auc_rf,2)))
  }
  
  #return(list(p_final=p_final,df_varimp=df_varimp))
  return(p_final)
}



# rf=res[1,]$rf[[1]]
# glmm=res[1,]$glmm_aic[[1]]
# indicator='exophagy'
# species="An. funestus"
# code_pays="BF"
# get_all_plots = TRUE

# same as fun_plot_pdp4 but with different plotting organization
fun_plot_pdp5 <- function(rf, glmm, indicator, species, code_pays, get_all_plots = TRUE){
  
  library(iml)
  library(vip)
  library(jtools)
  library(MLmetrics)
  library(precrec)
  library(pdp)
  library(caret)
  library(ranger)
  library(glmmTMB)
  library(broom.mixed)
  library(gridExtra)
  library(cowplot)
  #library(egg)
  
  auc_glmm <- NA
  prauc_glmm <- NA
  
  auc_rf <- NA
  prauc_rf <- NA
  
  if(!is.null(rf[[1]])){
    
    rf_mod = rf$mod
    df_rf = rf$df_mod %>% dplyr::select(resp_var, rf_mod$finalMode$xNames)
    df_cv_rf <-  rf$df_cv
    
    auc_rf <- AUC(y_true = df_cv_rf$obs,y_pred = df_cv_rf$pred)
    prauc_rf <- PRAUC(y_true = df_cv_rf$obs,y_pred = df_cv_rf$pred)
    
  }
  glmm_mod = glmm$mod
  df_glmm = glmm$mod@model$frame
  df_cv_glmm <-  glmm$df_cv
  
  df_mod=df_glmm
  r2 = MuMIn::r.squaredGLMM(glmm_mod@model)[1,1]
  
  auc_glmm <- AUC(y_true = df_cv_glmm$obs,y_pred = df_cv_glmm$pred)
  
  avg_prob <- nrow(df_glmm[which(df_glmm$resp_var==1),])/nrow(df_glmm)
  
  # get predictors labels
  df_vars_labs <- data.frame(var = setdiff(colnames(glmm_mod@model$frame), c("resp_var","codevillage","pointdecapture2")))
  
  
  if(nrow(df_vars_labs)>0){
    
    if(!is.null(rf[[1]])){
      df_vars_labs_rf <- data.frame(var = rf_mod$finalMode$xNames)
      df_vars_labs <- unique(rbind(df_vars_labs,df_vars_labs_rf))
    }
    
    df_vars_labs <- fun_get_predictors_labels(df_vars_labs,"var") %>% distinct()
    

    if(code_pays=="CI"){
      df_vars_labs <- df_vars_labs %>%
        mutate(label_detail = ifelse(var=="VCT2","Time since intro. of VC measures",label_detail)) %>%
        mutate(label_detail = ifelse(var=="VCT","Time since first entomological survey",label_detail))  
    }
    
    
    if(code_pays=="CI" & species=="An. funestus"){
      df_rf$VCM <- droplevels(df_rf$VCM)
    }
  
    
    if(indicator == "late_biting"){
      df_vars_labs <- df_vars_labs %>%
        mutate(label_detail = gsub("day of collection","day preceding collection",label_detail)) %>%
        mutate(label_detail = gsub("hour of collection","night of collection",label_detail))
    }
    
    if(!(indicator %in% c("presence","abundance"))){
      df_vars_labs <- df_vars_labs %>%
        mutate(label_detail = gsub("b/w 0 and 4.28571428571429 weeks","(month preceding coll.",label_detail)) %>%
        mutate(label_detail = gsub("b/w 0 and 0 weeks","(day of collection",label_detail)) %>%
        mutate(label_detail = gsub("\n2000 m buffer","",label_detail))
    }
    
    if(indicator %in% c("presence","abundance")){
      df_vars_labs <- df_vars_labs %>%
        mutate(label_detail = gsub("hour of collection","night of collection",label_detail))
    }
    
    df_vars_labs <- df_vars_labs %>%
      left_join(prediction_vars %>% dplyr::select(var=code,group_behaviour_resistance)) %>%
      mutate(group_behaviour_resistance = ifelse(is.na(group_behaviour_resistance),"Past weather",group_behaviour_resistance)) %>%
    mutate(group_behaviour_resistance = ifelse(var %in% c("lsm_c_pland_2000_3_1","lsm_c_pland_2000_4_1","lsm_c_pland_2000_4_11","lsm_c_pland_2000_4_16"),"Landscape",group_behaviour_resistance))
    
    # get glmm in tidy form
    if(!is.null(glmm[[1]])){
      glmm_tidy <- tidy(glmm_mod@model, conf.int = TRUE, exponentiate = ifelse(indicator == "abundance", FALSE, TRUE))
      glmm_tidy <- glmm_tidy %>%
        mutate(p.value2 = case_when(
          p.value <= 0.001 ~ "***",
          p.value > 0.001 & p.value <= 0.01  ~  "**",
          p.value > 0.01 & p.value <= 0.05 ~ "*",
          p.value > 0.05 ~ ""
        ))
    }
    
    
    # open iml models
    #rf_mod_iml <- Predictor$new(rf_mod, data = df_rf, class = ifelse(indicator!="physiological_resistance_kdrw","Presence","Absence"))
    #rf_mod_allvars_iml <- Predictor$new(rf_allvars_mod, data = df_rf_allvars, class = ifelse(indicator!="physiological_resistance_kdrw","Presence","Absence"))
    
    features = df_vars_labs %>%
      mutate(group_behaviour_resistance = fct_relevel(group_behaviour_resistance,c("Vector control","Landscape","Human behaviour","Past weather","Micro-climate","Other","Vector resistance")))
    features = features[order(features$group_behaviour_resistance,features$var),]
    features =  features$var
    
   #   features = df_vars_labs$var
   #   g <- glmm_tidy %>% arrange(p.value)
   #   features <- features[order(match(features, g$term))]
   #   features <- setdiff(features,c("VCM","VCT"))
   #   if(code_pays=='BF'){
   #   features <- c('VCM','VCT',features)
   # } else if(code_pays=="CI"){
   #   features <- c('VCM',features)
   # }
   #   
     
     
    # pred_wrapper_classif_werror <- function(object, newdata) {
    #   p <- predict(object, newdata = newdata, type ="prob")[,ifelse(indicator!="physiological_resistance_kdrw","Presence","Absence")]
    #   c("avg" = mean(p), "avg-1sd" = mean(p) - sd(p), "avg+1sd" = mean(p) + sd(p))
    # }
    
    var_to_plot <- "Presence"
    if(indicator=="physiological_resistance_kdrw" & !is.null(rf[[1]])){
      df_rf <- df_rf %>% mutate(resp_var=ifelse(resp_var == "Presence","Absence","Presence"))
      var_to_plot <- "Absence"
    }
    if(indicator=="presence" & species =="An. gambiae s.l." & !is.null(rf[[1]])){
      df_rf <- df_rf %>% mutate(resp_var=ifelse(resp_var == "Presence","Absence","Presence"))
      var_to_plot <- "Absence"
    }
    
    pred_wrapper_classif <- function(object, newdata) {
      p <- predict(object, newdata = newdata, type ="prob")[,var_to_plot]
      c("avg" = mean(p))
    }
    
    
    if(auc_rf<0.6){
      plot_pdp <- F
    } else {
      plot_pdp <- T
    }
    
    
    pdps <- lapply(features, FUN = function(feature) {
      
      rf_var_imp=NA
      glmm_var_imp=NA
      
      cat(feature)
      # write label
      lab_df <- df_vars_labs %>% filter(var==feature) %>% distinct()
      lab_x <- lab_df$label_detail
      lab_x <- gsub("\\n"," ",lab_x)
      lab_x <- gsub("\\(","\n",lab_x)
      lab_x <- gsub("\n","\n(",lab_x)
      lab_x <- paste0(lab_x,")")
      lab_x <- gsub(" \\n","\\\n",lab_x)
      lab_x <- gsub("\\(","",lab_x)
      lab_x <- gsub("\\)","",lab_x)
      
      #lab_x <- gsub("\\(.*","",lab_x)
      if(!is.na(lab_df$unit)){
       # lab_x <- paste0(lab_x,", ",lab_df$unit)
        lab_x <- paste0(lab_x,"\n  (",lab_df$unit,")")
      }
      # lab_x <- gsub("\\(","\n",lab_x)
      # lab_x <- gsub("\n","\n(",lab_x)
      # lab_x <- paste0(lab_x,")")
      
      # if(!grepl('\\(',lab_x)){
      #  lab_x <- gsub(")","",lab_x)
      # }
      # 
      # if(is.na(lab_df$unit)){
      #  lab_df$unit <- ""
      # }
      # 
      
      if(lab_x == "Time since introd. of LLIN\n  (month)") { lab_x = "Time since LLIN\ndistribution (month)"}
      lab_x = gsub("Distance to the edge of the village","\nDistance to the edge\nof the village",lab_x)
      lab_x = gsub("Nb. hours spent indoors by hum. pop.","Nb. hours spent\nindoors by hum. pop.\nnight of collection",lab_x)
      lab_x = gsub("stream\n(m.)","\nstream (m.)",lab_x)
      lab_x = gsub("Δ temperature","Δ temp.",lab_x)
      lab_x = gsub("Δ luminosity","Δ lumin.",lab_x)
      lab_x = gsub("Atmospheric pressure","Atmospheric press.",lab_x)
      lab_x = gsub("Place","Place of collection",lab_x)
      lab_x = gsub("Time since first entomological survey","Time since first\nentomological survey",lab_x)
      
      
      if(lab_df$var=="lsm_c_pland_2000_4_1"){  lab_x = "Other crops\n(% landscape)"}
      if(lab_df$var=="lsm_c_pland_2000_4_11"){  lab_x = "Rice fields\n(% landscape)"}
      if(lab_df$var=="lsm_c_pland_2000_4_16"){  lab_x = "Cotton fields"}
      
            # if(!is.na(lab_df$unit)){
      #   if(!grepl("collection",lab_x)){
      #     lab_x <- paste0(lab_x,"\n",lab_df$unit)
      #   } else {
      #     lab_x <- paste0(lab_x,", ",lab_df$unit)
      #   }
      # }
      
      
      if(!is.null(rf[[1]])){
        if(feature %in% rf_mod$finalMode$xNames){
          pd <- partial(rf_mod, pred.var = feature, pred.fun = pred_wrapper_classif, train = df_rf)
          pd$yhat[which(pd$yhat<0)] <-0 
          # get smoothed version of RF pdp
          if(is.numeric(df_rf[,feature][[1]])){
            p <- autoplot(pd, smooth = T)  
            dat <- ggplot_build(p)$data[[2]]
          } else {
            dat <- pd
            colnames(dat)[which(colnames(dat)==feature)]="x"
            colnames(dat)[which(colnames(dat)=="yhat")]="y"
          }
        }
      }
      
      if(!is.null(glmm[[1]])){
        if(feature %in% glmm_tidy$term){
          lab_x <- paste0(glmm_tidy$p.value2[which(glmm_tidy$term==feature)]," ",lab_x)
        }

        # if(!grepl("\n",lab_x)){
        #   lab_x <- paste0(lab_x,"\n")
        # }
        
        
        t <- grepl(feature,glmm_tidy$term)
        
        if(TRUE %in% t & is.factor(df_rf[,feature])){
          t <- glmm_tidy[t,]
          t$term <- gsub(feature,"",t$term)
          t <- t %>% dplyr::select(term,p.value2)
          colnames(t)[which(colnames(t)=='term')]=feature
          pd <- pd %>% left_join(t) 
          pd$p.value2[which(is.na(pd$p.value2))] <- ""
          pd$feature_rep <- pd[,feature]
          pd[,feature] = paste0(pd[,feature],"",pd$p.value2)
          
          pd_4_rep <- unique(pd[,c(feature,"feature_rep")])
          colnames(pd_4_rep) <- rev(colnames(pd_4_rep))
          df_rf <- left_join(df_rf,pd_4_rep)
          colnames(df_rf)[which(colnames(df_rf)==feature)]="useless"
          colnames(df_rf)[which(colnames(df_rf)=="feature_rep")]=feature
          df_rf[,feature] <- as.factor(df_rf[,feature] )
          
          dat$x <- pd[,feature]
          df_rf$useless <- NULL
        }
      }
      
      
      
      group = df_vars_labs$group_behaviour_resistance[which(df_vars_labs$var == feature)]
      
      # p <- ggplot() + 
      #   ylim(-0.08,1.05) +
      #   scale_y_continuous(breaks=c(0,0.5,1)) +
      #   theme_light() + 
      #   xlab(lab_df$unit) + 
      #   ggtitle(lab_x, subtitle = paste0('Group = ',group)) + 
      #   theme(plot.title = element_text(size = 7.5,colour = "grey10"),
      #         plot.subtitle = element_text(size = 6,colour = "grey20",face="italic"),
      #         axis.title.y = element_blank(),
      #         axis.text.x = element_text(size = 6),
      #         axis.text.y = element_text(size = 6),
      #         axis.title.x = element_text(size = 6,colour = "grey30"),
      #         legend.position = "none")
      

      
      if(code_pays=="CI" & indicator == "early_biting" & species=="An. gambiae s.l."){
        ymax <- 0.255
        geombin_ymax <- 0.25
        title_y <- 0.15
        breaks_scale_y <- c(0,0.25)
      } else {
        ymax <- 1.05
        geombin_ymax <- 1
        title_y <- 0.62
        if(feature=='VCM'){
          title_y <- 0.75
        }
        breaks_scale_y <- c(0,0.5,1)
      }
      
      
      if(code_pays=="CI" & feature == 'VCM' & species =="An. gambiae s.l."){
         size_x_text <- 4.5
      } else {
        size_x_text <- 6
      }
      
      p <- ggplot() + 
        ylim(-0.08,ymax) +
        scale_y_continuous(breaks=breaks_scale_y) +
        theme_light() + 
        #xlab(lab_df$unit) + 
        #ggtitle(group, subtitle = lab_x) + 
        theme(plot.subtitle = element_blank(),
              plot.title = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size = size_x_text),
              axis.text.y = element_text(size = 6),
              axis.title.x = element_blank(),
              #axis.title.x = element_text(size = 6,colour = "grey30"),
              legend.position = "none")#+ 
      # geom_rug(data = df_rf %>% filter(resp_var=="Absence"),aes_string(x = feature),sides="b",colour ="grey20") + 
      # geom_rug(data = df_rf %>% filter(resp_var=="Presence"),aes_string(x = feature),sides="t",colour ="grey20")
      #geom_bin2d(data = df_rf, aes_string(x = feature, y = "resp_var") ,  binwidth = c(max(df_rf[,feature])/15,.05)) +  scale_fill_continuous(type = "viridis")
      
      #modnames = c("RF","RF all preds","RF feat. select." ,"GLMM LRT","GLMM AIC"
      #scale_color_manual(values=c("#009E73", "#E69F00","black","red","grey")) +
      
    #  if(!(code_pays=="CI" & indicator=="late_biting" & species=="An. gambiae s.l." & feature == "RFD1F_2000_0_0" & get_all_plots==FALSE)){
      
      if(!is.null(rf[[1]])){
        if(feature %in% rf_mod$finalMode$xNames){
         # if((abs(max(pd$yhat)-min(pd$yhat))>=0.05 | get_all_plots == TRUE) | (!is.null(glmm[[1]]) & feature %in% colnames(df_glmm))){
          if(get_all_plots == TRUE | (!is.null(glmm[[1]]) & feature %in% colnames(df_glmm))){
            
            if(is.numeric(df_rf[,feature][[1]])){
              if(isTRUE(plot_pdp)){
              p <- p + 
                geom_line(data = dat, aes(x = x, y = y), size = 0.5, colour = "#009E73")
              }
              #geom_line(data = pd[pd$yhat.id == "avg", ], aes_string(x = feature, y = "yhat", group = "yhat.id"), size = 0.5, colour = "black") +
              #geom_line(data = pd[pd$yhat.id != "avg", ], aes_string(x = feature, y = "yhat", group = "yhat.id"), linetype = "dashed", size = 0.25) +
              
              #if(feature %in% c("NMA","LUS")){
              p <- p + geom_bin2d(data = df_rf %>% mutate(resp_var = ifelse(resp_var=="Presence",geombin_ymax,0)), aes_string(x = feature, y = "resp_var"), colour = "black") +  scale_fill_gradient(low = "gray90", high = "black")# scale_fill_continuous(type = "viridis")
              #} else {
              #  p <- p + geom_bin2d(data = df_rf %>% mutate(resp_var = ifelse(resp_var=="Presence",1,0)), aes_string(x = feature, y = "resp_var"), colour = "black",  binwidth = c(max(df_rf[,feature])/15,.05)) +  scale_fill_gradient(low = "gray90", high = "black")# scale_fill_continuous(type = "viridis")
              #}
              
              rf_var_imp <- sd(dat$y)
              
            } else if(is.factor(df_rf[,feature][[1]])){
              
              df_rf <- as.data.frame(df_rf)
              df_rf[,feature] <- gsub("Bef. LLIN dist.","< LLIN dist.",df_rf[,feature])
              df_rf[,feature] <- gsub("\\+","\n+",df_rf[,feature])
              df_rf[,feature] <- gsub("< LLIN dist.","< LLIN\ndist.",df_rf[,feature])
              df_rf[,feature] <- gsub("LLIN***","LLIN\n***",df_rf[,feature], fixed = T)
              
              dat$x <- gsub("\\+","\n+",dat$x)
              dat$x <- gsub("< LLIN dist.","< LLIN\ndist.",dat$x)
              dat$x <- gsub("LLIN***","LLIN\n***", dat$x , fixed = T)
              
              if(isTRUE(plot_pdp)){    
              if(feature=="kdrw" & indicator == "physiological_resistance_kdre"){
                dat$x[which( dat$x=="RR")] = "RR***"
                dat$x = as.factor(dat$x)
                dat$x = factor(dat$x, levels = c("SS","RS","RR***"))
              }
              if(feature=="kdrw" & indicator == "physiological_resistance_ace1"){
                dat$x[which( dat$x=="RR")] = "RR**"
                dat$x[which( dat$x=="RS")] = "RS*"
                  dat$x = as.factor(dat$x)
                  dat$x = factor(dat$x, levels = c("SS","RS*","RR**"))
                  
              }
              p <- p +  
                geom_col(data = dat, aes(x = x, y = y), fill = "#009E73")
              }
              
              if(feature=="VCM" &  indicator == "exophagy" & code_pays=="CI" & species=="An. gambiae s.l."){
                df_rf$VCM = as.factor(df_rf$VCM)
                df_rf$VCM = factor(df_rf$VCM, levels = c("< LLIN\ndist.","LLIN\n***","LLIN \n+ IEC*", "LLIN \n+ IRS**", "LLIN \n+ Larv.*"))
                  }
              
              if(feature=="kdrw" & indicator == "physiological_resistance_kdre"){
                df_rf$kdrw[which( df_rf$kdrw=="RR")] = "RR***"
                df_rf$kdrw = as.factor(df_rf$kdrw)
                df_rf$kdrw = factor(df_rf$kdrw, levels = c("SS","RS","RR***"))
              }
              if(feature=="kdrw" & indicator == "physiological_resistance_ace1"){
                df_rf$kdrw[which( df_rf$kdrw=="RS")] = "RS*"
                df_rf$kdrw[which( df_rf$kdrw=="RR")] = "RR**"
                df_rf$kdrw = as.factor(df_rf$kdrw)
                df_rf$kdrw = factor(df_rf$kdrw, levels = c("SS","RS*","RR**"))
              }
              
              if(feature=="int_ext"){
                df_rf$int_ext[which( df_rf$int_ext=="i")] = "int."
                df_rf$int_ext[which( df_rf$int_ext=="e***")] = "ext.***"
                df_rf$int_ext = factor(df_rf$int_ext, levels = c("int.","ext.***"))
                dat$x <- gsub("e***","ext.***", dat$x , fixed = T)
              }
              
               p <- p + 
                 geom_bin2d(data = df_rf %>% mutate(resp_var = ifelse(resp_var=="Presence",geombin_ymax,0)), aes_string(x = feature, y = "resp_var"), colour = "black") +   scale_fill_gradient(low = "gray90", high = "black") #scale_fill_continuous(type = "viridis")
              #geom_point(data = pd[pd$yhat.id == "avg", ], aes_string(x = feature, y = "yhat", group = "yhat.id"), size = 1, colour = "black") + 
              #geom_point(data = pd[pd$yhat.id != "avg", ], aes_string(x = feature, y = "yhat", group = "yhat.id"), size = 1, colour = "black") 
              
              rf_var_imp <- (max(dat$y)-min(dat$y))/4
              
            }
          }
          
        }
      }
      
      
    #}
      
      font_f <- 1
    if(any(grepl("\\*",dat$x))){ 
      font_f <- 2
    }
      
      if(!is.null(glmm[[1]])){
        if(feature %in% colnames(df_glmm)){
          pglmm = jtools::effect_plot(glmm_mod@model, pred = !!feature, interval = TRUE)
          dat = ggplot_build(pglmm)$data
          
          if(is.numeric(df_glmm[,feature][[1]])){
            dat[[1]]$x = dat[[1]]$x + attr(df_glmm[,feature],'scaled:center')
            dat[[2]]$x = dat[[2]]$x + attr(df_glmm[,feature],'scaled:center')
          }

         # if( is.numeric(df_glmm[,feature][[1]]) & ((get_all_plots == TRUE |  group=="Vector control") | (abs(max(dat[[1]]$y)-min(dat[[1]]$y))>=0.05 & glmm_tidy[grepl(feature,glmm_tidy$term),]$p.value < 0.1))){
          if( is.numeric(df_glmm[,feature][[1]]) & ((get_all_plots == TRUE |  group=="Vector control" | feature %in% c('lsm_c_pland_2000_3_1','lsm_c_pland_2000_4_1','lsm_c_pland_2000_4_11','lsm_c_pland_2000_4_16')) | (glmm_tidy[grepl(feature,glmm_tidy$term),]$p.value < 0.05))){
            
            p <- p + 
              geom_ribbon(data = dat[[2]], aes(x = x, y =y, ymin = ymin, ymax = ymax), alpha = 0.2, fill = "grey20") +
              geom_line(data = dat[[1]], aes(x = x, y =y), colour = "#E69F00", size = 0.5)
            
            glmm_var_imp <- sd(dat[[1]]$y)
            
         # } else if( is.factor(df_glmm[,feature][[1]]) & (( get_all_plots == TRUE |  group=="Vector control") | (abs(max(dat[[1]]$y)-min(dat[[1]]$y))>=0.05 & min(glmm_tidy[grepl(feature,glmm_tidy$term),]$p.value) < 0.1 ))){
          } else if( is.factor(df_glmm[,feature][[1]]) & (( get_all_plots == TRUE |  group=="Vector control" | feature %in% c('lsm_c_pland_2000_3_1','lsm_c_pland_2000_4_1','lsm_c_pland_2000_4_11','lsm_c_pland_2000_4_16')) | (min(glmm_tidy[grepl(feature,glmm_tidy$term),]$p.value) < 0.05 ))){

             if(feature == "VCM" &  indicator=="physiological_resistance_kdre" & species=="An. coluzzii"){dat[[2]]$ymax[2] = 0.49}
            if(feature == "VCM" &  indicator=="physiological_resistance_kdre" & species=="An. gambiae ss."){dat[[2]]$ymax[4] = 0.55}
            
             p <- p +  
              geom_errorbar(data = dat[[2]], aes(x = x, y =y, ymin = ymin, ymax = ymax), width=.1, colour ="grey20") + 
              geom_point(data = dat[[1]], aes(x = x, y = y), size = 1.5, colour = "#E69F00")
            
            glmm_var_imp <- (max(dat[[1]]$y)-min(dat[[1]]$y))/4
            
          }
        }
      }
      
      lab_x <- trimws(lab_x)
      
      if(grepl("\\*",lab_x)){
        font_f <- 2
      }
      
      p <- p +
        #annotate("label", x=-Inf,hjust = -0.09, y = 0.6, label = lab_x, size = 2, colour = "grey10", alpha = 0.7) #+ 
        annotate("text", x=-Inf,hjust = -0.1, y = title_y, label = lab_x, size = 2.2, colour = "grey10", fontface = font_f)  #  y = 0.71
      #annotate("text", x=-Inf,hjust = -0.18, y = 0.85, label = group, size = 2, colour = "grey20")
      
      cols <- unlist(purrr::map(ggplot_build(p)$data, ~colnames(.)))
      
      if((get_all_plots == FALSE & group!="Vector control" & !(feature %in% c('lsm_c_pland_2000_3_1','lsm_c_pland_2000_4_1','lsm_c_pland_2000_4_11','lsm_c_pland_2000_4_16')) & (is.null(glmm[[1]]) | min(glmm_tidy[grepl(feature,glmm_tidy$term),]$p.value) > 0.05 )) | (code_pays=="CI" & indicator=="early_biting" & species=="An. gambiae s.l." & feature == "HBB2")){
        return(NULL)
      } else if(("y" %in% cols | get_all_plots == TRUE) & length(ggplot_build(p)$data)>=3){
        p <- p + geom_hline(yintercept = avg_prob, color = "darkred", size = 0.4,linetype = "dashed")
        
        if(!is.data.frame(dat[[1]])){
          range_preds <- NA
        } else {
          range_preds = round(abs(max(dat[[1]]$y)-min(dat[[1]]$y)),2)
        }
        
        return(list(plot = p,rf_var_imp = rf_var_imp,glmm_var_imp = glmm_var_imp, range_preds = range_preds))
      } else {
        return(NULL)
      }
      
    })
    
    
    names(pdps) <- features
    # variable importance
    df_varimp <- data.frame(var = character(), RF = numeric(), GLMM = numeric(), stringsAsFactors = F)
    for(i in 1:length(pdps)){
      df_varimp[nrow(df_varimp) + 1,] = c(names(pdps)[[i]],pdps[[i]]$rf_var_imp,pdps[[i]]$glmm_var_imp)
    }
    df_varimp <- df_varimp %>% 
      pivot_longer(c( RF,GLMM)) %>% 
      mutate(value = as.numeric(value)) %>% 
      mutate(value = ifelse(is.na(value), 0 , value)) %>%
      left_join(df_vars_labs) %>%
      mutate(country = code_pays, indicator = indicator, species = species)
    
    
    varimplot <- ggplot(df_varimp, aes(y=reorder(label, value), x = value, fill = name,group = name, label = label)) + 
      geom_bar(position="dodge", stat="identity") + 
      scale_fill_manual("model", values = c("GLMM" = "#E69F00", "RF" = "#009E73")) + 
      theme_light() + 
      #geom_text(size=1.6,position = position_dodge(0.9),hjust=-0.1,label.padding = unit(0.2, "lines"))
      theme(axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            legend.position = "bottom") + 
      facet_grid(label_group~name)
    
    #pdps_plots = list(varimplot)
    pdps_plots = list()
    # ord <- df_varimp %>% arrange(desc(value)) 
    # ord <- unique(ord$var)
    # pdps <- pdps[ord]
    
    for(i in 1:length(pdps)){
      pdps_plots[[i+1]] <- pdps[[i]]$plot
    }
    
    range_preds <- data.frame(var = character(), range_pred = numeric())
    for(i in 1:length(pdps)){
      if(!is.null(pdps[[i]])){
      range_preds <- rbind(range_preds, data.frame(var= names(pdps[i]),range_pred=  pdps[[i]]$range_preds  ))
      }
    }

    
    # create plots for model quality
    df_qual <- data.frame(model = c("GLMM","RF"), quality = c(round(r2,2),round(auc_rf,2)), quality_lab = c(paste0("R² = ",round(r2,2)),paste0("AUC = ",round(auc_rf,2))), line = c(NA,.6))
    qual_plot <- ggplot(df_qual, aes(x=model,y=quality, label = quality_lab, fill = model)) + 
      geom_col() + 
      ylim(0,1) +
      scale_fill_manual(breaks = c("GLMM", "RF"), values=c("#E69F00", "#009E73")) +
      geom_text(vjust = ifelse(r2<.8,-0.5,0.5), size= 2) +
      facet_grid(.~model, scales = "free") +
      #geom_hline(aes(yintercept = line),linetype = "dashed", color = "darkred", size = 0.3) + 
      theme_light() + 
      xlab("") +
      #ggtitle('Power of the models') + 
      #labs(caption = '') +
      theme(#plot.title = element_text(size = 7.5,colour = "grey0"),
        plot.title = element_blank(),
        #plot.subtitle = element_text(size = 6,colour = "grey20",face="italic"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 6),
        axis.ticks.x=element_blank(),
        legend.position = "none",
        strip.text.x = element_text(size = 7, colour = "black"))
    
    pdps_plots[[1]] <- qual_plot 
    
    pdps_plots <- pdps_plots[which(!sapply(pdps_plots, is.null))]
    
    #p1 <- list(p,p2)               #c(p1,pdps)
    if(code_pays=='BF'){
      code_pays="Diébougou (BF)"
    } else {
      code_pays="Korhogo (IC)"
    }
    
    if(get_all_plots==F){
      n_row = 13 #10
    } else {
      n_row = 15 # 14
    }
    
    p_final <- grid.arrange(grobs = pdps_plots, nrow = n_row,ncol = 1, top =  textGrob(paste0(code_pays,"\n",species),x=0.028,hjust = 0,gp=gpar(fontsize=10)))
    # library(egg)
    #p_final <- grid.arrange(grobs = lapply(pdps_plots,set_panel_size, width = unit(2.5, "cm"),  height = unit(2, "cm")), ncol = 5,top =  textGrob(paste0(code_pays," - ",indicator,' - ',species,' - occurence = ',round(nrow(df_glmm[which(df_glmm$resp_var==1),])/nrow(df_glmm),2)*100," % (n = ",nrow(df_glmm[which(df_glmm$resp_var==1),]),", n tot = ",nrow(df_glmm),")\nAUC GLMM = ",round(auc_glmm,2)," ; AUC RF = ",round(auc_rf,2)),gp=gpar(fontsize=12)))
    
    p_final <- cowplot::ggdraw(p_final) + theme(plot.background = element_rect(fill="white", color = NA))
    
  } else {
    p_final <- ggplot() +theme_void() +geom_text(aes(0,0,label='N/A')) + ggtitle(paste0(code_pays," - ",indicator,' - ',species,' - occurence = ',round(nrow(df_glmm[which(df_glmm$resp_var==1),])/nrow(df_glmm),2)*100," % (n = ",nrow(df_glmm[which(df_glmm$resp_var==1),]),", n tot = ",nrow(df_glmm),")\nAUC GLMM = ",round(auc_glmm,2)," ; AUC RF = ",round(auc_rf,2)))
  }
  
  #return(list(p_final=p_final,df_varimp=df_varimp))
  #return(p_final)
  return(range_preds)
  
}


# rf=res[1,]$rf[[1]]
# glmm=res[1,]$glmm_aic[[1]]
# indicator='exophagy'
# species="An. funestus"
# code_pays="BF"
# get_all_plots = TRUE

fun_plot_pdp6 <- function(rf, glmm, indicator, species, code_pays, get_all_plots = TRUE){
  
  library(iml)
  library(vip)
  library(jtools)
  library(MLmetrics)
  library(precrec)
  library(pdp)
  library(caret)
  library(ranger)
  library(glmmTMB)
  library(broom.mixed)
  library(gridExtra)
  library(cowplot)
  #library(egg)
  
  auc_glmm <- NA
  prauc_glmm <- NA
  
  auc_rf <- NA
  prauc_rf <- NA
  
  if(!is.null(rf[[1]])){
    
    rf_mod = rf$mod
    df_rf = rf$df_mod %>% dplyr::select(resp_var, rf_mod$finalMode$xNames)
    df_cv_rf <-  rf$df_cv
    
    auc_rf <- AUC(y_true = df_cv_rf$obs,y_pred = df_cv_rf$pred)
    prauc_rf <- PRAUC(y_true = df_cv_rf$obs,y_pred = df_cv_rf$pred)
    
  }
  glmm_mod = glmm$mod
  df_glmm = glmm$mod@model$frame
  df_cv_glmm <-  glmm$df_cv
  
  auc_glmm <- AUC(y_true = df_cv_glmm$obs,y_pred = df_cv_glmm$pred)
  prauc_glmm <- PRAUC(y_true = df_cv_glmm$obs,y_pred = df_cv_glmm$pred)
  
  
  # get predictors labels
  df_vars_labs <- data.frame(var = setdiff(colnames(glmm_mod@model$frame), c("resp_var","codevillage","pointdecapture2")))
  
  
  # ##shapley
  # mod <- Predictor$new(rf_mod, data = df_rf)
  # df_rf_shap <- split(df_rf, seq(nrow(df_rf)))
  # shap_df <- furrr::future_map_dfr(df_rf_shap, ~Shapley$new(mod, x.interest = .)$results)
  # 
  # shap_df  <-  shap_df  %>% 
  #   filter(class=="Presence", feature!="resp_var")
  # 
  # p1 <- ggplot(shap_df, aes(x = phi, y = reorder(feature, phi))) +
  #   geom_violin() +
  #   xlab("SHAP value") +
  #   ylab(NULL) + geom_vline(aes(xintercept = 0))
  
  if(nrow(df_vars_labs)>0){
    
    if(!is.null(rf[[1]])){
      df_vars_labs_rf <- data.frame(var = rf_mod$finalMode$xNames)
      df_vars_labs <- unique(rbind(df_vars_labs,df_vars_labs_rf))
    }
    
    df_vars_labs <- fun_get_predictors_labels(df_vars_labs,"var") %>% distinct()
    
    if(indicator == "late_biting"){
      df_vars_labs <- df_vars_labs %>%
        mutate(label_detail = gsub("day of collection","day preceding collection",label_detail)) %>%
        mutate(label_detail = gsub("hour of collection","night of collection",label_detail))
    }
    
    if(!(indicator %in% c("presence","abundance"))){
      df_vars_labs <- df_vars_labs %>%
        mutate(label_detail = gsub("b/w 0 and 4.28571428571429 weeks","month preceding collection",label_detail)) %>%
        mutate(label_detail = gsub("b/w 0 and 0 weeks","day of collection",label_detail)) %>%
        mutate(label_detail = gsub("\n2000 m buffer","",label_detail))
    }
    
    if(indicator %in% c("presence","abundance")){
      df_vars_labs <- df_vars_labs %>%
        mutate(label_detail = gsub("hour of collection","night of collection",label_detail))
    }
    
    # get glmm in tidy form
    if(!is.null(glmm[[1]])){
      glmm_tidy <- tidy(glmm_mod@model, conf.int = TRUE, exponentiate = ifelse(indicator == "abundance", FALSE, TRUE))
      glmm_tidy <- glmm_tidy %>%
        mutate(p.value2 = case_when(
          p.value <= 0.001 ~ "***",
          p.value > 0.001 & p.value <= 0.01  ~  "**",
          p.value > 0.01 & p.value <= 0.05 ~ "*",
          p.value > 0.05 ~ ""
        ))
    }
    
    
    baseline_prauc <- df_cv_glmm %>% group_by(obs) %>% summarise(n=n())
    baseline_prauc <- baseline_prauc$n[which(baseline_prauc$obs==1)]/sum(baseline_prauc$n)
    # open iml models
    #rf_mod_iml <- Predictor$new(rf_mod, data = df_rf, class = ifelse(indicator!="physiological_resistance_kdrw","Presence","Absence"))
    #rf_mod_allvars_iml <- Predictor$new(rf_allvars_mod, data = df_rf_allvars, class = ifelse(indicator!="physiological_resistance_kdrw","Presence","Absence"))
    
    # draw ROC curves            
    if(!is.null(rf[[1]])){
      scores =  list(df_cv_rf$pred,df_cv_glmm$pred)
      modnames = c("GLMM","RF")
      cols = c("#E69F00","#009E73")
    } else {
      scores =  list(df_cv_glmm$pred)
      modnames = c("GLMM")
      cols = c("#E69F00")
    }
    
    
    samps <- list(scores = scores, labels = df_cv_glmm$obs,  modnames = modnames)
    mdat <- mmdata(samps[["scores"]], samps[["labels"]],modnames = samps[["modnames"]])
    m <- evalmod(mdat)
    p = autoplot(m, curvetype = "ROC") +
      theme(plot.title = element_blank(),
            axis.title.x = element_text(size = 7),
            axis.title.y = element_text(size = 7),
            axis.text.y = element_text(size = 7),
            axis.text.x = element_text(size = 7),
            legend.position=c(0.7, 0.2),
            legend.text = element_text(size = 7),
            legend.background=element_rect(fill = alpha("white", 0)),
            legend.key.size = unit(0.2, "cm")
      ) +
      scale_color_manual(values=cols) +
      annotate("text", x = 0.35, y = 0.95, label = paste0("AUC GLMM = ",round(auc_glmm,2)), size = 2.5) +
      annotate("text", x = 0.35, y = 0.85, label = paste0("AUC RF = ",round(auc_rf,2)), size = 2.5)
    
    
    p2 = autoplot(m, curvetype = "PRC") +
      theme(plot.title = element_blank(),
            axis.title.x = element_text(size = 7),
            axis.title.y = element_text(size = 7),
            axis.text.y = element_text(size = 7),
            axis.text.x = element_text(size = 7),
            legend.position=c(0.7, 0.2),
            legend.text = element_text(size = 7),
            legend.background=element_rect(fill = alpha("white", 0)),
            legend.key.size = unit(0.2, "cm")
      ) +
      scale_color_manual(values=cols) +
      annotate("text", x = 0.35, y = 0.95, label = paste0("PRC baseline = ",round(baseline_prauc,2)), size = 2.5) +
      annotate("text", x = 0.35, y = 0.85, label = paste0("PRC GLMM = ",round(prauc_glmm,2)), size = 2.5) +
      annotate("text", x = 0.35, y = 0.75, label = paste0("PRC RF = ",round(prauc_rf,2)), size = 2.5)
    
    
    #+ ggtitle(paste0(indicator,'-',species,"-",period_interv))
    
    # draw VIP
    # vi_rf <- vi(rf_mod$finalModel,nsim = 20 ,method = "permute", target = "resp_var",reference_class = "Presence", train = df_rf, metric = "auc")
    # vi_glmm <- vi(glmm_mod@model,nsim = 20 ,method = "permute", target = "resp_var", train = df_glmm, metric = "auc",reference_class = "1",allow.new.levels=TRUE)
    # 
    # vi_rf$model <- "RF"
    # vi_glmm$model <- "GLMM"
    # 
    # vi <- bind_rows(vi_rf,vi_glmm) %>%
    #   left_join(df_vars_labs, by = c("Variable" = "var")) %>%
    #   filter(!is.na(label)) %>%
    #   complete(label, nesting(model),fill = (list( Importance = 0, StDev = 0)))
    # 
    # vi_plot <- ggplot(vi, aes(x = Importance , y = label, label = label, group = model, fill = model)) +
    #   geom_bar(position = 'dodge', stat="identity", width = 0.6) + 
    #   geom_errorbar(aes(ymin=Importance-StDev, ymax=Importance+StDev)) +
    #   theme_bw() + 
    #   geom_text(size=1.6,position = position_dodge(0.9),hjust=-0.1,label.padding = unit(0.2, "lines")) + #,aes(fontface=2)
    #   theme(axis.text.y = element_blank(),
    #         axis.title.x = element_text(size = 7),
    #         plot.subtitle = element_text(size = 7, face="bold"),
    #         legend.position = "none"
    #   ) +
    #   ylab("") + 
    #   xlab("") +
    #   xlim(NA,max(vi$Importance, na.rm = T) + max(vi$Importance, na.rm = T)*2.5) +
    #   labs(subtitle = "Variable importance")
    
    
    # get rsquared from the multivariate models
    # df_mod=df_glmm
    # r2 = MuMIn::r.squaredGLMM(glmm_mod@model)
    # 
    
    # draw PDP
    features = df_vars_labs$var
    g <- glmm_tidy %>% arrange(p.value)
    features <- features[order(match(features, g$term))]
    
    
    var_to_plot <- "Presence"
    if(indicator=="physiological_resistance_kdrw" & !is.null(rf[[1]])){
      df_rf <- df_rf %>% mutate(resp_var=ifelse(resp_var == "Presence","Absence","Presence"))
      var_to_plot <- "Absence"
    }
    if(indicator=="presence" & species =="An. gambiae s.l." & !is.null(rf[[1]])){
      df_rf <- df_rf %>% mutate(resp_var=ifelse(resp_var == "Presence","Absence","Presence"))
      var_to_plot <- "Absence"
    }
    
    pred_wrapper_classif <- function(object, newdata) {
      p <- predict(object, newdata = newdata, type ="prob")[,var_to_plot]
      c("avg" = mean(p))
    }
    
    pred_wrapper_classif_werror <- function(object, newdata) {
      p <- predict(object, newdata = newdata, type ="prob")[,var_to_plot]
      c("avg" = mean(p), "avg-1sd" = mean(p) - sd(p), "avg+1sd" = mean(p) + sd(p))
    }
    
    pdps <- lapply(features, FUN = function(feature) {
      
      rf_var_imp=NA
      glmm_var_imp=NA
      
      cat(feature)
      # write label
      lab_df <- df_vars_labs %>% filter(var==feature) %>% distinct()
      lab_x <- lab_df$label_detail
      lab_x <- gsub("\\(","\n",lab_x)
      if(!is.na(lab_df$unit)){
        if(!grepl("collection",lab_x)){
          lab_x <- paste0(lab_x,"\n",lab_df$unit)
        } else {
          lab_x <- paste0(lab_x,", ",lab_df$unit)
        }
      }
      
      
      if(!is.null(rf[[1]])){
        if(feature %in% rf_mod$finalMode$xNames){
          pd2 <- partial(rf_mod, pred.var = feature, pred.fun = pred_wrapper_classif_werror, train = df_rf)
          pd <- partial(rf_mod, pred.var = feature, ice = T, train = df_rf, center = T)
          #pd$yhat[which(pd$yhat<0)] <-0 
          mean_pdp <- pd %>% group_by_(feature) %>% summarise(mean=mean(yhat, na.rm = T), sdplus = mean(yhat, na.rm = T) + sd(yhat, na.rm = T),sdmoins = mean(yhat, na.rm = T) - sd(yhat, na.rm = T) ) %>% pivot_longer(c(mean,sdplus,sdmoins))
          #pd$yhat[which(pd$yhat>1)] <-1 
          
          
          
          rf_mod_iml <- Predictor$new(rf_mod, data = df_rf,  type = "prob",class = ifelse(indicator!="physiological_resistance_kdrw","Presence","Absence"))
          ice = FeatureEffect$new(rf_mod_iml, feature = "DNMH", method = "pdp+ice", center.at = min(df_rf$DNMH))
          mean_pdp <- ice$results %>% filter(.type=='ice') %>% group_by_(feature) %>% summarise(mean=mean(.value, na.rm = T), sdplus = mean(.value, na.rm = T) + sd(.value, na.rm = T),sdmoins = mean(.value, na.rm = T) - sd(.value, na.rm = T) ) %>% pivot_longer(c(mean,sdplus,sdmoins))
          
          ggplot() + 
            geom_line(data= ice$results %>% filter(.type == "ice"), aes_string(x=feature, y=".value", group = ".id"), size = 0.02) + 
            #geom_line(data= ice$results %>% filter(.type == "pdp"), aes_string(x=feature, y=".value"), size = 1, color = "red") + 
            geom_hline(aes(yintercept = 0)) + 
            geom_line(data= mean_pdp, aes_string(x=feature, y="value", group="name"), size = 1, color = "red")
            #geom_smooth(data= ice$results %>% filter(.type == "pdp"), aes_string(x=feature, y=".value"), method = "loess", color = "red")
          
          
          
          # get smoothed version of RF pdp
          if(is.numeric(df_rf[,feature][[1]])){
            p <- autoplot(pd, smooth = T)  
            dat <- ggplot_build(p)$data[[2]]
          } else {
            dat <- pd
            colnames(dat)[which(colnames(dat)==feature)]="x"
            colnames(dat)[which(colnames(dat)=="yhat")]="y"
          }
        }
      }
      
      if(!is.null(glmm[[1]])){
        if(feature %in% glmm_tidy$term){
          lab_x <- paste0(glmm_tidy$p.value2[which(glmm_tidy$term==feature)]," ",lab_x)
        }
        if(!grepl("\n",lab_x)){
          lab_x <- paste0(lab_x,"\n")
        }
        
        
        t <- grepl(feature,glmm_tidy$term)
        
        if(TRUE %in% t & is.factor(df_rf[,feature])){
          t <- glmm_tidy[t,]
          t$term <- gsub(feature,"",t$term)
          t <- t %>% dplyr::select(term,p.value2)
          colnames(t)[which(colnames(t)=='term')]=feature
          pd <- pd %>% left_join(t) 
          pd$p.value2[which(is.na(pd$p.value2))] <- ""
          pd$feature_rep <- pd[,feature]
          pd[,feature] = paste0(pd[,feature],"",pd$p.value2)
          
          pd_4_rep <- unique(pd[,c(feature,"feature_rep")])
          colnames(pd_4_rep) <- rev(colnames(pd_4_rep))
          df_rf <- left_join(df_rf,pd_4_rep)
          colnames(df_rf)[which(colnames(df_rf)==feature)]="useless"
          colnames(df_rf)[which(colnames(df_rf)=="feature_rep")]=feature
          df_rf[,feature] <- as.factor(df_rf[,feature] )
          
          dat$x <- pd[,feature]
          df_rf$useless <- NULL
        }
      }
      
      p <- ggplot() + 
        ylim(-0.08,1.05) +
        theme_light() + 
        xlab(lab_x) + 
        theme(axis.title.y = element_blank(),
              axis.text.x = element_text(size = 7),
              axis.text.y = element_text(size = 6),
              axis.title.x = element_text(size = 8),
              legend.position = "none") #+ 
      # geom_rug(data = df_rf %>% filter(resp_var=="Absence"),aes_string(x = feature),sides="b",colour ="grey20") + 
      # geom_rug(data = df_rf %>% filter(resp_var=="Presence"),aes_string(x = feature),sides="t",colour ="grey20")
      #geom_bin2d(data = df_rf, aes_string(x = feature, y = "resp_var") ,  binwidth = c(max(df_rf[,feature])/15,.05)) +  scale_fill_continuous(type = "viridis")
      
      #modnames = c("RF","RF all preds","RF feat. select." ,"GLMM LRT","GLMM AIC"
      #scale_color_manual(values=c("#009E73", "#E69F00","black","red","grey")) +
      
      if(!is.null(rf[[1]])){
        if(feature %in% rf_mod$finalMode$xNames){
          #if((abs(max(pd$yhat)-min(pd$yhat))>=0.1 | get_all_plots == TRUE) | (!is.null(glmm[[1]]) & feature %in% colnames(df_glmm))){
          if(get_all_plots == TRUE | (!is.null(glmm[[1]]) & feature %in% colnames(df_glmm))){
            
            if(is.numeric(df_rf[,feature][[1]])){
              p <- p + 
                geom_line(data = dat, aes(x = x, y = y), size = 0.5, colour = "#009E73")
              #geom_line(data = pd[pd$yhat.id == "avg", ], aes_string(x = feature, y = "yhat", group = "yhat.id"), size = 0.5, colour = "black") +
              #geom_line(data = pd[pd$yhat.id != "avg", ], aes_string(x = feature, y = "yhat", group = "yhat.id"), linetype = "dashed", size = 0.25) +
              
              #if(feature %in% c("NMA","LUS")){
              p <- p + geom_bin2d(data = df_rf %>% mutate(resp_var = ifelse(resp_var=="Presence",1,0)), aes_string(x = feature, y = "resp_var"), colour = "black") +  scale_fill_gradient(low = "gray90", high = "black")# scale_fill_continuous(type = "viridis")
              #} else {
              #  p <- p + geom_bin2d(data = df_rf %>% mutate(resp_var = ifelse(resp_var=="Presence",1,0)), aes_string(x = feature, y = "resp_var"), colour = "black",  binwidth = c(max(df_rf[,feature])/15,.05)) +  scale_fill_gradient(low = "gray90", high = "black")# scale_fill_continuous(type = "viridis")
              #}
              
              rf_var_imp <- sd(dat$y)
              
            } else if(is.factor(df_rf[,feature][[1]])){
              
              df_rf <- as.data.frame(df_rf)
              df_rf[,feature] <- gsub("\\+","\n+",df_rf[,feature])
              dat$x <- gsub("\\+","\n+",dat$x)
              
              p <- p +  
                geom_col(data = dat, aes(x = x, y = y), fill = "#009E73") + 
                geom_bin2d(data = df_rf %>% mutate(resp_var = ifelse(resp_var=="Presence",1,0)), aes_string(x = feature, y = "resp_var"), colour = "black") +   scale_fill_gradient(low = "gray90", high = "black") #scale_fill_continuous(type = "viridis")
              #geom_point(data = pd[pd$yhat.id == "avg", ], aes_string(x = feature, y = "yhat", group = "yhat.id"), size = 1, colour = "black") + 
              #geom_point(data = pd[pd$yhat.id != "avg", ], aes_string(x = feature, y = "yhat", group = "yhat.id"), size = 1, colour = "black") 
              
              rf_var_imp <- (max(dat$y)-min(dat$y))/4
              
            }
          }
          
        }
      }
      
      
      
      if(!is.null(glmm[[1]])){
        if(feature %in% colnames(df_glmm)){
          pglmm = jtools::effect_plot(glmm_mod@model, pred = !!feature, interval = TRUE)
          dat = ggplot_build(pglmm)$data
          if(is.numeric(df_glmm[,feature][[1]])){
            dat[[1]]$x = dat[[1]]$x + attr(df_glmm[,feature],'scaled:center')
            dat[[2]]$x = dat[[2]]$x + attr(df_glmm[,feature],'scaled:center')
          }
          
          if(is.numeric(df_glmm[,feature][[1]])){
            p <- p + 
              geom_ribbon(data = dat[[2]], aes(x = x, y =y, ymin = ymin, ymax = ymax), alpha = 0.2, fill = "grey20") +
              geom_line(data = dat[[1]], aes(x = x, y =y), colour = "#E69F00", size = 0.5)
            
            glmm_var_imp <- sd(dat[[1]]$y)
            
          } else if(is.factor(df_glmm[,feature][[1]])){
            p <- p +  
              geom_errorbar(data = dat[[2]], aes(x = x, y =y, ymin = ymin, ymax = ymax), width=.1, colour ="grey20") + 
              geom_point(data = dat[[1]], aes(x = x, y = y), size = 1.5, colour = "#E69F00")
            
            glmm_var_imp <- (max(dat[[1]]$y)-min(dat[[1]]$y))/4
            
          }
        }
      }
      
      cols <- unlist(purrr::map(ggplot_build(p)$data, ~colnames(.)))
      if("y" %in% cols | get_all_plots == TRUE){
        return(list(plot = p,rf_var_imp = rf_var_imp,glmm_var_imp = glmm_var_imp))
      } else {
        return(NULL)
      }
      
    })
    
    
    names(pdps) <- features
    # variable importance
    df_varimp <- data.frame(var = character(), RF = numeric(), GLMM = numeric(), stringsAsFactors = F)
    for(i in 1:length(pdps)){
      df_varimp[nrow(df_varimp) + 1,] = c(names(pdps)[[i]],pdps[[i]]$rf_var_imp,pdps[[i]]$glmm_var_imp)
    }
    df_varimp <- df_varimp %>% 
      pivot_longer(c( RF,GLMM)) %>% 
      mutate(value = as.numeric(value)) %>% 
      mutate(value = ifelse(is.na(value), 0 , value)) %>%
      left_join(df_vars_labs) %>%
      mutate(country = code_pays, indicator = indicator, species = species)
    
    
    varimplot <- ggplot(df_varimp, aes(y=reorder(label, value), x = value, fill = name,group = name, label = label)) + 
      geom_bar(position="dodge", stat="identity") + 
      scale_fill_manual("model", values = c("GLMM" = "#E69F00", "RF" = "#009E73")) + 
      theme_light() + 
      #geom_text(size=1.6,position = position_dodge(0.9),hjust=-0.1,label.padding = unit(0.2, "lines"))
      theme(axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            legend.position = "bottom") + 
      facet_grid(label_group~name)
    
    pdps_plots = list(varimplot)
    
    ord <- df_varimp %>% arrange(desc(value)) 
    ord <- unique(ord$var)
    pdps <- pdps[ord]
    for(i in 1:length(pdps)){
      pdps_plots[[i+1]] <- pdps[[i]]$plot
    }
    pdps_plots <- pdps_plots[which(!sapply(pdps_plots, is.null))]
    
    #p1 <- list(p,p2)               #c(p1,pdps)
    p_final <- grid.arrange(grobs = pdps_plots, ncol = 5,top =  textGrob(paste0(code_pays," - ",indicator,' - ',species,' - occurence = ',round(nrow(df_glmm[which(df_glmm$resp_var==1),])/nrow(df_glmm),2)*100," % (n = ",nrow(df_glmm[which(df_glmm$resp_var==1),]),", n tot = ",nrow(df_glmm),")\nAUC GLMM = ",round(auc_glmm,2)," ; AUC RF = ",round(auc_rf,2)),gp=gpar(fontsize=10)))
    #p_final <- grid.arrange(grobs = lapply(pdps,set_panel_size, width = unit(2.5, "cm"),  height = unit(2, "cm")), ncol = 5,top =  textGrob(paste0(code_pays," - ",indicator,' - ',species,' - occurence = ',round(nrow(df_glmm[which(df_glmm$resp_var==1),])/nrow(df_glmm),2)*100," % (n = ",nrow(df_glmm[which(df_glmm$resp_var==1),]),", n tot = ",nrow(df_glmm),")\nAUC GLMM = ",round(auc_glmm,2)," ; AUC RF = ",round(auc_rf,2)),gp=gpar(fontsize=12)))
    
    p_final <- cowplot::ggdraw(p_final) + theme(plot.background = element_rect(fill="white", color = NA))
    
  } else {
    p_final <- ggplot() +theme_void() +geom_text(aes(0,0,label='N/A')) + ggtitle(paste0(code_pays," - ",indicator,' - ',species,' - occurence = ',round(nrow(df_glmm[which(df_glmm$resp_var==1),])/nrow(df_glmm),2)*100," % (n = ",nrow(df_glmm[which(df_glmm$resp_var==1),]),", n tot = ",nrow(df_glmm),")\nAUC GLMM = ",round(auc_glmm,2)," ; AUC RF = ",round(auc_rf,2)))
  }
  
  return(list(p_final=p_final,df_varimp=df_varimp))
  
}





