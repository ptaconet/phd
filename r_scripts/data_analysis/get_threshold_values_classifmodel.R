    thresholds = seq(0, 1, 0.001)
    aucs_thresholds <- NULL
    for(i in 1:length(thresholds)){
     
      pred_class <- ifelse(df_cv$pred < thresholds[i],0,1)
      if(length(unique(pred_class))==1){
        th_auc <- NA
      } else {
     # th_auc <- MLmetrics::F1_Score(y_true = df_cv$obs,y_pred = pred_class, positive = "1")
     # th_auc <- MLmetrics::PRAUC(y_true = df_cv$obs,y_pred = pred_class)
      th_auc <- MLmetrics::AUC(y_true = df_cv$obs,y_pred = pred_class)
      ## ci-dessous : exactement équivalent à th_auc <- MLmetrics::AUC(y_true = df_cv$obs,y_pred = pred_class). voir justification ici : http://www.medicalbiostatistics.com/roccurve.pdf
      # sn <- MLmetrics::Sensitivity(y_true = df_cv$obs,y_pred = pred_class, positive = "1")
      # sp <- MLmetrics::Specificity(y_true = df_cv$obs,y_pred = pred_class, positive = "1")
      # th_auc <- sqrt((1-sn)^2+ (1-sp)^2)
      }
      
      aucs_thresholds <-c(aucs_thresholds, th_auc)
      
    }
    
    max_metric <- max(aucs_thresholds, na.rm = T)
    threshold <- thresholds[which.max(aucs_thresholds)]
