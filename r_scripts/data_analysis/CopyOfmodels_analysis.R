library(tidyverse)
library(patchwork)
library(ggmap)
library(sf)
library(DBI)
library(sjPlot) # see : https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html


### connect to the database
path_to_db <- "data/react_db/react_db.gpkg" 
react_gpkg <- DBI::dbConnect(RSQLite::SQLite(),dbname = path_to_db) 

# open model results
model_results <- readRDS("/home/ptaconet/Bureau/data_analysis/model_results.rds")



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
  


##### functions for plots 
## map for presence

fun_plot_map <- function(model_results, species, codepays, indicator){
  
  df_map <- model_results %>%
    filter(response_var == species, code_pays == codepays, mod == indicator)
  
  roi <- st_read(path_to_db,"contexte_frontieresreact") %>% 
    filter(codepays == model_results$code_pays[[1]]) %>%
    st_transform(4326) %>%
    st_bbox() %>%
    as.numeric()
  myLocation <- c(roi[1], roi[2], roi[3], roi[4])
  myMap <- get_map(location=myLocation, source="osm",crop=FALSE)
  
  
  df_map <- df_map$df[[1]]
  
  if(indicator == "presence"){
    
    df_map <- df_map %>%
      group_by(codevillage, nummission, mean_date_mission) %>%
      summarise(resp_var = sum(resp_var), X = mean(X_4326), Y = mean(Y_4326)) %>%
     mutate(biting_status = ifelse(resp_var > 0, "presence", "absence"))
  
    map <- ggmap(myMap) + 
     geom_point(aes(x = X, y = Y, color = biting_status), data = df_map) + 
      facet_wrap(.~nummission) + 
      theme(legend.position = "bottom")
  
  } else if (indicator == "abundance"){
    df_map <- df_map %>%
      group_by(codevillage,nummission, mean_date_mission) %>%
      summarise(resp_var = sum(resp_var), X = mean(X_4326), Y = mean(Y_4326)) %>%
      rename(n_bites = resp_var)
    
    map <- ggmap(myMap) + 
      geom_point(aes(x = X, y = Y, size = n_bites), data = df_map, color="darkred", alpha = .5) + 
      facet_wrap(.~nummission)
    
  }
  #map <- shift_legend2(map)
  return(map)

}


# indicator by mission

fun_plot_indicator_bymission <- function(model_results, species, codepays, indicator){
  
  df <- model_results %>%
    filter(response_var == species, code_pays == codepays, mod == indicator)

  vct_villages <- dbReadTable(react_gpkg, 'recensement_villages_l1') %>% dplyr::select(codevillage,intervention)

  df <- df$df[[1]]

  df <- df %>% 
    left_join(vct_villages) %>%
    rename(treatment = intervention, village = codevillage)
  
  if(codepays == "BF"){
    inter = 3.5 
  } else if(codepays == "CI"){
    inter = 4.5
  }

if(indicator == "presence"){
  
  df_plot1 <- df %>%
    mutate(resp_var = ifelse(resp_var == 1, "presence", "absence")) %>%
    mutate(nummission = as.numeric(nummission))
    
  plot1 <- ggplot(df_plot1, aes(x = nummission, fill = resp_var)) + 
    geom_histogram(position = position_stack(reverse = TRUE), stat="count") +
    labs(fill = "Biting status", y ="number of sites sampled", x = "mission") +
    #geom_vline(xintercept = inter, colour = "red", size = 0.5, linetype="dotted") + 
    theme(legend.position = "bottom")
  
  plot1bis <- plot1 +
    facet_wrap(treatment~village, labeller = "label_both")
  #plot1bis <- shift_legend2(plot1bis)
  
  
} else if (indicator == "abundance"){
  
  plot1 <- ggplot(df, aes(x = nummission, y = resp_var, group = nummission)) + 
    geom_boxplot(outlier.shape=NA) +
    geom_jitter(position=position_jitter(0.1), cex=0.4) + 
    #theme_cowplot()+#guides(fill = FALSE, colour = FALSE)+
    #geom_vline(xintercept = inter, colour = "red", size = 0.5, linetype="dotted") + 
    theme(legend.position = "bottom")
  
  plot1bis <- plot1 +
    facet_wrap(treatment~village, labeller = "label_both")
  
}

  return(list(plot1 = plot1, plot1bis = plot1bis))

}
    


fun_plot_indicator_byinterv <- function(model_results, species, codepays, indicator){
# plot 2 is for stats by intervention vs control group

df <- model_results %>%
  filter(response_var == species, code_pays == codepays, mod == indicator)
df <- df$df[[1]]

vct_villages <- dbReadTable(react_gpkg, 'recensement_villages_l1') %>% dplyr::select(codevillage,intervention)

df <- df %>% left_join(vct_villages)

if(codepays == "BF"){
  inter = 3.5 
} else if(codepays == "CI"){
  inter = 4.5
}

if(indicator == "presence"){
  
df_plot2 <-  df %>%
  mutate(resp_var = ifelse(resp_var == 1, "presence", "absence")) %>%
  mutate(nummission = as.numeric(nummission)) %>%
  group_by(nummission, intervention, resp_var) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = resp_var, values_from = count) %>%
  mutate(presence = ifelse(is.na(presence), 0, presence)) %>%
  mutate(absence = ifelse(is.na(absence), 0, absence)) %>%
  mutate(perc_presence = presence/ (presence + absence) * 100) %>%
  mutate(indicator = ifelse(intervention == "Ctrle", "control", "treatment")) %>%
  dplyr::select(nummission,intervention,perc_presence,indicator) %>%
  as_tibble()

df_list <- by(df_plot2, df_plot2$intervention, function(sub)
  transform(rbind(sub, df_plot2[df_plot2$intervention == "Ctrle",]),
            new_group = sub$intervention[[1]])
)    
graph_df <- subset(do.call(rbind, df_list))
rows_control <- graph_df %>% filter(indicator == "control")
graph_df <- graph_df %>% filter(indicator != "control")
treatments <- setdiff(unique(graph_df$new_group),"Ctrle")
for(i in 1:length(treatments)){
  rows_control$new_group <- treatments[i]
  graph_df <- rbind(graph_df,rows_control)
}


plot2 <- ggplot(graph_df, aes(x = as.factor(nummission), y = perc_presence, fill=indicator)) + 
  geom_bar(position = 'dodge', stat = 'identity') +
  labs(title="Treatments vs Control Group", x="mission", y = "percentage of sites with presence of bites") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom") +
  #geom_vline(xintercept = inter, colour = "red", size = 0.5, linetype="dotted") +
  facet_wrap(~new_group)

} else if (indicator == "abundance"){
  
  df_plot2 <- df %>%
    mutate(indicator = ifelse(intervention == "Ctrle", "control", "treatment")) %>%
    mutate(new_group = intervention)
  
  rows_control <- df_plot2 %>% filter(indicator == "control")
  df_plot2 <- df_plot2 %>% filter(indicator != "control")
  treatments <- setdiff(unique(df_plot2$new_group),"Ctrle")
  for(i in 1:length(treatments)){
    rows_control$new_group <- treatments[i]
    df_plot2 <- rbind(df_plot2,rows_control)
  }
  
  
  plot2 <- ggplot(df_plot2, aes(x = as.factor(nummission), y = resp_var, fill=indicator)) + 
    geom_boxplot(width = 0.5, outlier.size = 0.2) +
    #geom_vline(xintercept = inter, colour = "red", size = 0.5, linetype="dotted") +
    facet_wrap(~new_group) + 
    theme(legend.position = "bottom") +
    labs(title="Treatments vs Control Group", x="mission", y = "number of bites within sites with positive biting rate")
  
}

return(plot2)
}



#### Results of the univariate analysis

fun_plot_res_univanalysis <- function(model_results, species, codepays, indicator){
  
df <- model_results %>%
  filter(response_var == species, code_pays == codepays, mod == indicator, model_type == "glmm")

 ts <- df %>% filter(intervention =="all")
# 
# ## Climatic data
# ts_rfd <- ts$ts_plots[[1]][[1]] + scale_x_date(name = "date",date_labels = "%m/%Y", date_breaks = "6 months")
# ts_tmin <- ts$ts_plots[[1]][[2]] + scale_x_date(name = "date",date_labels = "%m/%Y", date_breaks = "6 months") 
# ts_tmax <- ts$ts_plots[[1]][[3]] + scale_x_date(name = "date",date_labels = "%m/%Y", date_breaks = "6 months") 
# ts_tamp <- ts$ts_plots[[1]][[4]] + scale_x_date(name = "date",date_labels = "%m/%Y", date_breaks = "6 months") 
# 
 ccm_rfd <- ts$ccm_plots[[1]][[1]] + theme(legend.position = "none", plot.title = element_blank())
 ccm_tmin <- ts$ccm_plots[[1]][[2]] + theme(legend.position = "none", plot.title = element_blank())
 ccm_tmax <- ts$ccm_plots[[1]][[3]] + theme(legend.position = "none", plot.title = element_blank())
# ccm_tamp <- ts$ccm_plots[[1]][[4]] + theme(legend.position = "none", plot.title = element_blank())
# 


# corr_pre_interv <- df %>% filter(intervention == "pre_intervention")
# corr_pre_interv <- corr_pre_interv$df_corr_univ[[1]] %>% mutate(interv = "pre-intervention")
# corr_post_interv <- df %>% filter(intervention == "post_intervention")
# corr_post_interv <- corr_post_interv$df_corr_univ[[1]] %>% mutate(interv = "post-intervention")
corr_all_period <- df %>% filter(intervention == "all")
corr_all_period <- corr_all_period$df_corr_univ[[1]] %>% mutate(interv = "whole period")

# corr <- rbind(corr_pre_interv, corr_post_interv, corr_all_period)
# corr$lab <- factor(corr$lab, levels = unique(corr$lab[order(corr$correlation)]))
# corr$interv <- factor(corr$interv, levels = c("pre-intervention", "post-intervention", "whole period"))
# lc_plot1 =  ggplot(corr, aes(correlation, lab)) +
#   geom_point(size=1.5) + 
#   geom_text(aes(label=round(correlation, 2)),hjust=-0.3, vjust=0.3, size = 3) +
#   xlim(-1,1) +
#   ylab("variable") + 
#   xlab("Spearman correlation") + 
#   theme_bw() +
#   geom_vline(xintercept=0, linetype="dashed", color = "red") + 
#   guides(shape=FALSE) +
#   facet_grid(type~interv, scales = "free", space = "free")

# #corr_pre_interv$lab <- gsub("\\n","-",corr_pre_interv$lab)
# corr_pre_interv$lab <- gsub("Surface de","%",corr_pre_interv$lab)
# corr_pre_interv$lab <- factor(corr_pre_interv$lab, levels = unique(corr_pre_interv$lab[order(corr_pre_interv$correlation)]))
# corr_pre_interv_plot =  ggplot(corr_pre_interv, aes(correlation, lab)) +
#   geom_point(size=1.5) + 
#   geom_text(aes(label=round(correlation, 2)),hjust=-0.3, vjust=0.3, size = 3) +
#   xlim(-1,1) +
#   ylab("variable") + 
#   xlab("Spearman correlation") + 
#   theme_bw() +
#   geom_vline(xintercept=0, linetype="dashed", color = "red") + 
#   guides(shape=FALSE) +
#   facet_grid(type~., scales = "free", space = "free") + 
#   labs(x = NULL, y = NULL) + 
#   ggtitle("pre-intervention")
# 
# #corr_post_interv$lab <- gsub("\\n","-",corr_post_interv$lab)
# corr_post_interv$lab <- gsub("Surface de","%",corr_post_interv$lab)
# corr_post_interv$lab <- factor(corr_post_interv$lab, levels = unique(corr_post_interv$lab[order(corr_post_interv$correlation)]))
# corr_post_interv_plot =  ggplot(corr_post_interv, aes(correlation, lab)) +
#   geom_point(size=1.5) +
#   geom_text(aes(label=round(correlation, 2)),hjust=-0.3, vjust=0.3, size = 3) +
#   xlim(-1,1) +
#   ylab("variable") +
#   xlab("Spearman correlation") +
#   theme_bw() +
#   geom_vline(xintercept=0, linetype="dashed", color = "red") +
#   guides(shape=FALSE) +
#   facet_grid(type~., scales = "free", space = "free") +
#   labs(x = NULL, y = NULL) +
#   ggtitle("post-intervention")

corr_all_period$lab <- gsub("\\n","-",corr_all_period$lab)
corr_all_period$lab <- gsub("Surface de","%",corr_all_period$lab)
corr_all_period$lab <- factor(corr_all_period$lab, levels = unique(corr_all_period$lab[order(corr_all_period$correlation)]))
corr_all_period_plot =  ggplot(corr_all_period, aes(correlation, lab)) +
  geom_point(size=1.5) + 
  geom_text(aes(label=round(correlation, 2)),hjust=-0.3, vjust=0.3, size = 3) +
  xlim(-1,1) +
  ylab("variable") + 
  xlab("Spearman correlation") + 
  theme_bw() +
  geom_vline(xintercept=0, linetype="dashed", color = "red") +
  facet_grid(type~., scales = "free", space = "free") + 
  labs(y = NULL) + 
  ggtitle(paste0(indicator, " - univariate analysis"))


return(list(#ts_rfd = ts_rfd, 
            #ts_tmin = ts_tmin, 
            #ts_tmax = ts_tmax, 
            #ts_tamp = ts_tamp, 
            ccm_rfd = ccm_rfd, 
            ccm_tmin = ccm_tmin, 
            ccm_tmax = ccm_tmax,
            #ccm_tamp = ccm_tamp,
            #corr_pre_interv_plot = corr_pre_interv_plot,
            #corr_post_interv_plot = corr_post_interv_plot,
            corr_all_period_plot = corr_all_period_plot
            ))

}






#### Results of the multivariate analysis


fun_plot_res_multivanalysis_glmm <- function(model_results, species, codepays, indicator){

df <- model_results %>%
    filter(response_var == species, code_pays == codepays, mod == indicator, model_type == "glmm")
  
# row_pre_interv = df %>% filter(intervention =="pre_intervention")
# row_post_interv = df %>% filter(intervention =="post_intervention")
row_whole_period = df %>% filter(intervention =="all")

# mod_pre_interv <- row_pre_interv$model[[1]]@model
# mod_post_interv <- row_post_interv$model[[1]]@model
mod_whole_period <- row_whole_period$model[[1]]@model


# corr_multiv_pre_interv <- row_pre_interv$df_corr_multiv[[1]] %>% mutate(interv = "pre-intervention")
# corr_multiv_post_interv <- row_post_interv$df_corr_multiv[[1]] %>% mutate(interv = "post-intervention")
corr_multiv_all_period <- row_whole_period$df_corr_multiv[[1]] %>% mutate(interv = "whole period")

#cv <- round(row_whole_period$cv_lto[[1]]$mean_metric,2)
#if(indicator == "presence"){
#  cv <- paste0("AUC = ",cv)
#} else if (indicator == "abundandce"){
#  cv <- paste0("R^2 = ",cv)
#}

  labs_mod <- colnames(mod_whole_period$frame) %>% data.frame() %>% setNames("name") %>% left_join(corr_multiv_all_period) %>% filter(!is.na(lab))
  labs_mod1 <- as.character(labs_mod$lab)
  labs_mod1 <- gsub("\\n","-",labs_mod1)
  names(labs_mod1) <- as.character(labs_mod$name)
  labs_mod1 <- gsub("Surface de","%",labs_mod1)
  plot_model_whole_period <- sjPlot::plot_model(mod_whole_period, show.values = TRUE, value.offset = .3, axis.labels = labs_mod1, line.color = "red", title = paste0(indicator, " - multivariate analysis"))


  return(list(#plot_model_pre_interv = plot_model_pre_interv, 
            #plot_model_post_interv = plot_model_post_interv, 
            plot_model_whole_period = plot_model_whole_period,
            mod_whole_period = mod_whole_period))

}

# 
# t_pre_interv <- broom.mixed::tidy(mod_pre_interv, conf.int = TRUE) %>% mutate(interv = "pre-intervention")
# t_post_interv <- broom.mixed::tidy(mod_post_interv, conf.int = TRUE) %>% mutate(interv = "post-intervention")
# t_whole_period <- broom.mixed::tidy(mod_whole_period, conf.int = TRUE) %>% mutate(interv = "whole period")
# 
# # t <- rbind(t1,t2,t3) %>%
# #   left_join(corr_univ %>% dplyr::select(name,lab,type) %>% unique(), by = c("term" = "name")) %>%
# #   filter(effect == "fixed" & term != "(Intercept)")
# 
# 
# ggplot(t_pre_interv, aes(y=term, x=estimate)) + 
#   geom_pointrange(aes(xmin=estimate-std.error, xmax=estimate+std.error)) + 
#   geom_vline(xintercept=0, linetype="dashed", color = "red") + 
#   theme_bw()
# 






#### plot general data viz
## map
#map <- fun_plot_map(model_results, "ma_funestus_ss", "BF", "presence")
## stat by village
#plots_byvillage <- fun_plot_indicator_bymission(model_results, "ma_funestus_ss", "BF", "presence")

#map + plots_byvillage

## stat interv
#plots_interv <- fun_plot_indicator_byinterv(model_results, "ma_funestus_ss", "BF", "presence", "all")


### results of univariate analysis

# climatic data
# (plots_univ_analysis[[1]] / plots_univ_analysis[[5]]) | (plots_univ_analysis[[2]] / plots_univ_analysis[[6]]) | (plots_univ_analysis[[3]] / plots_univ_analysis[[7]]) | (plots_univ_analysis[[4]] / plots_univ_analysis[[8]])



### results of multivariate analysis
plots_univ_analysis_presence <- fun_plot_res_univanalysis(model_results, "ma_coluzzi", "BF", "presence")
plots_multiv_analysis_presence <- fun_plot_res_multivanalysis_glmm(model_results, "ma_coluzzi", "BF", "presence")
plots_univ_analysis_abundance <- fun_plot_res_univanalysis(model_results, "ma_coluzzi", "BF", "abundance")
plots_multiv_analysis_abundance <- fun_plot_res_multivanalysis_glmm(model_results, "ma_coluzzi", "BF", "abundance")

th_trmetrics_entomo_postedecapture_mod2 <- plots_multiv_analysis_presence$mod_whole_period$frame
residuals_plot_presence <- DHARMa::simulateResiduals(plots_multiv_analysis_presence$mod_whole_period, n = 1000)
th_trmetrics_entomo_postedecapture_mod2 <- plots_multiv_analysis_abundance$mod_whole_period$frame
residuals_plot_abundance <- DHARMa::simulateResiduals(plots_multiv_analysis_abundance$mod_whole_period, n = 1000)



(plots_univ_analysis_presence$corr_all_period_plot / plots_multiv_analysis_presence$plot_model_whole_period) | (plots_univ_analysis_abundance$corr_all_period_plot / plots_multiv_analysis_abundance$plot_model_whole_period)
plot(residuals_plot_presence)
plot(residuals_plot_abundance)
