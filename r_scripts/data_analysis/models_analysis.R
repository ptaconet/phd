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
  
#### plot general data viz
## map
roi <- st_read(path_to_db,"contexte_frontieresreact") %>% 
  filter(codepays == model_results$code_pays[[1]]) %>%
  st_transform(4326) %>%
  st_bbox() %>%
  as.numeric()
myLocation <- c(roi[1], roi[2], roi[3], roi[4])
myMap <- get_map(location=myLocation, source="osm",crop=FALSE)

df <- model_results %>% 
  filter(intervention == "all")
df <- df$df[[1]]

if(model_results$mod[[1]] == "presence"){
  df_map <- df %>%
    group_by(codevillage, nummission, mean_date_mission) %>%
    summarise(resp_var = sum(resp_var), X = mean(X_4326), Y = mean(Y_4326)) %>%
    mutate(biting_status = ifelse(resp_var > 0, "presence", "absence"))
  
  #title = "Proportion of sites with positive counts of bites by village (i.e. >= 1 bite)"
  
  map <- ggmap(myMap) + 
    geom_point(aes(x = X, y = Y, color = biting_status), data = df_map) + 
    facet_wrap(.~nummission)
  
} else if(model_results$mod[[1]] == "abundance"){
  df_map <- df %>%
    group_by(codevillage,nummission, mean_date_mission) %>%
    summarise(resp_var = sum(resp_var), X = mean(X_4326), Y = mean(Y_4326)) %>%
    rename(n_bites = resp_var)

    #title = "Number of bites by village"
    
    map <- ggmap(myMap) + 
      geom_point(aes(x = X, y = Y, size = n_bites), data = df_map, color="darkred", alpha = .5) + 
      facet_wrap(.~nummission)

}
map <- shift_legend2(map)

# indicator by mission

vct_villages <- dbReadTable(react_gpkg, 'recensement_villages_l1') %>% dplyr::select(codevillage,intervention)

if(model_results$code_pays[[1]] == "BF"){
  inter = 3.5 
} else if(model_results$code_pays[[1]] == "CI"){
  inter = 4.5
}

df <- df %>% left_join(vct_villages)


if(model_results$mod[[1]] == "presence"){
  
  
  df_plot1 <- df %>%
    mutate(resp_var = ifelse(resp_var == 1, "presence", "absence")) %>%
    mutate(nummission = as.numeric(nummission))
    
  # nummission, mean_date_mission, num_villages_sampled, num_sites_sampled, num_sites_with_presence
  df_tab <- df %>%
    group_by(nummission, mean_date_mission) %>%
    summarise(num_sites_sampled = n())
    
  
  plot1 <- ggplot(df_plot1, aes(x = nummission, fill = resp_var)) + 
    geom_histogram(position = position_stack(reverse = TRUE), stat="count") +
    labs(fill = "Biting status", y ="number of sites sampled", x = "mission") +
    geom_vline(xintercept = inter, colour = "red", size = 0.5, linetype="dotted") +
    facet_wrap(intervention~codevillage, labeller = "label_both")#+ 
    #ggtitle("Number of sites with absence and presence of bites by entomological mission")
  plot1 <- shift_legend2(plot1)
  
  
  df_plot2 <- df_plot1 %>%
    left_join(vct_villages) %>%
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
    transform(rbind(sub, dat[dat$intervention == "Ctrle",]),
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
    facet_wrap(~new_group) + 
    geom_vline(xintercept = inter, colour = "red", size = 0.5, linetype="dotted")

  
} else if (model_results$mod[[1]] == "abundance"){
  
  plot1 <- ggplot(df, aes(x = nummission, y = resp_var, group = nummission)) + 
    geom_boxplot(outlier.shape=NA) +
    geom_jitter(position=position_jitter(0.1), cex=0.4) + 
    #theme_cowplot()+#guides(fill = FALSE, colour = FALSE)+
    geom_vline(xintercept = inter, colour = "red", size = 0.5, linetype="dotted") +
    facet_wrap(intervention~codevillage, labeller = "label_both") #+ 
    #scale_colour_brewer(palette = "Dark2")
    #+scale_fill_brewer(palette = "Dark2")
  
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
    geom_vline(xintercept = inter, colour = "red", size = 0.5, linetype="dotted") +
    facet_wrap(~new_group) + 
    theme(legend.position = "bottom") +
    labs(title="Treatments vs Control Group", x="mission", y = "number of bites within sites with positive biting rate")
    

}
    
map_dims <- get_dim(map)
plot1_aligned <- set_dim(plot1, map_dims)

map + plot1 + plot2

#### Results of the univariate analysis

## Climatic data
clim_plots <- model_results %>% filter(intervention == "all")
ts_rfd <- clim_plots$ts_plots[[1]][[1]]
ts_tmin <- clim_plots$ts_plots[[1]][[2]]
ts_tmax <- clim_plots$ts_plots[[1]][[3]]
ts_tamp <- clim_plots$ts_plots[[1]][[4]]

ccm_rfd <- clim_plots$ccm_plots[[1]][[1]]
ccm_tmin <- clim_plots$ccm_plots[[1]][[2]]
ccm_tmax <- clim_plots$ccm_plots[[1]][[3]]
ccm_tamp <- clim_plots$ccm_plots[[1]][[4]]

corr_pre_interv <- model_results$df_corr_univ[[1]] %>% mutate(interv = "pre-intervention")
corr_post_interv <- model_results$df_corr_univ[[2]] %>% mutate(interv = "post-intervention")
corr_all_period <- model_results$df_corr_univ[[3]] %>% mutate(interv = "whole period")


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


corr_pre_interv$lab <- factor(corr_pre_interv$lab, levels = unique(corr_pre_interv$lab[order(corr_pre_interv$correlation)]))
lc_plot1 =  ggplot(corr_pre_interv, aes(correlation, lab)) +
  geom_point(size=1.5) + 
  geom_text(aes(label=round(correlation, 2)),hjust=-0.3, vjust=0.3, size = 3) +
  xlim(-1,1) +
  ylab("variable") + 
  xlab("Spearman correlation") + 
  theme_bw() +
  geom_vline(xintercept=0, linetype="dashed", color = "red") + 
  guides(shape=FALSE) +
  facet_grid(type~., scales = "free", space = "free") + 
  labs(x = NULL, y = NULL) + 
  ggtitle("pre-intervention")

corr_post_interv$lab <- factor(corr_post_interv$lab, levels = unique(corr_post_interv$lab[order(corr_post_interv$correlation)]))
lc_plot2 =  ggplot(corr_post_interv, aes(correlation, lab)) +
  geom_point(size=1.5) + 
  geom_text(aes(label=round(correlation, 2)),hjust=-0.3, vjust=0.3, size = 3) +
  xlim(-1,1) +
  ylab("variable") + 
  xlab("Spearman correlation") + 
  theme_bw() +
  geom_vline(xintercept=0, linetype="dashed", color = "red") + 
  guides(shape=FALSE) +
  facet_grid(type~., scales = "free", space = "free") + 
  labs(x = NULL, y = NULL) + 
  ggtitle("post-intervention")

corr_all_period$lab <- factor(corr_all_period$lab, levels = unique(corr_all_period$lab[order(corr_all_period$correlation)]))
lc_plot3 =  ggplot(corr_all_period, aes(correlation, lab)) +
  geom_point(size=1.5) + 
  geom_text(aes(label=round(correlation, 2)),hjust=-0.3, vjust=0.3, size = 3) +
  xlim(-1,1) +
  ylab("variable") + 
  xlab("Spearman correlation") + 
  theme_bw() +
  geom_vline(xintercept=0, linetype="dashed", color = "red") +
  facet_grid(type~., scales = "free", space = "free") + 
  labs(y = NULL) + 
  ggtitle("whole study period")

lc_plot1 / lc_plot2 / lc_plot3






#### Results of the multivariate analysis




mod_pre_interv = model_results$model[1][[1]]@model
mod_post_interv = model_results$model[2][[1]]@model
mod_whole_period = model_results$model[3][[1]]@model

corr_multiv_pre_interv <- model_results$df_corr_multiv[[1]] %>% mutate(interv = "pre-intervention")
corr_multiv_post_interv <- model_results$df_corr_multiv[[2]] %>% mutate(interv = "post-intervention")
corr_multiv_all_period <- model_results$df_corr_multiv[[3]] %>% mutate(interv = "whole period")


fun_plot_model <- function(model, df_corr, period){
  labs_mod <- colnames(model$frame) %>% data.frame() %>% setNames("name") %>% left_join(df_corr) %>% filter(!is.na(lab))
  labs_mod1 <- as.character(labs_mod$lab)
  names(labs_mod1) <- as.character(labs_mod$name)
  plot_model <- sjPlot::plot_model(model, show.values = TRUE, value.offset = .3, axis.labels = labs_mod1, title = period,  vline.color = "red") +   theme_bw() 
  return(plot_model)
}

plot_model_pre_interv <- fun_plot_model(mod_pre_interv, corr_multiv_pre_interv, "pre-intervention")
plot_model_post_interv <- fun_plot_model(mod_post_interv, corr_multiv_post_interv, "post-intervention")
plot_model_whole_period <- fun_plot_model(mod_whole_period, corr_multiv_all_period, "whole study period")



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










for (i in 1:nrow(model_results)){
model_results$df_corr[[i]]$lab <- factor(model_results$df_corr[[i]]$lab, levels = model_results$df_corr[[i]]$lab[order(model_results$df_corr[[i]]$correlation)])
plot =  ggplot(model_results$df_corr[[i]], aes(correlation, lab, shape = type)) +
    geom_point() + 
    xlim(-1,1) +
    ylab("variable") + 
    xlab("Spearman correlation") + 
    theme_bw() +
    geom_vline(xintercept=0, linetype="dashed", color = "red")

ggsave(paste0("/home/ptaconet/Bureau/data_analysis/plots/",model_results$response_var[i],"_",model_results$mod[i],model_results$intervention[i],".png"))
}


# model validation plot
for (i in 1:nrow(model)){
  th_trmetrics_entomo_postedecapture_mod2 <- model_results$model[[i]]@model$frame
  residuals_plot <- DHARMa::simulateResiduals(model_results$model[[i]]@model, n = 1000)
  plot(residuals_plot)

  predictors <- rownames(model_results$model[[i]]@summary$coefficients$cond)
  predictors <- setdiff(predictors,"(Intercept)")
  predictors <- data.frame(name = predictors, is_predictor = T)
  labs_predictors <- left_join(model_results$df_corr[[i]], predictors)
  labs_predictors <- filter(labs_predictors,is_predictor == TRUE) 
  #labs_predictors$name <- ifelse((labs_predictors$type == "climate-related" | is.na(labs_predictors$lab)), labs_predictors$name, labs_predictors$lab )
  
  labs_predictors$correlation <- labs_predictors$is_predictor <- NULL
  
  
  labs_predictors <- c("(Intercept)",labs_predictors$name)

  sjPlot::tab_model(model_results$model[[i]]@model)#, pred.labels = labs_predictors)

  sjPlot::plot_model(model_results$model[[i]]@model, pred.labels = labs_predictors)

}

