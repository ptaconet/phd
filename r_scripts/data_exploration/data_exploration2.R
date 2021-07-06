library(ggplot2)
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(purrr)
library(patchwork)
library(plotly)
library(sf)
library(googlesheets4)
library(furrr)
library(stringr)
library(patchwork)
library(lubridate)
library(ranger)
library(sp)
library(gstat)
library(raster)
library(mlr)
library(tuneRanger)
library(mlr)
library(GSIF)
library(gstat)
library(spdplyr)
require(ncf)
require(landscapemetrics)
require(correlation)


response_var <- "ma_funestus_ss"  # "ma_gambiae_ss"
code_pays <- "BF"            # "BF"
mod <- "presence" # presence
#th_season <- "wet"

### connect to the database
path_to_db <- "data/react_db/react_db.gpkg" 
react_gpkg <- DBI::dbConnect(RSQLite::SQLite(),dbname = path_to_db) 

### open the tables
## dates and positions of the entomological missions (1 row = 1 point de capture)
entomo_csh_metadata_l1 <- dbReadTable(react_gpkg, 'entomo_csh_metadata_l1') %>% filter(!(nummission %in% c("11","12","13","15"))) #%>% dplyr::select(-fid) %>% mutate(season = case_when(codepays=="BF" & nummission %in% c("1","2","4","5","6","7","15") ~ "dry",
#codepays=="BF" & nummission %in% c("3","11","12","13") ~ "wet",
#codepays=="CI" & nummission %in% c("2","3","6","7","8") ~ "dry",
# codepays=="CI" & nummission %in% c("1","4","5") ~ "wet"))

## table containing the response variables (ie variables to model)
trmetrics_entomo_postedecapture <- dbReadTable(react_gpkg, 'trmetrics_entomo_postedecapture') %>% dplyr::select(-fid) %>% left_join(entomo_csh_metadata_l1 %>% dplyr::select(idpointdecapture, codevillage, codepays, nummission)) %>% filter(!is.na(codevillage))

## tables containing the explanatory variables
# spatiotemporal explanatory variables
env_spatiotemporal <- dbReadTable(react_gpkg, 'env_spatiotemporal') %>% dplyr::select(-fid) %>% dplyr::rename(idpointdecapture = id)      %>% filter(var %in% c("TMIN1","TMAX1","TAMP1","VNV8","SMO1","RFD1_F","EVT8"), buffer==2000, lag_time <= 60)

# spatial-only explanatory variables
env_spatial <- dbReadTable(react_gpkg,'env_spatial') %>% dplyr::select(-fid) %>% dplyr::rename(idpointdecapture = id)

# non-spatial explanatory variables
env_static <- dbReadTable(react_gpkg, 'env_static') %>% dplyr::select(-fid) %>% dplyr::rename(idpointdecapture = id)

# variables for the night of catch
env_nightcatch <-  dbReadTable(react_gpkg, 'env_nightcatch') %>% dplyr::select(-fid) %>% dplyr::rename(idpointdecapture = id)

# variables for the night of catch at the postedecapture level
env_nightcatch_postedecapture <- dbReadTable(react_gpkg,"env_nightcatch_postedecapture") %>% dplyr::select(-fid)

# landcover variables
env_landcover <-  dbReadTable(react_gpkg, 'env_landcover') %>% dplyr::select(-fid) %>% dplyr::rename(idpointdecapture = id)
if(code_pays=="BF") {lid=c(1,2,3,4,5)} else {lid=c(7,8,9)}
env_landcover <- env_landcover %>% filter(layer_id %in% lid, buffer>50, !(metric %in% c("ed","np")))

## table of exhaustive definitions of the explanatory variables
googlesheets4::sheets_deauth()
prediction_vars <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1dIeSOa2WinXvOQGLmIjA0gFdsHnb6zMMsDME-G5pyMc/edit?usp=sharing", sheet = "var_explication", col_types="c")


### source home-made functions 
source("r_scripts/data_analysis_tests/functions_script_data_analysis.R")


th_trmetrics_entomo_postedecapture <- trmetrics_entomo_postedecapture %>% filter(codepays==code_pays)
th_trmetrics_entomo_postedecapture$resp_var <- th_trmetrics_entomo_postedecapture[,response_var]


### Spatio-temporal variables



### data pre-processing and preparation

# join codepays and variable type to the explanatory vars
env_spatiotemporal2 <- env_spatiotemporal %>%
  mutate(buffer = as.character(buffer)) %>%
  right_join(entomo_csh_metadata_l1[,c("idpointdecapture","codepays")]) %>%
  left_join(prediction_vars[,c("code","type_group1","short_name","temporal_aggregation_days")], by = c("var" = "code"))

# restructurate in lists
env_spatiotemporal2 <- env_spatiotemporal2 %>%
  dplyr::select(-c(lag_time, date)) %>%
  group_by(type_group1, var, codepays, buffer) %>%
  tidyr::nest(predictive_df = c(idpointdecapture, lag_n , val)) %>%
  mutate(fun_summarize_ccm = ifelse(var %in% c("RFD1_F","RFD1_L","RFD7_F","RFD7_L"), "sum", "mean")) %>%
  arrange(type_group1, var, codepays, as.numeric(buffer), fun_summarize_ccm) %>%
  filter(!(var %in% c("RFD1_L","TAMP7","TMAX7","TMIN7"))) %>%
  filter(!(var %in% c("RFD1_F","SMO1","VEV8","VNV8","EVT8","TMAX1","TMIN1","TAMP1") && buffer %in% c("500","1000"))) %>%
  filter(!(var %in% c("VNV30","VMV30","WNW30","WVV10","WVH10","LIG30") && buffer %in% c("500","1000","2000")))
  

env_spatiotemporal2 <- env_spatiotemporal2 %>% filter(codepays==code_pays)

###### Spatio-temporal variables

fun_process_spatiotemporal <- function(th_trmetrics_entomo_postedecapture, mod){

  if(mod == "abundance" ){
    th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% filter(resp_var > 0 )
  } else if (mod == "presence" ){
    th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% mutate(resp_var = ifelse(resp_var == 0,0,1 ))
  }
  
  # first deal with rainfall
  th_env_spatiotemporal <- env_spatiotemporal2 %>%
    mutate(predictive_df = pmap(list(predictive_df, var, buffer, fun_summarize_ccm), ~fun_ccm_df(..1, ..2, ..3, function_to_apply = ..4))) %>% # function "fun_ccm_df" prepares the data for the CCM (ie calculates the mean or sum of the explanatory variable for each available time lag)
    mutate(ccm_corrmat = future_map(predictive_df, ~fun_ccm_corrmat(., th_trmetrics_entomo_postedecapture, method = "spearman"))) %>%  # function "fun_ccm_corrmat_abundance" creates the CCM by using the spearman correlation coefficient 
    mutate(ccm_plot = pmap(list(ccm_corrmat, var, buffer, codepays), ~fun_ccm_plot(..1,..2,..3,..4))) %>% # function "fun_ccm_plot" plots the CCM
    mutate(ccm_maxcorr_vcor = map2_dbl(ccm_corrmat,var, ~fun_ccm_select_lags(.x,"correlation",.y))) %>%   # get max correlation value (wether positive or negative) for the CCM. for the daily resolution data, the time lag difference must be at least 8 days (to prevent from spurious relations)
    mutate(ccm_maxcorr_lag1 = map2_dbl(ccm_corrmat,var, ~fun_ccm_select_lags(.x,"time_lag_1",.y))) %>%   # get time lag 1 for max correlation value
    mutate(ccm_maxcorr_lag2 = map2_dbl(ccm_corrmat,var, ~fun_ccm_select_lags(.x,"time_lag_2",.y))) %>%   # get time lag 2 for max correlation value
    mutate(var_df = paste(var,buffer,ccm_maxcorr_lag2,ccm_maxcorr_lag1,sep="_")) # get name of the variable for the max correlation value 

  # to get temperature, run a glmm with the rainfall extracted previously + each temperature time series. The value that we put in the CCM is the resulting p-val
  
  
    ## variable sceening process

  # rule 1 : Variables with max abs correlation less than 0.2 are removed
  th_env_spatiotemporal <- th_env_spatiotemporal %>% filter(abs(ccm_maxcorr_vcor)>0.2)

  # rule2 : variables with multiple buffers : if correlation > 0.7, only greatest buffer is kept
  # th_env_spatiotemporal <- fun_remove_var_buffer(th_env_spatiotemporal, "VNV30", 0.6)
  # th_env_spatiotemporal <- fun_remove_var_buffer(th_env_spatiotemporal, "VMV30", 0.6)
  # th_env_spatiotemporal <- fun_remove_var_buffer(th_env_spatiotemporal, "WNW30", 0.6)
  # th_env_spatiotemporal <- fun_remove_var_buffer(th_env_spatiotemporal, "BRI30", 0.6)
  # th_env_spatiotemporal <- fun_remove_var_buffer(th_env_spatiotemporal, "WVV10", 0.6)
  # th_env_spatiotemporal <- fun_remove_var_buffer(th_env_spatiotemporal, "WVH10", 0.6)


  # on conserve pour chaque série stat la variable la mieux corrélée
  expl_ts_pos <- dplyr::select(th_env_spatiotemporal$predictive_df[[1]],  idpointdecapture, th_env_spatiotemporal$var_df[[1]])
  for(i in 2:nrow(th_env_spatiotemporal)){
    th_df <- dplyr::select(th_env_spatiotemporal$predictive_df[[i]],  idpointdecapture, th_env_spatiotemporal$var_df[[i]])
    expl_ts_pos <- expl_ts_pos %>% left_join(th_df)
  }

  # correlation matrix for the remaining variables

  m <- expl_ts_pos %>%
    #select_if(!grepl("TMAX|TMIN",colnames(.))) %>%
    select_if(is.numeric) %>%
    cor(.,method = "spearman", use = "na.or.complete")

  index <- which(abs(m) > .7 & abs(m) < 1, arr.ind = T)
  p <- cbind.data.frame(stock1 = rownames(m)[index[,1]],stock2 = colnames(m)[index[,2]])

  p <- p %>%
    left_join(th_env_spatiotemporal %>% dplyr::select(var,var_df), by=c("stock1"="var_df")) %>%
    left_join(prediction_vars %>% dplyr::select(code,priority), by=c("var"="code")) %>%
    left_join(th_env_spatiotemporal %>% dplyr::select(var,var_df), by=c("stock2"="var_df")) %>%
    left_join(prediction_vars %>% dplyr::select(code,priority), by=c("var.y"="code"))
  
  var_to_remove <- NULL
  if(nrow(p)>0){
  for(i in 1:nrow(p)){
    if(as.numeric(p$priority.x[i]) < as.numeric(p$priority.y[i])){
      var_to_remove <- c(var_to_remove,p$stock2[i])
    } else {
      var_to_remove <- c(var_to_remove,p$stock1[i])
    }
  }
  var_to_remove <- unique(var_to_remove)

  expl_ts_pos <- expl_ts_pos %>% dplyr::select(-var_to_remove)
  }
  
  return(list(th_env_spatiotemporal=th_env_spatiotemporal,expl_ts=expl_ts_pos))
}

  
#plots <- fun_spatiotemparal_plots(th_trmetrics_entomo_postedecapture,"TMAX1",code_pays,response_var)

th_env_spatiotemporal_presence <- th_env_spatiotemporal_presence %>% filter(!is.na(ccm_maxcorr_vcor)) %>% arrange(ccm_maxcorr_vcor) %>% mutate(lab = paste0(short_name, " - \nr(", ccm_maxcorr_lag1,",",ccm_maxcorr_lag2, ")")) %>%    mutate(lab = factor(lab, levels = .$lab))
th_env_spatiotemporal_abundance <- th_env_spatiotemporal_abundance %>% filter(!is.na(ccm_maxcorr_vcor)) %>% arrange(ccm_maxcorr_vcor) %>% mutate(lab = paste0(short_name, " -\n r(", ccm_maxcorr_lag1,",",ccm_maxcorr_lag2, ")")) %>%    mutate(lab = factor(lab, levels = .$lab))


# cleveland dotplot 
ggplot(th_env_spatiotemporal_presence, aes(ccm_maxcorr_vcor, lab)) +
  geom_point() + 
  xlim(-1,1) +
  ylab("variable") + 
  xlab("Spearman correlation") + 
  theme_bw()  +
  geom_vline(xintercept=0, linetype="dashed", color = "red")

ggplot(th_env_spatiotemporal_abundance, aes(ccm_maxcorr_vcor, lab)) +
  geom_point() + 
  xlim(-1,1) +
  ylab("variable") + 
  xlab("Spearman correlation") + 
  theme_bw()  +
  geom_vline(xintercept=0, linetype="dashed", color = "red")




fun_process_spatial <- function(th_trmetrics_entomo_postedecapture,mod){
  
  if(mod == "abundance" ){
    th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% filter(resp_var > 0 )
  } else if (mod == "presence" ){
    th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% mutate(resp_var = ifelse(resp_var == 0,0,1 ))
  }

#th_trmetrics_entomo_postedecapture <- th_trmetrics_entomo_postedecapture %>% filter(season == th_season)


final_cor_df <- data.frame(lab=character(),corr=numeric(),type=character())

###### Land cover variables
lco_metadata <- dbReadTable(react_gpkg, 'lco_metadata') # table containing pixel value and label for each land cover map
metrics_defs <- landscapemetrics::list_lsm() # list of landscape metrics
lco_priority <- read.csv("data/react_db/miscellaneous_data/landcover/lco_pix_priority.csv",stringsAsFactors = F, sep = ",")
lco_metadata <- lco_metadata %>% left_join(lco_priority)



# join landscape metrics and response variable. output is a data frame where each row is a sampling point and each column is a landscape metric 

th_env_landcover <- env_landcover %>%
  left_join(metrics_defs) %>%
  dplyr::select(-c(level,metric,name,type)) %>%
  pivot_wider(names_from = c(function_name,buffer,layer_id,pixval), values_from = val, names_sep = "_", values_fill = list(val = 0)) %>%
  mutate_all(funs(replace_na(.,0))) %>%
  right_join(th_trmetrics_entomo_postedecapture[,c("idpointdecapture","idpostedecapture","resp_var")]) #%>%
  #left_join(entomo_csh_metadata_l1[,c("idpointdecapture","codepays","season")]) %>%
  #group_split(codepays)

#if(code_pays=="BF") {env_landcover2=env_landcover2[[1]]}  else {env_landcover2=env_landcover2[[2]]}
#env_landcover2$codepays=NULL


cor_df <- data.frame(metric=character(),corr=numeric(),pval=numeric())
for (i in 2:(ncol(th_env_landcover)-3)){
  if(sum(th_env_landcover[,i])!=0){
    th_cor <- correlation::correlation(cbind(th_env_landcover[i],th_env_landcover$resp_var), method = "spearman")
    cor_df <- rbind(cor_df,data.frame(metric=colnames(th_env_landcover[i]),  corr=th_cor$r, pval=th_cor$p))
  }
  
}

cor_df <- cor_df %>%
  mutate(name = gsub("_"," ",metric)) %>%
  mutate(function_name = paste(word(name,1),word(name,2),word(name,3),sep="_")) %>%
  mutate(buffer = as.numeric(word(name,4))) %>%
  mutate(layer_id = as.numeric(word(name,5))) %>%
  mutate(pixval = as.numeric(word(name,6))) %>%
  left_join(lco_metadata %>% dplyr::select(pixval,pixlabel,layer_id))

## variable sceening process


# rule 1 : pval<=0.05 and abs(cor)>=0.1
cor_df <- cor_df %>% filter(pval<=0.05, abs(corr)>=0.2)

# rule 2 : for pland : range(pland)>10 (i.e. if the difference between the minimum and the maximum proportion of land for a given lsm is less than 10, then the variable is removed (because the difference is too small))
vars_rul2 <- cor_df %>% filter(grepl("pland",function_name))
vars_to_remove_rul2 <- NULL
if(nrow(vars_rul2)>0){
#for(i in 1:nrow(vars_rul2)){
#  range <- range(th_env_landcover[,as.character(vars_rul2$metric[i])], na.rm = T)
#  if((range[2]-range[1])<=15){  vars_to_remove_rul2 <- c(vars_to_remove_rul2, as.character(vars_rul2$metric[i])) }
#}
}
#cor_df <- cor_df %>% dplyr::filter(!(metric %in% vars_to_remove_rul2))

# rule 3 : for np : range(np)>5
vars_rul3 <- cor_df %>% filter(grepl("np",function_name))
vars_to_remove_rul3 <- NULL
if(nrow(vars_rul3)>0){
for(i in 1:nrow(vars_rul3)){
  range <- range(th_env_landcover[,as.character(vars_rul3$metric[i])], na.rm = T)
  if((range[2]-range[1])<=5){  vars_to_remove_rul3 <- c(vars_to_remove_rul3, as.character(vars_rul3$metric[i])) }
}
}
cor_df <- cor_df %>% dplyr::filter(!(metric %in% vars_to_remove_rul3))

# correlation matrix for the remaining variables
if(nrow(cor_df)>0){
m <- th_env_landcover %>%
  dplyr::select(as.character(cor_df$metric)) %>%
  select_if(is.numeric) %>%
  cor(.,method = "spearman", use = "na.or.complete")

index <- which(abs(m) > .7 & abs(m) < 1, # your criteria
               arr.ind = T) # the result of the which function is now in rows & columns
p <- cbind.data.frame(stock1 = rownames(m)[index[,1]], # get the row name 
                      stock2 = colnames(m)[index[,2]]) # get the column name
p <- p %>%
  left_join(cor_df, by=c("stock1"="metric")) %>%
  left_join(lco_metadata %>% dplyr::select(pixval,layer_id,priority2)) %>%
  dplyr::rename(priority2.x=priority2) %>%
  left_join(cor_df, by=c("stock2"="metric")) %>%
  dplyr::rename(layer_id=layer_id.y,pixval=pixval.y) %>%
  left_join(lco_metadata %>% dplyr::select(pixval,layer_id,priority2)) %>%
  dplyr::rename(priority2.y=priority2) %>%
  mutate(priority2.x=ifelse(is.na(priority2.x),60,priority2.x)) %>%
  mutate(priority2.y=ifelse(is.na(priority2.y),60,priority2.y)) 
                  
var_to_remove <- NULL
for(i in 1:nrow(p)){
   if(p$priority2.x[i] < p$priority2.y[i]){
     var_to_remove <- c(var_to_remove,p$stock2[i])
   } else if (p$priority2.x[i] > p$priority2.y[i]) {
     var_to_remove <- c(var_to_remove,p$stock1[i])
   } else if (p$priority2.x[i] == p$priority2.y[i]) {  # case the lc class and the lc layer are the same
     
      # if(grepl("pland",p$stock1[i])){
      #   var_to_remove <- c(var_to_remove,p$stock2[i])
      # } else if (grepl("pland",p$stock2[i])){
      #   var_to_remove <- c(var_to_remove,p$stock1[i])
      # } else if (grepl("np",p$stock1[i])){
      #   var_to_remove <- c(var_to_remove,p$stock2[i])
      # } else if (grepl("np",p$stock2[i])){
      #   var_to_remove <- c(var_to_remove,p$stock1[i])
      # if (p$buffer.x[i] < p$buffer.y[i]){
     #   var_to_remove <- c(var_to_remove,p$stock1[i])
     # } else if (p$buffer.x[i] > p$buffer.y[i]){
     #   var_to_remove <- c(var_to_remove,p$stock2[i])
     # }
     
      if(p$corr.x[i] < p$corr.y[i]){
        var_to_remove <- c(var_to_remove,p$stock1[i])
      } else {
        var_to_remove <- c(var_to_remove,p$stock2[i])
      }
     
   }
}


var_to_remove <- unique(var_to_remove)

cor_df <- cor_df %>% 
  dplyr::filter(!(metric %in% var_to_remove)) %>%
  mutate(abs_corr=abs(corr))

cor_df2 <- cor_df %>%
  group_by(layer_id,pixval) %>%
  summarise(abs_corr=max(abs(corr))) %>%
  as_tibble() %>%
  mutate(to_keep = TRUE)

cor_df <- cor_df %>%
  left_join(cor_df2) %>%
  filter(to_keep==TRUE)

cor_df_prd <- cor_df %>%
  filter(is.na(pixval)) %>%
  filter(corr==max(abs(corr)))
  
cor_df <- cor_df %>%
  filter(!is.na(pixval)) %>%
  bind_rows(cor_df_prd)


# variables à retenir pour le modèle final

}
if(nrow(cor_df)>0){
  th_env_landcover <- th_env_landcover %>% dplyr::select(idpointdecapture,as.character(cor_df$metric)) %>% unique(.)
  
  #th_env_landcover <- as.character(cor_df$metric)
  
  cor_df <- cor_df %>% 
    mutate(pixlabel = gsub("_"," ",pixlabel)) %>%
    left_join(metrics_defs, by="function_name") %>%
    mutate(lab = paste0(pixlabel, " - \n",name.y, " - \n", buffer, " m")) %>%
    dplyr::select(lab,corr) %>%
    mutate(type = "Land cover")
  final_cor_df <- rbind(final_cor_df,cor_df)
} else {
  th_env_landcover <- th_env_landcover %>% dplyr::select(idpointdecapture) %>% unique(.)
  #th_env_landcover <- NULL
}




###### for covariates covering the night of the catch 

th_env_nightcatch_postedecapture <- env_nightcatch_postedecapture %>% pivot_wider(names_from = var, values_from = val) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>% right_join(th_trmetrics_entomo_postedecapture[,c("idpostedecapture","resp_var")])
cor_df <- data.frame(metric=character(),corr=numeric(),pval=numeric())
for (i in 2:(ncol(th_env_nightcatch_postedecapture)-1)){
    th_cor <- correlation::correlation(cbind(th_env_nightcatch_postedecapture[i],th_env_nightcatch_postedecapture[,"resp_var"]), method = "spearman")
    cor_df <- rbind(cor_df,data.frame(metric=colnames(th_env_nightcatch_postedecapture[i]),  corr=th_cor$r, pval=th_cor$p))
}

cor_df <- cor_df %>% filter(pval<=0.05,abs(corr)>=0.2)

if(nrow(cor_df)>0){
  th_env_nightcatch_postedecapture <- th_env_nightcatch_postedecapture %>% dplyr::select(idpostedecapture,as.character(cor_df$metric)) %>% unique(.)
  #th_env_nightcatch_postedecapture <- as.character(cor_df$metric)
  
  cor_df <- cor_df %>% 
    left_join(prediction_vars, by = c("metric" = "code")) %>%
    dplyr::select(short_name,corr) %>%
    dplyr::rename(lab = short_name) %>%
    mutate(type = "Micro-climatic conditions \n during the night of catch")
  
  final_cor_df <- rbind(final_cor_df,cor_df)
  
} else {
  th_env_nightcatch_postedecapture <- th_env_nightcatch_postedecapture %>% dplyr::select(idpostedecapture) %>% unique(.)
  #th_env_nightcatch_postedecapture <- NULL
}


th_env_nightcatch  <- env_nightcatch %>% pivot_wider(names_from = var, values_from = val) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>% right_join(th_trmetrics_entomo_postedecapture[,c("idpointdecapture","resp_var")])
cor_df <- data.frame(metric=character(),corr=numeric(),pval=numeric())
for (i in 2:(ncol(th_env_nightcatch)-1)){
  th_cor <- correlation::correlation(cbind(th_env_nightcatch[i],th_env_nightcatch[,"resp_var"]), method = "spearman")
  cor_df <- rbind(cor_df,data.frame(metric=colnames(th_env_nightcatch[i]),  corr=th_cor$r, pval=th_cor$p))
}

cor_df <- cor_df %>% filter(pval<=0.05,abs(corr)>=0.2)

if(nrow(cor_df)>0){
th_env_nightcatch <- th_env_nightcatch %>% dplyr::select(idpointdecapture,as.character(cor_df$metric)) %>% unique(.)
#th_env_nightcatch <- as.character(cor_df$metric)
  
cor_df <- cor_df %>% 
  left_join(prediction_vars, by = c("metric" = "code")) %>%
  dplyr::select(short_name,corr) %>%
  dplyr::rename(lab = short_name) %>%
  mutate(type = "Macro-climatic conditions \n during the night of catch")

final_cor_df <- rbind(final_cor_df,cor_df)
} else {
  th_env_nightcatch <- th_env_nightcatch %>% dplyr::select(idpointdecapture) %>% unique(.)
  #th_env_nightcatch <- NULL
}




###### for spatial-only data
th_env_spatial <- env_spatial %>% pivot_wider(names_from = c("var","buffer"), values_from = val) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>% right_join(th_trmetrics_entomo_postedecapture[,c("idpointdecapture","resp_var")])
cor_df <- data.frame(metric=character(),corr=numeric(),pval=numeric())
for (i in 2:(ncol(th_env_spatial)-1)){
  th_cor <- correlation::correlation(cbind(th_env_spatial[i],th_env_spatial[,"resp_var"]), method = "spearman")
  cor_df <- rbind(cor_df,data.frame(metric=colnames(th_env_spatial[i]),  corr=th_cor$r, pval=th_cor$p))
}

cor_df <- cor_df %>% filter(pval<=0.05,abs(corr)>=0.2)
cor_df$var <- substr(cor_df$metric,1,3)
cor_df$buffer <- sub('.*\\_', '', cor_df$metric)
cor_df2 <- cor_df %>%
  group_by(var) %>%
  summarise(corr=max(abs(corr))) %>%
  left_join(cor_df) %>%
  mutate(corr=ifelse(is.na(metric),-corr,corr)) %>%
  left_join(cor_df, by=c("var","corr"))
  
if(nrow(cor_df2)>1){
  th_env_spatial <- th_env_spatial %>% dplyr::select(idpointdecapture,as.character(cor_df2$metric.y))  %>% unique(.)
  #th_env_spatial <- as.character(cor_df2$metric.y)
  
  cor_df <- cor_df2 %>% 
    left_join(prediction_vars, by = c("var" = "code")) %>%
    mutate(lab = paste0(short_name, " - \n", buffer.y," m")) %>%
    dplyr::select(lab,corr) %>%
    mutate(type = "Other spatial variables")
  
  
  final_cor_df <- rbind(final_cor_df,cor_df)
  
} else {
  th_env_spatial <- th_env_spatial %>% dplyr::select(idpointdecapture) %>% unique(.)
  #th_env_spatial <- NULL
  
}



###### for static variables
th_env_static <- env_static %>% pivot_wider(names_from = var, values_from = val) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>% mutate(VCT=as.numeric(VCT), WMD=as.numeric(WMD)) %>% right_join(th_trmetrics_entomo_postedecapture[,c("idpointdecapture","resp_var")])
cor_df <- data.frame(metric=character(),corr=numeric(),pval=numeric())
for (i in 2:(ncol(th_env_static)-4)){
  th_cor <- correlation::correlation(cbind(as.numeric(unlist(th_env_static[i])),th_env_static[,"resp_var"]), method = "spearman")
  cor_df <- rbind(cor_df,data.frame(metric=colnames(th_env_static[i]),  corr=th_cor$r, pval=th_cor$p))
}

cor_df <- cor_df %>% filter(pval<=0.05,abs(corr)>=0.2)

if(nrow(cor_df)>0){
  th_env_static <- th_env_static %>% dplyr::select(idpointdecapture,as.character(cor_df$metric),VCP,VCM,VCT)  %>% unique(.)
  #th_env_static <- c(as.character(cor_df$metric),"VCP","VCM","VCT")
  cor_df <- cor_df %>%
    left_join(prediction_vars, by = c("metric" = "code")) %>%
    mutate(lab = short_name) %>%
    dplyr::select(lab, corr) %>%
    mutate(type = "Other spatial-only variables")
  
  
  final_cor_df <- rbind(final_cor_df,cor_df)
} else {
  th_env_static <- th_env_static %>% dplyr::select(idpointdecapture,VCP,VCM,VCT)  %>% unique(.)
  #th_env_static <-  c("VCP","VCM","VCT")
}



final_cor_df <- final_cor_df %>% arrange(corr) %>%  mutate(lab = factor(lab, levels = .$lab))
  
return(list(final_cor_df=final_cor_df,th_env_static=th_env_static,th_env_spatial=th_env_spatial,th_env_nightcatch=th_env_nightcatch,th_env_nightcatch_postedecapture=th_env_nightcatch_postedecapture,th_env_landcover=th_env_landcover))

}




spatial_presence <- fun_process_spatial(th_trmetrics_entomo_postedecapture,"presence")
spatial_abundance <- fun_process_spatial(th_trmetrics_entomo_postedecapture,"abundance")




# 
# 
# spatial_wet_season_presence <- fun_process_spatial(th_trmetrics_entomo_postedecapture,"wet","presence")
# spatial_dry_season_presence <- fun_process_spatial(th_trmetrics_entomo_postedecapture,"dry","presence")
# 
# th_trmetrics_entomo_postedecapture_presence <- th_trmetrics_entomo_postedecapture %>% mutate(resp_var = ifelse(resp_var == 0,0,1 ))
# 
# th_env_landcover <- env_landcover %>%
#   left_join(metrics_defs) %>%
#   dplyr::select(-c(level,metric,name,type)) %>%
#   pivot_wider(names_from = c(function_name,buffer,layer_id,pixval), values_from = val, names_sep = "_", values_fill = list(val = 0)) %>%
#   mutate_all(funs(replace_na(.,0))) %>%
#   right_join(th_trmetrics_entomo_postedecapture_presence[,c("idpointdecapture","idpostedecapture","resp_var")])
# th_env_static <- env_static %>% pivot_wider(names_from = var, values_from = val) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>% mutate(VCT=as.numeric(VCT), WMD=as.numeric(WMD)) %>% right_join(th_trmetrics_entomo_postedecapture_presence[,c("idpointdecapture","resp_var")])
# th_env_spatial <- env_spatial %>% pivot_wider(names_from = c("var","buffer"), values_from = val) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>% right_join(th_trmetrics_entomo_postedecapture_presence[,c("idpointdecapture","resp_var")])
# th_env_nightcatch  <- env_nightcatch %>% pivot_wider(names_from = var, values_from = val) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>% right_join(th_trmetrics_entomo_postedecapture_presence[,c("idpointdecapture","resp_var")])
# th_env_nightcatch_postedecapture <- env_nightcatch_postedecapture %>% pivot_wider(names_from = var, values_from = val) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>% right_join(th_trmetrics_entomo_postedecapture_presence[,c("idpostedecapture","resp_var")])
# 
# th_env_landcover_presence <- th_env_landcover %>% dplyr::select(idpointdecapture,unique(c(spatial_wet_season_presence$th_env_landcover,spatial_dry_season_presence$th_env_landcover)))  %>% unique(.)
# th_env_static_presence <- th_env_static %>% dplyr::select(idpointdecapture,unique(c(spatial_wet_season_presence$th_env_static,spatial_dry_season_presence$th_env_static)))  %>% unique(.)
# th_env_spatial_presence <- th_env_spatial %>% dplyr::select(idpointdecapture,unique(c(spatial_wet_season_presence$th_env_spatial,spatial_dry_season_presence$th_env_spatial)))  %>% unique(.)
# th_env_nightcatch_presence <- th_env_nightcatch %>% dplyr::select(idpointdecapture,unique(c(spatial_wet_season_presence$th_env_nightcatch,spatial_dry_season_presence$th_env_nightcatch)))  %>% unique(.)
# th_env_nightcatch_postedecapture_presence <- th_env_nightcatch_postedecapture %>% dplyr::select(idpostedecapture,unique(c(spatial_wet_season_presence$th_env_nightcatch_postedecapture,spatial_dry_season_presence$th_env_nightcatch_postedecapture)))  %>% unique(.)
# 
# 
# 
# spatial_wet_season_abundance <- fun_process_spatial(th_trmetrics_entomo_postedecapture,"wet","abundance")
# spatial_dry_season_abundance <- fun_process_spatial(th_trmetrics_entomo_postedecapture,"dry","abundance")
# 
# th_trmetrics_entomo_postedecapture_abundance <- th_trmetrics_entomo_postedecapture %>% filter(resp_var > 0 )
# 
# th_env_landcover <- env_landcover %>%
#   left_join(metrics_defs) %>%
#   dplyr::select(-c(level,metric,name,type)) %>%
#   pivot_wider(names_from = c(function_name,buffer,layer_id,pixval), values_from = val, names_sep = "_", values_fill = list(val = 0)) %>%
#   mutate_all(funs(replace_na(.,0))) %>%
#   right_join(th_trmetrics_entomo_postedecapture_abundance[,c("idpointdecapture","idpostedecapture","resp_var")])
# th_env_static <- env_static %>% pivot_wider(names_from = var, values_from = val) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>% mutate(VCT=as.numeric(VCT), WMD=as.numeric(WMD)) %>% right_join(th_trmetrics_entomo_postedecapture_abundance[,c("idpointdecapture","resp_var")])
# th_env_spatial <- env_spatial %>% pivot_wider(names_from = c("var","buffer"), values_from = val) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>% right_join(th_trmetrics_entomo_postedecapture_abundance[,c("idpointdecapture","resp_var")])
# th_env_nightcatch  <- env_nightcatch %>% pivot_wider(names_from = var, values_from = val) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>% right_join(th_trmetrics_entomo_postedecapture_abundance[,c("idpointdecapture","resp_var")])
# th_env_nightcatch_postedecapture <- env_nightcatch_postedecapture %>% pivot_wider(names_from = var, values_from = val) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>% right_join(th_trmetrics_entomo_postedecapture_abundance[,c("idpostedecapture","resp_var")])
# 
# th_env_landcover_abundance <- th_env_landcover %>% dplyr::select(idpointdecapture,unique(c(spatial_wet_season_abundance$th_env_landcover,spatial_dry_season_abundance$th_env_landcover)))  %>% unique(.)
# th_env_static_abundance <- th_env_static %>% dplyr::select(idpointdecapture,unique(c(spatial_wet_season_abundance$th_env_static,spatial_dry_season_abundance$th_env_static)))  %>% unique(.)
# th_env_spatial_abundance <- th_env_spatial %>% dplyr::select(idpointdecapture,unique(c(spatial_wet_season_abundance$th_env_spatial,spatial_dry_season_abundance$th_env_spatial)))  %>% unique(.)
# th_env_nightcatch_abundance <- th_env_nightcatch %>% dplyr::select(idpointdecapture,unique(c(spatial_wet_season_abundance$th_env_nightcatch,spatial_dry_season_abundance$th_env_nightcatch)))  %>% unique(.)
# th_env_nightcatch_postedecapture_abundance <- th_env_nightcatch_postedecapture %>% dplyr::select(idpostedecapture,unique(c(spatial_wet_season_abundance$th_env_nightcatch_postedecapture,spatial_dry_season_abundance$th_env_nightcatch_postedecapture)))  %>% unique(.)
# 
# 
# # # cleveland dotplot 
#  ggplot(spatial_wet_season_presence$final_cor_df, aes(corr, lab, shape = type)) +
#    geom_point() + 
#    xlim(-1,1) +
#    ylab("variable") + 
#    xlab("Spearman correlation") + 
#    theme_bw() +
#    geom_vline(xintercept=0, linetype="dashed", color = "red")
#  ggplot(spatial_dry_season_presence$final_cor_df, aes(corr, lab, shape = type)) +
#    geom_point() + 
#    xlim(-1,1) +
#    ylab("variable") + 
#    xlab("Spearman correlation") + 
#    theme_bw()  +
#    geom_vline(xintercept=0, linetype="dashed", color = "red")
#  ggplot(spatial_wet_season_abundance$final_cor_df, aes(corr, lab, shape = type)) +
#    geom_point() + 
#    xlim(-1,1) +
#    ylab("variable") + 
#    xlab("Spearman correlation") + 
#    theme_bw()  +
#    geom_vline(xintercept=0, linetype="dashed", color = "red")
#  ggplot(spatial_dry_season_abundance$final_cor_df, aes(corr, lab, shape = type)) +
#    geom_point() + 
#    xlim(-1,1) +
#    ylab("variable") + 
#    xlab("Spearman correlation") + 
#    theme_bw()  +
#    geom_vline(xintercept=0, linetype="dashed", color = "red")








### prepare additional datasets for the coordinates of the points

# roi <- st_read(path_to_db,"contexte_frontieresreact") %>% 
#   filter(codepays==code_pays) %>%
#   st_transform(32630) %>%
#   as("Spatial")
# 
# #r_sppix <- raster::raster(extent(roi), crs = crs(roi), res = 10) %>% setValues(1) %>% as("SpatialPixelsDataFrame")
# 
# 
# roi <- as(roi,"sf")
# centroid <- st_centroid(roi)
# borders <- st_coordinates(roi) %>%
#   as.data.frame() %>%
#   st_as_sf(coords = c("X", "Y"), crs = 32630)
# 
 mean_coords_points = st_read(path_to_db, 'entomo_csh_metadata_l1', crs = 4326) %>%
   filter(codepays==code_pays) %>%
   group_by(codevillage,pointdecapture) %>%
   summarise(X=mean(X),Y=mean(Y)) %>%
   st_drop_geometry() %>%
   st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
   st_transform(32630) 
# 
# # matrix of distances between the sampling points
# dists_between_csh_points  <- as.data.frame(st_distance(mean_coords_points,mean_coords_points)) %>%
#   mutate_at(vars(starts_with("V")), as.numeric)
# colnames(dists_between_csh_points) <- gsub("V","VV",colnames(dists_between_csh_points))
# 
# mean_coords_csh_points <- mean_coords_points %>% 
#   st_drop_geometry() %>%
#   bind_cols(dists_between_csh_points)
# 
# # distances between the sampling points and the the corners + the center of the roi
# dists_to_center <- st_read(path_to_db, 'entomo_csh_metadata_l1', crs = 4326) %>%
#   filter(codepays==code_pays) %>%
#   st_transform(32630) %>%
#   mutate(D0 = as.numeric(st_distance(.,centroid)))
# 
# dists_to_borders  <-  st_read(path_to_db, 'entomo_csh_metadata_l1', crs = 4326) %>%
#   filter(codepays==code_pays) %>%
#   st_transform(32630)
# 
# dists_to_borders  <- as.data.frame(st_distance(dists_to_borders,borders)) %>%
#   mutate_at(vars(starts_with("V")), as.numeric)
# 
# colnames(dists_to_borders) <- gsub("V","D",colnames(dists_to_borders))
# 
# dists_csh_points <- bind_cols(dists_to_center,dists_to_borders) %>%
#   st_drop_geometry() %>%
#   dplyr::select(idpointdecapture,D0,D1,D2,D3,D4,D5)
# 
mean_coords_points$X = as.numeric(st_coordinates(mean_coords_points)[,1])
mean_coords_points$Y = as.numeric(st_coordinates(mean_coords_points)[,2])

mean_coords_points = st_drop_geometry(mean_coords_points)

#mean_coords_points
#mean_coords_csh_points
#dists_csh_points




######## join all the variables
df_final_presence <- th_trmetrics_entomo_postedecapture %>%
  mutate(resp_var = ifelse(resp_var == 0,0,1 )) %>%
  dplyr::select(resp_var,idpostedecapture,idpointdecapture) %>%
  mutate(int_ext = substr(idpostedecapture,nchar(idpostedecapture),nchar(idpostedecapture))) %>%
  left_join(entomo_csh_metadata_l1 %>% dplyr::select(idpointdecapture,nummission,codevillage,pointdecapture,date_capture)) %>%
  mutate(date_capture=lubridate::yday(as.Date(date_capture))) %>%
  left_join(expl_ts_presence) %>%
  left_join(spatial_presence$th_env_landcover) %>%
  left_join(spatial_presence$th_env_nightcatch_postedecapture) %>%
  left_join(spatial_presence$th_env_spatial) %>%
  left_join(spatial_presence$th_env_nightcatch) %>%
  left_join(spatial_presence$th_env_static) %>%
  left_join(mean_coords_points) %>%
  mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>%
  dplyr::select(-c(idpostedecapture,idpointdecapture))
  

# m <- df_final_presence %>%
#   select_if(is.numeric) %>%
#   cor(.,method = "spearman", use = "na.or.complete")
# 
# index <- which(abs(m) > .7 & abs(m) < 1, arr.ind = T)
# p <- cbind.data.frame(stock1 = rownames(m)[index[,1]],stock2 = colnames(m)[index[,2]])


df_final_abundance <-  th_trmetrics_entomo_postedecapture %>%
  filter(resp_var > 0 ) %>%
  dplyr::select(resp_var,idpostedecapture,idpointdecapture) %>%
  mutate(int_ext = substr(idpostedecapture,nchar(idpostedecapture),nchar(idpostedecapture))) %>%
  left_join(entomo_csh_metadata_l1 %>% dplyr::select(idpointdecapture,nummission,codevillage,pointdecapture,date_capture)) %>%
  mutate(date_capture=lubridate::yday(as.Date(date_capture))) %>%
  left_join(expl_ts_abundance) %>%
  left_join(spatial_abundance$th_env_landcover) %>%
  left_join(spatial_abundance$th_env_nightcatch_postedecapture) %>%
  left_join(spatial_abundance$th_env_spatial) %>%
  left_join(spatial_abundance$th_env_nightcatch) %>%
  left_join(spatial_abundance$th_env_static) %>%
  left_join(mean_coords_points) %>%
  mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>%
  dplyr::select(-c(idpostedecapture,idpointdecapture))


library(CAST)
library(caret)
indices <- CreateSpacetimeFolds(df_final_abundance,spacevar ="codevillage", k=length(unique(df_final_abundance$codevillage))) #timevar = "nummission",
  tr = trainControl(method="cv",
                  index = indices$index, 
                  indexOut = indices$indexOut#,
                  #summaryFunction = mymetric
)

predictors <- setdiff(colnames(df_final_abundance),c("nummission","codevillage","pointdecapture","date_capture","resp_var","X","Y"))

model <- caret::train(df_final_abundance[,predictors],df_final_abundance$resp_var,
                         method="ranger",tuneLength=10,#metric="spear_cor",
                         trControl=tr)
model


plot(model$finalModel$predictions,df_final_abundance$resp_var)

library(iml)

X <- df_final_abundance[which(names(df_final_abundance) %in% predictors)]
predictor <- Predictor$new(model, data = X, y = df_final_abundance$resp_var)
imp <- FeatureImp$new(predictor, loss = "rmse")
library("ggplot2")
plot(imp)
ale <- FeatureEffect$new(predictor, feature = "RFD1_F_2000_17_42", method = "pdp+ice")
ale$plot()
ia <- Interaction$new(predictor,feature = "RFD1_F_2000_17_42")
plot(ia) + xlim(0,1)

ale <- FeatureEffect$new(predictor, feature = "lsm_c_pland_500_3_4", method = "pdp+ice")
ale$plot()


# X <- df_final_abundance %>%
#      mutate(season = case_when(code_pays=="BF" & nummission %in% c("1","2","4","5","6","7","15") ~ "dry",
#                                 code_pays=="BF" & nummission %in% c("3","11","12","13") ~ "wet",
#                                 code_pays=="CI" & nummission %in% c("2","3","6","7","8") ~ "dry",
#                                 code_pays=="CI" & nummission %in% c("1","4","5") ~ "wet"))
# 
# X_dry <- X %>%
#   filter(season=="dry") %>%
#   dplyr::select(predictors)
#   
# X_wet <- X %>%
#   filter(season=="wet") %>%
#   dplyr::select(predictors)
# 
# shapley_dry <- NULL
# shapley_wet <- NULL
# 
# for(i in 1:nrow(X_dry)){
#   cat(i,"\n")
#   th_shapley <- Shapley$new(predictor, x.interest = X_dry[i, ])
#   shapley_dry <- rbind(shapley_dry,th_shapley$results)
# }
# 
# for(i in 1:nrow(X_wet)){
#   cat(i,"\n")
#   th_shapley <- Shapley$new(predictor, x.interest = X_wet[i, ])
#   shapley_wet <- rbind(shapley_wet,th_shapley$results)
# }
# 
#   ggplot(shapley_wet, aes(phi,feature)) +
#   geom_point(size = 0.2)
# 
# #shapley$plot()



df_final_presence <- df_final_presence %>%
  mutate(resp_var = ifelse(resp_var==0,"Absence","Presence")) %>%
  mutate(resp_var = as.factor(resp_var))

indices <- CreateSpacetimeFolds(df_final_presence,spacevar ="codevillage", k=length(unique(df_final_presence$codevillage))) #timevar = "nummission",
tr = trainControl(method="cv",
                  index = indices$index, 
                  indexOut = indices$indexOut,
                  sampling = "up",
                  summaryFunction = prSummary,
                  classProbs = TRUE
                  )

predictors <- setdiff(colnames(df_final_presence),c("nummission","codevillage","pointdecapture","date_capture","resp_var","X","Y"))


model <- caret::train(df_final_presence[,predictors],df_final_presence$resp_var,
                      method="ranger",tuneLength=10,#metric="spear_cor",
                      trControl=tr)
model


library(iml)
X <- df_final_presence[which(names(df_final_presence) %in% predictors)]
predictor <- Predictor$new(model, data = X, y = df_final_presence$resp_var)
imp <- FeatureImp$new(predictor, loss = "ce")
library("ggplot2")
plot(imp)
ale <- FeatureEffect$new(predictor, feature = "VCT", method = "pdp")
ale$plot()


ia <- Interaction$new(predictor,feature = "RFD1_F_2000_33_54")
plot(ia) + xlim(0,1)







mymetric <- function(data, lev = NULL, model = NULL){
  
  spear_cor <- cor(data$obs,data$pred,method = "spearman")
  #pears_cor <- cor(data$obs,data$pred,method = "pearson")
  rmse <- RMSE(data$obs,data$pred,na.rm = T)
  mae <- MAE(data$obs,data$pred,na.rm = T)
  sd <- sd(data$pred)
  #rsquared <- 1 - ((data$obs-data$pred)^2 /  (data$obs-mean(data$obs)))
  out <- c(rmse, mae, spear_cor, sd)
  

  
  names(out) <- c("RMSE", "MAE", "spear_cor", "sd")
  
  out
}





S.dist  <-  dnearneigh(coo, 0, 50000)  


