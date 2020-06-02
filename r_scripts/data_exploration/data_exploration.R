# -> tester automatiquement (en univarié) les éléments suivants : linéarité (coeff pearson), non-linéarité monotone (coeff spearman), non-linéarité non-monotone (gam)
# -> comment, dans tous ces cas, inclure l'aspect hiérarchique / transversal des données ? avec des effets aléatoires ? mais les coeff de person et spearman par ex. n'incluent pas la possibilité d'utiliser des effets aléatoires... 
# -> autre problématique: comment inclure la distribution binomiale négative des données ? 

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

resp_var <- "ma_gambiae_ss"  # "ma_gambiae_ss"
code_pays <- "BF"            # "BF"

### connect to the database
path_to_db <- "data/react_db/react_db.gpkg" 
react_gpkg <- DBI::dbConnect(RSQLite::SQLite(),dbname = path_to_db) 

### open the tables
## dates and positions of the entomological missions (1 row = 1 point de capture)
entomo_csh_metadata_l1 <- dbReadTable(react_gpkg, 'entomo_csh_metadata_l1') %>% dplyr::select(-fid)

## table containing the response variables (ie variables to model)
trmetrics_entomo_postedecapture <- dbReadTable(react_gpkg, 'trmetrics_entomo_postedecapture') %>% dplyr::select(-fid) %>% rename(id = idpointdecapture) 

## tables containing the explanatory variables
# spatiotemporal explanatory variables
env_spatiotemporal <- dbReadTable(react_gpkg, 'env_spatiotemporal') %>% dplyr::select(-fid) %>% filter(var!="WMW30")

# spatial-only explanatory variables
env_spatial <- dbReadTable(react_gpkg,'env_spatial') %>% dplyr::select(-fid)

# non-spatial explanatory variables
env_static <- dbReadTable(react_gpkg, 'env_static') %>% dplyr::select(-fid)

# variables for the night of catch
env_nightcatch <-  dbReadTable(react_gpkg, 'env_nightcatch') %>% dplyr::select(-fid)

# variables for the night of catch at the pointdecapture level
env_nightcatch_pointdecapture <- dbReadTable(react_gpkg,"env_nightcatch_postedecapture") %>% dplyr::select(-fid)

# landcover variables
env_landcover <-  dbReadTable(react_gpkg, 'env_landcover') %>% dplyr::select(-fid)

## table of exhaustive definitions of the explanatory variables
googlesheets4::sheets_deauth()
prediction_vars <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1dIeSOa2WinXvOQGLmIjA0gFdsHnb6zMMsDME-G5pyMc/edit?usp=sharing", sheet = "var_explication", col_types="c")


### source home-made functions 
source("r_scripts/data_exploration/functions.R")

####################################################################################################
####################################################################################################
####################################################################################################
#########  Abundance of vector species : Univariate analysis #########
####################################################################################################
####################################################################################################
####################################################################################################

# The goal of the univariate analysis is mainly to pre-filter the explanatory variables before the collinearity tests and the multivariate analysis. 
# Univariate analysis is however also interesting per se since it enables to have a first insight at the relationships between each explanatory variable and the response variable, e.g. through cross-correlation maps (see above) for time-related explanatory variables

################
###### Time-series variables : Cross correlation maps (CCM)
################

### data pre-processing and preparation

# join codepays and variable type to the explanatory vars
env_spatiotemporal <- env_spatiotemporal %>%
  mutate(buffer = as.character(buffer)) %>%
  right_join(entomo_csh_metadata_l1[,c("idpointdecapture","codepays")], by =  c("id" = "idpointdecapture")) %>%
  left_join(prediction_vars[,c("code","type_group1")], by = c("var" = "code"))
  
# retructurate in lists
env_spatiotemporal2 <- env_spatiotemporal %>%
  dplyr::select(-c(lag_time, date)) %>%
  group_by(type_group1, var, codepays, buffer) %>%
  tidyr::nest(predictive_df = c(id, lag_n , val)) %>%
  mutate(fun_summarize_ccm = ifelse(var %in% c("RFD1_F","RFD1_L","RFD7_F","RFD7_L"), "sum", "mean")) %>%
  arrange(type_group1, var, codepays, as.numeric(buffer), fun_summarize_ccm)


# for now we work only with 1 response variable (namely the abundance of anopheles gambiae SS in BF) and 1 buffer size (2000 m)
env_spatiotemporal2 <- env_spatiotemporal2 %>% filter(buffer==2000, codepays==code_pays)

################
###### Spatio-temporal variables
################

# We use cross correlation maps (CCM)
# theoretical background of the CCM : https://www.ncbi.nlm.nih.gov/pubmed/16187896
# applications : https://doi.org/10.1186/1756-3305-6-129  , mémoire M2 Rodolphe Mader (CONSTRUCTION D’UN MODELE PREDICTIF DE L’INCIDENCE DU PALUDISME ASSOCIANT DES VARIABLES CLIMATIQUES, ENVIRONNEMENTALES, ENTOMOLOGIQUES ET DE LUTTE ANTI -VECTORIELLE DANS LA COMMUNE DE FARAFANGANA (MADAGASCAR) : LIMITES ET PERSPECTIVES)

# NB : in this version we use the Spearman coefficient to build the CCM, with the following benefits and drawbacks : 
# - benefits : spearman coeff i) is very fast to compute (important argument in our case since we have hundreds of thousands of combinations to test), ii) takes into account the non-linear relationship between the variables (since it computes the correlations on the ranks), iii) provides both the strength and the sign (positive or negative) of the correlation
# - drawbacks : spearman coeff i) does not take into account the spatial autocorrelation , ii) does not take into account possible polynomial relationships between the variable 
# to overcome these drawbacks we could think of other ways to compute the CCM, e.g. using spatial Random Forest (see above), GAM, GLM, etc, with the following drawbacks however : longer computational time + no insight on the sign of the relationship (ie only strength) 

# For the positive counts, CCMs are generated by calculating the spearman correlation coefficient between the response variable and each available time lag of the explanatory variable
# above, the functions used in each "map" function are available in the script functions.R .
env_spatiotemporal2 <- env_spatiotemporal2 %>%
  mutate(predictive_df = pmap(list(predictive_df, var, buffer, fun_summarize_ccm), ~fun_ccm_df(..1, ..2, ..3, function_to_apply = ..4))) %>% # function "fun_ccm_df" prepares the data for the CCM (ie calculates the mean or sum of the explanatory variable for each available time lag)
  mutate(ccm_corrmat_sup0 = map(predictive_df, ~fun_ccm_corrmat_sup0_spearman(., trmetrics_entomo_postedecapture, resp_var))) %>%  # function "fun_ccm_corrmat_sup0" creates the CCM by using the spearman correlation coefficient 
  mutate(ccm_plot_sup0 = pmap(list(ccm_corrmat_sup0, var, buffer, codepays), ~fun_ccm_plot(..1,..2,..3,..4))) %>% # function "fun_ccm_plot" plots the CCM
  mutate(ccm_maxcorr_vcor = map_dbl(ccm_corrmat_sup0, function(x) ifelse( !all(is.na(x$correlation)), x$correlation[which.max(abs(x$abs_corr))], NA))) %>% # get max correlation value (wether positive or negative) for the CCM
  mutate(ccm_maxcorr_lag1 = map_dbl(ccm_corrmat_sup0, function(x) ifelse( !all(is.na(x$correlation)), x$time_lag_1[which.max(abs(x$abs_corr))], NA))) %>% # get time lag 1 for max correlation value
  mutate(ccm_maxcorr_lag2 = map_dbl(ccm_corrmat_sup0, function(x) ifelse( !all(is.na(x$correlation)), x$time_lag_2[which.max(abs(x$abs_corr))], NA))) %>% # get time lag 2 for max correlation value
  mutate(var_df = paste(var,buffer,ccm_maxcorr_lag2,ccm_maxcorr_lag1,sep="_")) # get name of the variable for the max correlation value

env_spatiotemporal2

# plot the CCM alone
# here we choose to plot the CCM for the rainfall (var=="RFD1_F"). 
# possible_vars = unique(env_spatiotemporal2$var)
env_spatiotemporal2$ccm_plot_sup0[[which(env_spatiotemporal2$var=="RFD1_F")]]
# red framed dot is the highest correlation coeff (either positive or negative). black framed dots are the top 3% highest correlation coeffs. grey dots are when the p-value of the spearman coefficient is not significative (>0.05)
# so here for example here the highest coefficient is 0.6 at a time lag r(0,46), meaning that : the sum of the rainfall between 0 days and 46 days before the HLC misssion is best correlated with the positive counts of An. gambiae ss in BF.

# other informative plots related to the time series and the CCM
plots <- fun_spatiotemparal_plots(resp_var,"RFD1_F",code_pays)  # function "fun_spatiotemparal_plots" is available in the script functions.R  . uses datasets such as env_spatiotemporal2, etc.
plots$plot1  # time series of positive counts of ma_gambiae_ss overlaid with rainfall (BF)
plots$plot2  # plot of ma_gambiae_ss against the rainfall data for the highest correlation coeff of the CCM


################
###### Land cover variables
################
## For the land cover variables (ie landscape metrics), the strategy is to compute the spearman correlation coefficient between the response variable and each landscape metric. 
# For the multivariate analysis we keep only significant correlations (pval < 0.05) with absolute correlation coefficient >= 0.2

lco_metadata <- dbReadTable(react_gpkg, 'lco_metadata') # table containing pixel value and label for each land cover map
metrics_defs <- landscapemetrics::list_lsm() # list of landscape metrics

# join landscape metrics and response variable. output is a data frame where each row is a sampling point and each column is a landscape metric 
env_landcover2 <- env_landcover %>%
  left_join(metrics_defs) %>%
  dplyr::select(-c(level,metric,name,type)) %>%
  pivot_wider(names_from = c(function_name,buffer,layer_id,pixval), values_from = val, names_sep = "@", values_fill = list(val = 0)) %>%
  right_join(trmetrics_entomo_postedecapture[,c("id",resp_var)]) %>%
  left_join(entomo_csh_metadata_l1[,c("idpointdecapture","codepays")], by = c("id" = "idpointdecapture")) %>%
  group_split(codepays)

# env_landcover2[[1]] is BF
# env_landcover2[[2]] is CI

# calculate spearman correlation and related p-value between the response variable and each landscape metric, for the positive counts
correlation <- env_landcover2 %>%
  map(~filter(.,ma_an > 0)) %>%
  map(~sapply(.[,2:(ncol(.)-2)], function(x) cor(x, .$ma_an, method = "spearman", use = "na.or.complete")))

correlation_pval <- env_landcover2 %>%
  map(~filter(.,ma_an > 0)) %>%
  map(~sapply(.[,2:(ncol(.)-2)], function(x) cor.test(x, .$ma_an, method = "spearman", use = "na.or.complete", exact = FALSE)$p.value)) 

# attach the pixel label and the land cover layer name to the correlation and the p-value dataframes
correlation_df <- correlation %>% 
  map(~fun_tidy_corr(.,"corr"))

correlation_pval_df <- correlation_pval %>% 
  map(~fun_tidy_corr(.,"pval"))

# join correlation coeff and p-value
correlation_df <- correlation_df %>%
  map2(.,correlation_pval_df,~left_join(.x,.y)) 

correlation_df[[1]]  # this data.frame shows the correlation coeff and pval for each {landscape metric, landcover layer} for the ma_gambiae_ss in the BF
# for instance we see that the highest correlation coefficient is 0.32052665. it is for the landscape metric "percentage of landscape" of the class "foret_ripicole" in a buffer of 250m around the sampling points

# for the multivariate analysis we keep only data with absolute correlation >= 0.2 and pvalue <= 0.05
correlation_df <- correlation_df %>%
  map(.,~filter(., pval < 0.05)) %>% 
  map(.,~filter(., abs(corr) >= 0.2))


correlation_df_bf <- correlation_df[[2]]$name2
correlation_df_bf <- gsub(" ","@",correlation_df_bf)

env_landcover_bf <- env_landcover %>%
  left_join(metrics_defs) %>%
  dplyr::select(-c(level,metric,name,type)) %>%
  pivot_wider(names_from = c(function_name,buffer,layer_id,pixval), values_from = val, names_sep = "@", values_fill = list(val = 0)) %>%
  dplyr::select(id,!!correlation_df_bf) 
colnames(env_landcover_bf) <- gsub("@","_",colnames(env_landcover_bf))

env_landcover_bf

env_landcover_bf <- env_landcover_bf %>%
  right_join(trmetrics_entomo_postedecapture[,c("id",resp_var)])

################
###### variables for the night of the catch
################

# join to response variable
env_nightcatch2 <- env_nightcatch %>%
  pivot_wider(names_from = var, values_from = val) %>%
  right_join(trmetrics_entomo_postedecapture) %>%
  left_join(entomo_csh_metadata_l1, by = c("id" = "idpointdecapture")) %>%
  group_split(codepays) %>%
  map(~dplyr::select(.,id,ma_gambiae_ss,RFH,WDR,WSP,LMN))

env_nightcatch_bf <- env_nightcatch2[[1]]  # [[1]] is for BF, [[2]] is for CI
env_nightcatch_bf$ma_gambiae_ss <- NULL


################
###### spatial but non temporal variables
################

env_spatial

################
###### non-spatial and non-temporal variables
################

env_static


################
###### Model using Random forest for spatial predictions
################
# theoretical background of the RFsp : https://peerj.com/articles/5518/   and source github related to the article from where some functions are extracted :  https://github.com/thengl/GeoMLA

## some data pre-processing to 


roi <- st_read(path_to_db,"contexte_frontieresreact") %>% 
  filter(codepays==code_pays) %>%
  st_transform(32630) %>%
  as("Spatial")

r_sppix <- raster::raster(extent(roi), crs = crs(roi), res = 10) %>% setValues(1) %>% as("SpatialPixelsDataFrame")


### prepare additional datasets that will be used in spatial Random Forest
roi <- as(roi,"sf")
centroid <- st_centroid(roi)
borders <- st_coordinates(roi) %>%
  as.data.frame() %>%
  st_as_sf(coords = c("X", "Y"), crs = 32630)

mean_coords_points = st_read(path_to_db, 'entomo_csh_metadata_l1', crs = 4326) %>%
  filter(codepays==code_pays) %>%
  group_by(codevillage,pointdecapture) %>%
  summarise(X=mean(X),Y=mean(Y)) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
  st_transform(32630) 

# matrix of distances between the sampling points
dists_between_csh_points  <- as.data.frame(st_distance(mean_coords_points,mean_coords_points)) %>%
  mutate_at(vars(starts_with("V")), as.numeric)
colnames(dists_between_csh_points) <- gsub("V","VV",colnames(dists_between_csh_points))

mean_coords_csh_points <- mean_coords_points %>% 
  st_drop_geometry() %>%
  bind_cols(dists_between_csh_points)

# distances between the sampling points and the the corners + the center of the roi
dists_to_center <- st_read(path_to_db, 'entomo_csh_metadata_l1', crs = 4326) %>%
  filter(codepays==code_pays) %>%
  st_transform(32630) %>%
  mutate(D0 = as.numeric(st_distance(.,centroid)))

dists_to_borders  <-  st_read(path_to_db, 'entomo_csh_metadata_l1', crs = 4326) %>%
  filter(codepays==code_pays) %>%
  st_transform(32630)

dists_to_borders  <- as.data.frame(st_distance(dists_to_borders,borders)) %>%
  mutate_at(vars(starts_with("V")), as.numeric)

colnames(dists_to_borders) <- gsub("V","D",colnames(dists_to_borders))

dists_csh_points <- bind_cols(dists_to_center,dists_to_borders) %>%
  st_drop_geometry() %>%
  dplyr::select(idpointdecapture,D0,D1,D2,D3,D4,D5)









env_static2 <- env_static %>%
  pivot_wider(names_from = var, values_from = val)

env_nightcatch2 <- env_nightcatch %>%
  pivot_wider(names_from = var, values_from = val) 

df_for_model <- trmetrics_entomo_postedecapture %>%
  dplyr::select(idpostedecapture,id,!!resp_var) %>%
  left_join(entomo_csh_metadata_l1 %>% dplyr::select(idpointdecapture,codevillage,nummission,pointdecapture,codepays,date_capture,X,Y), by=c("id"="idpointdecapture")) %>%
  left_join(env_static2) %>%
  left_join(env_nightcatch2) %>%
  #left_join(env_landcover_bf) %>%
  filter(codepays==code_pays)

for(i in 1:nrow(env_spatiotemporal2)){
  df_for_model <- df_for_model %>%
    left_join(env_spatiotemporal2$predictive_df[[i]] %>% dplyr::select(id,!!env_spatiotemporal2$var_df[i]))
  }

df_for_model <- df_for_model %>%
  mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>%
  #left_join(env_spatiotemporal2$predictive_df[[1]] %>% dplyr::select(id,!!env_spatiotemporal2$var_df)) %>%
  mutate(doy = yday(as.Date(date_capture))) %>%
  mutate(day_lt = as.integer(as.Date(date_capture))) %>%
  mutate(int_ext = substr(idpostedecapture,nchar(idpostedecapture),nchar(idpostedecapture))) %>%
  #left_join(mean_coords_csh_points) %>%
  left_join(dists_csh_points, by=c("id"="idpointdecapture"))

df_for_model <- df_for_model %>% mutate(ma_gambiae_ss = case_when(ma_gambiae_ss ==0 ~ "n",
                                                          ma_gambiae_ss>0 ~ "y"))

df_for_model <- mutate_if(df_for_model, is.character, as.factor)
df_for_model_sp <- SpatialPointsDataFrame(df_for_model, coords = as.data.frame(cbind(df_for_model$X,df_for_model$Y)), proj4string = CRS("+init=epsg:4326")) %>% spTransform(CRS("+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>% mutate(X=coordinates(.)[,1], Y=coordinates(.)[,2])

# integrating the matrix of distances between sampling points as covariates to account for spatial autocorrelation (ref: https://peerj.com/articles/5518/)
#df_for_model <- df_for_model %>%
#  dplyr::select(-c(idpostedecapture,id,codevillage,pointdecapture,codepays,date_capture,X,Y,starts_with("D"))) 

# integrating the geo coordinates of the sampling points + the distance to the center and edges of the study area (ref : https://sci-hub.tw/https://doi.org/10.1111%2Fejss.12687)
df_for_model <- df_for_model %>%
  dplyr::select(-c(idpostedecapture,id,codevillage,pointdecapture,codepays,date_capture)) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
  st_transform(32630) %>%
  mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) %>%
  st_drop_geometry()

# df_for_model <- df_for_model %>% mutate(weights = case_when(ma_gambiae_ss <=4 ~ 1,
#                                                                     ma_gambiae_ss>4 ~ 8))

# integrating codevillage and nummission in a MixRF model
#df_for_model <- df_for_model %>%
#  dplyr::select(-c(idpostedecapture,id,codepays,date_capture,starts_with("D"),starts_with("VV")))


# run RFsp model
# rt.rf <- makeRegrTask(data = df_for_model, target = resp_var, check.data=FALSE)
# t.rf <- tuneRanger(rt.rf, num.trees = 150, build.final.model = FALSE, parameters = list(replace = FALSE))
# pars.rf = list(mtry= t.rf$recommended.pars$mtry, min.node.size=t.rf$recommended.pars$min.node.size, sample.fraction=t.rf$recommended.pars$sample.fraction, num.trees=150, seed = 1)
# 
# fm <- as.formula(paste(resp_var," ~."))
# r <- ranger(fm, data = df_for_model %>% dplyr::select(-nummission),  mtry=t.rf$recommended.pars$mtry, min.node.size=t.rf$recommended.pars$min.node.size, sample.fraction=t.rf$recommended.pars$sample.fraction, importance = "permutation")

## cross validation using leave-1-mission-out
missions <- entomo_csh_metadata_l1 %>%
  filter(codepays==code_pays) %>%
  distinct(nummission) %>%
  pull(nummission)

for (i in 1:length(missions)){
  
  th_train <- df_for_model %>% filter(nummission != missions[i]) %>% dplyr::select(-nummission)
  th_test <- df_for_model_sp %>% filter(nummission == missions[i])
  
  #weights <- th_train$weights
  #th_train$weights = NULL
  
  rt.rf <- makeRegrTask(data = th_train, target = resp_var, check.data=FALSE)
  t.rf <- tuneRanger(rt.rf, num.trees = 500, build.final.model = FALSE, parameters = list(replace = FALSE))
  pars.rf = list(mtry= t.rf$recommended.pars$mtry, min.node.size=t.rf$recommended.pars$min.node.size, sample.fraction=t.rf$recommended.pars$sample.fraction, num.trees=500, seed = 1)
  rf <- ranger(as.formula(paste(resp_var," ~.")), data = th_train, mtry=t.rf$recommended.pars$mtry,min.node.size=t.rf$recommended.pars$min.node.size, sample.fraction=t.rf$recommended.pars$sample.fraction, importance = "permutation", num.trees=1000)
  
  th_test$pred <- predict(rf,th_test@data)$predictions
  th_test$resid <- (th_test$ma_gambiae_ss - th_test$pred)/sqrt(th_test$ma_gambiae_ss)
  RMSE = function(m, o){
    sqrt(mean((m - o)^2))
  }
  RMSE(th_test$pred,th_test$ma_gambiae_ss)
  
  correlog_resp <- spline.correlog(x = th_test@data$X,
                              y = th_test@data$Y,
                              z = th_test@data$ma_gambiae_ss)
  plot(correlog_resp)
  
  correlog_resid <- spline.correlog(x = th_test@data$X,
                              y = th_test@data$Y,
                              z = th_test@data$resid)
  plot(correlog_resid)
  
  
  
  
  
  
  # variogram for the response variable
  ov <- over(th_test, r_sppix)
  ov1 <- cbind(data.frame(th_test[resp_var]), ov) %>% dplyr::rename(x = V1,y = V2)
  fm <- as.formula(paste(resp_var," ~1"))
  v <- GSIF::fit.vgmModel(fm, ov1, r_sppix, dimensions="2D", width = 100)
  #plot(variogram(fm, th_test, width = 50), v$vgm)
  plot(x=v$svgm$dist, y=v$svgm$gamma, pch="+", col = "grey18", xlab='distance', cex=1.1, ylab='gamma', ylim = c(0, max(v$svgm$gamma)), main=main)
  vline <- gstat::variogramLine(v$vgm, maxdist=max(v$svgm$dist), n=length(v$svgm$dist))
  lines(x=vline$dist, y=vline$gamma, col="darkgrey", lwd=2)
  
  
  # variogram for the residuals
  ##ov <- over(th_test, r_sppix)
  #ov2 <- cbind(data.frame(th_test["resid"]), ov) %>% rename(x = V1,y = V2)
  #v <- GSIF::fit.vgmModel(resid~1, ov2, r_sppix, dimensions="2D", width = 100)
  #plot(variogram(resid~1, th_test, width = 100), v$vgm)
  vg <- variogram(resid~1, th_test, width = 50)
  points(x=vg$dist,y=vg$gamma, pch="+", col="red")
  bubble(th_test, "resid")
}

























########## static

env_static2 <- env_static %>%
  pivot_wider(names_from = var, values_from = val) %>%
  right_join(trmetrics_entomo_postedecapture) %>%
  left_join(entomo_csh_metadata_l1, by = c("id" = "idpointdecapture")) %>%
  group_split(codepays) %>%
  map(~dplyr::select(.,idpostedecapture,ma_gambiae_ss,WMD,BDE,VCT)) %>%
  map(~mutate_all(.,as.numeric))

res <- env_static2 %>%
  map(.,~cor(.,method = "spearman", use = "na.or.complete"))

library(corrplot)
corrplot(res[[1]],
         method = "pie",
         type = "upper", 
         diag= FALSE,
         tl.col = "dark grey", tl.srt = 45)









bf <- trmetrics_entomo_postedecapture %>% left_join(entomo_csh_metadata_l1) %>% filter(codepays=="BF")
bf_sup0 <- bf %>% filter(ma_gambiae_ss>0)
bf.binned <- mltools::bin_data(bf_sup0$ma_gambiae_ss, bins = 7, binType = "quantile")
ci.binned <- mltools::bin_data(ci_sup0$ma_gambiae_ss, bins = 10, binType = "quantile")

#MHserviceDemo$outcome_bin <- factor(MHserviceDemo$outcome > 0)
MH_gtree <- glmertree(ma_gambiae_ss ~ 1 | (1|codevillage/pointdecapture) | RFD1_F_2000_0_46, data = df_for_model, family = "poisson")
