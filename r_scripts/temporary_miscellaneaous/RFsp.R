require(sf)
require(tidyverse)
require(lubridate)
require(DBI)
react_gpkg<-"data/react_db/react_db.gpkg"  # Empty gpkg template is available here : http://www.geopackage.org/data/empty.gpkg
react_gpkg <- DBI::dbConnect(RSQLite::SQLite(),dbname = react_gpkg)

mean_coords = st_read(react_gpkg, 'entomo_csh_metadata_l1', crs = 4326) %>%
  filter(codepays=="CI") %>%
  group_by(codevillage,pointdecapture) %>%
  summarise(X=mean(X),Y=mean(Y)) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
  st_transform(32630) 

dists  <- as.data.frame(st_distance(mean_coords,mean_coords)) %>%
  mutate_at(vars(starts_with("V")), as.numeric) %>%
  replace(., is.na(.), Inf)

cols <- colnames(dists)
dn <- paste(cols, collapse="+")

mean_coords <- mean_coords %>% 
  st_drop_geometry() %>%
  bind_cols(dists)


entomo_csh_metadata_l1_e <- st_read(react_gpkg, 'entomo_csh_metadata_l1') %>%
  mutate(idpostedecapture=paste0(idpointdecapture,"e"))

entomo_csh_metadata_l1_i <- st_read(react_gpkg, 'entomo_csh_metadata_l1') %>%
  mutate(idpostedecapture=paste0(idpointdecapture,"i"))

entomo_csh_metadata_l1 <- rbind(entomo_csh_metadata_l1_e,entomo_csh_metadata_l1_i) %>%
  st_set_crs(4326) %>%
  st_transform(32630) %>%
  filter(codepays=="CI")


# entomo_csh_metadata_l1 <- SpatialPointsDataFrame(coords = cbind(entomo_csh_metadata_l1$X,entomo_csh_metadata_l1$Y), data = entomo_csh_metadata_l1,
#                                proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


# entomo_csh_metadata_l12 <- entomo_csh_metadata_l1 %>%
#   mutate(doy = yday(as.Date(date_capture))) %>%
#   group_by(codepays) %>%  #group_by(codepays,nummission) 
#   nest() %>%
#   mutate(dists = map(data, ~as.data.frame(st_distance(.,.)))) %>%
#   mutate(data = map(data, ~as.data.frame(.))) %>%
#   mutate(new_df = map2(data,dists, ~cbind(.x,.y))) %>%
#   unnest(new_df) %>%
#   dplyr::select(-c(dists,data)) %>%
#   ungroup() %>%
#   mutate_at(vars(starts_with("V")), as.numeric) %>%
#   dplyr::select(idpostedecapture,codepays,doy,starts_with("V")) %>%
#   replace(., is.na(.), Inf) %>%
#   dplyr::select(-codepays)

trmetrics_entomo_postedecapture <- entomo_csh_metadata_l1 %>% left_join(dbReadTable(react_gpkg, 'trmetrics_entomo_postedecapture'),by="idpostedecapture") %>% left_join(mean_coords)

trmetrics_entomo_postedecapture$doy <- yday(as.Date(trmetrics_entomo_postedecapture$date_capture))

test_ccm_bf_rain <- readRDS("test_ccm_bf_soilmoisture.Rda") %>% rename(idpointdecapture.x =  id) %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

entomo_csh_metadata_l12 <- trmetrics_entomo_postedecapture %>% mutate(postedecapture=substr(idpostedecapture,nchar(idpostedecapture),nchar(idpostedecapture))) %>% left_join(test_ccm_bf_rain, by = "idpointdecapture.x") %>% filter(ma_an>0) %>% dplyr::select(-c(X,Y)) %>% cbind(st_coordinates(.)) %>% st_drop_geometry()

train <- entomo_csh_metadata_l12 %>% filter(nummission!=1)

test <- entomo_csh_metadata_l12 %>% filter(nummission==2)

#test<-train

library(ranger)
library(sp)
library(gstat)
library(raster)
library(mlr)
library(tuneRanger)
table_imp<-data.frame(name=character(),importance=numeric(),pred_error=numeric(),r_squared=numeric())

for(i in 2:ncol(test_ccm_bf_rain)){
  cat(i,"\n")
  i=346
  fm <- as.formula(paste("ma_an ~ doy + ",dn,"+",colnames(test_ccm_bf_rain[i])))
  
  #th_test_ccm_bf_rain <- test_ccm_bf_rain[,c(1,i)]
  #th_rfd <- colnames(th_test_ccm_bf_rain[2])
  
  #th_entomo_csh_metadata_l12 <- entomo_csh_metadata_l12 %>% left_join(th_test_ccm_bf_rain, by = "idpointdecapture") %>% select(-idpointdecapture)
  
  # th_entomo_csh_metadata_l12 <- th_entomo_csh_metadata_l12 %>% mutate(weights = case_when(ma_an <20 ~ 1,
  #                                                           ma_an>=20 & ma_an<=200 ~ 2,
  #                                                           ma_an>200 ~ 3))
  # weights <- th_entomo_csh_metadata_l12$weights
  # th_entomo_csh_metadata_l12$weights=NULL
  #rt.ma_an <- mlr::makeRegrTask(data = train[,c("ma_an",colnames(test_ccm_bf_rain[i]),cols)], target = "ma_an")
  #t.ma_an <- tuneRanger::tuneRanger(rt.ma_an, num.trees = 500, build.final.model = FALSE, parameters = list(replace = FALSE))
  
  r<-ranger(fm, data = train, importance = "permutation")

  table_imp <- rbind(table_imp,data.frame(name=th_rfd,importance=as.numeric(ranger::importance(r)[which(names(ranger::importance(r))==th_rfd)]),pred_error=r$prediction.error,r_squared=r$r.squared))
  
  
  test$ma_an_pred <- predict(r,test)$predictions

   df<-data.frame(actual = test$ma_an, predictions = test$ma_an_pred)
   plot(df,xlim=c(0,300),ylim=c(0,300))
       segments(x0=0,y0=0,x1=500,y1=500,col="red")
   
   test$resid <- test$ma_an-test$ma_an_pred
   
   
   
   #plot(variogram(log(ma_an)~1, test))
   
   test <- SpatialPointsDataFrame(test,coords = as.data.frame(cbind(test$X,test$Y)),proj4string = CRS("+init=epsg:32630"))
   r_sppix <- raster::raster("/home/ptaconet/Bureau/roi_civ_rast.tif") %>% setValues(1) 
   r_sppix <- projectRaster(r_sppix,crs=CRS("+init=epsg:32630")) 
   r_sppix <- as(r_sppix, "SpatialPixelsDataFrame")
   ov <- over(test, r_sppix)
   ov <- cbind(data.frame(test["resid"]), ov) %>% rename(x = V1,y = V2)
   v <- GSIF::fit.vgmModel(resid~1, rmatrix = ov, r_sppix, dimensions="2D", cutoff=50000)
   plot(variogram(resid~1, test, cutoff=50000), v$vgm)
   
   bubble(test, "resid")
   
   ov <- over(test, r_sppix)
   ov <- cbind(data.frame(test["ma_an"]), ov) %>% dplyr::rename(x = V1,y = V2)
   v <- GSIF::fit.vgmModel(ma_an~1, rmatrix = ov, r_sppix, dimensions="2D")  # pour les distributions binomiale neg, on met le log (voir exemple dans help(variogram))
   plot(variogram(ma_an~1, test), v$vgm)
   
   
}


rm.zinc.coord=test %>% dplyr::select(ma_an,SMO1_2000_6_22,starts_with("V"))
rt.zinc.coord <- makeRegrTask(data = rm.zinc.coord, target = "ma_an", check.data=FALSE)
t.zinc <- tuneRanger(rt.zinc.coord, num.trees = 150, build.final.model = FALSE, parameters = list(replace = FALSE))
pars.zinc = list(mtry= t.zinc.coord$recommended.pars$mtry, min.node.size=t.zinc.coord$recommended.pars$min.node.size, sample.fraction=t.zinc.coord$recommended.pars$sample.fraction, num.trees=150, seed = 1)

test <- SpatialPointsDataFrame(test,coords = as.data.frame(cbind(test$X,test$Y)),proj4string = CRS("+init=epsg:32630"))


# varn="ma_an"
# points=test
# covs=r_sppix
# cpus=1
# method="ranger"
# OK=FALSE
# spcT=FALSE
# pars.ranger=pars.zinc
# predDist = ss
# ss <- seq(0,1,0.01)

source('/home/ptaconet/GeoMLA/RF_vs_kriging/R/RFsp_functions.R')

cv.RF = cv_numeric(varn="ma_an", points=test, covs=r_sppix, cpus=5, method="ranger", OK=TRUE, spcT=FALSE, pars.ranger=pars.zinc, predDist = seq(0,1,0.01))


require(GSIF)
require(gstat)
v <- GSIF::fit.vgmModel(ma_an~1, ov, r_sppix, dimensions="2D")
plot(x=v$svgm$dist, y=v$svgm$gamma, pch="+", col = "grey18", xlab='distance', cex=1.1, ylab='gamma', ylim = c(0, max(v$svgm$gamma)), main=main)
vline <- gstat::variogramLine(v$vgm, maxdist=max(v$svgm$dist), n=length(v$svgm$dist))
lines(x=vline$dist, y=vline$gamma, col="darkgrey", lwd=2)
test$r1 <- cv.RF$CV_residuals$Observed-cv.RF$CV_residuals$Predicted
v.r1 <- gstat::variogram(r1~1, test) 
points(x=v.r1$dist, y=v.r1$gamma, pch="+", col = "red", cex=1.4)







i=2
test <- entomo_csh_metadata_l12 %>% filter(nummission==i)
test <- SpatialPointsDataFrame(test,coords = as.data.frame(cbind(test$X,test$Y)),proj4string = CRS("+init=epsg:32630"))
ov <- over(test, r_sppix)
ov <- cbind(data.frame(test["ma_an"]), ov) %>% dplyr::rename(x = V1,y = V2)
v <- GSIF::fit.vgmModel(log(ma_an)~1, rmatrix = ov, r_sppix, dimensions="2D")  # pour les distributions binomiale neg, on met le log (voir exemple dans help(variogram))
plot(variogram(log(ma_an)~1, test), v$vgm)











correlation_df=table_imp
correlation_df$importance<-scale(correlation_df$r_squared)
correlation_df$time_lag_1 <- as.numeric(sub('.*\\_', '', correlation_df$name))
correlation_df$time_lag_2 <- as.numeric(stringr::str_match( correlation_df$name, '([^_]+)(?:_[^_]+){1}$')[,2])  

abs_corr <- correlation_df %>% filter(importance == max(importance, na.rm = T))


if(nrow(correlation_df)>10){
  abs_corr2 <- correlation_df %>% arrange(desc(importance)) %>% top_frac(.03) # 3 % top correlations will be blacked borders
} else {
  abs_corr2 <- abs_corr
}

ccm_plot <- ggplot(data = correlation_df, aes(time_lag_1, time_lag_2, fill = importance)) +
  geom_tile(color = "white", show.legend = TRUE, size = 0.05) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                        limits = c(min(correlation_df$importance),max(correlation_df$importance)), space = "Lab", 
                       name="Variable importance") +
  geom_tile(data = abs_corr2 , color = "black", size = 0.3, show.legend = FALSE) +
  geom_tile(data = abs_corr , color = "deeppink3", size = 0.6, show.legend = FALSE) +
  theme_minimal() + 
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.title = element_text(size = 8)
  ) +
  #ggtitle(paste0("CCM for : area = ",country," ; variable = ",var," ; buffer = ",buffer," m")) +
  #ggtitle(paste0(country," - CCM for buffer " ,buffer," m")) +
  annotate("text", size = 3,x = min(correlation_df$time_lag_1), y = max(correlation_df$time_lag_2), vjust = "inward", hjust = "inward", label = paste0("r(0,0) = ", round(correlation_df$importance[1],3),"\nr(",abs_corr$time_lag_1,",",abs_corr$time_lag_2,") = ",round(abs_corr$importance,3))) +
  coord_fixed()
