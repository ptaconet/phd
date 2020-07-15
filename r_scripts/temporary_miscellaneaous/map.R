library(ggmap)
library(tidyverse)
library(sf)
library(DBI)
library(patchwork)

resp_var <- "ma_gambiae_ss"  # "ma_gambiae_ss"
code_pays <- "BF"            # "BF"

### connect to the database
path_to_db <- "data/react_db/react_db.gpkg" 
react_gpkg <- DBI::dbConnect(RSQLite::SQLite(),dbname = path_to_db) 

### open the tables
## dates and positions of the entomological missions (1 row = 1 point de capture)
entomo_csh_metadata_l1 <- st_read(path_to_db, 'entomo_csh_metadata_l1') %>% mutate(date_capture=as.Date(date_capture))
recensement_villages_l1 <- st_read(path_to_db, 'recensement_villages_l1')
roi <- st_read(path_to_db,"contexte_frontieresreact") %>% 
  filter(codepays==code_pays) %>%
  st_transform(4326)

## table containing the response variables (ie variables to model)
trmetrics_entomo_postedecapture <- st_read(path_to_db, 'trmetrics_entomo_postedecapture') %>% left_join(entomo_csh_metadata_l1 %>% dplyr::select(idpointdecapture, codevillage, codepays, X, Y, nummission, date_capture))

mean_date <- entomo_csh_metadata_l1 %>%
  group_by(nummission) %>%
  summarise(mean_date=mean(date_capture)) %>%
  st_drop_geometry()

pts <- trmetrics_entomo_postedecapture %>%
  filter(codepays==code_pays) %>%
  group_by(codevillage,nummission) %>%
  summarise(date = mean(date_capture), ma_gambiae_ss = sum(ma_gambiae_ss), X = mean(X), Y = mean(Y)) %>%
  left_join(mean_date)
  


myLocation <- as.numeric(st_bbox(roi))
myMap<-get_map(location=myLocation, source="google", maptype="hybrid", crop=FALSE)
p1 = ggmap(myMap) +
  geom_point(aes(x = X, y = Y, size=ma_gambiae_ss), data = pts, alpha = .5, color="darkred") + 
  facet_grid(.~mean_date)


# add rainfall
env_spatiotemporal <- dbReadTable(react_gpkg, 'env_spatiotemporal') %>% dplyr::select(-fid) %>% filter(var %in% c("RFD1_F","SMO1"), buffer==2000) %>% rename(idpointdecapture = id)

env_spatiotemporal <- env_spatiotemporal %>%
  pivot_wider(names_from = var, values_from = val) %>%
  left_join(entomo_csh_metadata_l1) %>%
  filter(codepays==code_pays) %>%
  mutate(date=as.Date(date)) %>%
  group_by(date) %>%
  summarise(SMO1=mean(SMO1,na.rm = T), RFD1_F=mean(RFD1_F,na.rm = T))


p2 = ggplot(env_spatiotemporal,aes(date,RFD1_F)) + geom_line()

p1/p2
