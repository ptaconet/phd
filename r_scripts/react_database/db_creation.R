library(RSQLite)
library(dplyr)
library(sf)
library(rgdal)
library(gdalUtils)
library(raster)
library(gdalUtils)
library(readxl)
library(lubridate)
library(stringr)
library(purrr)
library(tidyverse)
library(googlesheets4)

path_to_amal_database<-"data/react_db/miscellaneous_data/React_dbase_V7.db"
path_to_gpkg_database<-"data/react_db/react_db.gpkg"  # Empty gpkg template is available here : http://www.geopackage.org/data/empty.gpkg
path_to_gpkg_light_database<-"data/react_db/react_db_light.gpkg"

path_to_metadata_table<-"https://docs.google.com/spreadsheets/d/1O7JUhoAzXtoFWjiLOz7HgDLe--zxkZKR96_BhzU7DME/edit?usp=sharing"

download.file("http://www.geopackage.org/data/empty.gpkg",path_to_gpkg_database)
file.copy(path_to_gpkg_database,path_to_gpkg_light_database,overwrite =T)

## Connect to DBs
amal_db <- dbConnect(RSQLite::SQLite(),path_to_amal_database)
react_gpkg <- dbConnect(RSQLite::SQLite(),path_to_gpkg_database)
react_gpkg_light <- dbConnect(RSQLite::SQLite(),path_to_gpkg_light_database)

## Open and load tables metadata and metadata_mapping
sheets_deauth()
metadata_tables<-googlesheets4::read_sheet(path_to_metadata_table,sheet = "metadata_tables",col_types="c")
metadata_contacts<-googlesheets4::read_sheet(path_to_metadata_table,sheet = "contacts",col_types="c")
metadata_mapping<-googlesheets4::read_sheet(path_to_metadata_table,sheet = "metadata_mapping",col_types="c")
metadata_data_dictionary_table<-googlesheets4::read_sheet(path_to_metadata_table,sheet = "data_dictionary_columns",col_types="c")

dbWriteTable(react_gpkg,"metadata_tables",metadata_tables,overwrite=TRUE)
dbWriteTable(react_gpkg,"metadata_contacts",metadata_contacts,overwrite=TRUE)
dbWriteTable(react_gpkg,"metadata_mapping",metadata_mapping,overwrite=TRUE)
dbWriteTable(react_gpkg,"metadata_data_dictionary_table",metadata_data_dictionary_table,overwrite=TRUE)
dbWriteTable(react_gpkg_light,"metadata_tables",metadata_tables %>% filter(identifier %in% c(
"recensement_individus_l0",
"recensement_menages_l0",
"recensement_villages_l1",
"entomo_comportementhumain_l0",
"entomo_csh_ctrlequalite_l0",
"entomo_csh_metadata_l0",
"entomo_csh_metadata_l1",
"entomo_idmoustiques_l0",
"entomo_hobo_l0",
"entomo_hygro_l0",
"entomo_baro_l0",
"entomo_gites_larvaires_ci_l0",
"entomo_cfr_bf_l0",
"entomo_ctp_bf_l0",
"epidemio_active_bf_l0",
"epidemio_active_ci_l0",
"epidemio_active_l1",
"epidemio_passive_l0",
"epidemio_goutteepaisse_l1",
"interv_ivm_bf_recenses_l0",
"interv_ivm_bf_traites_l0",
"interv_irs_bf_ctrlequalite_chimique_l0",
"interv_irs_ci_ctrlequalite_chimique_l0",
"interv_larvicide_ci_ctrlequalite_l0",
"interv_irs_ctrlequalite_cone_l0",
"interv_iec_bf_ctrlequalite_l0",
"interv_iec_ci_ctrlequalite_l0",
"contexte_paysafriqueouest",
"contexte_villesafriqueouest",
"contexte_frontieresreact",
"metadata_tables",
"metadata_contacts",
"metadata_mapping",
"metadata_data_dictionary_table")),overwrite=TRUE)
dbWriteTable(react_gpkg_light,"metadata_contacts",metadata_contacts,overwrite=TRUE)
dbWriteTable(react_gpkg_light,"metadata_mapping",metadata_mapping,overwrite=TRUE)
dbWriteTable(react_gpkg_light,"metadata_data_dictionary_table",metadata_data_dictionary_table,overwrite=TRUE)

## Create and load tables :

# initiate data dictionary
data_dictionnary <- data.frame(name=character(),table=character(),label=character(),type=character(),description=character(),additional_information=character())

data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(metadata_tables),table="metadata_tables"))
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(metadata_contacts),table="metadata_contacts"))
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(metadata_mapping),table="metadata_mapping"))
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(metadata_data_dictionary_table),table="metadata_data_dictionary_table"))

# recensement_individus_l0
source("r_scripts/react_database/recensement_individus_l0.R") # source("https://raw.githubusercontent.com/ptaconet/malamodpkg/master/database/recensement_individus_l0.R")
individus <- cbind(fid = 1:nrow(individus), individus)
dbWriteTable(react_gpkg,"recensement_individus_l0",individus,overwrite=TRUE)
dbWriteTable(react_gpkg_light,"recensement_individus_l0",individus,overwrite=TRUE)
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(individus),table="recensement_individus_l0"))

# recensement_menages_l0
#source("r_scripts/react_database/raw_menages.R")
source("r_scripts/react_database/recensement_menages_l0.R")
menages_sf<-st_as_sf(menages,coords =  c("coordgpsLongitude", "coordgpsLatitude"), crs = 4326 )
menages_sf<-cbind(menages_sf,st_coordinates(menages_sf))
st_write(menages_sf, path_to_gpkg_database, "recensement_menages_l0", delete_layer = TRUE)
st_write(menages_sf, path_to_gpkg_light_database, "recensement_menages_l0", delete_layer = TRUE)
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(menages_sf),table="recensement_menages_l0"))

# recensement_villages_l1
source("r_scripts/react_database/recensement_villages_l1.R")
villages_sf<-st_as_sf(villages,coords =  c("X", "Y"), crs = 4326 )
villages_sf<-cbind(villages_sf,st_coordinates(villages_sf))
st_write(villages_sf, path_to_gpkg_database, "recensement_villages_l1", delete_layer = TRUE)
st_write(villages_sf, path_to_gpkg_light_database, "recensement_villages_l1", delete_layer = TRUE)
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(villages_sf),table="recensement_villages_l1"))

# entomo_comportementhumain_l0
source("r_scripts/react_database/entomo_comportementhumain_l0.R")
df_humanbehavior_sf<-st_as_sf(df_humanbehavior,coords =  c("longitude", "latitude"), crs = 4326 )
df_humanbehavior_sf <- cbind(fid = 1:nrow(df_humanbehavior_sf), df_humanbehavior_sf)
st_write(df_humanbehavior_sf, path_to_gpkg_database, "entomo_comportementhumain_l0", delete_layer = TRUE)
st_write(df_humanbehavior_sf, path_to_gpkg_light_database, "entomo_comportementhumain_l0", delete_layer = TRUE)
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(df_humanbehavior_sf),table="entomo_comportementhumain_l0"))

# entomo_csh_ctrlequalite_l0
source("r_scripts/react_database/entomo_csh_ctrlequalite_l0.R") # créé le data.frame nommé "all_data" contenant les données brutes de supervision capture
supervcapture_sf<-st_as_sf(all_data,coords =  c("longitude", "latitude"), crs = 4326 )
supervcapture_sf<-cbind(supervcapture_sf,st_coordinates(supervcapture_sf))
st_write(supervcapture_sf, path_to_gpkg_database, "entomo_csh_ctrlequalite_l0", delete_layer = TRUE)
st_write(supervcapture_sf, path_to_gpkg_light_database, "entomo_csh_ctrlequalite_l0", delete_layer = TRUE)
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(supervcapture_sf),table="entomo_csh_ctrlequalite_l0"))

# entomo_csh_metadata_l0
source("r_scripts/react_database/entomo_csh_metadata_l0.R")
raw_dates_hlc <- cbind(fid = 1:nrow(raw_dates_hlc), raw_dates_hlc)
dbWriteTable(react_gpkg,"entomo_csh_metadata_l0",raw_dates_hlc,overwrite=TRUE)
dbWriteTable(react_gpkg_light,"entomo_csh_metadata_l0",raw_dates_hlc,overwrite=TRUE)
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(raw_dates_hlc),table="entomo_csh_metadata_l0"))

# entomo_csh_metadata_l1
source("r_scripts/react_database/entomo_csh_metadata_l1.R")
hlc_dates_loc_times_sf<-st_as_sf(hlc_dates_loc_times,coords =  c("longitude", "latitude"), crs = 4326 )
hlc_dates_loc_times_sf<-cbind(hlc_dates_loc_times_sf,st_coordinates(hlc_dates_loc_times_sf))
st_write(hlc_dates_loc_times_sf, path_to_gpkg_database, "entomo_csh_metadata_l1", delete_layer = TRUE)  # use delete_layer=TRUE to delete first
st_write(hlc_dates_loc_times_sf, path_to_gpkg_light_database, "entomo_csh_metadata_l1", delete_layer = TRUE)  # use delete_layer=TRUE to delete first
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(hlc_dates_loc_times_sf),table="entomo_csh_metadata_l1"))

# entomo_idmoustiques_l0
source("r_scripts/react_database/entomo_idmoustiques_l0.R")
entomo <- cbind(fid = 1:nrow(df_mosquitoes), df_mosquitoes)
dbWriteTable(react_gpkg,"entomo_idmoustiques_l0",entomo,overwrite=TRUE)
dbWriteTable(react_gpkg_light,"entomo_idmoustiques_l0",entomo,overwrite=TRUE)
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(entomo),table="entomo_idmoustiques_l0"))

# Entomo - micro-climate from sensors
source("r_scripts/react_database/entomo_env_sensors.R")
df_hobo <- cbind(fid = 1:nrow(df_hobo), df_hobo)
df_hygro <- cbind(fid = 1:nrow(df_hygro), df_hygro)
df_baro <- cbind(fid = 1:nrow(df_baro), df_baro)
dbWriteTable(react_gpkg,"entomo_hobo_l0",df_hobo,overwrite=TRUE)
dbWriteTable(react_gpkg,"entomo_hygro_l0",df_hygro,overwrite=TRUE)
dbWriteTable(react_gpkg,"entomo_baro_l0",df_baro,overwrite=TRUE)
dbWriteTable(react_gpkg_light,"entomo_hobo_l0",df_hobo,overwrite=TRUE)
dbWriteTable(react_gpkg_light,"entomo_hygro_l0",df_hygro,overwrite=TRUE)
dbWriteTable(react_gpkg_light,"entomo_baro_l0",df_baro,overwrite=TRUE)
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(df_hobo),table="entomo_hobo_l0"))
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(df_hygro),table="entomo_hygro_l0"))
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(df_baro),table="entomo_baro_l0"))

# Entomo - gites larvaires
source("r_scripts/react_database/entomo_gites_larvaires_ci_l0.R")
entomo_gites_larvaires_ci_l0_sf<-st_as_sf(entomo_gites_larvaires_ci_l0,coords =  c("part1coordgpsgitepteaulongitude", "part1coordgpsgitepteaulatitude"), crs = 4326 )
entomo_gites_larvaires_ci_l0_sf<-cbind(entomo_gites_larvaires_ci_l0_sf,st_coordinates(entomo_gites_larvaires_ci_l0_sf))
st_write(entomo_gites_larvaires_ci_l0_sf, path_to_gpkg_database, "entomo_gites_larvaires_ci_l0", delete_layer = TRUE)
st_write(entomo_gites_larvaires_ci_l0_sf, path_to_gpkg_light_database, "entomo_gites_larvaires_ci_l0", delete_layer = TRUE)
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(entomo_gites_larvaires_ci_l0_sf),table="entomo_gites_larvaires_ci_l0"))

# Entomo - CFR residuelle
entomo_cfr_bf_l0 <- read_excel("data/react_db/miscellaneous_data/CFR_data_final_pooda.xlsx")
entomo_cfr_bf_l0 <- cbind(fid = 1:nrow(entomo_cfr_bf_l0), entomo_cfr_bf_l0)
dbWriteTable(react_gpkg,"entomo_cfr_bf_l0",entomo_cfr_bf_l0,overwrite=TRUE)
dbWriteTable(react_gpkg,"entomo_cfr_bf_l0",entomo_cfr_bf_l0,overwrite=TRUE)
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(entomo_cfr_bf_l0),table="entomo_cfr_bf_l0"))

# epidemio_xxx (toutes les tables épidémio)
source("r_scripts/react_database/epidemio.R")
act<-st_as_sf(act,coords =  c("X", "Y"), crs = 4326 )
act<-cbind(act,st_coordinates(act))
BF_act <- cbind(fid = 1:nrow(BF_act), BF_act)
CI_act <- cbind(fid = 1:nrow(CI_act), CI_act)
pas <- cbind(fid = 1:nrow(pas), pas)
GE_stats <- cbind(fid = 1:nrow(GE_stats), GE_stats)
st_write(act, path_to_gpkg_database, "epidemio_active_l1", delete_layer = TRUE)
dbWriteTable(react_gpkg,"epidemio_active_bf_l0",BF_act,overwrite=TRUE)
dbWriteTable(react_gpkg,"epidemio_active_ci_l0",CI_act,overwrite=TRUE)
dbWriteTable(react_gpkg,"epidemio_passive_l0",pas,overwrite=TRUE)
dbWriteTable(react_gpkg,"epidemio_goutteepaisse_l1",GE_stats,overwrite=TRUE)
st_write(act, path_to_gpkg_light_database, "epidemio_active_l1", delete_layer = TRUE)
dbWriteTable(react_gpkg_light,"epidemio_active_bf_l0",BF_act,overwrite=TRUE)
dbWriteTable(react_gpkg_light,"epidemio_active_ci_l0",CI_act,overwrite=TRUE)
dbWriteTable(react_gpkg_light,"epidemio_passive_l0",pas,overwrite=TRUE)
dbWriteTable(react_gpkg_light,"epidemio_goutteepaisse_l1",GE_stats,overwrite=TRUE)
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(act),table="epidemio_active_l1"))
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(BF_act),table="epidemio_active_bf_l0"))
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(CI_act),table="epidemio_active_ci_l0"))
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(pas),table="epidemio_passive_l0"))
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(GE_stats),table="epidemio_goutteepaisse_l1"))

# interv_ivm_l0
source("r_scripts/react_database/interv_ivm_l0.R")
interv_ivm_traites_l0 <- cbind(fid = 1:nrow(interv_ivm_traites_l0), interv_ivm_traites_l0)
interv_ivm_recenses_l0 <- cbind(fid = 1:nrow(interv_ivm_recenses_l0), interv_ivm_recenses_l0)
dbWriteTable(react_gpkg,"interv_ivm_bf_traites_l0",interv_ivm_traites_l0,overwrite=TRUE)
dbWriteTable(react_gpkg,"interv_ivm_bf_recenses_l0",interv_ivm_recenses_l0,overwrite=TRUE)
dbWriteTable(react_gpkg_light,"interv_ivm_bf_traites_l0",interv_ivm_traites_l0,overwrite=TRUE)
dbWriteTable(react_gpkg_light,"interv_ivm_bf_recenses_l0",interv_ivm_recenses_l0,overwrite=TRUE)
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(interv_ivm_traites_l0),table="interv_ivm_bf_traites_l0"))
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(interv_ivm_recenses_l0),table="interv_ivm_bf_recenses_l0"))

#interv_larvicide_ci_ctrlequalite_l0
source("r_scripts/react_database/interv_larvicide_ci_ctrlequalite_l0.R")
larvicide_ci_ctrlequalite <- cbind(fid = 1:nrow(larvicide_ci_ctrlequalite), larvicide_ci_ctrlequalite)
dbWriteTable(react_gpkg,"interv_larvicide_ci_ctrlequalite_l0",larvicide_ci_ctrlequalite,overwrite=TRUE)
dbWriteTable(react_gpkg_light,"interv_larvicide_ci_ctrlequalite_l0",larvicide_ci_ctrlequalite,overwrite=TRUE)
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(larvicide_ci_ctrlequalite),table="interv_larvicide_ci_ctrlequalite_l0"))

#interv_irs_ctrlequalite_cone_l0
source("r_scripts/react_database/interv_irs_ctrlequalite_cone_l0.R")
interv_irs_ctrlequalite_cone_l0 <- cbind(fid = 1:nrow(interv_irs_ctrlequalite_cone_l0), interv_irs_ctrlequalite_cone_l0)
dbWriteTable(react_gpkg,"interv_irs_ctrlequalite_cone_l0",interv_irs_ctrlequalite_cone_l0,overwrite=TRUE)
dbWriteTable(react_gpkg_light,"interv_irs_ctrlequalite_cone_l0",interv_irs_ctrlequalite_cone_l0,overwrite=TRUE)
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(interv_irs_ctrlequalite_cone_l0),table="interv_irs_ctrlequalite_cone_l0"))

# trmetrics_entomo, trmetrics_epidemio, trmetrics_humbevr
source("r_scripts/react_database/metrics.R")
trmetrics_entomo <- cbind(fid = 1:nrow(trmetrics_entomo), trmetrics_entomo)
trmetrics_epidemio <- cbind(fid = 1:nrow(trmetrics_epidemio), trmetrics_epidemio)

trmetrics_entomo_pointdecapture <- cbind(fid = 1:nrow(trmetrics_entomo_pointdecapture), trmetrics_entomo_pointdecapture)
dbWriteTable(react_gpkg,"trmetrics_entomo_pointdecapture",trmetrics_entomo_pointdecapture,overwrite=TRUE)

trmetrics_entomo_postedecapture <- cbind(fid = 1:nrow(trmetrics_entomo_postedecapture), trmetrics_entomo_postedecapture)
dbWriteTable(react_gpkg,"trmetrics_entomo_postedecapture",trmetrics_entomo_postedecapture,overwrite=TRUE)


dbWriteTable(react_gpkg,"trmetrics_entomo",trmetrics_entomo,overwrite=TRUE)
dbWriteTable(react_gpkg_light,"trmetrics_entomo",trmetrics_entomo,overwrite=TRUE)
dbWriteTable(react_gpkg,"trmetrics_epidemio",trmetrics_epidemio,overwrite=TRUE)
dbWriteTable(react_gpkg_light,"trmetrics_epidemio",trmetrics_epidemio,overwrite=TRUE)

data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(trmetrics_entomo),table="trmetrics_entomo"))
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(trmetrics_epidemio),table="trmetrics_epidemio"))

# Data dictionnary of transmission metrics covariates :
trmetrics_dictionary<-googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1dIeSOa2WinXvOQGLmIjA0gFdsHnb6zMMsDME-G5pyMc/edit?usp=sharing",sheet = "var_reponse",col_types="c")
trmetrics_dictionary <- cbind(fid = 1:nrow(trmetrics_dictionary), trmetrics_dictionary)
dbWriteTable(react_gpkg,"trmetrics_dictionary",trmetrics_dictionary,overwrite=TRUE)
dbWriteTable(react_gpkg_light,"trmetrics_dictionary",trmetrics_dictionary,overwrite=TRUE)
data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(trmetrics_dictionary),table="trmetrics_dictionary"))



# contexte_paysafriqueouest  downloaded here : https://data.humdata.org/dataset/west-and-central-africa-administrative-boundaries-levels
adm_bound_sf<-read_sf("data/react_db/miscellaneous_data/wca_adm0/wca_adm0.shp")
adm_bound_sf<-st_transform(adm_bound_sf,crs=4326)
st_write(adm_bound_sf, path_to_gpkg_database, "contexte_paysafriqueouest", update = TRUE)
st_write(adm_bound_sf, path_to_gpkg_light_database, "contexte_paysafriqueouest", update = TRUE)

# contexte_villesafriqueouest
ancillary_africa_cities<-read_sf("data/react_db/miscellaneous_data/africa_places/places.shp")
ancillary_africa_cities<-st_intersection(ancillary_africa_cities,adm_bound_sf)
st_write(ancillary_africa_cities, path_to_gpkg_database, "contexte_villesafriqueouest", update = TRUE)
st_write(ancillary_africa_cities, path_to_gpkg_light_database, "contexte_villesafriqueouest", update = TRUE)

# contexte_frontieresreact
roi_civ_sf<-read_sf("data/korhogo/ROI.kml")
roi_civ_sf$codepays<-"CI"
roi_civ_sf<-roi_civ_sf[,"codepays"]
roi_civ_sf<-st_cast(roi_civ_sf,"POLYGON")
roi_bf_sf<-read_sf("data/diebougou/ROI.kml")
roi_bf_sf$codepays<-"BF"
roi_bf_sf<-roi_bf_sf[,"codepays"]
roi_bf_sf<-st_zm(roi_bf_sf,drop = TRUE, what = "ZM")
roi<-rbind(roi_civ_sf,roi_bf_sf)
roi<-st_transform(roi,crs=32630)
st_write(roi, path_to_gpkg_database, "contexte_frontieresreact", update = TRUE)
st_write(roi, path_to_gpkg_light_database, "contexte_frontieresreact", update = TRUE)

# LU/LC training and validation parcels (raw and segmented)
ground_truth_korhogo_raw<-st_read("data/korhogo/Ground_truth/civ_groundtruth_vector_32630.gpkg")
ground_truth_korhogo_raw <- cbind(pk = 1:nrow(ground_truth_korhogo_raw), ground_truth_korhogo_raw)
st_write(ground_truth_korhogo_raw, path_to_gpkg_database, "lco_groundtruth_ci_l0", update = TRUE)
ground_truth_korhogo_revised<-st_read("data/korhogo/Ground_truth/civ_groundtruth_objects_segmentation_v_classes_update.gpkg")
ground_truth_korhogo_revised <- cbind(pk = 1:nrow(ground_truth_korhogo_revised), ground_truth_korhogo_revised)
st_write(ground_truth_korhogo_revised, path_to_gpkg_database, "lco_groundtruth_ci_l1", update = TRUE)

ground_truth_diebougou_raw<-st_read("data/diebougou/Ground_truth/bf_groundtruth_vector_32630.gpkg")
st_write(ground_truth_diebougou_raw, path_to_gpkg_database, "lco_groundtruth_bf_l0", update = TRUE)
ground_truth_diebougou_revised<-st_read("data/diebougou/Ground_truth/groundtruth_bf_v_classes_update.gpkg")
ground_truth_diebougou_revised <- cbind(pk = 1:nrow(ground_truth_diebougou_revised), ground_truth_diebougou_revised)
st_write(ground_truth_diebougou_revised, path_to_gpkg_database, "lco_groundtruth_ci_l1", update = TRUE)

# LU/LC training/validation parcels with zonal statistics
lulc_zonal_stats_civ<-st_read("data/korhogo/Ground_truth/ground_truth_stats.gpkg")
lulc_zonal_stats_civ <- cbind(pk = 1:nrow(lulc_zonal_stats_civ), lulc_zonal_stats_civ)
st_write(lulc_zonal_stats_civ, path_to_gpkg_database, "lco_groundtruth_ci_zonalstats", update = TRUE)

lulc_zonal_stats_bf<-st_read("data/diebougou/Ground_truth/ground_truth_stats.gpkg")
lulc_zonal_stats_bf <- cbind(pk = 1:nrow(lulc_zonal_stats_bf), lulc_zonal_stats_bf)
st_write(lulc_zonal_stats_bf, path_to_gpkg_database, "lco_groundtruth_bf_zonalstats", update = TRUE)


  ## LU/LC maps
  # BF
  cat("loading BF LU/LC rasters...\n")
  path_to_LU_L1_bf<-"data/diebougou/landcover_vhrs/classification/classification_L1.tif"
  path_to_LU_L2_bf<-"data/diebougou/landcover_vhrs/classification/classification_L2.tif"
  path_to_LU_L3_bf<-"data/diebougou/landcover_vhrs/classification/classification_L3.tif"
  path_to_LU_L4_bf<-"data/diebougou/landcover_vhrs/classification/classification_L4.tif"
  path_to_LU_L5_bf<-"data/diebougou/landcover_vhrs/classification/classification_L5.tif"
  gdal_translate(path_to_LU_L1_bf,path_to_gpkg_database,ot="Int16",of="GPKG",b=1,co=c("APPEND_SUBDATASET=YES","RASTER_TABLE=lco_l1_bf"))
  gdal_translate(path_to_LU_L2_bf,path_to_gpkg_database,ot="Int16",of="GPKG",b=1,co=c("APPEND_SUBDATASET=YES","RASTER_TABLE=lco_l2_bf"))
  gdal_translate(path_to_LU_L3_bf,path_to_gpkg_database,ot="Int16",of="GPKG",b=1,co=c("APPEND_SUBDATASET=YES","RASTER_TABLE=lco_l3_bf"))
  gdal_translate(path_to_LU_L4_bf,path_to_gpkg_database,ot="Int16",of="GPKG",b=1,co=c("APPEND_SUBDATASET=YES","RASTER_TABLE=lco_l4_bf"))
  gdal_translate(path_to_LU_L5_bf,path_to_gpkg_database,ot="Int16",of="GPKG",b=1,co=c("APPEND_SUBDATASET=YES","RASTER_TABLE=lco_l5_bf"))

  path_to_LU_L1_classes<-"data/diebougou/landcover_vhrs/classification/classification_L1.csv"
  path_to_LU_L2_classes<-"data/diebougou/landcover_vhrs/classification/classification_L2.csv"
  path_to_LU_L3_classes<-"data/diebougou/landcover_vhrs/classification/classification_L3.csv"
  path_to_LU_L4_classes<-"data/diebougou/landcover_vhrs/classification/classification_L4.csv"
  path_to_LU_L5_classes<-"data/diebougou/landcover_vhrs/classification/classification_L5.csv"

  LU_L1_classes<-read.csv(path_to_LU_L1_classes)
  LU_L2_classes<-read.csv(path_to_LU_L2_classes)
  LU_L3_classes<-read.csv(path_to_LU_L3_classes)
  LU_L4_classes<-read.csv(path_to_LU_L4_classes)
  LU_L5_classes<-read.csv(path_to_LU_L5_classes)

  LU_L1_classes$classif_level<-1
  LU_L2_classes$classif_level<-2
  LU_L3_classes$classif_level<-3
  LU_L4_classes$classif_level<-4
  LU_L5_classes$classif_level<-5

  LU_classes_bf<-rbind(LU_L1_classes,LU_L2_classes,LU_L3_classes,LU_L4_classes,LU_L5_classes) %>%
    arrange(classif_level,pixval) %>%
    mutate(classif_name=case_when(classif_level==1 ~ "lco_l1_bf",
                                  classif_level==2 ~ "lco_l2_bf",
                                  classif_level==3 ~ "lco_l3_bf",
                                  classif_level==4 ~ "lco_l4_bf",
                                  classif_level==5 ~ "lco_l5_bf")) %>%
    mutate(path_to_raster=case_when(classif_level==1 ~ path_to_LU_L1_bf,
                                    classif_level==2 ~ path_to_LU_L2_bf,
                                    classif_level==3 ~ path_to_LU_L3_bf,
                                    classif_level==4 ~ path_to_LU_L4_bf,
                                    classif_level==5 ~ path_to_LU_L5_bf
                                    ))


  # CIV
  cat("loading CIV LU/LC rasters...\n")
  path_to_LU_L1_civ<-"data/korhogo/landcover_vhrs/classification/classification_L1.tif"
  path_to_LU_L2_civ<-"data/korhogo/landcover_vhrs/classification/classification_L2.tif"
  path_to_LU_L3_civ<-"data/korhogo/landcover_vhrs/classification/classification_L3.tif"
  path_to_LU_L4_civ<-"data/korhogo/landcover_vhrs/classification/classification_L4.tif"
  path_to_LU_L5_civ<-"data/korhogo/landcover_vhrs/classification/classification_L5.tif"
  gdal_translate(path_to_LU_L1_civ,path_to_gpkg_database,ot="Int16",of="GPKG",b=1,co=c("APPEND_SUBDATASET=YES","RASTER_TABLE=lco_l1_ci"))
  gdal_translate(path_to_LU_L2_civ,path_to_gpkg_database,ot="Int16",of="GPKG",b=1,co=c("APPEND_SUBDATASET=YES","RASTER_TABLE=lco_l2_ci"))
  gdal_translate(path_to_LU_L3_civ,path_to_gpkg_database,ot="Int16",of="GPKG",b=1,co=c("APPEND_SUBDATASET=YES","RASTER_TABLE=lco_l3_ci"))
  gdal_translate(path_to_LU_L4_civ,path_to_gpkg_database,ot="Int16",of="GPKG",b=1,co=c("APPEND_SUBDATASET=YES","RASTER_TABLE=lco_l4_ci"))
  gdal_translate(path_to_LU_L5_civ,path_to_gpkg_database,ot="Int16",of="GPKG",b=1,co=c("APPEND_SUBDATASET=YES","RASTER_TABLE=lco_l5_ci"))

  path_to_LU_L1_classes<-"data/korhogo/landcover_vhrs/classification/classification_L1.csv"
  path_to_LU_L2_classes<-"data/korhogo/landcover_vhrs/classification/classification_L2.csv"
  path_to_LU_L3_classes<-"data/korhogo/landcover_vhrs/classification/classification_L3.csv"
  path_to_LU_L4_classes<-"data/korhogo/landcover_vhrs/classification/classification_L4.csv"
  path_to_LU_L5_classes<-"data/korhogo/landcover_vhrs/classification/classification_L5.csv"

  LU_L1_classes<-read.csv(path_to_LU_L1_classes)
  LU_L2_classes<-read.csv(path_to_LU_L2_classes)
  LU_L3_classes<-read.csv(path_to_LU_L3_classes)
  LU_L4_classes<-read.csv(path_to_LU_L4_classes)
  LU_L5_classes<-read.csv(path_to_LU_L5_classes)

  LU_L1_classes$classif_level<-1
  LU_L2_classes$classif_level<-2
  LU_L3_classes$classif_level<-3
  LU_L4_classes$classif_level<-4
  LU_L5_classes$classif_level<-5

  LU_classes_civ<-rbind(LU_L1_classes,LU_L2_classes,LU_L3_classes,LU_L4_classes,LU_L5_classes) %>%
    arrange(classif_level,pixval) %>%
    mutate(classif_name=case_when(classif_level==1 ~ "lco_l1_ci",
                                  classif_level==2 ~ "lco_l2_ci",
                                  classif_level==3 ~ "lco_l3_ci",
                                  classif_level==4 ~ "lco_l4_ci",
                                  classif_level==5 ~ "lco_l5_ci")) %>%
    mutate(path_to_raster=case_when(classif_level==1 ~ path_to_LU_L1_civ,
                                    classif_level==2 ~ path_to_LU_L2_civ,
                                    classif_level==3 ~ path_to_LU_L3_civ,
                                    classif_level==4 ~ path_to_LU_L4_civ,
                                    classif_level==5 ~ path_to_LU_L5_civ
    ))


  ## ESA AFRICA LC
  africa_lc_pixval<-read.csv("data/landcovers/ESACCI-LC/ESACCI-LC_S2_Prototype_ColorLegend.csv",sep=";") %>%
    dplyr::select(NB_LAB,LCCOwnLabel) %>%
    setNames(c("pixval","lc_class")) %>%
    mutate(classif_name="ESACCI-LC-L4-LC10-Map-20m-P1Y-2016-v1.0") %>%
    mutate(classif_level=NA) %>%
    mutate(path_to_raster="data/landcovers/ESACCI-LC/ESACCI-LC-L4-LC10-Map-20m-P1Y-2016-v1.0.tif")

  ## ESA Globcover
  globcover<-gdalUtils::gdalinfo("data/landcovers/landcover_globcover_esa/W020N20_ProbaV_LC100_epoch2015_global_v2.0.1_discrete-classification_EPSG-4326.tif")[57:58] %>%
    map(~strsplit(.,","))
  globcover_lc_class<-globcover[[1]][[1]][2:length(globcover[[1]][[1]])] %>% gsub(" ","",.)
  globcover_pixval<-globcover[[2]][[1]][2:length(globcover[[2]][[1]])]  %>% gsub(" ","",.) %>% as.integer()
  globcover_lc_pixval<-data.frame(pixval=globcover_pixval,lc_class=globcover_lc_class,stringsAsFactors = F) %>%
    mutate(classif_name="W020N20_ProbaV_LC100_epoch2015_global_v2.0.1") %>%
    mutate(classif_level=NA) %>%
    mutate(path_to_raster="data/landcovers/landcover_globcover_esa/W020N20_ProbaV_LC100_epoch2015_global_v2.0.1_discrete-classification_EPSG-4326.tif")


  LU_classes<-rbind(LU_classes_bf,LU_classes_civ,africa_lc_pixval,globcover_lc_pixval) %>%
    setNames(c("pixval","pixlabel","classif_level","classif_label","classif_path"))

   layer_id<-unique(LU_classes$classif_label) %>%
     as.data.frame(stringsAsFactors=F) %>%
     setNames("classif_label") %>%
     mutate(layer_id=as.integer(seq(1,nrow(.),1)))
   
   LU_classes<-left_join(LU_classes,layer_id)

   lco_pix_priority <- read.csv("data/react_db/miscellaneous_data/landcover/lco_pix_priority.csv",stringsAsFactors = F)
   LU_classes <- LU_classes %>%
     left_join(lco_pix_priority)
   
  dbWriteTable(react_gpkg,"lco_metadata",LU_classes,overwrite=T)
  data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(LU_classes),table="lco_metadata"))

    ## Built up surfaces
  #source("r_scripts/react_database/builtup.R)
  path_to_builtup_civ<-"data/korhogo/landcover_vhrs/classification/bati_raster.tif"
  path_to_builtup_bf<-"data/diebougou/Classification/bati_raster.tif"
  gdal_translate(path_to_builtup_civ,path_to_gpkg_database,ot="Int16",of="GPKG",b=1,co=c("APPEND_SUBDATASET=YES","RASTER_TABLE=lco_builtup_ci")) # Tip : Setting Int16 as 'ot' value enables to store source NA as NA in output (and not 0)
  gdal_translate(path_to_builtup_bf,path_to_gpkg_database,ot="Int16",of="GPKG",b=1,co=c("APPEND_SUBDATASET=YES","RASTER_TABLE=lco_builtup_bf"))


  ## Pedology (raster)
  path_to_pedology_civ<-"data/korhogo/pedology/pedology.tif"
  path_to_pedology_bf<-"data/diebougou/pedology/pedology.tif"
  gdal_translate(path_to_pedology_civ,path_to_gpkg_database,ot="Int16",of="GPKG",b=1,co=c("APPEND_SUBDATASET=YES","RASTER_TABLE=lco_pedology_ci"))
  gdal_translate(path_to_pedology_bf,path_to_gpkg_database,ot="Int16",of="GPKG",b=1,co=c("APPEND_SUBDATASET=YES","RASTER_TABLE=lco_pedology_bf"))

  ## Environmental covariates


  # Data dictionnary of environmental covariates :
  #environmental_covariates_dictionary <- read.csv(system.file("extdata/environmental_covariates_dictionary.csv", package = "malamodpkg"),stringsAsFactors = F,fileEncoding = "latin1")
  environmental_covariates_dictionary<-googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1dIeSOa2WinXvOQGLmIjA0gFdsHnb6zMMsDME-G5pyMc/edit?usp=sharing",sheet = "var_prediction",col_types="c")
  environmental_covariates_dictionary <- cbind(fid = 1:nrow(environmental_covariates_dictionary), environmental_covariates_dictionary)
  dbWriteTable(react_gpkg,"env_dictionary",environmental_covariates_dictionary,overwrite=TRUE)
  data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(environmental_covariates_dictionary),table="env_dictionary"))

  path_to_bf_folder<-"data/diebougou"
  path_to_civ_folder<-"data/korhogo"

  # timeseries
  paths<-c("TMIN1","TMAX1","TAMP1","TMIN8","TMAX8","TAMP8","VNV8","VEV8","EVT8","SMO1","RFD1_F","RFD1_L","LIG30","DTL7","SPIs1","SPIs2")
  path_to_bf_ts<-paste0(path_to_bf_folder,"/",paths,".csv")
  path_to_civ_ts<-paste0(path_to_civ_folder,"/",paths,".csv")
  path_to_ts<-c(path_to_bf_ts,path_to_civ_ts)

  data_ts<-path_to_ts %>%
    map(~read.csv(.,stringsAsFactors = F)) %>%
    do.call(rbind,.) %>%
    mutate(val = as.numeric(val)) %>%
    mutate(date = as.character(date)) %>%
    mutate(lag_time=as.integer(lag_time)) %>%
    mutate(lag_n=as.integer(lag_n))
  data_ts <- cbind(fid = 1:nrow(data_ts), data_ts)
  dbWriteTable(react_gpkg,"env_spatiotemporal",data_ts,overwrite=TRUE)
  data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(data_ts),table="env_timeseries"))

  # nightcatch
  paths<-c("RFH","WDR","WSP","LMN","NMA")
  path_to_bf_nightcatch<-paste0(path_to_bf_folder,"/",paths,".csv")
  path_to_civ_nightcatch<-paste0(path_to_civ_folder,"/",paths,".csv")
  path_to_nightcatch<-c(path_to_bf_nightcatch,path_to_civ_nightcatch)

  data_nightcatch<-path_to_nightcatch %>%
    map(~read_csv(.)) %>%
    do.call(rbind,.)
  data_nightcatch <- cbind(fid = 1:nrow(data_nightcatch), data_nightcatch)
  dbWriteTable(react_gpkg,"env_nightcatch",data_nightcatch,overwrite=TRUE)
  data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(data_nightcatch),table="env_nightcatch"))

  # nightcatch_postedecapture
  paths<-c("NMT_NML_NMH_NDP")
  path_to_bf_nightcatch_postedecapture<-paste0(path_to_bf_folder,"/",paths,".csv")
  path_to_civ_nightcatch_postedecapture<-paste0(path_to_civ_folder,"/",paths,".csv")
  path_to_nightcatch_postedecapture<-c(path_to_bf_nightcatch_postedecapture,path_to_civ_nightcatch_postedecapture)
  
  data_nightcatch_postedecapture<-path_to_nightcatch_postedecapture %>%
    map(~read_csv(.)) %>%
    do.call(rbind,.)
  data_nightcatch_postedecapture <- cbind(fid = 1:nrow(data_nightcatch_postedecapture), data_nightcatch_postedecapture)
  dbWriteTable(react_gpkg,"env_nightcatch_postedecapture",data_nightcatch_postedecapture,overwrite=TRUE)
  data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(data_nightcatch_postedecapture),table="env_nightcatch_postedecapture"))
  
  # static_buffer
  paths<-c("TEL_TSL_TAS_WAC_TCI_TWI","WAD_WLS_WAL","POP","POH","HYS","BCH")
  path_to_bf_static<-paste0(path_to_bf_folder,"/",paths,".csv")
  path_to_civ_static<-paste0(path_to_civ_folder,"/",paths,".csv")
  path_to_static<-c(path_to_bf_static,path_to_civ_static)

  data_staticbuffer<-path_to_static %>%
    map(~read_csv(.)) %>%
    do.call(rbind,.) %>%
    mutate(val=as.numeric(val))
  data_staticbuffer <- cbind(fid = 1:nrow(data_staticbuffer), data_staticbuffer)
  dbWriteTable(react_gpkg,"env_spatial",data_staticbuffer,overwrite=TRUE)
  data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(data_staticbuffer),table="env_spatial"))
  
  # static_nobuffer
  paths<-c("WMD","BDE","VCP_VCM_VCT")
  path_to_bf_static<-paste0(path_to_bf_folder,"/",paths,".csv")
  path_to_civ_static<-paste0(path_to_civ_folder,"/",paths,".csv")
  path_to_static<-c(path_to_bf_static,path_to_civ_static)
  
  data_staticnobuffer<-path_to_static %>%
    map(~read.csv(.,colClasses = "character")) %>%
    do.call(rbind,.)
  
  data_staticnobuffer <- cbind(fid = 1:nrow(data_staticnobuffer), data_staticnobuffer)
  dbWriteTable(react_gpkg,"env_static",data_staticnobuffer,overwrite=TRUE)
  data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(data_staticnobuffer),table="env_static"))
  
  # landcover
  path_to_lsm_civ<-"data/korhogo/LSM.csv"
  path_to_lsm_bf<-"data/diebougou/LSM.csv"

  lsm<-rbind(read.csv(path_to_lsm_civ,stringsAsFactors = F),read.csv(path_to_lsm_bf,stringsAsFactors = F)) #%>% mutate_all(as.character)
  lsm <- cbind(fid = 1:nrow(lsm), lsm)
  dbWriteTable(react_gpkg,"env_landcover",lsm,overwrite=TRUE)
  data_dictionnary <- rbind(data_dictionnary,data.frame(name=colnames(lsm),table="env_lsm"))

  
  
  
  dbSendQuery(react_gpkg,"VACUUM") # It is very important to Vacuum. Not vacuuming may prevent the DB to be opened.
  dbDisconnect(react_gpkg)
  dbSendQuery(react_gpkg_light,"VACUUM") # It is very important to Vacuum. Not vacuuming may prevent the DB to be opened.
  dbDisconnect(react_gpkg_light)








## Create data dictionnary
data_dictionnary$label <- data_dictionnary$type <- data_dictionnary$description <- data_dictionnary$additional_information <- NA

# fill-in some columns

data_dictionnary <- data_dictionnary %>%
  mutate(label = case_when(
      name=="fid" ~ "unique numeric identifier",
      name=="codevillage" ~ "alpha-3 code identifier of the village",
      name=="codepays" ~ "alpha-2 code identifier of the country",
  )) %>%
  mutate(type = case_when(
      name %in% c("fid","codevillage") ~ "string"
      ))

write.csv(data_dictionnary,"data_dictionnary.csv",row.names = F)
