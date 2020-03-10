rm(list = ls())
require(RSQLite)
require(gdalUtils)
require(raster)
require(sf)
require(dplyr)
require(fasterize)

path_to_processing_folder<-"/home/ptaconet/react/datasets/data_BF"  #<Path to the processing folder (i.e. where all the data produced by the workflow will be stored)>
path_to_otbApplications_folder<-"/home/ptaconet/OTB-6.6.1-Linux64/bin"
path_to_spot67_preprocessed_pan<-"VHR_SPOT6/processed_data/PAN.TIF"
path_to_gpkg_database<-"/home/ptaconet/react/datasets/react_db.gpkg"
path_to_lulc_in_db<-"landcover_bf_L3"
path_to_rat_in_db<-"landcover_bf_pixval2class"
threshold_builtup<-6  # for BF : 6  ; for CIV : 4
threshold_reock<-0.2 # for BF : 0.2 ; for CIV :

react_gpkg <- dbConnect(RSQLite::SQLite(),path_to_gpkg_database)
path_to_simple_texture<-"VHR_SPOT6/processed_data/simple_texture_2_2.TIF"
path_to_inertia_treshold_texture<-"VHR_SPOT6/processed_data/texture_inertia_threshold.tif"
path_to_landcover_table="classif.tif"
path_to_landcover_bati<-"classif_bati.tif"
path_to_vector_inertia_threshold<-"VHR_SPOT6/processed_data/inertia_threshold_vect.gpkg"
path_to_lc_vector_bati<-"VHR_SPOT6/processed_data/bati_lc_vect.gpkg"
#path_to_output_final_bati_vect<-"built_up.gpkg"
path_to_output_bati_raster<-"Classification/bati_raster.tif"


setwd(path_to_processing_folder)


## Compute textures using as input the orthorectified Spot 6/7 panchromatic image
rast<-raster(path_to_spot67_preprocessed_pan)
min<-as.numeric(cellStats(rast,min))
max<-as.numeric(cellStats(rast,max))
otb_appli<-paste0(file.path(path_to_otbApplications_folder,"otbcli_HaralickTextureExtraction")," -in ",file.path(path_to_processing_folder,path_to_spot67_preprocessed_pan)," -parameters.xrad 2 -parameters.yrad 2 -parameters.nbbin 64 -parameters.min ",min," -parameters.max ",max," -texture simple -out ",file.path(path_to_processing_folder,path_to_simple_texture))
system(otb_appli)


## Threshold inertia and vectorize result
gdal_appli<-paste0("gdal_calc.py -A ",file.path(path_to_processing_folder,path_to_simple_texture)," --A_band 5 --outfile=",file.path(path_to_processing_folder,path_to_inertia_treshold_texture)," --calc=\"1*(A>",threshold_builtup,")\" --NoDataValue=0 --type='Byte'")
system(gdal_appli)
gdal_appli<-paste0("gdal_polygonize.py ",file.path(path_to_processing_folder,path_to_inertia_treshold_texture)," ",file.path(path_to_processing_folder,path_to_vector_inertia_threshold)," -b 1 None DN")
system(gdal_appli)

## Open classification dataset (level 3) from the DB and extract only bati in vector format
gdal_translate(path_to_gpkg_database,file.path(path_to_processing_folder,path_to_landcover_table),ot="UInt16",of="GTiff",oo=paste0("TABLE=",path_to_lulc_in_db))
query<-paste0("SELECT pixval from ",path_to_rat_in_db," WHERE classif_level='classification_L3' AND lc_class='bati'")
pixval_bati<-dbGetQuery(react_gpkg,query)
gdal_appli<-paste0("gdal_calc.py -A ",file.path(path_to_processing_folder,path_to_landcover_table)," --A_band 1 --outfile=",file.path(path_to_processing_folder,path_to_landcover_bati)," --calc=\"1*(A==", as.numeric(pixval_bati),")\" --NoDataValue=0 --type='Byte'")
system(gdal_appli)
gdal_appli<-paste0("gdal_polygonize.py ",file.path(path_to_processing_folder,path_to_landcover_bati)," ",file.path(path_to_processing_folder,path_to_lc_vector_bati)," -b 1 None DN")
system(gdal_appli)


### Improve: remove roads by : 1) filtering using the LULC classification and 2) using shape

## Intersect buffer of 200 m of built up from classif with inertia thresholded
bati_lc_vect<-read_sf(path_to_lc_vector_bati)
inertia_vect<-read_sf(path_to_vector_inertia_threshold)

bati_lc_vect_simplified<-st_simplify(bati_lc_vect,dTolerance = 5)
bati_lc_vect_buffer<-st_buffer(bati_lc_vect_simplified,200)

inertia_vect_intersect<-st_intersection(inertia_vect,bati_lc_vect_buffer)



## Threshold with shape indices to remove long raads (formulas here : https://github.com/gerrymandr/compactr/blob/master/compactness.R)
# 'An eccentric but convex or nearly convex shape,
#' such as a cigar, will have a high (1 or near 1) Minimum Convex Polygon score
#' but a low (< 0.1) Reock score.'

area_convex_hull = function(poly1, ch = NULL) {
  require(sf, quietly = TRUE)
  require(units, quietly = TRUE)
  if (is.null(ch)) {
    ch = st_convex_hull(st_geometry(poly1))
  }
  return(drop_units(st_area(poly1) / st_area(ch)))
}

reock = function(poly1, mbc = NULL) {
  require(sf, quietly = TRUE)
  require(units, quietly = TRUE)
  if (is.null(mbc)) {
    require(lwgeom, quietly = TRUE)
    mbc = st_minimum_bounding_circle(st_convex_hull(st_geometry(poly1)))
  }

  return(drop_units(st_area(poly1) / st_area(mbc)))
}

inertia_vect_intersect_buff<-st_buffer(inertia_vect_intersect,15)
inertia_vect_intersect_buff<-st_union(inertia_vect_intersect_buff)
inertia_vect_intersect_buff<-st_cast(inertia_vect_intersect_buff,"POLYGON")
inertia_vect_intersect_buff<-st_sf(inertia_vect_intersect_buff)
inertia_vect_intersect_buff$shape_reock<-reock(inertia_vect_intersect_buff)
inertia_vect_intersect_buff$area_convex_hull<-area_convex_hull(inertia_vect_intersect_buff)

inertia_vect_intersect_buff$id<-seq(1,nrow(inertia_vect_intersect_buff))
#inertia_vect_intersect_buff <- inertia_vect_intersect_buff %>% filter(shape_reock>=0.2 | id== )
#inertia_vect_intersect <- inertia_vect_intersect %>% filter(shape_reock>=threshold_reock & )



inertia_vect_intersect<-st_cast(inertia_vect_intersect,"MULTIPOLYGON")
output_res<-1.633175
r <- raster(inertia_vect_intersect, res = output_res)
# Rasterize using fasterize (fast version of rasterize)
r <- fasterize::fasterize(inertia_vect_intersect, r, field = "DN")
# Write the classification raster
writeRaster(r,path_to_output_bati_raster, overwrite=TRUE, datatype='INT2S')


#sf::st_write(inertia_vect_intersect,path_to_output_final_bati_vect,layer_options = "OVERWRITE=true")



dbDisconnect(react_gpkg)
file.remove(c(path_to_inertia_treshold_texture,path_to_landcover_table,path_to_landcover_bati,path_to_vector_inertia_threshold,path_to_lc_vector_bati))


