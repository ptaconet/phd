
require(raster)
require(RSAGA)

path_to_processing_folder="/home/ptaconet/react/datasets/data_CIV/tests_extraction_route"
path_to_texture<-"texture_2_2_8_sample.tif"
path_to_texture_restringed<-"texture_2_2_8_sample_restring.tif"
path_to_output_unsuperv_classif<-"texture_2_2_8_unsuperv_classif.tif"
path_to_output_unsuperv_classif_binary<-"texture_2_2_8_unsuperv_classif_binary.tif"
path_to_output_unsuperv_classif_binary_vect<-"texture_2_2_8_unsuperv_classif_binary.gpkg"
num_clusters_unsuperv_classif<-3  # must be set by the user after tests  #for civ1 : 10

setwd(path_to_processing_folder)

#gdal_appli<-paste0("gdal_calc.py -A ",file.path(path_to_processing_folder,path_to_texture)," --A_band 1 --outfile=",file.path(path_to_processing_folder,path_to_texture_restringed)," --calc=\"A*logical_and(A>9000, A<30000)\" --overwrite TRUE")
gdal_appli<-paste0("gdal_calc.py -A ",file.path(path_to_processing_folder,path_to_texture)," --A_band 1 --outfile=",file.path(path_to_processing_folder,path_to_texture_restringed)," --calc=\"A*(A>9000)\" --overwrite TRUE")

system(gdal_appli)


## Unsupervised classification of the image with kmeans
#from https://rspatial.org/rs/4-unsupclassification.html
pedo <- stack(path_to_texture_restringed)
nr <- getValues(pedo)
# It is important to set the seed generator because `kmeans` initiates the centers in random locations
set.seed(99)
# We want to create num_clusters_unsuperv_classif clusters, allow 500 iterations, start with 5 random sets using "Lloyd" method
kmncluster <- kmeans(na.omit(nr), centers = num_clusters_unsuperv_classif, iter.max = 500, nstart = 5, algorithm="Lloyd")

# First create a copy of the pedo layer
knr <- pedo
# Now replace raster cell values with kmncluster$cluster array
knr[] <- kmncluster$cluster

writeRaster(knr[[1]],path_to_output_unsuperv_classif, overwrite=TRUE)


## On ne garde que le 2
gdal_appli<-paste0("gdal_calc.py -A ",file.path(path_to_processing_folder,path_to_output_unsuperv_classif)," --A_band 1 --outfile=",file.path(path_to_processing_folder,path_to_output_unsuperv_classif_binary)," --calc=\"1*(A==2)\" --NoDataValue=0 --overwrite TRUE")
system(gdal_appli)

## On vectorise
gdal_appli<-paste0("gdal_polygonize.py ",file.path(path_to_processing_folder,path_to_output_unsuperv_classif_binary)," ",file.path(path_to_processing_folder,path_to_output_unsuperv_classif_binary_vect)," -b 1 None DN")
system(gdal_appli)
