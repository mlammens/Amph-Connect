## ************************************************************************** ##
## spatial_analysis.R
##
## Author: Matthew Aiello-Lammens and Michael Tierney
## Date Created: 2018-22-01
##
## Purpose: 
## Construct masks to apply to different species SDM results. Each species'
## mask is adjusted to account for that species habitat requirements.
##
## ************************************************************************** ##


## Load libraries
## -------------------------------------------------------------------------- ##
library(raster)
library(rgdal)
library(rasterVis)
library(sf)
library(maptools)


## Set your qgis directory. This will be different for Matt or Mike
qgis_dir <- "/Users/maiellolammens/Dropbox/Projects/Amphibian-Connectivity/QGIS Files/" # Matt
#gqis_dir <- "/Users/mtier_000/Dropbox/QGIS Files/" # Mike

## Load in background layers
## -------------------------------------------------------------------------- ##

## Rasters
roads <- raster(paste0(qgis_dir, "Study Region Rds raster/projected rds/hdr.adf"))
wetlands <- raster(paste0(qgis_dir, "Study Region wetlands raster/projected wetlands/hdr.adf"))
lakes <- raster(paste0(qgis_dir, "Study Region Lakes raster/projected raster/hdr.adf"))
streams <- raster(paste0(qgis_dir, "Study Region Streams raster/projected streams/hdr.adf"))

## Use resample to set the same origin for all raster layers
wetlands <- resample(wetlands, roads)
lakes <- resample(lakes, roads)
streams <- resample(streams, roads)

# Shapes / polygons
study_area <- readOGR(dsn = paste0(qgis_dir, "Aiello-Lammens-Main-Study-Region.shp"))
wetlands_shp <- readOGR(dsn = paste0(qgis_dir, "Study-Region-shp-files/wcdecwet/wcdecwet.shp"))
streams_shp <- readOGR(dsn = paste0(qgis_dir, "Study-Region-shp-files/bmstream/bmstream.shp"))
lakes_shp <- readOGR(dsn = paste0(qgis_dir, "Study-Region-shp-files/bmlake/bmlake.shp"))
steep_slopes <- readOGR(dsn = paste0(qgis_dir, "shapefiles from Rubbo/Shapefiles/ExtractedDataNew/SteepSlopes_D-F.shp"))
mixed_forests <- readOGR( dsn = paste0(qgis_dir, "shapefiles from Rubbo/Shapefiles/MixedForest.shp"))
conifer_forests <- readOGR(dsn =  paste0(qgis_dir, "shapefiles from Rubbo/Shapefiles/ConiferousForest.shp"))
forests <- raster::union(mixed_forests, conifer_forests)
int_woodland_pool <- readOGR(dsn =  paste0(qgis_dir, "shapefiles from Rubbo/Shapefiles/VernalPool.shp"))

















## Load in SDM results
snappers <- raster("/Users/mtier_000/Dropbox/DEC-Estuary-Program-Grant-Execution/species_distributions/snapping-turtle.asc")

## Change resolution of snappers
snappers_30m <- resample(snappers, roads)


# ## Make wet_area map
# ## -------------------------------------------------------------------------- ##
# 
# ## Replace all of the NAs with 0s
# wetlands[is.na(wetlands[])] <- 0
# lakes[is.na(lakes[])] <- 0
# streams[is.na(streams[])] <- 0
# 
# ## Add the layers
# wet_area <- wetlands + lakes + streams
# 
# ## wet_area for sets of species
# wet_area_turtles <- lakes
# wet_area_sallys <- wetlands + streams
# wet_area_frogs <- wetlands + lakes + streams
# ## again, this is the habitat for the individual salamanders
# spotted_marbled_habitat <- int_woodland_pool + mixed_forests
# twolined_habitat <- streams
# redback_habitat <- mixed_forests
# newt_habitat <- lakes + mixed_forests
# 
# ## Convert to binary
# wet_area[wet_area[]>0] <- 1
# wet_area_turtles[wet_area_turtles[]>0] <- 1


## Load shape files of wet areas so we can mask the HS layer by wet areas
## -------------------------------------------------------------------------- ##
wetlands_shp <- readOGR(dsn = "/Users/mtier_000/Dropbox/QGIS Files/Study-Region-shp-files/wcdecwet/wcdecwet.shp")
streams_shp <- readOGR(dsn = "/Users/mtier_000/Dropbox/QGIS Files/Study-Region-shp-files/bmstream/bmstream.shp")
lakes_shp <- readOGR(dsn = "/Users/mtier_000/Dropbox/QGIS Files/Study-Region-shp-files/bmlake/bmlake.shp")



## Transform the shp files into the same crs as wetlands
wetlands_shp_wgs84 <- spTransform(wetlands_shp, crs(wetlands))
streams_shp_wgs84 <- spTransform(streams_shp, crs(wetlands))
lakes_shp_wgs84 <- spTransform(lakes_shp, crs(wetlands))
forests_shp_wgs84 <- spTransform(forests, crs(wetlands))

## Convert the streams shp file from lines to polygons
streams_shp_wgs84_poly <- SpatialLines2PolySet(streams_shp_wgs84)

#######start back up from here......
streams_shp_wgs84_poly <- PolySet2SpatialPolygons(streams_shp_wgs84_poly)




## Merge the three wet shp files into the proper group
#wet_area_shp <- raster::union(wetlands_shp_wgs84, lakes_shp_wgs84)
wet_area_turtles <- raster::union(lakes_shp_wgs84, forests_shp_wgs84)
wet_area_shp <- raster::union(wet_area_shp, streams_shp_wgs84_poly)
#merge shapefiles to create suitable habitat for individual species
redbacks_habitat <- shapefile::union("/Users/mt24348p/Dropbox/QGIS Files/shapefiles from Rubbo/Shapefiles/ConiferousForest.shp", "/Users/mt24348p/Dropbox/QGIS Files/shapefiles from Rubbo/Shapefiles/HardwoodForest.shp")
twolined_habitat <- streams_shp_wgs84_poly
slimy_habitat <- steep_slopes
spotted_marble_habitat <- int_woodland_pool



#save wet_area_shp as a raster file
#check to see if this is a raster or shape and save appropriately
wet_area_shp

#distiguish between turtles and sallys
#writeOGR(obj = wet_area_shp, dsn = "/Users/mtier_000/Dropbox/QGIS Files/", layer = "wet_area_shp", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(obj = wet_area_turtles, dsn = "/Users/mtier_000/Dropbox/QGIS Files/", layer = "wet_area_turtles", driver = "ESRI Shapefile", overwrite_layer = TRUE)

?writeOGR()

## Mask the HS layer with the wet_area_shp file
snappers_30m_no_roads_masked <- mask(x = snappers_30m_no_roads, mask = wet_area_shp)

## Write the final raster to file
#writeRaster(x = snappers_30m_no_roads_masked, filename = "demographic_models/Demographics-Snapping Turtles/snappers_masked.asc")
writeRaster(x = snappers_30m_no_roads_masked, filename = "../Dropbox/DEC-Estuary-Program-Grant-Execution/species_distributions/snappers_masked.asc", overwrite=TRUE)
