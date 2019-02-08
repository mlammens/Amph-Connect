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
library(PBSmapping)
library(dplyr)


## ************************************************************************** ##
## Read in spatial data for making mask layers
## ************************************************************************** ##

## Set your qgis directory. This will be different for Matt or Mike
#qgis_dir <- "/Users/maiellolammens/Dropbox/Projects/Amphibian-Connectivity/QGIS Files/" # Matt
qgis_dir <- "/home/mlammens/Dropbox/Projects/Amphibian-Connectivity/QGIS Files/" # Matt-Linux
#gqis_dir <- "/Users/mtier_000/Dropbox/QGIS Files/" # Mike

## Set your grant execution directory.
#grant_dir <- "/Users/maiellolammens/Dropbox/Projects/Amphibian-Connectivity/DEC-Estuary-Program-Grant-Execution/" # Matt
grant_dir <- "/home/mlammens/Dropbox/Projects/Amphibian-Connectivity/DEC-Estuary-Program-Grant-Execution/" # Matt-Linux


## Load in background layers
## -------------------------------------------------------------------------- ##

## Rasters
roads <- raster(paste0(qgis_dir, "Study Region Rds raster/projected rds/hdr.adf"))
wetlands <- raster(paste0(qgis_dir, "Study Region wetlands raster/projected wetlands/hdr.adf"))
lakes <- raster(paste0(qgis_dir, "Study Region Lakes raster/projected raster/hdr.adf"))
streams <- raster(paste0(qgis_dir, "Study Region Streams raster/projected streams/hdr.adf"))
landcov <- raster(paste0(qgis_dir, "Study-Region-shp-files/wclandcov/wclandcov/hdr.adf"))
landcov_wgs84 <- projectRaster(landcov, crs = proj4string(roads), method = "ngb")

# ## Use resample to set the same origin for all raster layers
# wetlands <- resample(wetlands, roads)
# lakes <- resample(lakes, roads)
# streams <- resample(streams, roads)

# Shapes / polygons
study_area <- readOGR(dsn = paste0(qgis_dir, "Aiello-Lammens-Main-Study-Region.shp"))
wetlands_shp <- readOGR(dsn = paste0(qgis_dir, "Study-Region-shp-files/wcdecwet/wcdecwet.shp"))
streams_shp <- readOGR(dsn = paste0(qgis_dir, "Study-Region-shp-files/bmstream/bmstream.shp"))
lakes_shp <- readOGR(dsn = paste0(qgis_dir, "Study-Region-shp-files/bmlake/bmlake.shp"))
streams_200_buffer <- readOGR(dsn = paste0(qgis_dir, "Study-Region-shp-files/bmstream/bmstream_200buff_nolake.shp"))
lakes_200buff_shp <- readOGR(dsn = paste0(qgis_dir, "Study-Region-shp-files/bmlake/bmlake_200buff_area.shp"))
wcsteep_slopes_shp <- readOGR(dsn = paste0(qgis_dir, "Study-Region-shp-files/wcslopes/wcslope.shp"))

# Pocantico watershed only
steep_slopes <- readOGR(dsn = paste0(qgis_dir, "shapefiles from Rubbo/Shapefiles/SteepSlopes_D-F.shp"))
mixed_forests <- readOGR( dsn = paste0(qgis_dir, "shapefiles from Rubbo/Shapefiles/MixedForest.shp"))
conifer_forests <- readOGR(dsn =  paste0(qgis_dir, "shapefiles from Rubbo/Shapefiles/ConiferousForest.shp"))
forests <- raster::union(mixed_forests, conifer_forests)
int_woodland_pool <- readOGR(dsn =  paste0(qgis_dir, "shapefiles from Rubbo/Shapefiles/VernalPool.shp"))

## Transform the shp files into the same crs as 'study_area'
wetlands_shp <- spTransform(wetlands_shp, crs(study_area))
streams_shp <- spTransform(streams_shp, crs(study_area))
lakes_shp <- spTransform(lakes_shp, crs(study_area))
steep_slopes <- spTransform(steep_slopes, crs(study_area))
forests_shp <- spTransform(forests, crs(study_area))
int_woodland_pool_wgs84 <-spTransform(int_woodland_pool, crs(study_area))
streams_200_buffer <- spTransform(streams_200_buffer, crs(study_area))
lakes_200buff_shp <- spTransform(lakes_200buff_shp, crs(study_area))
wcsteep_slopes_shp <- spTransform(wcsteep_slopes_shp, crs(study_area))

# This section was replaced by files created in QGIS
# ## Convert the streams shp file from lines to polygons
# ## WARNING - THIS STEP CAN TAKE UP TO 20 MINUTES ###
# ## TIME IS LESS THAN 4 MINUTES ON LINUX BOX ###
# streams_shp_poly <- SpatialLines2PolySet(streams_shp)
# streams_shp_poly <- PolySet2SpatialPolygons(streams_shp_poly)


## ************************************************************************** ##
## Make species specific mask layers
## -------------------------------------------------------------------------- ##
## As of 2019-02-02 much of the species specific information is based on 
## literature reviews done by M. Tierney and critical feedback from 
## Mike Rubbo. Much of this information has been compiled into the 
## 'Habitat_species-list.xlsx' file.
##
## -------------------------------------------------------------------------- ##
## Species List
##
# red-spotted newt
# 
# spotted salamander
# marbled salamander
# northern redback
# Northern slimy
# northern twolined
# 
# american toad
# fowler's toad
# 
# wood frog
# spring peeper
# gray treefrogs
# pickerel frog
# green frog
# bull frog
# 
# snapping turtle
# painted turtle
## 

## -------------------------------------------------------------------------- ##
## Snapping Turtle and Painte Turtle
##
## These species require small to large bodies of flat water and areas 
## of land around these bodies that aren't too built up.
## We are using the lakes layer as a mask. We could consider buffering this
## layer by about 30m, but this process takes an extrodinary amount of time
## and the lake layer is already quite extensive.
## -------------------------------------------------------------------------- ##

turtle_mask <- crop(lakes_shp, study_area)

## -------------------------------------------------------------------------- ##
## Red-spotted Newt
##
## For the full region, use lakes and wetlands
## -------------------------------------------------------------------------- ##

newt_mask <- raster::union(crop(lakes_shp, study_area), crop(wetlands_shp, study_area))

## -------------------------------------------------------------------------- ##
## Spotted and Marbled Salamanders and Wood Frogs
##
## These species require vernal pools. Thus, they can only be examined in 
## the Pocantico river watershed, where we have a map of vernal pools.
## -------------------------------------------------------------------------- ##

vernal_pool_mask <- raster::buffer(int_woodland_pool, width = 200)
vernal_pool_mask <- spTransform(vernal_pool_mask, crs(study_area))

## -------------------------------------------------------------------------- ##
## Northern redback salamanders
##
## Use forest values. For this, use the Westchester landcover layers, only
## values of 1 - evergreen vegetation and 2 - deciduous vegetation.
## -------------------------------------------------------------------------- ##

wcforests <- landcov_wgs84
wcforests[!(wcforests[] %in% c(1,2))] <- NA
wcforests <- crop(wcforests, study_area)

northern_redback_mask <- wcforests

## -------------------------------------------------------------------------- ##
## Slimy salamander
##
## Steep slopes
## -------------------------------------------------------------------------- ##
slimy_mask <- crop(wcstep_slopes_shp, study_area)


## -------------------------------------------------------------------------- ##
## American toad and Fowler's toad
##
## Use wetlands layer
## -------------------------------------------------------------------------- ##

toad_mask <- crop(wetlands_shp, study_area)

## -------------------------------------------------------------------------- ##
## Spring peeper and gray tree frog
##
## Ideally these would include vernal pools, but that is not possible for the
## full region. We can do this for the Pocantico in the future.
## For now, we will use lakes and wetlands
## -------------------------------------------------------------------------- ##

frog_wetlands_lakes <- union(crop(lakes_shp, study_area), 
                             crop(wetlands_shp, study_area))

## -------------------------------------------------------------------------- ##
## Pickerel frog, green frog, and bull frog
##
## As per Rubbo's suggestsion, using ponds and lakes, which corresponds to 
## only the lake_shp file
## -------------------------------------------------------------------------- ##

frog_lakes <- crop(lakes_shp, study_area)

## ************************************************************************** ##


## ************************************************************************** ##
## Mask SDM layers
##
## Each of the species SDM results should be masked by the species specific
## masks created above. 
## Additionally - each species should be masked so that values below the 
## **average test Omission Rate at 10%** are NOT considered habitat.
## For each species these species are noted below, as calculated using 
## the ENMeval results for each SDM:
##
# red-spotted newt = 0.11
# 
# spotted salamander = 0.11
# marbled salamander = 0.13
# northern redback = 0.10
# Northern slimy = 0.14
# northern twolined = 0.11
# 
# american toad = 0.13
# fowler's toad = 0.11
# 
# wood frog = 0.1
# spring peeper = 0.12
# gray treefrogs = 0.11
# pickerel frog = 0.17
# green frog = 0.11
# bull frog = 0.11
# 
# snapping turtle = 0.09
# painted turtle = 0.1
##
## ************************************************************************** ##

## Load mask_by_roads function
source(paste0(grant_dir, "analysis/mask_by_roads_function.R"))

## -------------------------------------------------------------------------- ##
## FUNCTION: apply_species_mask
##
## Author: Matthew Aiello-Lammens
## Date Created: ￼2019-02-04
##
## Purpose:
## Takes a species specific mask, and masks the SDM out for that species
##
## -------------------------------------------------------------------------- ##
apply_species_mask <- function(species_sdm, qgis_dir = qgis_dir, study_area = study_area, 
                               roads = roads, species_mask) {
  species_30m_noroads <- 
    mask_by_roads(sdm_layer = species_sdm, 
                  road_file = paste0(qgis_dir, "Study Region Rds raster/projected rds/hdr.adf"), 
                  study_area = study_area)
  
  # Resample the raster layer to the roads resolution without masking roads
  species_30m <- resample(species_sdm, roads)
  
  ## Mask the sdm layers by the turlte_mask
  species_30m_masked <- mask(species_30m, species_mask)
  species_30m_noroads_masked <- mask(species_30m_noroads, species_mask)
  
  ## Make a masked raster stack
  species_30m_stack <- raster::stack(species_30m_masked, species_30m_noroads_masked)
  names(species_30m_stack) <- c("roads", "noroads")
  
  return(species_30m_stack)
}


## Load in SDM results
## -------------------------------------------------------------------------- ##

sdm_results <- list.files(path = paste0(grant_dir,"species_distributions/sdm_results/"), pattern = "asc", full.names = TRUE)

## Snapping turtles
## -------------------------------------------------------------------------- ##
snappers <- raster(paste0(grant_dir, "species_distributions/snapping-turtle.asc"))
painted <- raster(paste0(grant_dir,"species_distributions/painted-turtle.asc"))

snapping_turtle_30m_stack <- 
  apply_species_mask(snappers, qgis_dir = qgis_dir, study_area = study_area,
                     roads = roads, species_mask = turtle_mask)

painted_turtle_30m_stack <-
  apply_species_mask(painted, qgis_dir = qgis_dir, study_area = study_area,
                     roads = roads, species_mask = turtle_mask)



## ************************************************************************** ##

culverts <- read.csv(paste0(grant_dir, "data/NAACC-culvert-crossings/crossings_detailed.csv"))

snapping_turtle_culverts <- 
  as.data.frame(extract(snapping_turtle_30m_stack, 
                        y = select(culverts, GPS_X_Coordinate, GPS_Y_Coordinate)))

painted_turtle_culverts <-
  as.data.frame(extract(painted_turtle_30m_stack,
                        y = select(culverts, GPS_X_Coordinate, GPS_Y_Coordinate)))

## ************************************************************************** ##
## -------------------------------------------------------------------------- ##
## FUNCTION: assess_culvert_pri
##
## Author: Matthew Aiello-Lammens
## Date Created: ￼2019-02-05
##
## Purpose:
## Assess culvert priority
## 0 - culvert not within species habitat
## 1 - culvert is within species habitat
## 2 - culvert is within species habitat AND on a road
## -------------------------------------------------------------------------- ##

assess_culvert_pri <- function(culv_hs_vals){
  # Make a vector of 0s that is equal to the length of the number of rows
  # in the culv_hs_vals data.frame
  culv_pri <- rep(0, 1, nrow(culv_hs_vals))
  
  # Assign a value of 1 to culverts that are in species habitat without
  # excluding roads
  culv_pri[which(!is.na(culv_hs_vals$roads))] <- 1
  
  # Assign a value of 2 to culverts that are in species habitat, but have a
  # value of 0 because of road presence
  culv_pri[which(culv_hs_vals$noroads == 0)] <-2
  
  return(culv_pri)
}


# culverts$turtle_priority <- 0
# culverts$turtle_priority <- ifelse(!is.na(snapper_culverts_roads), yes = 1, no = 0)
# culverts$turtle_priority[which(snapper_culverts_noroads == 0)] <- 2

turtle_pri <- assess_culvert_pri(snapping_turtle_culverts)
painted_pri <- assess_culvert_pri(painted_turtle_culverts)



