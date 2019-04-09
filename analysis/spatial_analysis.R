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
wcforests <- resample(wcforests, roads, method = "ngb")

northern_redback_mask <- wcforests

## -------------------------------------------------------------------------- ##
## Slimy salamander
##
## Steep slopes
## -------------------------------------------------------------------------- ##
slimy_mask <- crop(wcsteep_slopes_shp, study_area)


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

frog_wetlands_lakes <- raster::union(crop(lakes_shp, study_area), 
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

# for(spec in sdm_results){
#   # Get a basename for each species
#   spec_name <- gsub(basename(spec), pattern = "\\.asc", replace = "")
#   spec_name <- gsub(spec_name, pattern = "\\-", replace = "_")
#   
#   # Apply the `apply_species_mask` function to each of these layers, creating a raster stack for each
#   eval(parse(text = paste( paste0(parse(text = spec_name), "_30m_stack"), 
#                            "<-",
#                            "apply_species_mask(raster(spec), qgis_dir = qgis_dir, study_area = study_area, roads = roads, species_mask = turtle_mask)")))
#   
#   
#   print(spec_name)
# }

snapping_turtle_30m_stack <-
  apply_species_mask(raster(paste0(grant_dir,"species_distributions/sdm_results/snapping-turtle.asc")), 
                     qgis_dir = qgis_dir, study_area = study_area, roads = roads, species_mask = turtle_mask)
painted_turtle_30m_stack <-
  apply_species_mask(raster(paste0(grant_dir,"species_distributions/sdm_results/painted-turtle.asc")), 
                     qgis_dir = qgis_dir, study_area = study_area, roads = roads, species_mask = turtle_mask)
wood_frog_30m_stack <-
  apply_species_mask(raster(paste0(grant_dir,"species_distributions/sdm_results/wood-frog.asc")), 
                     qgis_dir = qgis_dir, study_area = study_area, roads = roads, species_mask = vernal_pool_mask)
spring_peeper_30m_stack <-
  apply_species_mask(raster(paste0(grant_dir,"species_distributions/sdm_results/spring-peeper.asc")), 
                     qgis_dir = qgis_dir, study_area = study_area, roads = roads, species_mask = frog_wetlands_lakes)
gray_treefrog_30m_stack <-
  apply_species_mask(raster(paste0(grant_dir,"species_distributions/sdm_results/gray-treefrog.asc")), 
                     qgis_dir = qgis_dir, study_area = study_area, roads = roads, species_mask = frog_wetlands_lakes)
pickerel_frog_30m_stack <-
  apply_species_mask(raster(paste0(grant_dir,"species_distributions/sdm_results/pickerel-frog.asc")), 
                     qgis_dir = qgis_dir, study_area = study_area, roads = roads, species_mask = frog_lakes)
green_frog_30m_stack <-
  apply_species_mask(raster(paste0(grant_dir,"species_distributions/sdm_results/green-frog.asc")), 
                     qgis_dir = qgis_dir, study_area = study_area, roads = roads, species_mask = frog_lakes)
bull_frog_30m_stack <-
  apply_species_mask(raster(paste0(grant_dir,"species_distributions/sdm_results/bull-frog.asc")), 
                     qgis_dir = qgis_dir, study_area = study_area, roads = roads, species_mask = frog_lakes)
american_toad_30m_stack <-
  apply_species_mask(raster(paste0(grant_dir,"species_distributions/sdm_results/american-toad.asc")), 
                     qgis_dir = qgis_dir, study_area = study_area, roads = roads, species_mask = toad_mask)
fowlers_toad_30m_stack <-
  apply_species_mask(raster(paste0(grant_dir,"species_distributions/sdm_results/fowlers-toad.asc")), 
                     qgis_dir = qgis_dir, study_area = study_area, roads = roads, species_mask = toad_mask)
spotted_salamander_30m_stack <-
  apply_species_mask(raster(paste0(grant_dir,"species_distributions/sdm_results/spotted-salamander.asc")), 
                     qgis_dir = qgis_dir, study_area = study_area, roads = roads, species_mask = vernal_pool_mask)
marbled_salamander_30m_stack <-
  apply_species_mask(raster(paste0(grant_dir,"species_distributions/sdm_results/marbled_salamander.asc")), 
                     qgis_dir = qgis_dir, study_area = study_area, roads = roads, species_mask = vernal_pool_mask)
slimy_salamander_30m_stack <-
  apply_species_mask(raster(paste0(grant_dir,"species_distributions/sdm_results/slimy_salamander.asc")), 
                     qgis_dir = qgis_dir, study_area = study_area, roads = roads, species_mask = slimy_mask)
redback_salamander_30m_stack <-
  apply_species_mask(raster(paste0(grant_dir,"species_distributions/sdm_results/redback-salamander.asc")), 
                     qgis_dir = qgis_dir, study_area = study_area, roads = roads, species_mask = northern_redback_mask)
two_lined_salamander_30m_stack <-
  apply_species_mask(raster(paste0(grant_dir,"species_distributions/sdm_results/two-lined-salamander.asc")), 
                     qgis_dir = qgis_dir, study_area = study_area, roads = roads, species_mask = streams_200_buffer)
red_spotted_newt_30m_stack <-
  apply_species_mask(raster(paste0(grant_dir,"species_distributions/sdm_results/red-spotted-newt.asc")), 
                     qgis_dir = qgis_dir, study_area = study_area, roads = roads, species_mask = newt_mask)



## Read in all of the sdm results rasters as a stack
sdm_stack <- stack(snapping_turtle_30m_stack$roads,
                   painted_turtle_30m_stack$roads,
                   wood_frog_30m_stack$roads,
                   spring_peeper_30m_stack$roads,
                   gray_treefrog_30m_stack$roads,
                   pickerel_frog_30m_stack$roads,
                   green_frog_30m_stack$roads,
                   bull_frog_30m_stack$roads,
                   american_toad_30m_stack$roads,
                   fowlers_toad_30m_stack$roads,
                   spotted_salamander_30m_stack$roads,
                   marbled_salamander_30m_stack$roads,
                   slimy_salamander_30m_stack$roads,
                   redback_salamander_30m_stack$roads,
                   two_lined_salamander_30m_stack$roads,
                   red_spotted_newt_30m_stack$roads
                   )

names(sdm_stack) <- c("snapping_turtle",
                      "painted_turtle",
                      "wood_frog",
                      "spring_peeper",
                      "gray_tree_frog",
                      "pickerel_frog",
                      "green_frog",
                      "bull_frog",
                      "american_toad",
                      "fowlers_toad",
                      "spotted_salamander",
                      "marbled_salamander",
                      "slimy_salamander",
                      "redback_salamander",
                      "two_lined_salamander",
                      "red_spotted_newt")


plot(sdm_stack)

## ************************************************************************** ##


## ************************************************************************** ##
## Get SDM values at culvert locations for each species

## Read in the culvert file
culverts <- read.csv(paste0(grant_dir, "data/NAACC-culvert-crossings/crossings_detailed.csv"))

## Extract the SDM values at culvert locations for each species
snapping_turtle_culverts <- 
  as.data.frame(extract(snapping_turtle_30m_stack, 
                        y = select(culverts, GPS_X_Coordinate, GPS_Y_Coordinate)))
painted_turtle_culverts <-
  as.data.frame(extract(painted_turtle_30m_stack,
                        y = select(culverts, GPS_X_Coordinate, GPS_Y_Coordinate)))
wood_frog_culverts <-
  as.data.frame(extract(wood_frog_30m_stack,
                        y = select(culverts, GPS_X_Coordinate, GPS_Y_Coordinate)))
spring_peeper_culverts <-
  as.data.frame(extract(spring_peeper_30m_stack,
                        y = select(culverts, GPS_X_Coordinate, GPS_Y_Coordinate)))
gray_tree_frog_culverts <-
  as.data.frame(extract(gray_treefrog_30m_stack,
                        y = select(culverts, GPS_X_Coordinate, GPS_Y_Coordinate)))
pickerel_frog_culverts <-
  as.data.frame(extract(pickerel_frog_30m_stack,
                        y = select(culverts, GPS_X_Coordinate, GPS_Y_Coordinate)))
green_frog_culverts <-
  as.data.frame(extract(green_frog_30m_stack,
                        y = select(culverts, GPS_X_Coordinate, GPS_Y_Coordinate)))
bull_frog_culverts <-
  as.data.frame(extract(bull_frog_30m_stack,
                        y = select(culverts, GPS_X_Coordinate, GPS_Y_Coordinate)))
american_toad_culverts <-
  as.data.frame(extract(american_toad_30m_stack,
                        y = select(culverts, GPS_X_Coordinate, GPS_Y_Coordinate)))
fowlers_toad_culverts <-
  as.data.frame(extract(fowlers_toad_30m_stack,
                        y = select(culverts, GPS_X_Coordinate, GPS_Y_Coordinate)))
spotted_salamander_culverts <-
  as.data.frame(extract(spotted_salamander_30m_stack,
                        y = select(culverts, GPS_X_Coordinate, GPS_Y_Coordinate)))
marbled_culverts <-
  as.data.frame(extract(marbled_salamander_30m_stack,
                        y = select(culverts, GPS_X_Coordinate, GPS_Y_Coordinate)))
slimy_salamander_culverts <-
  as.data.frame(extract(slimy_salamander_30m_stack,
                        y = select(culverts, GPS_X_Coordinate, GPS_Y_Coordinate)))
redback_salamander_culverts <-
  as.data.frame(extract(redback_salamander_30m_stack,
                        y = select(culverts, GPS_X_Coordinate, GPS_Y_Coordinate)))
two_lined_salamander_culverts <-
  as.data.frame(extract(two_lined_salamander_30m_stack,
                        y = select(culverts, GPS_X_Coordinate, GPS_Y_Coordinate)))
red_spotted_newt_culverts <-
  as.data.frame(extract(red_spotted_newt_30m_stack,
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

## Apply culvert prioritization function
culverts$turtle_pri <- assess_culvert_pri(snapping_turtle_culverts)
culverts$painted_pri <- assess_culvert_pri(painted_turtle_culverts)
culverts$woodfrog_pri <- assess_culvert_pri(wood_frog_culverts)
culverts$spring_peeper_pri <- assess_culvert_pri(spring_peeper_culverts)
culverts$gray_tree_frog_pri <- assess_culvert_pri(gray_tree_frog_culverts)
culverts$pickerel_pri <- assess_culvert_pri(pickerel_frog_culverts)
culverts$green_frog_pri <- assess_culvert_pri(green_frog_culverts)
culverts$bull_frog_pri <- assess_culvert_pri(bull_frog_culverts)
culverts$american_toad_pri <- assess_culvert_pri(american_toad_culverts)
culverts$fowlers_toad_pri <- assess_culvert_pri(fowlers_toad_culverts)
culverts$spotted_salamander_pri <- assess_culvert_pri(spotted_salamander_culverts)
culverts$marbled_salamander_pri <- assess_culvert_pri(marbled_culverts)
culverts$slimy_salamander_pri <- assess_culvert_pri(slimy_salamander_culverts)
culverts$redback_salamander_pri <- assess_culvert_pri(redback_salamander_culverts)
culverts$two_lined_salamander_pri <- assess_culvert_pri(two_lined_salamander_culverts)
culverts$red_spotted_newt <- assess_culvert_pri(red_spotted_newt_culverts)

## Calculate total and mean priorties
all_pri <- select(culverts, ends_with("pri"))
culverts$all_pri_total <- apply(all_pri, MARGIN = 1, sum)
culverts$all_pri_mean <- apply(all_pri, MARGIN = 1, mean)

## Write the new culvert data.frame
#write.csv(x = culverts, file = paste0(grant_dir, "data/NAACC-culvert-crossings/crossings_with_priorities.csv"), row.names = FALSE)

write_asc <- TRUE

if(write_asc){
  writeRaster(snapping_turtle_30m_stack$noroads, "species_distributions/sdm_masked/snapping-turtle-masked.asc")
  
}
