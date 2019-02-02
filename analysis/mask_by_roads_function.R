## ************************************************************************** ##
## mask_by_roads_function.R
##
## Author: Matthew Aiello-Lammens and Michael Tierney
## Date Created: 2019-02-02
##
## Purpose:
## Remove roads from a suitability map. This script converts pixels on a
## suitability map, such as one produced by an SDM, that overlap a road with
## a 0 value. This effectively means the road is **not** habitat, and thus
## the organism cannot be found there.
##
## ************************************************************************** ##

mask_by_roads <- function(sdm_layer, road_file = "/Users/mtier_000/Dropbox/QGIS Files/Study Region Rds raster/projected rds/hdr.adf", study_area ){
  #get the road raster file
  roads <- raster(road_file)
  
  #make a 30m sdm layer
  sdm_layer_30m <- resample(sdm_layer, roads)
  
  ## Get HS values at roads only
  sdm_layer_30m_at_roads <- sdm_layer_30m * roads
  
  ## Replace all of the NAs with 0s
  sdm_layer_30m_at_roads[is.na(sdm_layer_30m_at_roads[])] <- 0
  
  ## Subtract the HS values at the roads
  sdm_layer_30m_no_roads <- sdm_layer_30m - sdm_layer_30m_at_roads
  
  ## Mask the HS layer by the study area polygon
  sdm_layer_30m_no_roads <- mask(sdm_layer_30m_no_roads, study_area)
  
  return(sdm_layer_30m_no_roads)
}
