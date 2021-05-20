climate_match <- function(region, 
                          species, 
                          scenario = "all",
                          n_totaal,
                          perc_climate,
                          coord_unc, 
                          BasisOfRecord){
  
  # Setup ####
  library(rgbif)
  library(sp)
  library(rgdal)
  library(raster)
  library(rworldmap)
  library(tidyverse)
  
  # Checks ####
  ## Region ##
  if(is.na(region)){
    stop("region is missing")
  }else{
    region <- tolower(region)
    
    worldmap <- getMap(resolution = "low")
    
    valid_regions <- tolower(unique(worldmap$REGION))
    
    if(region %in% valid_regions){
      region_shape <- subset(worldmap, tolower(worldmap$REGION) == region) 
    }else{
      stop("the provided region is not valid")
    }
  }
  
  # Download data ####
  # Climate matching occurence data ####
  # Determine future scenarios ####
  # Per scenario filter ####
  # Thresholds ####
  # Return ####
  
}