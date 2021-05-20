climate_match <- function(region, 
                          taxonkey, 
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
  
  crs_wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
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
  
  ## Species ##
  taxonkey <- as.integer(taxonkey)
  
  if(is.na(taxonkey)){
    stop("taxonkey is missing or not valid")
  }
  
  taxonkey_set1 <- pred_in("taxonKey", taxonkey)
  
  # Download data ####
  # Climate matching occurrence data ####
  # Determine future scenarios ####
  # Per scenario filter ####
  # Thresholds ####
  # Return ####
  
}