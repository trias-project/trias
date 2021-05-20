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
  source(file = "./R/get_cred.R")
  
  # Checks ####
  ## Region ##
  if(is.na(region)){
    stop("region is missing")
  }else{
    region <- tolower(region)
    
    worldmap <- getMap(resolution = "low")
    
    valid_countries <- tolower(unique(worldmap$NAME))
    
    if(region %in% valid_countries){
      region_shape <- subset(worldmap, tolower(worldmap$NAME) == region) 
    }else{
      valid_continents <- tolower(unique(worldmap$REGION))
      if(region %in% valid_continents){
        region_shape <- subset(worldmap, tolower(worldmap$REGION) == region) 
      }else{
        stop("the provided region is not valid")
      }
    }
  }
  
  ## Species ##
  taxonkey <- as.numeric(unique(taxonkey))
  
  if(length(taxonkey) == 1 & is.na(taxonkey)){
    stop("taxonkey is missing or not valid")
  }
  
  taxonkey_set1 <- pred_in("taxonKey", taxonkey)
  
  # Download data ####
  gbif_user <- get_cred("gbif_user")
  gbif_pwd <- get_cred("gbif_pwd")
  gbif_email <- get_cred("gbif_email")
  
  set1 <- occ_download(taxonkey_set1, 
                       pred("hasCoordinate", TRUE),
                       user = gbif_user, 
                       pwd = gbif_pwd, 
                       email = gbif_email)
  
  repeat{
    Sys.sleep(time = 5)
    test_set1 <- occ_download_meta(set1)
    if(test_set1$status == "SUCCEEDED"){
      data <- occ_download_get(set1,
                               overwrite = TRUE) %>% 
        occ_download_import()
      break()
    }
    print(test_set1$status)
  }
  
  if(nrow(data) == 0){
    stop("no occurrences of ", 
         paste(taxonkey, collapse = ", "), 
         " were found on gbif")
  }
  
  # Climate matching occurrence data ####
  # Determine future scenarios ####
  # Per scenario filter ####
  # Thresholds ####
  # Return ####
  
}