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
  
  # Data prep ####
  
  coord <- data %>% 
    dplyr::select(decimalLongitude, decimalLatitude)
  
  data_sp <- SpatialPointsDataFrame(coord,
                                    data = data,
                                    proj4string = crs_wgs)
  
  # Climate matching occurrence data ####
  ## Get observed shapes ##
  load(file = "./data/observed.rda")
  load(file = "./data/future.rda")
  load(file = "./data/legend.rda")
  
  timeperiodes <- c("1901-1925", 
                    "1926-1950",
                    "1951-1975",
                    "1976-2000",
                    "2001-2025")
  
  data_overlay <- data.frame()
  
  for(t in timeperiodes){
    
    # Determine subset parameters
    start <- as.numeric(substr(t, 0, 4))
    end <- as.numeric(substr(t, 6, 9))
    
    # Subset spatial data
    data_sp_sub <- subset(data_sp, 
                          data_sp@data$year >= start & 
                            data_sp@data$year <= end)
    
    # Overlay with observed climate
    if(nrow(data_sp_sub)>0){
      if(start <= 2000){
        obs_shape <- get(t)
        data_sub_over <- over(data_sp_sub, obs_shape)
        data_sub_overlay <- bind_cols(data_sp_sub@data, data_sub_over) 
        
        data_sub_overlay <- data_sub_overlay %>% 
          left_join(KG_Rubel_Kotteks_Legend, by = c("GRIDCODE")) 
      }else{
        obs_shape <- get("1980-2016")
        data_sub_over <- over(data_sp_sub, obs_shape)
        data_sub_overlay <- bind_cols(data_sp_sub@data, data_sub_over) 
        
        data_sub_overlay <- data_sub_overlay %>% 
          rename(GRIDCODE = gridcode,
                 ID = Id) %>% 
          mutate(GRIDCODE = as.double(GRIDCODE)) %>% 
          left_join(KG_Beck_Legend, by = c("GRIDCODE"))
      }
      
      if(nrow(data_overlay) == 0){
        data_overlay <- data_sub_overlay
      }else{
        data_overlay <- rbind(data_overlay, data_sub_overlay)
      }
    }else{
      warning(paste0("No data was present in the gbifdataset for ", t))
    }
  }
  
  ## Calculate threshold parameters ##
  data_overlay_unfiltered <- data_overlay %>% 
    group_by(acceptedTaxonKey, acceptedScientificName, Classification) %>% 
    add_tally(name = "n_climate") %>% 
    ungroup() %>% 
    group_by(acceptedTaxonKey, acceptedScientificName) %>% 
    add_tally(name = "n_totaal") %>% 
    ungroup() %>% 
    mutate(perc_climate = n_climate/n_totaal) %>% 
    distinct(acceptedTaxonKey, acceptedScientificName, Classification, .keep_all = TRUE) %>% 
    select(-decimalLatitude, -decimalLongitude, -GRIDCODE, -ID, -eventDate, -year)
  
  # Determine future scenarios ####
  # Per scenario filter ####
  # Thresholds ####
  # Return ####
  
}