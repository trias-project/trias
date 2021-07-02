climate_match <- function(region, 
                          taxonkey, 
                          zipfile,
                          scenario = "all",
                          n_totaal,
                          perc_climate ,
                          coord_unc, 
                          BasisOfRecord){
  
  # Setup ####
  library(rgbif)
  library(sp)
  library(rgdal)
  library(raster)
  library(rworldmap)
  library(leaflet)
  library(tidyverse)
  
  crs_wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  devtools::load_all()
  
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
  
  if (!missing(zipfile)) {
    
    if (!file.exists(zipfile)) {
      
      warning(paste0(zipfile, " cannot be found. Rerunning gbif download"))
      rerun <- 1
    }else{
      rerun <- menu(choices = c("yes", "no"), 
                    title = "rerun gbif download?",
                    graphics = TRUE)
    }
    if (rerun != 1) {
      data <- read_tsv(unz(zipfile, "occurrence.txt"), 
                       col_types = c(decimalLatitude = col_number(),
                                     decimalLongitude = col_number())) %>% 
        filter(acceptedTaxonKey %in% taxonkey)
    }else{
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
    }
  }
  
  # Data prep ####
  if(missing(coord_unc)){
    coord_unc <- max(data$coordinateUncertaintyInMeters, na.rm = TRUE)
  }
  
  if(missing(BasisOfRecord)){
    BasisOfRecord <- unique(data$basisOfRecord)
  }
  
  SPECIES <- data %>% 
    filter(taxonRank == "SPECIES", 
           taxonomicStatus == "ACCEPTED") %>% 
    distinct(acceptedTaxonKey, genus, specificEpithet) %>% 
    mutate(ASN_2 = paste(genus, specificEpithet)) %>% 
    rename(TK_2 = acceptedTaxonKey) %>% 
    distinct(TK_2, ASN_2) %>% 
    group_by(TK_2) %>% 
    add_tally()
  
  data_redux <- data %>% 
    mutate(acceptedScientificName = paste(genus, specificEpithet)) %>% 
    left_join(SPECIES, by = c("acceptedScientificName" = "ASN_2")) %>% 
    mutate(acceptedTaxonKey = TK_2) %>% 
    filter(!is.na(acceptedTaxonKey),
           !is.na(eventDate), 
           !is.na(decimalLatitude),
           eventDate >= "1950-01-01",
           basisOfRecord %in% BasisOfRecord,
           coordinateUncertaintyInMeters <= coord_unc,
           occurrenceStatus == "PRESENT") %>% 
    dplyr::select(gbifID, eventDate, year, month, day, taxonKey, 
                  acceptedTaxonKey, acceptedScientificName, decimalLatitude, 
                  decimalLongitude, coordinateUncertaintyInMeters, countryCode) 
  
  coord <- data_redux %>% 
    dplyr::select(decimalLongitude, decimalLatitude)
  
  data_sp <- SpatialPointsDataFrame(coord,
                                    data = data_redux,
                                    proj4string = crs_wgs)
  
  # Load datapackages ####
  load(file = "./data/observed.rda")
  load(file = "./data/future.rda")
  load(file = "./data/legend.rda")
  
  # Climate matching occurrence data ####
  
  timeperiodes <- c("1901-1925", 
                    "1926-1950",
                    "1951-1975",
                    "1976-2000",
                    "2001-2025")
  
  data_overlay <- data.frame()
  
  for(t in timeperiodes){
    # Import legends
    KG_Rubel_Kotteks_Legend <- legend$KG_A1FI
    KG_Beck_Legend <- legend$KG_Beck_Legend
    
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
        obs_shape <- observed[[t]]
        data_sub_over <- sp::over(data_sp_sub, obs_shape)
        data_sub_overlay <- bind_cols(data_sp_sub@data, data_sub_over) 
        
        data_sub_overlay <- data_sub_overlay %>% 
          mutate(GRIDCODE = as.double(GRIDCODE)) %>% 
          left_join(KG_Rubel_Kotteks_Legend, by = c("GRIDCODE")) 
      }else{
        obs_shape <- observed$"1980-2016"
        data_sub_over <- sp::over(data_sp_sub, obs_shape)
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
    distinct(acceptedTaxonKey, acceptedScientificName, Classification, 
             .keep_all = TRUE) %>% 
    select(taxonKey, 
           acceptedScientificName, 
           Classification, 
           n_climate, 
           n_totaal, 
           perc_climate)
  
  # Determine future scenarios ####
  scenarios <- c("2001-2025-A1FI",
                 "2026-2050-A1FI",
                 "2051-2075-A1FI",
                 "2071-2100_Beck",
                 "2076-2100-A1FI")
  
  # Create empty output 
  
  output <- data.frame() %>% 
    mutate(scenario = "",
           KG_GridCode = as.integer(""))
  
  # Calculate KG codes 
  for(s in scenarios){
    shape <- future[[s]]
    
    if(c("gridcode") %in% colnames(shape@data)){
      shape@data <- shape@data %>% 
        rename(GRIDCODE = gridcode) 
    }
    
    shape@data <- shape@data %>% 
      mutate(GRIDCODE = as.integer(GRIDCODE)) 
    
    girdcode_intersect <- raster::intersect(shape, region_shape)
    
    for(g in girdcode_intersect@data$GRIDCODE){
      output <- output %>% 
        add_row(scenario = s,
                KG_GridCode = g)
    }
  }
  
  output_1 <- output %>% 
    filter(grepl(pattern = "Beck", scenario)) %>% 
    left_join(KG_Beck_Legend, by = c("KG_GridCode" = "GRIDCODE"))
  
  output_2 <- output %>% 
    filter(!grepl(pattern = "Beck", scenario)) %>% 
    left_join(KG_Rubel_Kotteks_Legend, by = c("KG_GridCode" = "GRIDCODE"))
  
  output_final <- rbind(output_1, output_2)
  
  future <- output_final %>% 
    filter(!is.na(Classification))
  
  # Per scenario filter ####
  
  cm <- data.frame()
  
  for(b in unique(future$scenario)){
    future_scenario <- future %>% 
      filter(scenario == b)
    
    cm_int <- data_overlay_unfiltered %>% 
      filter(Classification %in% future_scenario$Classification) %>% 
      mutate(scenario = b)
    
    if(nrow(cm) == 0){
      cm <- cm_int
    }else{
      cm <- rbind(cm, cm_int)
    }
  }
  
  # Thresholds ####
  if(missing(n_totaal)){
    warning("no n_totaal threshold was provided. defaults to 0!")
    n_totaal <- 0
  }
  if(missing(perc_climate)){
    warning("no perc_climate threshold was provided. defaults to 0%!")
    perc_climate <- 0
  }
  
  data_overlay_scenario_filtered <- cm %>% 
    filter(n_totaal >= n_totaal,
           perc_climate >= perc_climate)
  
  # map current climate suitability ####
  
  # Get Current climate
  current_climate <- observed$`1980-2016`
  
  current_climate@data <- current_climate@data %>% 
    mutate(gridcode = as.double(gridcode)) %>% 
    left_join(legend$KG_Beck_Legend, by = c("gridcode" = "GRIDCODE"))
  
  # Combine climate shape with climate matched observations
  current_climate <- sp::merge(current_climate, data_overlay_unfiltered, 
                               by = "Classification",
                               all.y = TRUE,
                               duplicateGeoms = TRUE)
  
  # create color palette 
  pal_current <- colorNumeric("RdBu", 
                          domain = current_climate$perc_climate,
                          na.color =  "#e0e0e0",
                          reverse = TRUE)
  
  # create current climate map
  current_climate_map <- leaflet(current_climate) %>% 
    addPolygons(color = "#bababa",
                fillColor = ~pal_current(perc_climate),
                fillOpacity = 0.8,
                stroke = TRUE,
                weight = 0.5,
                group = ~acceptedScientificName) %>% 
    addLegend(pal = pal_current,
              values = current_climate$perc_climate)
  
  
  # map future climate suitability ####
  
  # Return ####
  return(list(unfiltered = data_overlay_unfiltered, 
              filtered = data_overlay_scenario_filtered,
              cm = cm,
              future = future,
              spatial = data_sp_sub))
}