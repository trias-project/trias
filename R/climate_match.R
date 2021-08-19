#' Create a set of climate matching outputs 
#' 
#' This function creates a set of climate matching outputs for a species or set
#' of species for a region or nation.
#' 
#' @param region (character) the full name of the target nation or region
#' @param taxonkey (character or vector) containing gbif - taxonkey(s)
#' @param zipfile (optional character) The path (inclu. extension) of a zipfile 
#' from a previous gbif-download. This zipfile should contain data of the 
#' species specified by the taxonkey
#' @param scenario (character) the future scenarios we are interested in.
#'  (default) all future scenarios are used
#' @param n_limit (optional numeric) the minimal number of total observations a 
#' species must have to be included in the outputs
#' @param cm_limit (optional numeric) the minimal percentage of the total 
#' number of observations within the climate zones of the region a species must 
#' have to be included in the outputs
#' @param coord_unc (optional numeric) the maximal coordinate uncertainty a 
#' observation can have to be included in the analysis
#' @param BasisOfRecord (optional character) an additional filter for 
#' observations based on the GBIF field "BasisOfRecord"
#' @param maps (boolean) indicating whether the maps should be created. 
#' (default) TRUE, the maps are created.
#' 
#' @return list with: 
#' - `unfiltered`: a dataframe containing a summary per species and climate classification. 
#' The climate classification is a result of a 
#' overlay of the observations, filtered by coord_unc & BasisOfRecord, with the 
#' climate at the time of observation
#' - `cm`: a dataframe containing the per scenario overlap with the future 
#' climate scenarios for the target nation or region and based on the `unfiltered` dataframe
#' - `filtered`: the climate match dataframe on which the n_limit & 
#' climate_limit thresholds have been applied
#' - `future`: a dataframe containing a list per scenario of future climate 
#' zones in the target nation or region
#' - `spatial` a spatialpointsdataframe containing the observations used 
#' in the analysis
#' - `current_map` a leaflet object displaying the degree of wordlwide 
#' climate match with the climate from 1980 till 2016
#' - `future_maps` a list of leaflet objects for each future climate 
#' scenario, displaying the degree of climate match
#' - `single_species_maps` a list of leaflet objects per taxonkey displaying 
#' the current and future climate scenarios
#' 
#' @export
#' @importFrom rgbif occ_download occ_download_meta occ_download_get pred
#'   occ_download_import pred_in
#' @importFrom sp SpatialPointsDataFrame over merge CRS
#' @importFrom raster intersect
#' @importFrom rworldmap getMap
#' @importFrom leaflet leaflet addPolygons colorNumeric addCircleMarkers
#'   addLegend addLayersControl
#' @importFrom dplyr %>% filter distinct mutate rename group_by add_tally
#'   bind_cols ungroup select left_join add_row
#' @importFrom readr read_tsv col_number col_character
#' @importFrom purrr list_along
#' @importFrom graphics legend
#' @importFrom stats setNames
#' @importFrom utils menu
climate_match <- function(region, 
                          taxonkey, 
                          zipfile,
                          scenario = "all",
                          n_limit,
                          cm_limit,
                          coord_unc, 
                          BasisOfRecord,
                          maps = TRUE){
  
  # Setup ####
  crs_wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  
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
  
  if (base::missing(zipfile)) {
    rerun <- 1
  }else{
    if (!file.exists(zipfile)) {
      warning(paste0(zipfile, " cannot be found. Rerunning gbif download"))
      rerun <- 1
    }else{
      rerun <- menu(choices = c("yes", "no"), 
                    title = "rerun gbif download?",
                    graphics = TRUE)
    }
  }
  
  if (rerun != 1 ) {
    data <- read_tsv(unz(zipfile, "occurrence.txt"), 
                     col_types = c(decimalLatitude = col_number(),
                                   decimalLongitude = col_number(),
                                   establishmentMeans = col_character())) %>% 
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
  
  
  # Data prep ####
  if(base::missing(coord_unc)){
    coord_unc <- max(data$coordinateUncertaintyInMeters, na.rm = TRUE)
  }
  
  if(base::missing(BasisOfRecord)){
    BasisOfRecord <- unique(data$basisOfRecord)
  }
  
  SPECIES <- data %>% 
    dplyr::filter(taxonRank == "SPECIES", 
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
    dplyr::filter(!is.na(acceptedTaxonKey),
                  !is.na(eventDate), 
                  !is.na(decimalLatitude),
                  eventDate >= "1901-01-01",
                  basisOfRecord %in% BasisOfRecord,
                  coordinateUncertaintyInMeters <= coord_unc | is.na(coordinateUncertaintyInMeters),
                  occurrenceStatus == "PRESENT") %>% 
    dplyr::select(gbifID, eventDate, year, month, day, taxonKey, 
                  acceptedTaxonKey, acceptedScientificName, decimalLatitude, 
                  decimalLongitude, coordinateUncertaintyInMeters, countryCode) %>% 
    distinct(eventDate, year, month, day, taxonKey, 
             acceptedTaxonKey, acceptedScientificName, decimalLatitude, 
             decimalLongitude, coordinateUncertaintyInMeters, countryCode)
  
  if(nrow(data_redux) == 0){
    stop(paste0("No useable data for ", paste(taxonkey, collapse = ","), " left after filters. Try omiting or changing the filter setup."))
  }
  
  coord <- data_redux %>% 
    dplyr::select(decimalLongitude, decimalLatitude)
  
  data_sp <- SpatialPointsDataFrame(coord,
                                    data = data_redux,
                                    proj4string = crs_wgs)
  
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
    KG_Beck <- legend$KG_Beck
    
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
          left_join(KG_Beck, by = c("GRIDCODE"))
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
           Description,
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
    
    girdcode_intersect <- intersect(shape, region_shape)
    
    for(g in girdcode_intersect@data$GRIDCODE){
      output <- output %>% 
        add_row(scenario = s,
                KG_GridCode = g)
    }
  }
  
  output_1 <- output %>% 
    dplyr::filter(grepl(pattern = "Beck", scenario)) %>% 
    left_join(KG_Beck, by = c("KG_GridCode" = "GRIDCODE"))
  
  output_2 <- output %>% 
    dplyr::filter(!grepl(pattern = "Beck", scenario)) %>% 
    left_join(KG_Rubel_Kotteks_Legend, by = c("KG_GridCode" = "GRIDCODE"))
  
  output_final <- rbind(output_1, output_2)
  
  future_climate <- output_final %>% 
    dplyr::filter(!is.na(Classification))
  
  # Per scenario filter ####
  
  cm <- data.frame()
  
  for(b in unique(future_climate$scenario)){
    future_scenario <- future_climate %>% 
      dplyr::filter(scenario == b)
    
    cm_int <- data_overlay_unfiltered %>% 
      dplyr::filter(Classification %in% future_scenario$Classification) %>% 
      mutate(scenario = b)
    
    if(nrow(cm) == 0){
      cm <- cm_int
    }else{
      cm <- rbind(cm, cm_int)
    }
  }
  
  # Thresholds ####
  if(missing(n_limit)){
    warning("no n_totaal threshold was provided. defaults to 0!")
    n_limit <- 0
  }
  if(missing(cm_limit)){
    warning("no perc_climate threshold was provided. defaults to 0%!")
    cm_limit <- 0
  }
  
  data_overlay_scenario_filtered <- cm %>% 
    dplyr::filter(n_totaal >= n_limit,
                  perc_climate >= cm_limit)
  
  # MAPS ####
  if(maps == TRUE){
    ## map current climate suitability ####
    
    # Get Current climate
    current_climate_shape <- observed$`1980-2016`
    
    current_climate_shape@data <- current_climate_shape@data %>% 
      mutate(gridcode = as.double(gridcode)) %>% 
      left_join(legend$KG_Beck, by = c("gridcode" = "GRIDCODE"))
    
    sea <- subset(current_climate_shape, is.na(current_climate_shape$Classification))
    
    # Combine climate shape with climate matched observations
    current_climate <- current_climate_shape
    
    for(t in taxonkey){
      temp_data <- data_overlay_unfiltered %>% 
        dplyr::filter(taxonKey == t) %>% 
        select(-Description)
      
      species <- unique(temp_data$acceptedScientificName)
      
      if(is_empty(species)){
        next
      }else{
        temp_climate <- sp::merge(current_climate_shape, temp_data, 
                                  by = "Classification",
                                  all.y = TRUE,
                                  duplicateGeoms = TRUE)
        
        temp_climate@data <- temp_climate@data %>% 
          mutate(taxonKey = t,
                 acceptedScientificName = species)
        
        if(ncol(current_climate)!=ncol(temp_climate)){
          current_climate <- temp_climate
        }else{
          current_climate <- rbind.SpatialPolygonsDataFrame(current_climate, 
                                                            temp_climate)
        }
      }
    }
    
    current_climate@data <- current_climate@data %>% 
      mutate(popup = paste0("<strong>Classification: </strong>", Description, " (", Classification, ")",
                            "</br><strong>ScientificName: </strong>", 
                            acceptedScientificName,
                            "</br><strong>%obs in climate: </strong>", 
                            round(perc_climate*100, 2), "%"))
    
    current_climate <- subset(current_climate, !is.na(current_climate$Classification))
    
    # create color palette 
    pal_current <- colorBin("OrRd", 
                            domain = seq(from = 0, 
                                         to = 1, 
                                         by = 0.1),
                            na.color =  "#f7f7f7",
                            bins = 9,
                            reverse = FALSE)
    
    # create current climate map
    current_climate_map <- leaflet(current_climate) %>% 
      addPolygons(color = "#bababa",
                  fillColor = ~pal_current(perc_climate),
                  fillOpacity = 0.8,
                  stroke = TRUE,
                  weight = 0.5,
                  group = ~acceptedScientificName,
                  popup = ~popup) %>% 
      addCircleMarkers(data = data_sp_sub,
                       group = ~acceptedScientificName,
                       color = "black",
                       radius = 1) %>% 
      addLegend(colors = "black",
                labels = "observations",
                position = "bottomleft") %>% 
      addLegend(pal = pal_current,
                values = seq(from = 0, 
                             to = 1, 
                             by = 0.1),
                position = "bottomleft",
                title = "Climate match") %>% 
      addLayersControl(baseGroups = ~acceptedScientificName) %>% 
      addPolygons(data = sea,
                  fillColor = "#e0e0e0",
                  weight = 0.5)
    
    ## map future climate suitability ####
    
    # Create basemap
    
    future_climate_map <- leaflet(sea) %>% 
      addPolygons(data = sea,
                  fillColor = "#e0e0e0",
                  weight = 0.5) %>% 
      addLegend(colors = "black",
                labels = "observations",
                position = "bottomleft")
    
    # Create scenario maps
    future_scenario_maps <- list_along(scenarios)
    names(future_scenario_maps) <- scenarios
    
    for (i in 1:length(scenarios)) {
      
      s <- scenarios[i]
      
      # Get scenario shape
      scenario_shape <- future[[s]]
      
      # Attach legends
      if(grepl("Beck", s)){
        scenario_shape@data <- scenario_shape@data %>% 
          mutate(gridcode = as.double(gridcode)) %>% 
          left_join(legend$KG_Beck, by = c("gridcode" = "GRIDCODE"))
      }else{
        scenario_shape@data <- scenario_shape@data %>% 
          mutate(GRIDCODE = as.double(GRIDCODE)) %>% 
          left_join(KG_Rubel_Kotteks_Legend, by = c("GRIDCODE"))
      }
      
      # Combine climate shape with climate matched observations
      temp_shape <- scenario_shape
      
      for(t in taxonkey){
        
        temp_data <- data_overlay_unfiltered %>% 
          dplyr::filter(taxonKey == t) %>% 
          select(-Description)
        
        species <- unique(temp_data$acceptedScientificName)
        
        if(is_empty(species)){
          next
        }else{
          temp_climate <- sp::merge(scenario_shape, temp_data, 
                                    by = "Classification",
                                    all.y = TRUE,
                                    duplicateGeoms = TRUE)
          
          temp_climate@data <- temp_climate@data %>% 
            mutate(taxonKey = t,
                   acceptedScientificName = species)
          
          if(ncol(temp_shape) != ncol(temp_climate)){
            temp_shape <- temp_climate
          }else{
            temp_shape <- rbind.SpatialPolygonsDataFrame(temp_shape, 
                                                         temp_climate)
          }
        }
      }
      
      temp_shape@data <- temp_shape@data %>% 
        mutate(popup = paste0("<strong>Classification: </strong>", Description, " (", Classification, ")", 
                              "</br><strong>ScientificName: </strong>", 
                              acceptedScientificName,
                              "</br><strong>%obs in climate: </strong>", 
                              round(perc_climate*100, 2), "%"))
      
      temp_shape <- subset(temp_shape, !is.na(temp_shape$Classification))
      
      # Add layer to map
      scenario_map <- future_climate_map %>% 
        addPolygons(data = temp_shape,
                    color = "#bababa",
                    fillColor = ~pal_current(perc_climate),
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.5,
                    group = ~acceptedScientificName,
                    popup = ~popup) %>% 
        addCircleMarkers(data = data_sp_sub,
                         group = ~ acceptedScientificName,
                         color = "black",
                         radius = 1) %>% 
        addLegend(pal = pal_current,
                  values = seq(from = 0, 
                               to = 1, 
                               by = 0.1),
                  position = "bottomleft",
                  title = paste0("<strong>Climate match</strong></br>",
                                 s)) %>% 
        addLayersControl(baseGroups = ~temp_shape@data$acceptedScientificName)
      
      
      future_scenario_maps[[i]] <- scenario_map
    }
    
    ## Single species - climate suitability maps ####
    
    # Create basemap
    
    single_species_map <- leaflet(sea) %>% 
      addPolygons(data = sea,
                  fillColor = "#e0e0e0",
                  weight = 0.5) %>% 
      addLegend(colors = "black",
                labels = "observations",
                position = "bottomleft")
    
    # Create single species maps
    
    scenarios_2 <- c("1980-2016", scenarios)
    
    single_species_maps <- list_along(taxonkey)
    names(single_species_maps) <- taxonkey
    
    for (i in 1:length(taxonkey)) {
      
      t <- taxonkey[i]
      
      temp_data <- data_overlay_unfiltered %>% 
        dplyr::filter(taxonKey == t) %>% 
        select(-Description)
      
      species <- unique(temp_data$acceptedScientificName)
      
      if(is_empty(species)){
        next
      }else{
        temp_shape <- data.frame()
        
        for(s in scenarios_2){
          
          if(s == "1980-2016"){
            scenario_shape <- observed[[s]]
          }else{
            scenario_shape <- future[[s]]
          }
          if (grepl("Beck", s) | s == "1980-2016") {
            scenario_shape@data <- scenario_shape@data %>% 
              mutate(GRIDCODE = as.double(gridcode),
                     ID = Id) %>% 
              select(-gridcode, -Id) %>% 
              left_join(legend$KG_Beck, by = "GRIDCODE")
          }else{
            scenario_shape@data <- scenario_shape@data %>% 
              mutate(GRIDCODE = as.double(GRIDCODE)) %>% 
              left_join(legend$KG_A1FI, by = "GRIDCODE")
          }
          
          temp_climate <- sp::merge(scenario_shape, temp_data, 
                                    by = "Classification",
                                    all.y = TRUE,
                                    duplicateGeoms = TRUE)
          
          temp_climate@data <- temp_climate@data %>% 
            mutate(taxonKey = t,
                   acceptedScientificName = species,
                   scenario = s)
          
          if(class(temp_shape) == "data.frame"){
            temp_shape <- temp_climate
          }else{
            temp_shape <- rbind.SpatialPolygonsDataFrame(temp_shape, 
                                                         temp_climate)
          }
        }
      }
      
      temp_shape@data <- temp_shape@data %>% 
        mutate(popup = paste0("<strong>Classification: </strong>", Description, " (", Classification, ")", 
                              "</br><strong>ScientificName: </strong>", 
                              acceptedScientificName,
                              "</br><strong>%obs in climate: </strong>", 
                              round(perc_climate*100, 2), "%",
                              "</br><strong>scenario: </strong>",
                              scenario))
      
      temp_shape <- subset(temp_shape, !is.na(temp_shape$Classification))
      
      # Add layer to map
      scenario_map <- single_species_map %>% 
        addPolygons(data = temp_shape,
                    color = "#bababa",
                    fillColor = ~pal_current(perc_climate),
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.5,
                    group = ~scenario,
                    popup = ~popup) %>% 
        addCircleMarkers(data = data_sp_sub,
                         color = "black",
                         radius = 1) %>% 
        addLegend(pal = pal_current,
                  values = seq(from = 0, 
                               to = 1, 
                               by = 0.1),
                  position = "bottomleft",
                  title = "<strong>Climate match</strong>") %>% 
        addLayersControl(baseGroups = ~temp_shape@data$scenario)
      
      
      single_species_maps[[i]] <- scenario_map
    }  
  }else{
    message("maps are disabled")
    current_climate_map <- NULL
    future_scenario_maps <- NULL
    single_species_maps <- NULL
  }
  
  
  # Return ####
  return(list(unfiltered = data_overlay_unfiltered, 
              cm = cm,
              filtered = data_overlay_scenario_filtered,
              future = future_climate,
              spatial = data_sp_sub,
              current_map = current_climate_map,
              future_maps = future_scenario_maps,
              single_species_maps = single_species_maps))
}