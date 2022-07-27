#' Create a set of climate matching outputs 
#' 
#' This function creates a set of climate matching outputs for a species or set
#' of species for a region or nation.
#' 
#' @param region (optional character) the full name of the target nation or 
#' region 
#' region can also be a custom region (SpatialPolygon or sf object).
#' @param taxon_key (character or vector) containing GBIF - taxonkey(s)
#' @param zip_file (optional character) The path (inclu. extension) of a zipfile 
#' from a previous GBIF-download. This zipfile should contain data of the 
#' species specified by the taxon_key
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
#' - `single_species_maps` a list of leaflet objects per taxon_key displaying 
#' the current and future climate scenarios
#' 
#' @export
#' @importFrom rgbif occ_download occ_download_meta occ_download_get pred
#'   occ_download_import pred_in
#' @importFrom sp SpatialPointsDataFrame over merge CRS 
#'   rbind.SpatialPolygonsDataFrame spTransform
#' @importFrom raster intersect
#' @importFrom rworldmap getMap
#' @importFrom leaflet leaflet addPolygons colorNumeric addCircleMarkers
#'   addLegend addLayersControl colorBin
#' @importFrom dplyr %>% filter distinct mutate rename group_by add_tally
#'   bind_cols ungroup select left_join add_row
#' @importFrom readr read_tsv col_number col_character
#' @importFrom purrr list_along
#' @importFrom graphics legend
#' @importFrom stats setNames
#' @importFrom utils menu
#' @importFrom rlang is_empty
#' @importFrom methods as
#' @examples
#' \dontrun{
#' # use rworldmap shapes 
#' region <- "europe"
#' 
#' # provide GBIF taxon_key (-list)
#' taxon_key <- c(2865504, 5274858)
#' 
#' # download zip_file from GBIF
#' # goto https://www.gbif.org/occurrence/download/0001221-210914110416597
#' 
#' zip_file <- "./<path to zip_file>/0001221-210914110416597.zip"
#' 
#' # calculate all climate match outputs
#' # with GBIF download
#' climate_match(region,
#'               taxon_key, 
#'               n_limit = 0.2,
#'               cm_limit = 90
#' )
#' # calculate only data climate match outputs
#' # using a pre-downloaded zip_file
#' climate_match(region,
#'               taxon_key, 
#'               zip_file,
#'               n_limit = 0.2,
#'               cm_limit = 90,
#'               maps = FALSE
#' )
#' # calculate climate match outputs based 
#' # on human observations with a 100m 
#' # coordinate uncertainty
#' climate_match(region,
#'               taxon_key, 
#'               zip_file,
#'               n_limit = 0.2,
#'               cm_limit = 90,
#'               coord_unc = 100,
#'               BasisOfRecord = "HUMAN_OBSERVATION",
#'               maps = FALSE
#' }
climate_match <- function(region, 
                          taxon_key, 
                          zip_file,
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
  if (missing(region)) {
    # no region is provided => worldwide
    region_shape <- getMap(resolution = "low")
  }else{
    if (is.character(region)) {
      # region is a character => select region from worldmap
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
    }else{
      # region is a environment object => get object into function 
      region_shape <- region
      
      if (class(region_shape) == "sf") {
        # region_shape is sf
        region_shape <- as(region_shape, "spatial")
      }
      
      if (class(region_shape) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")) {
        # region_shape is sp or converted from sf into sp and is a spatialpolygon(dataframe)
      }else{
        # region_shape is sp or converted from sf into sp but is not spatialpolygon(dataframe)
        stop("region is not a spatial object of the correct class")
      }
    }
  }
  
  region_shape <- spTransform(region_shape, crs_wgs)
  
  ## Species ##
  taxon_key <- as.numeric(unique(taxon_key))
  
  if(length(taxon_key) == 1){
    if(is.na(taxon_key)){
      stop("taxon_key is missing or not valid")
    }
  }
  
  taxon_key <- subset(taxon_key, !is.na(taxon_key))
  
  taxon_key_set1 <- pred_in("taxonKey", taxon_key)
  
  # Download data ####
  
  if (base::missing(zip_file)) {
    rerun <- 2
  }else{
    if (!file.exists(zip_file)) {
      warning(paste0(zip_file, " cannot be found. Rerunning GBIF download"))
      rerun <- 2
    }else{
      rerun <- menu(choices = c("no", "yes"), 
                    title = "rerun GBIF download?",
                    graphics = TRUE)
    }
  }
  
  if (rerun != 2 ) {
    data <- read_tsv(unz(zip_file, "occurrence.txt"), 
                     col_types = c(decimalLatitude = col_number(),
                                   decimalLongitude = col_number(),
                                   establishmentMeans = col_character()),
                     col_select = c("acceptedTaxonKey",
                                    "decimalLatitude",
                                    "decimalLongitude",
                                    "establishmentMeans",
                                    "coordinateUncertaintyInMeters",
                                    "basisOfRecord",
                                    "taxonRank",
                                    "taxonomicStatus",
                                    "genus",
                                    "specificEpithet",
                                    "eventDate",
                                    "occurrenceStatus",
                                    "gbifID",
                                    "year",
                                    "countryCode"
                     )) %>% 
      filter(acceptedTaxonKey %in% taxon_key)
  }else{
    gbif_user <- get_cred("gbif_user")
    gbif_pwd <- get_cred("gbif_pwd")
    gbif_email <- get_cred("gbif_email")
    
    set1 <- occ_download(taxon_key_set1, 
                         pred("hasCoordinate", TRUE),
                         pred_gt("year", 1900),
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
        
        data <- data %>% 
          select("acceptedTaxonKey",
                 "decimalLatitude",
                 "decimalLongitude",
                 "establishmentMeans",
                 "coordinateUncertaintyInMeters",
                 "basisOfRecord",
                 "taxonRank",
                 "taxonomicStatus",
                 "genus",
                 "specificEpithet",
                 "eventDate",
                 "occurrenceStatus",
                 "gbifID",
                 "year",
                 "countryCode")
      }
      print(test_set1$status)
    }
    
    if(nrow(data) == 0){
      stop("no occurrences of ", 
           paste(taxon_key, collapse = ", "), 
           " were found on GBIF")
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
    dplyr::filter(.data$taxonRank == "SPECIES", 
                  .data$taxonomicStatus == "ACCEPTED") %>% 
    distinct(.data$acceptedTaxonKey, .data$genus, .data$specificEpithet) %>% 
    mutate(ASN_2 = paste(.data$genus, .data$specificEpithet)) %>% 
    rename(TK_2 = .data$acceptedTaxonKey) %>% 
    distinct(.data$TK_2, .data$ASN_2) %>% 
    group_by(.data$TK_2) %>% 
    ungroup()
  
  data_redux <- data %>% 
    mutate(acceptedScientificName = paste(.data$genus, .data$specificEpithet)) %>% 
    left_join(SPECIES, by = c("acceptedScientificName" = "ASN_2")) %>% 
    mutate(acceptedTaxonKey = .data$TK_2) %>% 
    dplyr::filter(!is.na(.data$acceptedTaxonKey),
                  !is.na(.data$eventDate), 
                  !is.na(.data$decimalLatitude),
                  .data$eventDate >= "1901-01-01",
                  .data$basisOfRecord %in% BasisOfRecord,
                  .data$coordinateUncertaintyInMeters <= coord_unc | is.na(.data$coordinateUncertaintyInMeters),
                  .data$occurrenceStatus == "PRESENT") %>% 
    dplyr::select(.data$gbifID, 
                  .data$year, 
                  .data$acceptedTaxonKey, 
                  .data$acceptedTaxonKey, 
                  .data$acceptedScientificName, 
                  .data$decimalLatitude, 
                  .data$decimalLongitude, 
                  .data$coordinateUncertaintyInMeters, 
                  .data$countryCode) %>% 
    mutate(year_cat = case_when(year <= 1925 ~ "1901-1925",
                                year <= 1950 ~ "1926-1950",
                                year <= 1975 ~ "1951-1975",
                                year <= 2000 ~ "1976-2000",
                                year > 2000 ~ "2001-2025",
                                TRUE ~ NA_character_)) %>% 
    group_by(.data$year_cat, 
             .data$acceptedTaxonKey, 
             .data$decimalLatitude, 
             .data$decimalLongitude) %>% 
    summarize(n_obs = n()) %>% 
    ungroup()
  
  if(nrow(data_redux) == 0){
    stop(paste0("No useable data for ", paste(taxon_key, collapse = ","), " left after filters. Try omiting or changing the filter setup."))
  }
  
  remove(data)
  
  coord <- data_redux %>% 
    dplyr::select(.data$decimalLongitude, .data$decimalLatitude)
  
  data_sp <- SpatialPointsDataFrame(coord,
                                    data = data_redux,
                                    proj4string = crs_wgs)
  
  data_sf <- sf::st_as_sf(data_sp, 4326)
  
  remove(data_sp)
  
  # Climate matching occurrence data ####
  
  timeperiodes <- c("1901-1925", 
                    "1926-1950",
                    "1951-1975",
                    "1976-2000",
                    "2001-2025")
  
  data_overlay <- data.frame()
  
  for(t in timeperiodes){
    print(t)
    # Import legends
    KG_Rubel_Kotteks_Legend <- legends$KG_A1FI
    KG_Beck <- legends$KG_Beck
    
    # Determine subset parameters
    start <- as.numeric(substr(t, 0, 4))
    #end <- as.numeric(substr(t, 6, 9))
    
    # Subset spatial data
    data_sf_sub <- data_sf %>% 
      filter(.data$year_cat == t)
    
    print(nrow(data_sf_sub))
    
    # Overlay with observed climate
    if(nrow(data_sf_sub)>0){
        obs_shape <- sf::st_as_sf(observed[[t]])
      }
      if(nrow(data_sf_sub) <= 11000){
        data_sf_sub$GRIDCODE <- apply(sf::st_intersects(obs_shape, 
                                                        data_sf_sub, 
                                                        sparse = FALSE), 2, 
                                      function(col) {obs_shape[which(col),
                                      ]$GRIDCODE})
        pb <- txtProgressBar(min = 0, 
                             max = length(data_sf_sub$GRIDCODE), 
                             style = 3)
        for(i in 1:length(data_sf_sub$GRIDCODE)){
          setTxtProgressBar(pb, i)
          data_sf_sub$GRIDCODE2[i] <- as.double(data_sf_sub$GRIDCODE[[i]][1])
        }
        
        data_sf_sub <- data_sf_sub %>% 
          mutate(GRIDCODE = as.double(GRIDCODE2)) %>% 
          select(-GRIDCODE2) %>% 
          left_join(KG_Rubel_Kotteks_Legend, by = c("GRIDCODE")) 
      }else{
        cuts <- ceiling(nrow(data_sf_sub)/10000)
        pb_2 <- txtProgressBar(min = 0, 
                               max = cuts, 
                               style = 3)
        
        data_overlay_sub <- data.frame()
        
        for(x in 1:cuts){
          print(paste0(x, "/", cuts))
          setTxtProgressBar(pb_2, x)
          y <- ((x - 1)*10000) + 1 
          z <- x * 10000
          if(z < nrow(data_sf_sub)){
            data_sf_sub_sub <- data_sf_sub[y:z,]
          }else{
            data_sf_sub_sub <- data_sf_sub[y:nrow(data_sf_sub),]
          }
          data_sf_sub_sub$GRIDCODE <- apply(sf::st_intersects(obs_shape, 
                                                              data_sf_sub_sub, 
                                                              sparse = FALSE), 2, 
                                            function(col) {obs_shape[which(col),
                                            ]$GRIDCODE})
          
          pb_3 <- txtProgressBar(min = 0, 
                                 max = length(data_sf_sub_sub$GRIDCODE), 
                                 style = 3)
          
          for(i in 1:length(data_sf_sub_sub$GRIDCODE)){
            setTxtProgressBar(pb_3, i)
            data_sf_sub_sub$GRIDCODE2[i] <- as.double(data_sf_sub_sub$GRIDCODE[[i]][1])
          }
          
          data_sf_sub_sub <- data_sf_sub_sub %>% 
            mutate(GRIDCODE = as.double(GRIDCODE2)) %>% 
            select(-GRIDCODE2) %>% 
            left_join(KG_Rubel_Kotteks_Legend, by = c("GRIDCODE")) 
          
          if(nrow(data_overlay_sub) == 0){
            data_overlay_sub <- data_sf_sub_sub
          }else{
            data_overlay_sub <- rbind(data_overlay_sub, data_sf_sub_sub)
          }
          remove(data_sf_sub_sub)
          gc()
        }
        data_sf_sub <- data_overlay_sub
      }
    }else{
      warning(paste0("No data was present in the GBIF dataset for ", t))
      next
    }
    # Recombine timeperiodes
    if(nrow(data_overlay) == 0){
      data_overlay <- data_sf_sub
    }else{
      data_overlay <- rbind(data_overlay, data_sf_sub)
    }
    # Cleanup
    remove(obs_shape)
    remove(data_sf_sub)
    gc()
  }
}

## Calculate threshold parameters ####
data_overlay_unfiltered <- data_overlay %>% 
  group_by(.data$acceptedTaxonKey, .data$Classification) %>% 
  add_tally(name = "n_climate") %>% 
  ungroup() %>% 
  group_by(.data$acceptedTaxonKey) %>% 
  add_tally(name = "n_totaal") %>% 
  ungroup() %>% 
  mutate(perc_climate = .data$n_climate/.data$n_totaal) %>% 
  distinct(.data$acceptedTaxonKey, .data$Classification, 
           .keep_all = TRUE) %>% 
  left_join(SPECIES, by = c("acceptedTaxonKey" = "TK_2")) %>% 
  select(.data$taxonKey, 
         .data$acceptedScientificName, 
         .data$Classification, 
         .data$Description,
         .data$n_climate, 
         .data$n_totaal, 
         .data$perc_climate)

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
      rename(GRIDCODE = .data$gridcode) 
  }
  
  shape@data <- shape@data %>% 
    mutate(GRIDCODE = as.integer(.data$GRIDCODE)) 
  
  girdcode_intersect <- intersect(shape, region_shape)
  
  for(g in girdcode_intersect@data$GRIDCODE){
    output <- output %>% 
      add_row(scenario = s,
              KG_GridCode = g)
  }
}

output_1 <- output %>% 
  dplyr::filter(grepl(pattern = "Beck", .data$scenario)) %>% 
  left_join(KG_Beck, by = c("KG_GridCode" = "GRIDCODE"))

output_2 <- output %>% 
  dplyr::filter(!grepl(pattern = "Beck", .data$scenario)) %>% 
  left_join(KG_Rubel_Kotteks_Legend, by = c("KG_GridCode" = "GRIDCODE"))

output_final <- rbind(output_1, output_2)

future_climate <- output_final %>% 
  dplyr::filter(!is.na(.data$Classification))

# Per scenario filter ####

cm <- data.frame()

for(b in unique(future_climate$scenario)){
  future_scenario <- future_climate %>% 
    dplyr::filter(.data$scenario == b)
  
  cm_int <- data_overlay_unfiltered %>% 
    dplyr::filter(.data$Classification %in% future_scenario$Classification) %>% 
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
  dplyr::filter(.data$n_totaal >= n_limit,
                .data$perc_climate >= cm_limit)

# MAPS ####
if(maps == TRUE){
  ## map current climate suitability ####
  
  # Get Current climate
  current_climate_shape <- observed$`1980-2016`
  
  current_climate_shape@data <- current_climate_shape@data %>% 
    mutate(gridcode = as.double(.data$gridcode)) %>% 
    left_join(legends$KG_Beck, by = c("gridcode" = "GRIDCODE"))
  
  sea <- subset(current_climate_shape, is.na(current_climate_shape$Classification))
  
  # Combine climate shape with climate matched observations
  current_climate <- current_climate_shape
  
  for(t in taxon_key){
    temp_data <- data_overlay_unfiltered %>% 
      dplyr::filter(.data$taxonKey == t) %>% 
      select(-.data$Description)
    
    species <- unique(temp_data$acceptedScientificName)
    
    if(is_empty(species)){
      next
    }else{
      temp_climate <- sp::merge(current_climate_shape, temp_data, 
                                by = "Classification",
                                all.y = TRUE,
                                duplicateGeoms = TRUE)
      
      temp_climate@data <- temp_climate@data %>% 
        mutate(taxon_key = t,
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
    mutate(popup = paste0("<strong>Classification: </strong>", .data$Description, " (", .data$Classification, ")",
                          "</br><strong>ScientificName: </strong>", 
                          .data$acceptedScientificName,
                          "</br><strong>%obs in climate: </strong>", 
                          round(.data$perc_climate*100, 2), "%"))
  
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
                fillColor = ~pal_current(.data$perc_climate),
                fillOpacity = 0.8,
                stroke = TRUE,
                weight = 0.5,
                group = ~.data$acceptedScientificName,
                popup = ~.data$popup) %>% 
    addCircleMarkers(data = data_sp,
                     group = ~.data$acceptedScientificName,
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
    addLayersControl(baseGroups = ~.data$acceptedScientificName) %>% 
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
        mutate(gridcode = as.double(.data$gridcode)) %>% 
        left_join(legends$KG_Beck, by = c("gridcode" = "GRIDCODE"))
    }else{
      scenario_shape@data <- scenario_shape@data %>% 
        mutate(GRIDCODE = as.double(.data$GRIDCODE)) %>% 
        left_join(KG_Rubel_Kotteks_Legend, by = c("GRIDCODE"))
    }
    
    # Combine climate shape with climate matched observations
    temp_shape <- scenario_shape
    
    for(t in taxon_key){
      
      temp_data <- data_overlay_unfiltered %>% 
        dplyr::filter(.data$taxonKey == t) %>% 
        select(-.data$Description)
      
      species <- unique(temp_data$acceptedScientificName)
      
      if(is_empty(species)){
        next
      }else{
        temp_climate <- sp::merge(scenario_shape, temp_data, 
                                  by = "Classification",
                                  all.y = TRUE,
                                  duplicateGeoms = TRUE)
        
        temp_climate@data <- temp_climate@data %>% 
          mutate(taxon_key = t,
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
      mutate(popup = paste0("<strong>Classification: </strong>", .data$Description, " (", .data$Classification, ")", 
                            "</br><strong>ScientificName: </strong>", 
                            .data$acceptedScientificName,
                            "</br><strong>%obs in climate: </strong>", 
                            round(.data$perc_climate*100, 2), "%"))
    
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
      addCircleMarkers(data = data_sp,
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
  
  single_species_maps <- list_along(taxon_key)
  names(single_species_maps) <- taxon_key
  
  for (i in 1:length(taxon_key)) {
    
    t <- taxon_key[i]
    
    temp_data <- data_overlay_unfiltered %>% 
      dplyr::filter(taxon_key == t) %>% 
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
            mutate(GRIDCODE = as.double(.data$gridcode),
                   ID = .data$Id) %>% 
            select(-gridcode, -Id) %>% 
            left_join(legends$KG_Beck, by = "GRIDCODE")
        }else{
          scenario_shape@data <- scenario_shape@data %>% 
            mutate(GRIDCODE = as.double(.data$GRIDCODE)) %>% 
            left_join(legends$KG_A1FI, by = "GRIDCODE")
        }
        
        temp_climate <- sp::merge(scenario_shape, temp_data, 
                                  by = "Classification",
                                  all.y = TRUE,
                                  duplicateGeoms = TRUE)
        
        temp_climate@data <- temp_climate@data %>% 
          mutate(taxon_key = t,
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
      mutate(popup = paste0("<strong>Classification: </strong>", .data$Description, " (", .data$Classification, ")", 
                            "</br><strong>ScientificName: </strong>", 
                            .data$acceptedScientificName,
                            "</br><strong>%obs in climate: </strong>", 
                            round(.data$perc_climate*100, 2), "%",
                            "</br><strong>scenario: </strong>",
                            .data$scenario))
    
    temp_shape <- subset(temp_shape, !is.na(temp_shape$Classification))
    
    # Subset observations
    data_sp_species_obs <- subset(data_sp, 
                                  data_sp$taxonKey == t)
    
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
      addCircleMarkers(data = data_sp_species_obs,
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
            spatial = data_sp,
            current_map = current_climate_map,
            future_maps = future_scenario_maps,
            single_species_maps = single_species_maps))
}
