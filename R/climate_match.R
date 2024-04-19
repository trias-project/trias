#' Create a set of climate matching outputs 
#' 
#' This function creates a set of climate matching outputs for a species or set
#' of species for a region or nation.
#' 
#' @param region (optional character) the full name of the target nation or 
#' region 
#' region can also be a custom region (sf object).
#' @param taxon_key (character or vector) containing GBIF - taxonkey(s)
#' @param zip_file (optional character) The path (inclu. extension) of a zipfile 
#' from a previous GBIF-download. This zipfile should contain data of the 
#' species specified by the taxon_key
#' @param scenario (character) the future scenarios we are interested in.
#'  (default) all future scenarios are used.
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
#' - `spatial` a sf object containing the observations used 
#' in the analysis
#' - `current_map` a leaflet object displaying the degree of wordlwide 
#' climate match with the climate from 1980 till 2016
#' - `future_maps` a list of leaflet objects for each future climate 
#' scenario, displaying the degree of climate match
#' - `single_species_maps` a list of leaflet objects per taxon_key displaying 
#' the current and future climate scenarios
#' 
#' @export
#' @importFrom dplyr %>% .data
#' @examples
#' \dontrun{
#' region <- "europe"
#' 
#' # provide GBIF taxon_key(s)
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
#'               n_limit = 90,
#'               cm_limit = 0.2
#' )
#' # calculate only data climate match outputs
#' # using a pre-downloaded zip_file
#' climate_match(region,
#'               taxon_key, 
#'               zip_file,
#'               n_limit = 90,
#'               cm_limit = 0.2,
#'               maps = FALSE
#' )
#' # calculate climate match outputs based 
#' # on human observations with a 100m 
#' # coordinate uncertainty
#' climate_match(region,
#'               taxon_key, 
#'               zip_file,
#'               n_limit = 90,
#'               cm_limit = 0.2,
#'               coord_unc = 100,
#'               BasisOfRecord = "HUMAN_OBSERVATION",
#'               maps = FALSE
#' )
#' }
climate_match <- function(region, 
                          taxon_key, 
                          zip_file,
                          scenario = "all",
                          n_limit,
                          cm_limit,
                          coord_unc, 
                          BasisOfRecord,
                          maps = TRUE) {
  # Checks ####
  ## Region ##
  if (missing(region)) {
    # no region is provided => worldwide
    region_shape <- rnaturalearth::ne_countries()
  } else {
    if (is.character(region)) {
      # region is a character => select region from worldmap
      
      region <- tolower(region)
      worldmap<- rnaturalearth::ne_countries()
     
      valid_countries <- tolower(unique(worldmap$name_long))
      
      
      if(region %in% valid_countries){
        region_shape <- subset(worldmap, tolower(worldmap$name_long) == region) 
      } else {
        valid_continents <- tolower(unique(worldmap$continent))
        if(region %in% valid_continents){
          region_shape <- subset(worldmap, tolower(worldmap$continent) == region) 
        } else {
          stop("the provided region is not valid")
        }
      }
    } else{
      # region is a environment object => get object into function 
      region_shape <- region
      
      if (inherits(region_shape, "SpatialPolygonsDataFrame")) {
        # region_shape is SpatialPolygonsDataFrame
        region_shape <- sf::st_as_sf(region_shape)
      }
      
      assertthat::assert_that(
        class(region_shape) %in% c("sf",
                                   "data.frame"),
        msg = paste(
          "Region is an invalid spatial object.",
          "Supported class: sf")
      )
    }
  }
  
  region_shape<-region_shape %>% sf::st_set_crs(4326)
  
  ## Species
  taxon_key <- as.numeric(unique(taxon_key))
  
  if (length(taxon_key) == 1) {
    if (is.na(taxon_key)) {
      stop("taxon_key is missing or not valid")
    }
  }
  
  taxon_key <- subset(taxon_key, !is.na(taxon_key))
  
  taxon_key_set1 <- rgbif::pred_in("taxonKey", taxon_key)
  
  # Download data ####
  
  if (base::missing(zip_file)) {
    rerun <- 2
  } else {
    if (!file.exists(zip_file)) {
      warning(paste0(zip_file, " cannot be found. Rerunning GBIF download"))
      rerun <- 2
    } else {
      rerun <- utils::menu(choices = c("no", "yes"), 
                               title = "rerun GBIF download?",
                               graphics = TRUE)
    }
  }
  
  if (rerun != 2 ) {
    data <- readr::read_tsv(unz(zip_file, "occurrence.txt"), 
                     col_types = c(decimalLatitude = readr::col_number(),
                                   decimalLongitude = readr::col_number(),
                                   establishmentMeans = readr::col_character()),
                     col_select = c("acceptedTaxonKey",
                                    "species",
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
      dplyr::filter(acceptedTaxonKey %in% taxon_key)
  }else{
    gbif_user <- get_cred("gbif_user")
    gbif_pwd <- get_cred("gbif_pwd")
    gbif_email <- get_cred("gbif_email")
    
    set1 <- rgbif::occ_download(taxon_key_set1, 
                                rgbif::pred("hasCoordinate", TRUE),
                                rgbif::pred_gt("year", 1900),
                                user = gbif_user, 
                                pwd = gbif_pwd, 
                                email = gbif_email)
    #Follow the status of the download    
    rgbif::occ_download_wait(set1)
    
    #Retrieve downloaded records
   data <- rgbif::occ_download_get(set1,overwrite = TRUE) %>%
      rgbif::occ_download_import() 
    
    #Retrieve citation of downloaded dataset
    print(rgbif::gbif_citation(rgbif::occ_download_meta(set1))$download)
    
    data <- data %>% 
      dplyr::select("acceptedTaxonKey",
                    "species",
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
    warning(paste0("No BasisOfRecord parameter was provided, using the following",
                   " values available in the data: ",paste(BasisOfRecord, collapse = ", ")))
  }
  
  SPECIES <- data %>% 
    dplyr::filter(taxonRank %in% c("SPECIES", "VARIETY"), 
                  taxonomicStatus == "ACCEPTED") %>% 
    dplyr::distinct(acceptedTaxonKey,
                    genus,
                    specificEpithet) %>% 
    dplyr::mutate(ASN_2 = paste(genus, specificEpithet)) %>% 
    dplyr::rename(TK_2 = acceptedTaxonKey) %>% 
    dplyr::distinct(TK_2, ASN_2) 
  
  data_redux <- data %>% 
    dplyr::mutate(acceptedScientificName = paste(genus,
                                                 specificEpithet)) %>% 
    dplyr::left_join(SPECIES, by = c("species" = "ASN_2")) %>% 
    dplyr::mutate(acceptedTaxonKey = TK_2) %>% 
    dplyr::filter(
      !is.na(acceptedTaxonKey),
      !is.na(eventDate), 
      !is.na(decimalLatitude),
      eventDate >= "1901-01-01",
      basisOfRecord %in% BasisOfRecord,
      coordinateUncertaintyInMeters <= coord_unc | 
        is.na(coordinateUncertaintyInMeters),
      occurrenceStatus == "PRESENT") %>% 
    dplyr::select(gbifID, 
                  year, 
                  acceptedTaxonKey, 
                  acceptedScientificName, 
                  decimalLatitude, 
                  decimalLongitude, 
                  coordinateUncertaintyInMeters, 
                  countryCode) %>% 
    dplyr::mutate(year_cat = dplyr::case_when(year <= 1925 ~ "1901-1925",
                                       year <= 1950 ~ "1926-1950",
                                       year <= 1975 ~ "1951-1975",
                                       year <= 2000 ~ "1976-2000",
                                       year > 2000 ~ "2001-2025",
                                       TRUE ~ NA_character_)) %>% 
    dplyr::group_by(year_cat, 
                    acceptedTaxonKey, 
                    decimalLatitude, 
                    decimalLongitude) %>% 
    dplyr::summarize(n_obs = dplyr::n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(SPECIES, by = c("acceptedTaxonKey" = "TK_2")) %>% 
    dplyr::rename(acceptedScientificName = ASN_2)
  
  if(nrow(data_redux) == 0){
    stop(
      paste0("No useable data for ",
             paste(taxon_key, collapse = ","),
             paste(" left after filters.",
                   "Try omiting or changing the filter setup.")
      )
    )
  }
  
  remove(data)
  
 data_sf <- data_redux %>% 
    sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326, remove = FALSE)
  
  
  
 ### Climate matching occurrence data ####
 ### Link each occurrence point to the climatic region and corresponding gridcode at the time of its observation
  timeperiodes <- c("1901-1925", 
                    "1926-1950",
                    "1951-1975",
                    "1976-2000",
                    "2001-2025")
  
  data_overlay <- data.frame()
  
  suppressWarnings(
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
      dplyr::filter(year_cat == t)
    
    print(nrow(data_sf_sub))
    
    # Overlay with observed climate
    if(nrow(data_sf_sub)>0){
      if(start <= 2000){
        obs_shape <- sf::st_as_sf(observed[[t]])
      }else{
        t <- "2001-2025-A1FI"
        obs_shape <- sf::st_as_sf(future[[t]])
      }
      
      if(nrow(data_sf_sub) <= 11000){
        data_sf_sub$GRIDCODE <- suppressMessages(apply(sf::st_intersects(obs_shape, 
                                                        data_sf_sub, 
                                                        sparse = FALSE), 2, 
                                      function(col) {obs_shape[which(col),
                                      ]$GRIDCODE}))
        pb <- utils::txtProgressBar(min = 0, 
                             max = length(data_sf_sub$GRIDCODE), 
                             style = 3)
        for(i in 1:length(data_sf_sub$GRIDCODE)){
          utils::setTxtProgressBar(pb, i)
          data_sf_sub$GRIDCODE2[i] <- as.double(data_sf_sub$GRIDCODE[[i]][1])
        }
        
        data_sf_sub <- data_sf_sub %>% 
          dplyr::mutate(GRIDCODE = as.double(GRIDCODE2)) %>% 
          dplyr::select(-GRIDCODE2) %>% 
          dplyr::left_join(KG_Rubel_Kotteks_Legend, by = c("GRIDCODE")) 
      }else{
        cuts <- ceiling(nrow(data_sf_sub)/10000)
        pb_2 <- utils::txtProgressBar(min = 0, 
                               max = cuts, 
                               style = 3)
        
        data_overlay_sub <- data.frame()
        
        for(x in 1:cuts){
          print(paste0(x, "/", cuts))
          utils::setTxtProgressBar(pb_2, x)
          y <- ((x - 1)*10000) + 1 
          z <- x * 10000
          if(z < nrow(data_sf_sub)){
            data_sf_sub_sub <- data_sf_sub[y:z,]
          }else{
            data_sf_sub_sub <- data_sf_sub[y:nrow(data_sf_sub),]
          }
          data_sf_sub_sub$GRIDCODE <- apply(
            sf::st_intersects(obs_shape, 
                              data_sf_sub_sub, 
                              sparse = FALSE), 2, 
                              function(col) {
                                obs_shape[which(col), ]$GRIDCODE})
          
          pb_3 <- utils::txtProgressBar(min = 0, 
                                 max = length(data_sf_sub_sub$GRIDCODE), 
                                 style = 3)
          
          for (i in 1:length(data_sf_sub_sub$GRIDCODE)) {
            utils::setTxtProgressBar(pb_3, i)
            data_sf_sub_sub$GRIDCODE2[i] <- as.double(
              data_sf_sub_sub$GRIDCODE[[i]][1]
            )
          }
          
          data_sf_sub_sub <- data_sf_sub_sub %>% 
            dplyr::mutate(GRIDCODE = as.double(GRIDCODE2)) %>% 
            dplyr::select(-GRIDCODE2) %>% 
            dplyr::left_join(KG_Rubel_Kotteks_Legend, by = c("GRIDCODE")) 
          
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
    } else {
      warning(paste0("No data was present in the GBIF dataset for ", t))
      next
    }
    # Recombine timeperiodes
    if (nrow(data_overlay) == 0) {
      data_overlay <- data_sf_sub
    } else {
      data_overlay <- rbind(data_overlay, data_sf_sub)
    }
    # Cleanup
    remove(obs_shape)
    remove(data_sf_sub)
    gc()
  })
  
  #Drop geometry column
  data_overlay<-sf::st_drop_geometry(data_overlay)
  
  ## Calculate threshold parameters ####
  data_overlay_unfiltered <- data_overlay %>% 
    dplyr::group_by(acceptedTaxonKey, Classification) %>% 
    dplyr::mutate(n_climate = sum(n_obs, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(acceptedTaxonKey) %>% 
    dplyr::mutate(n_totaal = sum(n_obs, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(perc_climate = n_climate/n_totaal) %>% 
    dplyr::distinct(acceptedTaxonKey, Classification,
                    .keep_all = TRUE)  %>% 
    dplyr::select(acceptedTaxonKey, 
                  acceptedScientificName, 
                  Classification, 
                  Description,
                  n_climate, 
                  n_totaal, 
                  perc_climate)
  
  
  ###Create a dataframe with the climate regions, their gridcodes and corresponding information that occur in the region of 
  ###interest under each future scenario  ###
  
  # Determine future scenarios ####
  scenarios <- c("2001-2025-A1FI",
                 "2026-2050-A1FI",
                 "2051-2075-A1FI",
                 "2071-2100_Beck",
                 "2076-2100-A1FI")
  
  # Create empty output 
  output <- data.frame() %>% 
    dplyr::mutate(scenario = "",
                  KG_GridCode = as.integer(""))
  
  # Calculate KG codes 
  sf::sf_use_s2(FALSE)
  for (s in scenarios) {
    shape <- sf::st_as_sf(future[[s]])%>% 
      sf::st_set_crs(4326)
    if (c("gridcode") %in% colnames(shape)) {
      shape <- shape %>% 
        dplyr::rename(GRIDCODE = gridcode) 
    }
    
    shape <- shape %>% 
      dplyr::mutate(GRIDCODE = as.integer(GRIDCODE)) 
    
    #Rebuild the geometry as planar geometry, needed to get st_intersection to work
   region_shape<-sf::st_make_valid(region_shape)
base::suppressMessages(base::suppressWarnings(gridcode_intersect<-sf::st_intersection(shape,region_shape)))
    
    #Take the gridcodes present in the region of interest under each scenario and add them to the output dataframe
    for (g in gridcode_intersect$GRIDCODE) {
      output <- output %>% 
        dplyr::add_row(scenario = s,
                       KG_GridCode = g)
    }
  }
  
  sf::sf_use_s2(TRUE)
  
  output_1 <- output %>% 
    dplyr::filter(grepl(pattern = "Beck", scenario)) %>% 
    dplyr::left_join(KG_Beck, by = c("KG_GridCode" = "GRIDCODE"))
  
  output_2 <- output %>% 
    dplyr::filter(!grepl(pattern = "Beck", scenario)) %>% 
    dplyr::left_join(y = KG_Rubel_Kotteks_Legend,
                     by = c("KG_GridCode" = "GRIDCODE")
    )
  
  output_final <- rbind(output_1, output_2)
  
  future_climate <- output_final %>% 
    dplyr::filter(!is.na(Classification))
  
  
  ###Link the previous dataframe (with the climatic regions  under all
  ### future scenarios) to the data regarding percentage climate matching in each of these regions
  # Per scenario filter ####
  cm <- data.frame()
  
  for (b in unique(future_climate$scenario)) {
    future_scenario <- future_climate %>% 
      dplyr::filter(scenario == b)
    
    cm_int <- data_overlay_unfiltered %>% 
      dplyr::filter(
        Classification %in% future_scenario$Classification
      ) %>% 
      dplyr::mutate(scenario = b)
    
    if (nrow(cm) == 0) {
      cm <- cm_int
    } else {
      cm <- rbind(cm, cm_int)
    }
  }
  
  # Thresholds ####
  if (missing(n_limit)) {
    warning("no n_totaal threshold was provided. defaults to 0!")
    n_limit <- 0
  }
  if (missing(cm_limit)) {
    warning("no perc_climate threshold was provided. defaults to 0%!")
    cm_limit <- 0
  }
  
  data_overlay_scenario_filtered <- cm %>% 
    dplyr::filter(n_totaal >= n_limit,
                  perc_climate >= cm_limit)
  
  
  #### MAPS ####
  if (maps == TRUE) {
    ## map current climate suitability ####
    
    # Get Current climate
    current_climate_shape <- sf::st_as_sf(observed$`1980-2016`)
    
    current_climate_shape <- current_climate_shape %>% 
      dplyr::mutate(gridcode = as.double(gridcode)) %>% 
      dplyr::left_join(legends$KG_Beck, by = c("gridcode" = "GRIDCODE"))
    
    sea <- subset(
      current_climate_shape,
      is.na(current_climate_shape$Classification)
    )
    
    # Combine climate shape with climate matched observations
    current_climate <- current_climate_shape
    
    for(t in taxon_key){
      temp_data <- data_overlay_unfiltered %>% 
        dplyr::filter(acceptedTaxonKey == t) %>% 
        dplyr::select(-Description)
      
      species <- unique(temp_data$acceptedScientificName)
      
      if(rlang::is_empty(species)){
        next
      }else{
        temp_climate <- dplyr::left_join(current_climate_shape,
                                  data.frame(temp_data), 
                                  by = "Classification")
        
        temp_climate <- temp_climate %>% 
          dplyr::mutate(taxon_key = t,
                        acceptedScientificName = species)
        
        if(ncol(current_climate)!=ncol(temp_climate)){
          current_climate <- temp_climate
        }else{
          current_climate <- rbind(current_climate, temp_climate)
        }
      }
    }
    
    
    
    current_climate <- current_climate %>% 
      dplyr::mutate (popup="") 
    
   #Create a popup for each climatic region where %obs in climate shows the percentage when this value is present in the datafram
  #When this field is empty in the dataframe, this value becomes 0% in the popup
  for(x in 1: nrow(current_climate)){
        if (!is.na(current_climate$perc_climate[x])) {
          current_climate$popup[x] =  paste0("<strong>Classification: </strong>", 
                                             current_climate$Description[x], " (",
                                             current_climate$Classification[x], ")",
                       "</br><strong>ScientificName: </strong>", "<em>",
                       current_climate$acceptedScientificName[x], "</em>",
                       "</br><strong>%obs in climate: </strong>", 
                       round(current_climate$perc_climate[x]*100, 2), "%")
        }else{
          current_climate$popup[x] =  paste0("<strong>Classification: </strong>", 
                                             current_climate$Description[x], " (",
                                             current_climate$Classification[x], ")",
                 "</br><strong>ScientificName: </strong>", "<em>",
                 current_climate$acceptedScientificName[x], "</em>",
                 "</br><strong>%obs in climate: </strong> 0%")
        }
      }
          
    
    current_climate <- subset(
      current_climate,
      !is.na(current_climate$Classification)
    )
    
    # create color palette 
    pal_current <- leaflet::colorBin(palette = "YlOrRd", 
                                     domain = c(NA, seq(from = 0, to = 1, by = 0.1)),
                                     
                                     na.color =  "#f7f7f7",
                                     
                                     bins = 10,
                                     reverse = FALSE)
   
    
    # create current climate map
    current_climate_map <- suppressWarnings(leaflet::leaflet(current_climate, options = leaflet::leafletOptions(minZoom = 0.75, maxBoundsViscosity= 1.0)) %>% 
      leaflet::addProviderTiles( "CartoDB.VoyagerNoLabels",
                                 options=list(noWrap=TRUE))%>%
      leaflet::setMaxBounds( lng1 = -180
                             , lat1 = -90
                             , lng2 = 180
                             , lat2 = 90 )%>%
      leaflet::setView( lng = 0
                        , lat = 0
                        , zoom = 1 ) %>%
      leaflet::addPolygons(color = "#bababa",
                           fillColor = ~pal_current(perc_climate),
                           fillOpacity = 1,
                           stroke = TRUE,
                           weight = 0.8,
                           group = ~current_climate$acceptedScientificName,
                           popup = ~current_climate$popup,
                           highlightOptions = leaflet::highlightOptions(weight = 2,
                                                                        color = "#6b6b6b",
                                                                        bringToFront = FALSE)) %>% 
      leaflet::addCircleMarkers(data = data_sf,
                                group = ~data_sf$acceptedScientificName,
                                color = "black",
                                radius = 1) %>% 
      leaflet::addLegend(colors = "black",
                         labels = "Observations",
                         position = "bottomleft") %>% 
      leaflet::addLegend(
        colors=c("#f7f7f7","#FFFFCC" ,"#FFEFA5", "#FEDD7F" ,"#FFBF5A", "#FE9E43", "#FD7434", "#F44025" ,"#DA151F", "#B60026", "#800026"),
        labels = c("0",
                   "0< - 10", 
                   "10 - 20",
                   "20 - 30",
                   "30 - 40",
                   "40 - 50",
                   "50 - 60",
                   "60 - 70",
                   "70 - 80",
                   "80 - 90",
                   "90 - 100"),
        position = "bottomleft",
        title = "Climate match (%)</br><span style='font-weight:lighter;'>Current climate</span>") %>% 
      leaflet::addLayersControl(
        baseGroups = ~data_sf$acceptedScientificName))
    
    ## map future climate suitability ####
    
    
    # Create scenario maps
    future_scenario_maps <- purrr::rep_along(scenarios, list())
    names(future_scenario_maps) <- scenarios
    
    for (i in 1:length(scenarios)) {
      
      s <- scenarios[i]
      
      # Get scenario shape
      scenario_shape <- sf::st_as_sf(future[[s]])
      
      # Attach legends
      if(grepl("Beck", s)){
        scenario_shape <- scenario_shape %>% 
          dplyr::mutate(gridcode = as.double(gridcode)) %>% 
          dplyr::left_join(legends$KG_Beck, by = c("gridcode" = "GRIDCODE"))
      }else{
        scenario_shape <- scenario_shape %>% 
          dplyr::mutate(GRIDCODE = as.double(GRIDCODE)) %>% 
          dplyr::left_join(KG_Rubel_Kotteks_Legend, by = c("GRIDCODE"))
      }
      
      # Combine climate shape with climate matched observations
      temp_shape <- scenario_shape
      
      for(t in taxon_key){
        
        temp_data <- data_overlay_unfiltered %>% 
          dplyr::filter(acceptedTaxonKey == t) %>% 
          dplyr::select(-Description)
        
        species <- unique(temp_data$acceptedScientificName)
        
        if(rlang::is_empty(species)){
          next
        }else{
          temp_climate <- dplyr::left_join(scenario_shape, as.data.frame(temp_data), 
                                    by = "Classification")
          
          temp_climate <- temp_climate %>% 
            dplyr::mutate(taxon_key = t,
                          acceptedScientificName = species)
          
          if(ncol(temp_shape) != ncol(temp_climate)){
            temp_shape <- temp_climate
          }else{
            temp_shape <- rbind(temp_shape, temp_climate)
          }
        }
      }
      
      temp_shape <- temp_shape %>% 
        dplyr::mutate (popup="") 
      
      #Create a popup for each climatic region where %obs in climate shows the percentage when this value is present in the datafram
      #When this field is empty in the dataframe, this value becomes 0% in the popup
      for(x in 1: nrow(temp_shape)){
        if (!is.na(temp_shape$perc_climate[x])) {
          temp_shape$popup[x] =  paste0("<strong>Classification: </strong>", 
                                             temp_shape$Description[x], " (",
                                             temp_shape$Classification[x], ")",
                                             "</br><strong>ScientificName: </strong>", "<em>",
                                             temp_shape$acceptedScientificName[x], "</em>",
                                             "</br><strong>%obs in climate: </strong>", 
                                             round(temp_shape$perc_climate[x]*100, 2), "%")
        }else{
          temp_shape$popup[x] =  paste0("<strong>Classification: </strong>", 
                                             temp_shape$Description[x], " (",
                                             temp_shape$Classification[x], ")",
                                             "</br><strong>ScientificName: </strong>", "<em>",
                                             temp_shape$acceptedScientificName[x], "</em>",
                                             "</br><strong>%obs in climate: </strong> 0%")
        }
      }
      
      
      temp_shape <- subset(temp_shape, !is.na(temp_shape$Classification))
      
      
      # Add layer to map
      scenario_map <- suppressWarnings(leaflet::leaflet(temp_shape, options = leaflet::leafletOptions(minZoom = 0.75, maxBoundsViscosity= 1.0)) %>% 
        leaflet::addProviderTiles( "CartoDB.VoyagerNoLabels",
                                   options=list(noWrap=TRUE))%>%
        leaflet::setMaxBounds( lng1 = -180
                               , lat1 = -90
                               , lng2 = 180
                               , lat2 = 90 )%>%
        leaflet::setView( lng = 0
                          , lat = 0
                          , zoom = 1 ) %>%
        leaflet::addPolygons(data = temp_shape,
                             color = "#bababa",
                             fillColor = ~pal_current(temp_shape$perc_climate),
                             fillOpacity = 1,
                             stroke = TRUE,
                             weight = 0.8,
                             group = ~temp_shape$acceptedScientificName,
                             popup = ~temp_shape$popup,
                             highlightOptions = leaflet::highlightOptions(weight = 2,
                                                                          color = "#6b6b6b",
                                                                          bringToFront = FALSE)) %>% 
        leaflet::addCircleMarkers(data = data_sf,
                                  group = ~data_sf$acceptedScientificName,
                                  color = "black",
                                  radius = 1) %>% 
        leaflet::addLegend(colors = "black",
                           labels = "Observations",
                           position = "bottomleft")%>% 
        leaflet::addLayersControl(
          baseGroups= ~temp_shape$acceptedScientificName) %>% 
          leaflet::addLegend(
            colors=c("#f7f7f7","#FFFFCC" ,"#FFEFA5", "#FEDD7F" ,"#FFBF5A", "#FE9E43", "#FD7434", "#F44025" ,"#DA151F", "#B60026", "#800026"),
            labels = c("0",
                       "0< - 10", 
                       "10 - 20",
                       "20 - 30",
                       "30 - 40",
                       "40 - 50",
                       "50 - 60",
                       "60 - 70",
                       "70 - 80",
                       "80 - 90",
                       "90 - 100"),
            position = "bottomleft",
          title = paste0("<strong>Climate match (%)</strong></br><span style='font-weight:lighter;'>"
                         , s, "</span>"))) 
      
      
      future_scenario_maps[[i]] <- scenario_map
    }
    
    ## Single species - climate suitability maps ####
    
    
    # Create single species maps
    
    scenarios_2 <- c("1980-2016", scenarios)
    
    single_species_maps <- purrr::rep_along(taxon_key, list())
    names(single_species_maps) <- taxon_key
    
    for (i in 1:length(taxon_key)) {
      
      t <- taxon_key[i]
      
      temp_data <- data_overlay_unfiltered %>% 
        dplyr::filter(acceptedTaxonKey == t) %>% 
        dplyr::select(-Description)
      
      species <- unique(temp_data$acceptedScientificName)
      
      if(rlang::is_empty(species)){
        next
      }else{
        temp_shape <- data.frame()
        
        for(s in scenarios_2){
          
          if(s == "1980-2016"){
            scenario_shape <- sf::st_as_sf(observed[[s]])
          }else{
            scenario_shape <- sf::st_as_sf(future[[s]])
          }
          if (grepl("Beck", s) | s == "1980-2016") {
            scenario_shape<- scenario_shape %>% 
              dplyr::mutate(GRIDCODE = as.double(gridcode),
                            ID = Id) %>% 
              dplyr::select(-c(gridcode, Id)) %>% 
              dplyr::left_join(legends$KG_Beck, by = "GRIDCODE")
          }else{
            scenario_shape <- scenario_shape %>% 
              dplyr::mutate(GRIDCODE = as.double(GRIDCODE)) %>% 
              dplyr::left_join(legends$KG_A1FI, by = "GRIDCODE")
          }
          
          temp_climate <- dplyr::left_join(scenario_shape, as.data.frame(temp_data), 
                                    by = "Classification")
                                   
          
          temp_climate <- temp_climate %>% 
            dplyr::mutate(taxon_key = t,
                          acceptedScientificName = species,
                          scenario = s)
          
          if (length(unique(temp_shape$scenario))==0){
            temp_shape <- temp_climate
          }else{
            temp_shape <- rbind(temp_shape, temp_climate)
          }
        }
      }
      
      temp_shape <- temp_shape %>% 
        dplyr::mutate (popup="") 
      
      #Create a popup for each climatic region where %obs in climate shows the percentage when this value is present in the datafram
      #When this field is empty in the dataframe, this value becomes 0% in the popup
      for(x in 1: nrow(temp_shape)){
        if (!is.na(temp_shape$perc_climate[x])) {
          temp_shape$popup[x] =  paste0("<strong>Classification: </strong>", 
                                        temp_shape$Description[x], " (",
                                        temp_shape$Classification[x], ")",
                                        "</br><strong>ScientificName: </strong>", "<em>",
                                        temp_shape$acceptedScientificName[x], "</em>",
                                        "</br><strong>%obs in climate: </strong>", 
                                        round(temp_shape$perc_climate[x]*100, 2), "%")
        }else{
          temp_shape$popup[x] =  paste0("<strong>Classification: </strong>", 
                                        temp_shape$Description[x], " (",
                                        temp_shape$Classification[x], ")",
                                        "</br><strong>ScientificName: </strong>", "<em>",
                                        temp_shape$acceptedScientificName[x], "</em>",
                                        "</br><strong>%obs in climate: </strong> 0%")
        }
      }
      
      temp_shape <- subset(temp_shape, !is.na(temp_shape$Classification))
      
      # Subset observations
      data_sf_species_obs <- data_sf %>% 
        dplyr::filter(acceptedTaxonKey == t)
      
      # Add layer to map
      scenario_map <- suppressWarnings(leaflet::leaflet(temp_shape, options = leaflet::leafletOptions(minZoom = 0.75, maxBoundsViscosity= 1.0)) %>% 
        leaflet::addProviderTiles( "CartoDB.VoyagerNoLabels",
                                   options=list(noWrap=TRUE))%>%
        leaflet::setMaxBounds( lng1 = -180
                               , lat1 = -90
                               , lng2 = 180
                               , lat2 = 90 )%>%
        leaflet::setView( lng = 0
                          , lat = 0
                          , zoom = 1 ) %>%
        leaflet::addMapPane("background", zIndex = 400) %>%  
        leaflet::addMapPane("foreground", zIndex = 500) %>% 
        leaflet::addPolygons(
          data = temp_shape,
          color = "#bababa",
          fillColor = ~pal_current(perc_climate),
          fillOpacity = 1,
          stroke = TRUE,
          weight = 0.8,
          group = ~temp_shape$scenario,
          popup = ~temp_shape$popup,
          options = leaflet::pathOptions(pane = "background"),
          highlightOptions = leaflet::highlightOptions(weight = 2,
                                                       color = "#6b6b6b",
                                                       bringToFront = FALSE)) %>% 
        leaflet::addCircleMarkers(
          data = data_sf_species_obs,
          color = "black",
          radius = 1,
          options = leaflet::pathOptions(pane = "foreground")) %>% 
          leaflet::addLegend(
            colors=c("#f7f7f7","#FFFFCC" ,"#FFEFA5", "#FEDD7F" ,"#FFBF5A", "#FE9E43", "#FD7434", "#F44025" ,"#DA151F", "#B60026", "#800026"),
            labels = c("0",
                       "0< - 10", 
                       "10 - 20",
                       "20 - 30",
                       "30 - 40",
                       "40 - 50",
                       "50 - 60",
                       "60 - 70",
                       "70 - 80",
                       "80 - 90",
                       "90 - 100"),
            position = "bottomleft",
            title =  paste0("<strong>Climate match (%)</strong><br><em><span style='font-weight:lighter;'>",
                                           temp_shape$acceptedScientificName[1], "</span>")) %>% 
        leaflet::addLayersControl(baseGroups = ~temp_shape$scenario)%>% 
      leaflet::addLegend(colors = "black",
                           labels = "Observations",
                           position = "bottomleft"))
      
      single_species_maps[[i]] <- scenario_map
    }  
  }else{
    message("maps are disabled")
    current_climate_map <- NULL
    future_scenario_maps <- NULL
    single_species_maps <- NULL
  }
  
  
  
  # Return ####
  return(list(unfiltered = as.data.frame(data_overlay_unfiltered), 
              cm =as.data.frame(cm),
              filtered = as.data.frame(data_overlay_scenario_filtered),
              future = as.data.frame(future_climate),
              spatial = data_sf,
              current_map = current_climate_map,
              future_maps = future_scenario_maps,
              single_species_maps = single_species_maps))
}
