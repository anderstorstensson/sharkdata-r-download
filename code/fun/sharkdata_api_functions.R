# Install and load required packages
# install.packages("dplyr")
# install.packages("jsonlite")
# install.packages("sf")

library(tidyverse)
library(sf)
library(jsonlite)

# Function to load sharkdata
load_sharkdata <- function() {
  fromJSON('https://sharkdata.smhi.se/datasets/list.json')
}

# Function for listing available dataset types
load_dataset_types <- function() {
  datasets <- load_sharkdata()
  dataset_types <- unique(datasets$datatype)
  return(dataset_types)
}

# Function for loading available dataset names
load_dataset_names <- function(dataset_type, year = NA, data_deliverer = NA) {
  datasets <- load_sharkdata()
  
  filtered_datasets <- datasets %>%
    filter(datatype == dataset_type) %>%
    year_filter(year) %>%
    data_deliverer_filter(data_deliverer)
  
  return(filtered_datasets)
}

# Function to filter datasets based on year
year_filter <- function(datasets, year) {
  if (!any(is.na(year))) {
    datasets %>% filter(grepl(paste(year, collapse="|"), dataset_name))
  } else {
    datasets
  }
}

# Function to filter datasets based on data deliverer
data_deliverer_filter <- function(datasets, data_deliverer) {
  if (!any(is.na(data_deliverer))) {
    datasets %>% filter(grepl(paste(data_deliverer, collapse="|"), dataset_name))
  } else {
    datasets
  }
}

# Function for downloading datasets
download_sharkdata <- function(dataset_names) {
  datasets <- load_sharkdata()
  filtered_datasets <- datasets %>%
    filter(dataset_name %in% dataset_names)
  
  data_shark <- data.frame()
  
  if (length(unique(filtered_datasets$datatype)) > 1) {
    stop("Only one datatype can be downloaded at a time")
  } else {
    validate_dataset_names(filtered_datasets, dataset_names)
    
    # Set up progress bar
    pb <- txtProgressBar(min = 0, max = length(dataset_names), style = 3)
    
    for (i in 1:length(dataset_names)) {
      temp <- download_file(dataset_names[i])
      data_ix <- read_data(temp, filtered_datasets, dataset_names[i])
      
      # Check if dataset_file_name has unique values
      if (length(unique(data_ix$dataset_file_name)) == 1) {
        # Convert all columns to character
        data_ix <- mutate_all(data_ix, as.character)
        
        data_shark <- bind_rows(data_shark, data_ix)
      } else {
        warning("Skipping dataset ", dataset_names[i], " due to missing or multiple dataset_file_name entries.")
      }
      
      # Update progress bar at the end of each iteration
      setTxtProgressBar(pb, i)
    }
  }
  
  # Close progress bar
  close(pb)
  
  # Automatically convert columns to appropriate types
  data_shark <- type_convert(data_shark, col_types = cols())
  
  # Add Dyntaxa higher taxonomy from file
  data_shark <- data_shark %>% 
    left_join(load_dyntaxa_taxonomy(data_shark$dyntaxa_id), by = "dyntaxa_id")
  
  # Add WoRMS higher taxonomy from file
  data_shark <- data_shark %>% 
    left_join(load_worms_taxonomy(data_shark$aphia_id), by = "aphia_id")
  
  # Extract geographical information from coordinates
  geographical_info <- get_geographical_info(data_shark$sample_latitude_dd, 
                                             data_shark$sample_longitude_dd) %>%
    distinct()
  
  # Join data with geographical information 
  data_shark <- data_shark %>%
    left_join(geographical_info,
              by = c("sample_latitude_dd", "sample_longitude_dd"))
  
  return(data_shark)
}

# Helper function to download file
download_file <- function(dataset_name) {
  temp <- tempfile()
  download.file(paste("https://sharkdata.smhi.se/datasets/",
                      dataset_name,
                      "/shark_archive.zip",
                      sep = ""),
                temp,
                mode = "wb",
                quiet = TRUE)
  return(temp)
}

# Helper function to read data
read_data <- function(temp, filtered_datasets, dataset_name) {
  dataset_file_name <- filtered_datasets$dataset_file_name[filtered_datasets$dataset_name == dataset_name]
  
  data_ix <- read_delim(unz(temp, "shark_data.txt"),
                        locale = locale(encoding = "ISO8859-1"),
                        col_types = cols(),
                        progress = FALSE)
  
  data_ix$dataset_file_name <- dataset_file_name
  return(data_ix)
}

# Helper function to validate dataset names
validate_dataset_names <- function(filtered_datasets, dataset_names) {
  if (!all(dataset_names %in% filtered_datasets$dataset_name)) {
    stop("Not all dataset names are in the database")
  }
}

# Function to load higher taxonomy from Dyntaxa file
load_dyntaxa_taxonomy <- function(dyntaxa_id_input) {
  shark_species_list <- read_species_list("shark_species_list_utf8.txt")
  species <- gather_species_info(shark_species_list)
  shark_species_list <- add_species_info(shark_species_list, species) %>%
    filter(dyntaxa_id %in% dyntaxa_id_input)

  return(shark_species_list)
}

# Function to read and convert species list
read_species_list <- function(filename) {
  read_delim(file.path("config/shark_download_config", filename), 
             delim = "\t", 
             col_names = TRUE, 
             col_types = cols(),
             progress = FALSE,
             locale = readr::locale(encoding = "UTF-8")) %>%
    select(`Taxon-id`,
           Taxontyp,
           `Gällande namn`,
           Klass,
           Ordning,
           Familj,
           Släkte) %>%
    rename("dyntaxa_id" = `Taxon-id`,
           "taxon_name" = `Gällande namn`,
           "taxon_class" = Klass,
           "taxon_order" = Ordning,
           "taxon_family" = Familj,
           "taxon_genus" = Släkte)
}

# Function to gather species information
gather_species_info <- function(shark_species_list) {
  species <- shark_species_list %>%
    filter(Taxontyp == "species") %>%
    select(taxon_name) %>%
    mutate(taxon_species = taxon_name)
  
  species_form <- shark_species_list %>%
    filter(Taxontyp %in% c("variety", "form")) %>%
    select(Taxontyp, taxon_name) %>%
    mutate(taxon_species = word(taxon_name, 1, 2))
  
  species <- bind_rows(species, species_form) %>%
    select(-Taxontyp) %>%
    distinct()
  
  return(species)
}

# Helper function to add species information to species list
add_species_info <- function(shark_species_list, species) {
  shark_species_list <- shark_species_list %>%
    left_join(species, by = "taxon_name") %>%
    relocate(taxon_species, .after = taxon_genus) %>%
    select(-Taxontyp)
  
  return(shark_species_list)
}

# Function to add higher taxonomy from WoRMS file
load_worms_taxonomy <- function(aphia_id_input) {
  taxa_worms <- read_delim(file.path("config/shark_download_config", "taxa_worms_utf8.txt"),
                           delim = "\t",
                           col_names = TRUE,
                           col_types = cols(),
                           progress = FALSE,
                           locale = readr::locale(encoding = "UTF-8")) %>%
    select(aphia_id,
           rank,
           scientific_name,
           class,
           order,
           family,
           genus) %>%
    rename("worms_class" = class,
           "worms_order" = order,
           "worms_family" = family,
           "worms_genus" = genus)
  
  species <- gather_worms_species_info(taxa_worms)
  taxa_worms <- add_worms_species_info(taxa_worms, species) %>%
    filter(aphia_id %in% aphia_id_input)
  
  return(taxa_worms)
}

# Function to gather WoRMS species information
gather_worms_species_info <- function(taxa_worms) {
  species <- taxa_worms %>%
    filter(rank == "Species") %>%
    select(scientific_name, aphia_id) %>%
    rename(worms_species = scientific_name)
  
  return(species)
}

# Function to add WoRMS species information to taxa list
add_worms_species_info <- function(taxa_worms, species) {
  taxa_worms <- taxa_worms %>%
    left_join(species, by = "aphia_id") %>%
    relocate(worms_species, .after = worms_genus) %>%
    select(-rank, -scientific_name)
  
  return(taxa_worms)
}



# Function to add geographical information from coordinates
get_geographical_info <- function(latitude_dd, longitude_dd) {

  # Read shapefiles and list of basin names
  layer <- st_read(file.path("config/sharkweb_shapefiles", "Havsomr_SVAR_2016_3b_CP1252.shp"), 
                   options = "ENCODING=WINDOWS-1252", 
                   quiet = TRUE)
  
  basin_names <- read_delim(file.path("config/shark_download_config", "sea_basin_utf8.txt"), 
                            delim = "\t", 
                            col_names = TRUE, 
                            progress = FALSE,
                            locale = locale(encoding = "UTF-8"),
                            col_types = cols())
  
  # Set CRS of basin layer and transform
  layer <- st_set_crs(layer, 3006) %>%
    st_transform(4326)
  
  # Add geometry information to data
  data <- data.frame("sample_latitude_dd" = latitude_dd, 
                     "sample_longitude_dd" = longitude_dd) %>%
    mutate(lon = sample_longitude_dd,
           lat = sample_latitude_dd)
  
  # Gather all unique positions and convert to sf
  points_sf <- st_as_sf(data %>%
                          distinct(),
                        coords = c("lon", "lat"),
                        crs = st_crs(layer))
  
  # Assign geo info by position
  data_basin <- data %>%
    group_by(sample_latitude_dd, sample_longitude_dd) %>%
    left_join(st_join(points_sf, layer) %>%
                select(sample_latitude_dd, sample_longitude_dd, TYPOMRNAMN, NAMN, BASIN_NR),
              by = c("sample_latitude_dd", "sample_longitude_dd")) %>%
    ungroup() %>%
    left_join(basin_names, by = "BASIN_NR") %>%
    select(-lon, -lat, -BASIN_NR, -geometry) %>%
    rename(location_type_area = TYPOMRNAMN,
           location_svar_sea_area_name = NAMN)
  
  return(data_basin)
}

# Function to check if the dataset versions are up to date
check_data_version <- function(dataset_file_name) {
  dataset_name <- gsub("_version_.*", "", dataset_file_name)
  
  datasets <- load_sharkdata() %>%
    filter(dataset_name %in% dataset_name)
  
  status_list <- data.frame(dataset_name = unique(dataset_name), 
                            dataset_file_name = unique(dataset_file_name),
                            up_to_date = unique(dataset_file_name) %in% datasets$dataset_file_name)
  
  return(status_list)
}

# Function to update data if an updated version is available
update_data <- function(data) {
  
  status_list <- check_data_version(data$dataset_file_name)
  
  if (any(status_list$up_to_date == FALSE)) {
    datasets_to_update <- status_list %>%
      filter(up_to_date == FALSE)
    
    data <- filter_outdated_datasets(data, datasets_to_update)
    updated_data <- download_sharkdata(datasets_to_update$dataset_name)
    
    # Bind data together
    data <- bind_rows(data, updated_data)
    
    # Automatically convert columns to appropriate types
    data <- type_convert(data, col_types = cols())
    
    return(data)
  } else {
    message("Data are already up to date")
  }
}

# Function to filter outdated datasets
filter_outdated_datasets <- function(data, datasets_to_update) {
  data %>%
    filter(!dataset_name %in% datasets_to_update$dataset_name)
}
