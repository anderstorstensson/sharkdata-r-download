# Install and load required packages
# install.packages("dplyr")
# install.packages("jsonlite")
library(dplyr)
library(jsonlite)

# Define function for listing available dataset types
load_dataset_types <- function() {
  # Load JSON data from sharkdata
  datasets <- fromJSON('https://sharkdata.smhi.se/datasets/list.json')
  
  # Find unique dataset types
  dataset_types <- unique(datasets$datatype)
  
  return(dataset_types)
}

# Define function for loading available dataset names
load_dataset_names <- function(dataset_type, year = NA, data_deliverer = NA) {
  
    # Load JSON data from sharkdata
    datasets <- fromJSON('https://sharkdata.smhi.se/datasets/list.json')

    # Filter datasets based on dataset_name column
    filtered_datasets_datatype <- datasets %>%
      filter(datatype == dataset_type) 
    
    # Filter datasets based on year
    if(!any(is.na(year))) {
      filtered_datasets_year <- c()
      for (i in 1:length(year)) {
        filtered_datasets_i <- filtered_datasets_datatype %>%
          filter(grepl(as.character(year[i]), dataset_name))
        
        filtered_datasets_year <- rbind(filtered_datasets_year, filtered_datasets_i)
      }
    } else {
      filtered_datasets_year = filtered_datasets_datatype
    }
    
    # Filter datasets based on data deliverer
    if(!any(is.na(data_deliverer))) {
      filtered_datasets <- c()
      for (i in 1:length(data_deliverer)) {
        filtered_datasets_i <- filtered_datasets_year %>%
          filter(grepl(data_deliverer[i], dataset_name))
        
        filtered_datasets <- rbind(filtered_datasets, filtered_datasets_i)
      }
    } else {
      filtered_datasets = filtered_datasets_year
    }
    
  return(filtered_datasets)
}

# Define function for downloading datasets
download_shark_data <- function(dataset_name) {
  data_shark = data.frame()
  
  for(i in 1:length(dataset_name)) {
    temp = tempfile()
    
    download.file(paste("https://sharkdata.smhi.se/datasets/", 
                        dataset_name[i], 
                        "/shark_archive.zip", 
                        sep = ""), 
                  temp, 
                  mode="wb", 
                  quiet	= TRUE)
    
    # Read data and convert to UTF-8
    data_ix = read_delim(unz(temp, "shark_data.txt"), 
                         locale = locale(encoding = "ISO8859-1"),
                         col_types = cols(),
                         progress = FALSE) %>%
      dplyr::mutate_if(is.character, .funs = function(x){return(`Encoding<-`(x, "utf8"))})
    
    data_shark = plyr::rbind.fill(data_shark, data_ix)
    
    cat("Downloading ", i, "/", length(dataset_name), " datasets", "\n")
  }
  return(data_shark)
}
