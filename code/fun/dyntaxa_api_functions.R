library(tidyverse)
library(jsonlite)
library(httr)

get_dyntaxa_taxonomy <- function(taxon_ids, subscription_key) {
  if (length(taxon_ids) == 0) {
    stop("taxon_ids should not be empty.")
  }
  
  if (any(is.na(taxon_ids))) {
    stop("taxon_ids should not contain NA.")
  }
  
  url <- "https://api.artdatabanken.se/taxonservice/v1/taxa?culture=sv_SE"
  
  headers <- c(
    'Content-Type' = 'application/json-patch+json',
    'Cache-Control' = 'no-cache',
    'Ocp-Apim-Subscription-Key' = subscription_key
  )
  
  # Request body
  data <- list(
    taxonIds = as.list(taxon_ids)
  )
  
  body <- toJSON(data, auto_unbox = TRUE)
  
  response <- POST(url, add_headers(.headers=headers), body = body)
  
  # Check if the request was successful (status code 200)
  if (status_code(response) == 200) {
    # Convert JSON content to a data frame
    df <- fromJSON(content(response, "text"), flatten = TRUE)
    return(df)
  } else {
    # If the request was not successful, return an error message
    return(paste("Error: ", status_code(response), " - ", content(response, "text")))
  }
}

get_dyntaxa_parent_ids <- function(taxon_ids, subscription_key) {
  if (length(taxon_ids) == 0) {
    stop("taxon_ids should not be empty.")
  }
  
  if (any(is.na(taxon_ids))) {
    stop("taxon_ids should not contain NA.")
  }
  
  url <- paste0("https://api.artdatabanken.se/taxonservice/v1/taxa/", taxon_ids, "/parentids")
  
  headers <- c(
    'Cache-Control' = 'no-cache',
    'Ocp-Apim-Subscription-Key' = subscription_key
  )
  
  # Set up the progress bar
  pb <- txtProgressBar(min = 0, max = length(taxon_ids), style = 3)
  
  responses <- lapply(seq_along(url), function(i) {
    setTxtProgressBar(pb, i)
    return(GET(url[i], add_headers(headers)))
  })
  
  results <- lapply(responses, function(response) {
    if (http_status(response)$category == "Success") {
      result <- content(response, "text")
      parsed_result <- c(fromJSON(result)$taxonIds)
      parsed_result <- parsed_result[parsed_result != 0]  # Remove root
      return(parsed_result)
    } else {
      stop(paste("Error:", http_status(response)$reason))
    }
  })
  
  results <- Map(function(vec, val) c(vec, val), results, taxon_ids)
  
  return(results)
}

# Full taxonomy table
construct_dyntaxa_table <- function(parent_ids, subscription_key) {
  if (!is.list(parent_ids)) {
    parent_ids <- list(parent_ids)
  }
  
  if (any(is.na(unlist(parent_ids)))) {
    stop("parent_ids should not contain NA.")
  }
  
  taxa <- data.frame()
  
  # Set up progress bar
  pb <- txtProgressBar(min = 0, max = length(parent_ids), style = 3)
  
  # Initialize counters for debugging
  if_counter <- 0
  else_counter <- 0
  
  for (i in seq_along(parent_ids)) {
    ids <- parent_ids[[i]]
    single <- unique(ids)
    
    taxa_i <- data.frame()
    
    for (id in 1:length(single)) {
      if (single[id] %in% taxa$taxon_id) {
        if_counter <- if_counter + 1 # For debugging
        
        selected <- taxa %>%
          filter(taxon_id == single[id])
        taxon_id <- selected$taxon_id
        name <- selected$name
        rank <- selected$rank
        hierarchy <- selected$hierarchy
        guid <- selected$guid
        author <- selected$author
      } else {
        else_counter <- else_counter + 1 # For debugging
        
        taxa_ix <- get_dyntaxa_taxonomy(single[id], subscription_key)
        taxon_id <- taxa_ix$taxonId
        name <- taxa_ix$names %>%
          map_df(as.data.frame) %>%
          filter(nameShort == "sci" & isRecommended == TRUE) %>%
          slice(1) %>%
          pull(name)
        author <- taxa_ix$names %>%
          map_df(as.data.frame) %>%
          filter(nameShort == "sci" & isRecommended == TRUE) %>%
          slice(1) %>%
          pull(author)
        rank <- taxa_ix$category.value
        hierarchy <- ifelse(
          length(taxa_i) > 0,
          paste(paste(taxa_i$name, collapse = " - "), name, sep = " - "),
          paste0(taxa_ix$names %>%
                   map_df(as.data.frame) %>%
                   filter(nameShort == "sci" & isRecommended == TRUE) %>%
                   slice(1) %>%
                   pull(name))) 
        guid <- taxa_ix$names %>%
          map_df(as.data.frame) %>%
          filter(nameShort == "Guid" & isRecommended == TRUE) %>%
          slice(1) %>%
          pull(name)
        guid <- ifelse(length(guid) == 0, NA, guid)
        author <- ifelse(length(author) == 0, NA, author)
      }
      
      taxa_i <- bind_rows(
        taxa_i,
        data.frame(taxon_id, name, rank, author, hierarchy, guid)
      )
    }
    
    taxa_i <- taxa_i %>%
      distinct() %>%
      pivot_wider(names_from = rank, values_from = name) %>%
      left_join(., taxa_i, by = c("taxon_id", "hierarchy", "guid", "author"))
    
    shark_taxonomy <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
    taxa_i <- taxa_i %>%
      mutate(across(all_of(shark_taxonomy[shark_taxonomy %in% taxa_i$rank]), fill_na_below_first_non_na))
    
    taxa <- bind_rows(taxa, taxa_i) %>%
      distinct()
    
    # Update progress bar at the end of each iteration
    setTxtProgressBar(pb, i)
  }
  
  # Close progress bar
  close(pb)
  
  taxa_filtered <- taxa %>%
    select(taxon_id, name, rank, author, any_of(shark_taxonomy), hierarchy, guid) %>%
    filter(rank %in% shark_taxonomy) %>%
    distinct()
  
  # Print the counters, for debugging
  cat("Cached taxa requests:", if_counter, "\n")
  cat("Unique taxa requests:", else_counter, "\n")
  
  return(taxa_filtered)
}


# Helper function
fill_na_below_first_non_na <- function(x) {
  non_na_values <- x[!is.na(x)]
  if (length(non_na_values) > 0) {
    first_non_na_index <- which(!is.na(x))[1]
    x[first_non_na_index:length(x)] <- non_na_values[1]
  }
  return(x)
}

# Wrapped function for updating taxonomy data
update_dyntaxa_taxonomy <- function(dyntaxa_ids, subscription_key) {
  cat("Collecting parent IDs from Dyntaxa\n")
  parents_ids <- get_dyntaxa_parent_ids(dyntaxa_ids, subscription_key)
  cat("\nCollecting full taxonomy records from Dyntaxa\n")
  tax_table <- construct_dyntaxa_table(parents_ids, subscription_key)
  tax_table_shark <- tax_table %>%
    select(-rank, -author, -guid) %>%
    rename("dyntaxa_id" = taxon_id,
           "scientific_name" = name,
           "taxon_kingdom" = Kingdom,
           "taxon_phylum" = Phylum,
           "taxon_class" = Class,
           "taxon_order" = Order,
           "taxon_family" = Family,
           "taxon_genus" = Genus,
           "taxon_species" = Species,
           "taxon_hierarchy" = hierarchy)
  return(tax_table_shark)
}

# Get list of best matches from name
best_match_dyntaxa <- function(searchString, subscriptionKey, searchFields = 'Both', isRecommended = 'NotSet', 
                               isOkForObservationSystems = 'NotSet', culture = 'sv_SE', 
                               page = 1, pageSize = 100) {
  
  url <- "https://api.artdatabanken.se/taxonservice/v1/taxa/search"
  headers <- c(
    'Cache-Control' = 'no-cache',
    'Ocp-Apim-Subscription-Key' = subscriptionKey
  )
  
  query <- list(
    searchString = searchString,
    searchFields = searchFields,
    isRecommended = isRecommended,
    isOkForObservationSystems = isOkForObservationSystems,
    culture = culture,
    page = page,
    pageSize = pageSize
  )
  
  response <- GET(url, query = query, add_headers(.headers = headers))
  
  result <- list(
    statusCode = status_code(response),
    responseBody = fromJSON(content(response, "text"))
  )
  
  return(result)
}



# Get t
match_dyntaxa <- function(taxon_names, subscriptionKey, searchFields = 'Both', isRecommended = 'NotSet', 
                          isOkForObservationSystems = 'NotSet', culture = 'sv_SE', 
                          page = 1, pageSize = 100) {
  
  # Make sure there are no NA
  taxon_names <- taxon_names[!is.na(taxon_names)]
  
  url <- "https://api.artdatabanken.se/taxonservice/v1/taxa/names"
  headers <- c(
    'Cache-Control' = 'no-cache',
    'Ocp-Apim-Subscription-Key' = subscriptionKey
  )
  
  result_list <- map(taxon_names, ~{
    query <- list(
      searchString = .x,
      searchFields = searchFields,
      isRecommended = isRecommended,
      isOkForObservationSystems = isOkForObservationSystems,
      culture = culture,
      page = page,
      pageSize = pageSize
    )
    
    response <- GET(url, query = query, add_headers(.headers = headers))
    
    result <- list(
      taxon_name = .x,
      statusCode = status_code(response),
      responseBody = fromJSON(content(response, "text"))
    )
    
    if (length(result$responseBody$data) > 0) {
      taxon_id <- result$responseBody$data$taxonInformation$taxonId[1]
      name <- result$responseBody$data$name[1]
      return(data.frame(search_pattern = result$taxon_name, taxon_id = taxon_id, best_match = name))
    } else {
      return(data.frame(search_pattern = result$taxon_name, taxon_id = NA, best_match = NA))
    }
  })
  
  result_df <- do.call(rbind, result_list)
  return(result_df)
}

