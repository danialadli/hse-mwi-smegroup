# Data Layer Module
# Purpose: Load and process all data operations
# Created: 2026-01-13 16:08:48 UTC
# Author: danialadli

#' Load Data Layer
#'
#' Initialize and load all data operations for the HSE-MWI-SMEGroup project
#'
#' @return Invisible NULL
#' @export
#'
#' @examples
#' \dontrun{
#'   load_data_layer()
#' }
load_data_layer <- function() {
  # Import required libraries
  library(dplyr)
  library(tidyr)
  library(readr)
  library(data.table)

  invisible(NULL)
}

#' Load Raw Data
#'
#' Load raw data from source files
#'
#' @param data_path Character string specifying path to data files
#' @param file_pattern Character string for file pattern matching
#'
#' @return Data frame or list of data frames
#' @export
load_raw_data <- function(data_path, file_pattern = "*.csv") {
  if (!dir.exists(data_path)) {
    stop("Data path does not exist: ", data_path)
  }

  files <- list.files(
    path = data_path,
    pattern = file_pattern,
    full.names = TRUE
  )

  if (length(files) == 0) {
    warning("No files found matching pattern: ", file_pattern)
    return(NULL)
  }

  # Load all matching files
  data_list <- lapply(files, function(file) {
    tryCatch(
      readr::read_csv(file, show_col_types = FALSE),
      error = function(e) {
        warning("Error reading file ", file, ": ", e$message)
        NULL
      }
    )
  })

  names(data_list) <- basename(files)
  return(data_list)
}

#' Process Data Operations
#'
#' Apply transformations and processing to loaded data
#'
#' @param data Data frame to process
#' @param operations List of operations to apply
#'
#' @return Processed data frame
#' @export
process_data <- function(data, operations = NULL) {
  if (is.null(data) || nrow(data) == 0) {
    warning("Data is empty or NULL")
    return(data)
  }

  processed_data <- data

  # Apply standard cleaning operations
  processed_data <- processed_data %>%
    # Remove completely empty rows
    filter(!if_all(everything(), is.na)) %>%
    # Remove completely empty columns
    select(!where(~all(is.na(.)))) %>%
    # Trim whitespace from character columns
    mutate(across(where(is.character), str_trim))

  # Apply custom operations if provided
  if (!is.null(operations) && is.list(operations)) {
    for (operation in operations) {
      if (is.function(operation)) {
        processed_data <- operation(processed_data)
      }
    }
  }

  return(processed_data)
}

#' Validate Data Quality
#'
#' Check data quality and return validation report
#'
#' @param data Data frame to validate
#'
#' @return List containing validation results
#' @export
validate_data <- function(data) {
  validation_report <- list(
    total_rows = nrow(data),
    total_cols = ncol(data),
    missing_values = colSums(is.na(data)),
    data_types = sapply(data, class),
    duplicates = sum(duplicated(data)),
    memory_usage = object.size(data)
  )

  class(validation_report) <- c("validation_report", "list")
  return(validation_report)
}

#' Print Validation Report
#'
#' @param x Validation report object
#' @param ... Additional arguments
#'
#' @return Invisible x
#' @export
print.validation_report <- function(x, ...) {
  cat("Data Validation Report\n")
  cat("======================\n\n")
  cat("Total Rows:", x$total_rows, "\n")
  cat("Total Columns:", x$total_cols, "\n")
  cat("Duplicate Rows:", x$duplicates, "\n")
  cat("Memory Usage:", format(x$memory_usage, units = "auto"), "\n\n")
  cat("Missing Values by Column:\n")
  print(x$missing_values)
  cat("\nData Types by Column:\n")
  print(x$data_types)

  invisible(x)
}

#' Cache Data
#'
#' Cache processed data to improve performance
#'
#' @param data Data frame to cache
#' @param cache_path Character string specifying cache directory
#' @param name Character string for cache file name
#'
#' @return Logical indicating success
#' @export
cache_data <- function(data, cache_path = "cache", name = "data_cache") {
  if (!dir.exists(cache_path)) {
    dir.create(cache_path, recursive = TRUE)
  }

  cache_file <- file.path(cache_path, paste0(name, ".rds"))

  tryCatch(
    {
      saveRDS(data, file = cache_file)
      message("Data cached successfully to: ", cache_file)
      TRUE
    },
    error = function(e) {
      warning("Failed to cache data: ", e$message)
      FALSE
    }
  )
}

#' Load Cached Data
#'
#' Load previously cached data
#'
#' @param cache_path Character string specifying cache directory
#' @param name Character string for cache file name
#'
#' @return Data frame or NULL if cache not found
#' @export
load_cached_data <- function(cache_path = "cache", name = "data_cache") {
  cache_file <- file.path(cache_path, paste0(name, ".rds"))

  if (!file.exists(cache_file)) {
    message("Cache file not found: ", cache_file)
    return(NULL)
  }

  tryCatch(
    {
      data <- readRDS(cache_file)
      message("Data loaded from cache: ", cache_file)
      data
    },
    error = function(e) {
      warning("Failed to load cached data: ", e$message)
      NULL
    }
  )
}

#' Initialize Data Layer
#'
#' Orchestrates the loading of the initial dataset based on config
#'
#' @param data_path Path to the data directory
#' @return A list or environment containing the loaded datasets
#' @export
initialize_data_layer <- function(data_path) {
  # 1. Load dependencies
  load_data_layer() 
  
  # 2. Check cache first (optimization)
  cached <- load_cached_data(name = "main_dataset")
  if (!is.null(cached)) {
    return(list(main = cached))
  }
  
  # 3. If no cache, load raw data
  # Note: You need to ensure the directory exists or handle the error gracefully
  tryCatch({
    raw_data <- load_raw_data(data_path)
    
    # 4. Return data structure expected by the app
    # If raw_data is NULL (no files), return empty list to prevent crash
    if (is.null(raw_data)) return(list())
    
    return(raw_data)
  }, error = function(e) {
    warning("Data initialization failed: ", e$message)
    return(list())
  })
}
