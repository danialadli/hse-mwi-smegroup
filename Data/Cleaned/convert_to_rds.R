# CSV to RDS Conversion Script
# Converts CSV files to RDS format for faster loading in the Shiny app
# Run this script when CSV files are updated

# This script:
# 1. Checks for CSV files in Data/Cleaned directory
# 2. Converts them to RDS if RDS doesn't exist or CSV is newer
# 3. Maintains same data structure - only changes storage format

convert_csv_to_rds <- function(csv_file, force = FALSE) {
  rds_file <- gsub("\\.csv$", ".rds", csv_file)
  
  # Check if conversion is needed
  if (!force && file.exists(rds_file)) {
    csv_time <- file.info(csv_file)$mtime
    rds_time <- file.info(rds_file)$mtime
    
    if (rds_time >= csv_time) {
      cat("Skipping", basename(csv_file), "- RDS is up to date\n")
      return(invisible(NULL))
    }
  }
  
  cat("Converting", basename(csv_file), "to RDS...")
  
  # Determine column classes for specific files
  col_classes <- if (grepl("Mental_Wellness_Index|Converted_Measures|Percentile_Ranked", csv_file)) {
    c("ZCTA" = "character")
  } else if (grepl("GEOID", readLines(csv_file, n = 1))) {
    c("GEOID" = "character")
  } else {
    NULL
  }
  
  # Read CSV
  data <- if (!is.null(col_classes)) {
    read.csv(csv_file, colClasses = col_classes)
  } else {
    read.csv(csv_file)
  }
  
  # Remove row names before saving to RDS to avoid conflicts
  rownames(data) <- NULL
  
  # Save as RDS
  saveRDS(data, rds_file, compress = TRUE)
  
  cat(" Done! (", nrow(data), "rows,", ncol(data), "columns)\n")
  
  invisible(rds_file)
}

# Main execution
# Get the directory containing this script
script_args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", script_args[grep("--file=", script_args)])

if (length(script_path) > 0) {
  script_dir <- dirname(script_path)
} else {
  # Running interactively
  script_dir <- getwd()
}

setwd(script_dir)

# List of key CSV files to convert
csv_files <- c(
  "HSE_MWI_Data_Information.csv",
  "HSE_MWI_ZCTA_Converted_Measures.csv",
  "HSE_MWI_ZCTA_Mental_Wellness_Index_Black.csv",
  "HSE_MWI_ZCTA_Mental_Wellness_Index_Population.csv",
  "HSE_MWI_ZCTA_No_Directionality_Percentile_Ranked_Measures.csv",
  "HSE_MWI_ZCTA_Percentile_Ranked_Measures.csv"
)

cat("\n=== CSV to RDS Conversion ===\n")
cat("Working directory:", getwd(), "\n\n")

# Convert each file
for (csv_file in csv_files) {
  if (file.exists(csv_file)) {
    convert_csv_to_rds(csv_file)
  } else {
    cat("Warning:", csv_file, "not found\n")
  }
}

cat("\n=== Conversion Complete ===\n")
cat("RDS files are ready for use in the Shiny app.\n")
cat("These files will be automatically used for faster loading.\n\n")
