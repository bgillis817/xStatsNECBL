combine_navs_csvs_by_id <- function(folder_id) {
  
  cat("COMBINING CSV FILES (by folder ID)\n")
  cat("===========================\n")
  
  folder <- googledrive::drive_get(googledrive::as_id(folder_id))
  cat("Found folder:", folder$name, "\n")
  
  csv_files <- googledrive::drive_ls(folder, pattern = "\\.csv$")
  
  if (nrow(csv_files) == 0) {
    cat("No CSV files found in folder\n")
    return(NULL)
  }
  
  cat("Found", nrow(csv_files), "CSV files in Google Drive\n")
  
  combined_data <- data.frame()
  
  for (i in 1:nrow(csv_files)) {
    file_name <- csv_files$name[i]
    cat("Processing:", file_name, "\n")
    
    tryCatch({
      temp_file <- tempfile(fileext = ".csv")
      googledrive::drive_download(csv_files$id[i], path = temp_file, overwrite = TRUE, verbose = FALSE)
      
      current_data <- read_csv(temp_file, show_col_types = FALSE)
      
      essential_columns <- c("Batter", "BatterTeam", "Date", "Inning", "PAofInning", 
                             "ExitSpeed", "Angle", "PlayResult", "Bearing", "KorBB", "PitchCall")
      available_columns <- intersect(essential_columns, names(current_data))
      current_data <- current_data %>% select(all_of(available_columns))
      cat("  Filtered to", ncol(current_data), "essential columns\n")
      
      unlink(temp_file)
      
      required_cols <- c("ExitSpeed", "Angle", "PlayResult")
      alternative_names <- list(
        ExitSpeed = c("ExitSpeed", "Exit_Speed", "exit_speed", "EV", "ev"),
        Angle = c("Angle", "Launch_Angle", "launch_angle", "LA", "la"),
        PlayResult = c("PlayResult", "Play_Result", "play_result", "Result", "result", "Outcome", "outcome")
      )
      
      for (req_col in required_cols) {
        possible_names <- alternative_names[[req_col]]
        for (alt_name in possible_names) {
          if (alt_name %in% names(current_data) && !req_col %in% names(current_data)) {
            names(current_data)[names(current_data) == alt_name] <- req_col
            break
          }
        }
      }
      
      current_data$source_file <- file_name
      current_data$file_index <- i
      
      if (nrow(combined_data) == 0) {
        combined_data <- current_data
      } else {
        combined_data <- bind_rows(combined_data, current_data)
      }
      
      cat("  Added", nrow(current_data), "rows\n")
      
    }, error = function(e) {
      cat("  Error processing", file_name, ":", e$message, "\n")
    })
  }
  
  cat("Combined", nrow(combined_data), "total rows from", nrow(csv_files), "files\n")
  
  return(combined_data)
}


clean_and_standardize_data <- function(combined_data) {
  
  cat("\n=== DATA CLEANING AND STANDARDIZATION ===\n")
  
  if (is.null(combined_data) || nrow(combined_data) == 0) {
    cat("No data provided for cleaning\n")
    return(NULL)
  }
  
  cat("Starting with", nrow(combined_data), "balls in play\n")
  
  required_cols <- c("ExitSpeed", "Angle", "PlayResult")
  missing_cols <- required_cols[!required_cols %in% names(combined_data)]
  if (length(missing_cols) > 0) {
    cat("Missing required columns:", paste(missing_cols, collapse = ", "), "\n")
    return(NULL)
  }
  
  clean_data <- combined_data %>%
    filter(!is.na(ExitSpeed), ExitSpeed >= 20, ExitSpeed <= 120) %>%
    filter(!is.na(Angle), Angle >= -90, Angle <= 90) %>%
    mutate(
      outcome_clean = case_when(
        PlayResult %in% c("Single", "single", "1B") ~ "single",
        PlayResult %in% c("Double", "double", "2B") ~ "double", 
        PlayResult %in% c("Triple", "triple", "3B") ~ "triple",
        PlayResult %in% c("HomeRun", "home_run", "HR", "Home Run") ~ "home_run",
        PlayResult %in% c("Out", "out", "O") ~ "out",
        TRUE ~ "unknown"
      )
    ) %>%
    filter(outcome_clean != "unknown") %>%
    mutate(
      ev_outlier = ExitSpeed > quantile(ExitSpeed, 0.99) | ExitSpeed < quantile(ExitSpeed, 0.01),
      la_outlier = abs(Angle) > quantile(abs(Angle), 0.99),
      data_quality = case_when(
        ev_outlier | la_outlier ~ "outlier",
        TRUE ~ "clean"
      )
    )
  
  cat("Clean data summary:\n")
  cat("  - Total clean BIP:", nrow(clean_data), "\n")
  outcome_summary <- table(clean_data$outcome_clean)
  print(outcome_summary)
  
  return(clean_data)
}
