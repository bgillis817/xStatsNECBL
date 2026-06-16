# Load required libraries
suppressPackageStartupMessages({
  if (!require(pacman)) install.packages("pacman")
  pacman::p_load(
    dplyr, readr, data.table,
    ggplot2, gridExtra, viridis,
    tidyr, lubridate, stringr, googledrive
  )
})

# Authenticate to Google Drive using a service account (non-interactive)
if (file.exists("service_account.json")) {
  googledrive::drive_auth(path = "service_account.json")
} else {
  googledrive::drive_deauth()
}

# ===================================================================
# PHASE 1: CSV COMBINATION
# ===================================================================

combine_navs_csvs <- function(folder_path = "Navs CSVs", use_google_drive = TRUE) {
  
  cat("COMBINING CSV FILES\n")
  cat("===========================\n")
  
  if (use_google_drive) {
    cat("Looking in Google Drive folder:", folder_path, "\n")
    
    # Find the folder in Google Drive
    folder <- drive_find(folder_path, type = "folder")
    
    if (nrow(folder) == 0) {
      cat("Google Drive folder not found:", folder_path, "\n")
      return(NULL)
    }
    
    # Get all CSV files in the folder
    csv_files <- drive_ls(folder, pattern = "\\.csv$")
    
    if (nrow(csv_files) == 0) {
      cat("No CSV files found in the Google Drive folder.\n")
      return(NULL)
    }
    
    cat("Found", nrow(csv_files), "CSV files in Google Drive\n")
    
    # Initialize combined data
    combined_data <- data.frame()
    
    # Process each CSV file from Google Drive
    for (i in 1:nrow(csv_files)) {
      file_name <- csv_files$name[i]
      cat("Processing:", file_name, "\n")
      
      tryCatch({
        # Download to temporary location
        temp_file <- tempfile(fileext = ".csv")
        drive_download(csv_files$id[i], path = temp_file, overwrite = TRUE, verbose = FALSE)
        
        # Read the downloaded file
        current_data <- read_csv(temp_file, show_col_types = FALSE)
        
        # Clean up temp file
        unlink(temp_file)
        
        # Standardize column names
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
        
        # Add source file info
        current_data$source_file <- file_name
        current_data$file_index <- i
        
        # Combine data
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
    
  } else {
    # Original local file logic
    cat("Looking in local folder:", folder_path, "\n")
    
    # Set working directory
    if (dir.exists(folder_path)) {
      setwd(folder_path)
      cat("Working directory set to:", getwd(), "\n")
    } else {
      cat("Folder not found:", folder_path, "\n")
      return(NULL)
    }
    
    # Find all CSV files
    csv_files <- list.files(pattern = "\\.csv$", full.names = FALSE)
    
    if (length(csv_files) == 0) {
      cat("No CSV files found in the folder.\n")
      return(NULL)
    }
    
    cat("Data Found", length(csv_files), "CSV files\n")
    
    # Initialize combined data
    combined_data <- data.frame()
    
    # Process each CSV file
    for (i in 1:length(csv_files)) {
      file <- csv_files[i]
      cat("Processing:", file, "\n")
      
      tryCatch({
        current_data <- read_csv(file, show_col_types = FALSE)
        
        # Standardize column names
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
        
        # Add source file info
        current_data$source_file <- file
        current_data$file_index <- i
        
        # Combine data
        if (nrow(combined_data) == 0) {
          combined_data <- current_data
        } else {
          combined_data <- bind_rows(combined_data, current_data)
        }
        
        cat("  Added", nrow(current_data), "rows\n")
        
      }, error = function(e) {
        cat("  Error:", e$message, "\n")
      })
    }
    
    cat("Combined", nrow(combined_data), "total rows from", length(csv_files), "files\n")
  }
  
  return(combined_data)
}


clean_and_standardize_data <- function(combined_data) {
  
  cat("\n=== DATA CLEANING AND STANDARDIZATION ===\n")
  
  if (is.null(combined_data) || nrow(combined_data) == 0) {
    cat("No data provided for cleaning\n")
    return(NULL)
  }
  
  cat("Starting with", nrow(combined_data), "balls in play\n")
  
  # Check for required columns
  required_cols <- c("ExitSpeed", "Angle", "PlayResult")
  missing_cols <- required_cols[!required_cols %in% names(combined_data)]
  if (length(missing_cols) > 0) {
    cat("Missing required columns:", paste(missing_cols, collapse = ", "), "\n")
    return(NULL)
  }
  
  # Data cleaning pipeline
  clean_data <- combined_data %>%
    # Remove invalid exit velocities
    filter(!is.na(ExitSpeed), ExitSpeed >= 20, ExitSpeed <= 120) %>%
    # Remove invalid launch angles  
    filter(!is.na(Angle), Angle >= -90, Angle <= 90) %>%
    # Standardize outcome names
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
    # Keep only valid outcomes
    filter(outcome_clean != "unknown") %>%
    # Add data quality flags
    mutate(
      ev_outlier = ExitSpeed > quantile(ExitSpeed, 0.99) | ExitSpeed < quantile(ExitSpeed, 0.01),
      la_outlier = abs(Angle) > quantile(abs(Angle), 0.99),
      data_quality = case_when(
        ev_outlier | la_outlier ~ "outlier",
        TRUE ~ "clean"
      )
    )
  
  # Data summary
  cat("Clean data summary:\n")
  cat("  - Total clean BIP:", nrow(clean_data), "\n")
  outcome_summary <- table(clean_data$outcome_clean)
  print(outcome_summary)
  
  return(clean_data)
}


engineer_xwoba_features <- function(clean_data) {
  
  cat("\n=== FEATURE ENGINEERING ===\n")
  
  # Extract key variables
  ev <- clean_data$ExitSpeed
  la <- clean_data$Angle
  n <- length(ev)
  
  # Physics calculations
  la_rad <- la * pi / 180
  
  cat("🔧 Creating feature categories:\n")
  
  # 1. Raw measurements
  cat("  - Raw measurements\n")
  raw_features <- data.frame(
    exit_velocity = ev,
    launch_angle = la
  )
  
  # 2. Polynomial features
  cat("  - Polynomial features\n")
  poly_features <- data.frame(
    ev_squared = ev^2,
    ev_cubed = ev^3,
    la_squared = la^2,
    la_cubed = la^3
  )
  
  # 3. Interaction features
  cat("  - Interaction features\n")
  interaction_features <- data.frame(
    ev_la = ev * la,
    ev_la_squared = ev * la^2,
    ev_squared_la = ev^2 * la,
    ev_squared_la_squared = ev^2 * la^2
  )
  
  # 4. Distance/Movementbased features
  cat("  - Physics-based features\n")
  physics_features <- data.frame(
    ev_la_ratio = ev / (abs(la) + 1),
    optimal_distance = sqrt((ev - 100)^2 + (la - 25)^2),
    sweet_spot_score = 1 / (1 + sqrt((ev - 100)^2 + (la - 25)^2)),
    optimal_angle_diff = abs(la - 25),
    angle_efficiency = cos(la_rad) * ev / 100,
    velocity_percentile = rank(ev) / n,
    angle_percentile = rank(la) / n
  )
  
  # 5. Trig featuree
  cat("  - Trigonometric features\n")
  trig_features <- data.frame(
    la_sin = sin(la_rad),
    la_cos = cos(la_rad),
    la_tan = tan(la_rad),
    ev_sin = ev * sin(la_rad),
    ev_cos = ev * cos(la_rad)
  )
  
  # 6. Categorical features
  cat("  - Categorical features\n")
  categorical_features <- data.frame(
    ev_elite = as.numeric(ev >= 100),
    ev_hard = as.numeric(ev >= 95 & ev < 100),
    ev_medium = as.numeric(ev >= 80 & ev < 95),
    ev_soft = as.numeric(ev < 80),
    la_popup = as.numeric(la > 50),
    la_flyball = as.numeric(la > 25 & la <= 50),
    la_line_drive = as.numeric(la >= 10 & la <= 25),
    la_ground_ball = as.numeric(la < 10),
    barrel = as.numeric(ev >= 98 & la >= 8 & la <= 32),
    solid_contact = as.numeric(ev >= 90 & la >= 8 & la <= 40),
    weak_contact = as.numeric(ev < 70 | abs(la) > 45)
  )
  
  # 7. Stat features
  cat("  - Statistical features\n")
  statistical_features <- data.frame(
    ev_z_score = scale(ev)[,1],
    la_z_score = scale(la)[,1],
    ev_rank = rank(ev),
    la_rank = rank(la)
  )
  
  # 8. Domain specific features
  cat("  - Domain-specific features\n")
  domain_features <- data.frame(
    hr_zone = as.numeric(ev >= 100 & la >= 20 & la <= 35),
    double_zone = as.numeric(ev >= 90 & la >= 10 & la <= 25),
    out_zone_popup = as.numeric(la > 45),
    out_zone_weak = as.numeric(ev < 70),
    high_value_zone = as.numeric(ev >= 90 & la >= 10 & la <= 40)
  )
  
  # 9. Sweet spot
  cat("  - Sweet spot analysis features\n")
  sweet_spot_features <- data.frame(
    sweet_spot_gradient = exp(-((ev - 100)^2 / 100 + (la - 25)^2 / 100)),
    barrel_plus = as.numeric(ev >= 100 & la >= 10 & la <= 30),
    elite_contact = as.numeric(ev >= 105 & la >= 15 & la <= 35),
    power_zone = as.numeric(ev >= 98 & la >= 22 & la <= 38),
    expected_distance_long = as.numeric(ev >= 95 & la >= 10 & la <= 40)
  )
  
  # Combine all features
  all_features <- cbind(
    raw_features,
    poly_features, 
    interaction_features,
    physics_features,
    trig_features,
    categorical_features,
    statistical_features,
    domain_features,
    sweet_spot_features
  )
  
  cat("Feature engineering complete; Created", ncol(all_features), "features\n")
  
  return(all_features)
}


# PHASE 4: SWEET SPOT ANALYSIS

analyze_sweet_spots <- function(clean_data, features) {
  
  cat("\n=== SWEET SPOT ANALYSIS ===\n")
  
  # Extract key variables
  ev <- clean_data$ExitSpeed
  la <- clean_data$Angle
  outcome <- clean_data$outcome_clean
  
  # Calculate actual wOBA for each ball
  woba_weights <- list(single = 0.888, double = 1.271, triple = 1.616, home_run = 2.101, out = 0.000)
  actual_woba <- ifelse(outcome == "single", woba_weights$single,
                        ifelse(outcome == "double", woba_weights$double,
                               ifelse(outcome == "triple", woba_weights$triple,
                                      ifelse(outcome == "home_run", woba_weights$home_run, woba_weights$out))))
  
  # Create EV/LA grid analysis
  cat("Creating EV/LA performance grid...\n")
  
  ev_bins <- seq(20, 120, by = 10)
  la_bins <- seq(-90, 90, by = 10)
  
  sweet_spot_grid <- data.frame()
  
  for (i in 1:(length(ev_bins)-1)) {
    for (j in 1:(length(la_bins)-1)) {
      ev_range <- c(ev_bins[i], ev_bins[i+1])
      la_range <- c(la_bins[j], la_bins[j+1])
      
      mask <- ev >= ev_range[1] & ev < ev_range[2] & la >= la_range[1] & la < la_range[2]
      
      if (sum(mask) >= 5) {
        sweet_spot_grid <- rbind(sweet_spot_grid, data.frame(
          ev_min = ev_range[1], ev_max = ev_range[2],
          la_min = la_range[1], la_max = la_range[2],
          sample_count = sum(mask),
          mean_woba = mean(actual_woba[mask]),
          hr_rate = mean(outcome[mask] == "home_run"),
          hit_rate = mean(outcome[mask] != "out")
        ))
      }
    }
  }
  
  # Sweet spot scoring
  sweet_spot_scores <- data.frame(
    exit_velocity = ev,
    launch_angle = la,
    actual_outcome = outcome,
    actual_woba = actual_woba,
    combined_score = (pmin(1, pmax(0, (ev - 60) / 40)) * 0.6 +
                        ifelse(la >= 10 & la <= 35, 1,
                               ifelse(la < 10, la / 10, 
                                      ifelse(la > 35, (90 - la) / 55, 0))) * 0.4)
  )
  
  cat("✅ Sweet spot analysis complete!\n")
  
  return(list(
    grid_analysis = sweet_spot_grid,
    sweet_spot_scores = sweet_spot_scores
  ))
}


prepare_ml_training_datasets <- function(clean_data, features, sweet_spot_analysis) {
  
  cat("\n=== PREPARING DATASETS ===\n")
  
  # 2024 MLB wOBA weights (can add other stuff as time goes)
  woba_weights <- list(
    single = 0.888,
    double = 1.271,
    triple = 1.616, 
    home_run = 2.101,
    out = 0.000
  )
  
  # Create binary target variables
  targets <- data.frame(
    is_single = as.numeric(clean_data$outcome_clean == "single"),
    is_double = as.numeric(clean_data$outcome_clean == "double"),
    is_triple = as.numeric(clean_data$outcome_clean == "triple"), 
    is_homerun = as.numeric(clean_data$outcome_clean == "home_run"),
    is_out = as.numeric(clean_data$outcome_clean == "out")
  )
  
  # Calculate actual wOBA
  actual_woba <- ifelse(clean_data$outcome_clean == "single", woba_weights$single,
                        ifelse(clean_data$outcome_clean == "double", woba_weights$double,
                               ifelse(clean_data$outcome_clean == "triple", woba_weights$triple,
                                      ifelse(clean_data$outcome_clean == "home_run", woba_weights$home_run,
                                             woba_weights$out))))
  
  # Create training package
  training_package <- list(
    features = features,
    targets = targets,
    outcomes = clean_data$outcome_clean,
    actual_woba = actual_woba,
    sweet_spot_data = sweet_spot_analysis,
    raw_measurements = data.frame(
      exit_velocity = clean_data$ExitSpeed,
      launch_angle = clean_data$Angle,
      outcome_clean = clean_data$outcome_clean
    ),
    metadata = list(
      total_samples = nrow(clean_data),
      feature_count = ncol(features),
      woba_weights = woba_weights,
      outcome_distribution = table(clean_data$outcome_clean),
      data_date_created = Sys.time()
    )
  )
  
  cat("Training package complete:\n")
  cat("  - Samples:", nrow(clean_data), "\n")
  cat("  - Features:", ncol(features), "\n")
  cat("  - Mean wOBA:", round(mean(actual_woba), 3), "\n")
  
  return(training_package)
}

# ===================================================================
# MAIN PIPELINE FUNCTION
# ===================================================================

run_complete_xwoba_pipeline <- function(folder_path = "Navs CSVs", use_google_drive = TRUE) {
  
  cat("STARTING COMPLETE xwOBA PIPELINE\n")
  cat("====================================\n")
  
  # Phase 1: Combine CSV files
  cat("\nPHASE 1: COMBINING CSV FILES\n")
  combined_data <- combine_navs_csvs(folder_path, use_google_drive)
  if (is.null(combined_data)) return(NULL)
  
  # Save combined data
 # saveRDS(combined_data, "navs_all_data.rds")
cat("Data processed\n")
  
  # Phase 2: Clean and standardize
  cat("\nPHASE 2: DATA CLEANING\n")
  clean_data <- clean_and_standardize_data(combined_data)
  if (is.null(clean_data)) return(NULL)
  
  # Phase 3: Feature engineering
  cat("\n PHASE 3: FEATURE ENGINEERING\n")
  features <- engineer_xwoba_features(clean_data)
  
  # Phase 4: Sweet spot analysis
  cat("\n PHASE 4: SWEET SPOT ANALYSIS\n")
  sweet_spot_analysis <- analyze_sweet_spots(clean_data, features)
  
  # Phase 5: Prepare training datasets
  cat("\n PHASE 5: TRAINING DATA PREPARATION\n")
  training_package <- prepare_ml_training_datasets(clean_data, features, sweet_spot_analysis)
  
  # Save final package
  final_package <- list(
    training_data = training_package,
    clean_data = clean_data,
    engineered_features = features,
    sweet_spot_analysis = sweet_spot_analysis
  )
  
  #saveRDS(final_package, "xwoba_data_engineering_package.rds")
 # write.csv(cbind(training_package$features, training_package$targets), 
         #   "xwoba_training_data.csv", row.names = FALSE)
  
  cat("\n PIPELINE COMPLETE\n")
  cat("=====================\n")
  cat("Combined", nrow(clean_data), "balls in play\n")
  cat("Created", ncol(features), "features\n")
  cat("Sweet spot analysis complete\n")
cat("Files created:\n")
cat("   - xwoba_data_engineering_package.rds\n")
cat("   - xwoba_training_data.csv\n")
cat("   - data processed\n")
  return(final_package)
}
library(googledrive)
library(readr)
library(dplyr)
library(caret)
library(xgboost)

# Authenticate to Google Drive using a service account (non-interactive)
if (file.exists("service_account.json")) {
  googledrive::drive_auth(path = "service_account.json")
} else {
  googledrive::drive_deauth()
}


combine_navs_csvs <- function(folder_path = "Navs CSVs", use_google_drive = TRUE) {
  
  cat("COMBINING CSV FILES\n")
  cat("===========================\n")
  
  if (use_google_drive) {
    cat("Looking in Google Drive folder:", folder_path, "\n")
    
    # Find the folder in Google Drive
    folder <- drive_find(folder_path, type = "folder")
    
    if (nrow(folder) == 0) {
      cat("Google Drive folder not found:", folder_path, "\n")
      return(NULL)
    }
    
    # Get all CSV files in the folder
    csv_files <- drive_ls(folder, pattern = "\\.csv$")
    
    if (nrow(csv_files) == 0) {
      cat("No CSV files found in the Google Drive folder.\n")
      return(NULL)
    }
    
    cat("Found", nrow(csv_files), "CSV files in Google Drive\n")
    
    # Initialize combined data
    combined_data <- data.frame()
    
    # Process each CSV file from Google Drive
    for (i in 1:nrow(csv_files)) {
      file_name <- csv_files$name[i]
      cat("Processing:", file_name, "\n")
      
      tryCatch({
        # Download to temporary location
        temp_file <- tempfile(fileext = ".csv")
        drive_download(csv_files$id[i], path = temp_file, overwrite = TRUE, verbose = FALSE)
        
        # Read the downloaded file
        current_data <- read_csv(temp_file, show_col_types = FALSE)
        
        # Clean up temp file
        unlink(temp_file)
        
        # Standardize column names
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
        
        # Add source file info
        current_data$source_file <- file_name
        current_data$file_index <- i
        
        # Combine data
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
    
  } else {
    # Original local file logic
    cat("Looking in local folder:", folder_path, "\n")
    
    # Set working directory
    if (dir.exists(folder_path)) {
      setwd(folder_path)
      cat("Working directory set to:", getwd(), "\n")
    } else {
      cat("Folder not found:", folder_path, "\n")
      return(NULL)
    }
    
    # Find all CSV files
    csv_files <- list.files(pattern = "\\.csv$", full.names = FALSE)
    
    if (length(csv_files) == 0) {
      cat("No CSV files found in the folder.\n")
      return(NULL)
    }
    
    cat("Data Found", length(csv_files), "CSV files\n")
    
    # Initialize combined data
    combined_data <- data.frame()
    
    # Process each CSV file
    for (i in 1:length(csv_files)) {
      file <- csv_files[i]
      cat("Processing:", file, "\n")
      
      tryCatch({
        current_data <- read_csv(file, show_col_types = FALSE)
        
        # Standardize column names
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
        
        # Add source file info
        current_data$source_file <- file
        current_data$file_index <- i
        
        # Combine data
        if (nrow(combined_data) == 0) {
          combined_data <- current_data
        } else {
          combined_data <- bind_rows(combined_data, current_data)
        }
        
        cat("  Added", nrow(current_data), "rows\n")
        
      }, error = function(e) {
        cat("  Error:", e$message, "\n")
      })
    }
    
    cat("Combined", nrow(combined_data), "total rows from", length(csv_files), "files\n")
  }
  
  return(combined_data)
}

# ===================================================================
# PHASE 2: COMPLETE PIPELINE
# ===================================================================

run_complete_pipeline <- function() {
  
  cat("=== NAVS CSV PIPELINE START ===\n\n")
  
  # STEP 1: Combine CSVs from Google Drive
  cat("STEP 1: Combining CSV files from Google Drive...\n")
  combined_data <- combine_navs_csvs(folder_path = "Navs CSVs", use_google_drive = TRUE)
  
  if (is.null(combined_data) || nrow(combined_data) == 0) {
    cat("ERROR: No data was combined. Check Google Drive folder and files.\n")
    return(NULL)
  }
  
  # STEP 2: Save combined data
  cat("\nSTEP 2: Saving combined data...\n")
  saveRDS(combined_data, "navs_all_data.rds")
  cat("Saved", nrow(combined_data), "rows to navs_all_data.rds\n")
  
  # STEP 3: Data quality check
  cat("\nSTEP 3: Data quality check...\n")
  cat("Dataset dimensions:", nrow(combined_data), "rows x", ncol(combined_data), "columns\n")
  cat("Column names:", paste(names(combined_data), collapse = ", "), "\n")
  
  # Check required columns
  required_cols <- c("ExitSpeed", "Angle", "PlayResult")
  missing_cols <- setdiff(required_cols, names(combined_data))
  if(length(missing_cols) > 0) {
    cat("WARNING: Missing required columns:", paste(missing_cols, collapse = ", "), "\n")
  } else {
    cat("SUCCESS: All required columns present\n")
  }
  
  # Data completeness
  for(col in required_cols) {
    if(col %in% names(combined_data)) {
      complete_pct <- sum(!is.na(combined_data[[col]])) / nrow(combined_data) * 100
      cat(col, ":", round(complete_pct, 1), "% complete\n")
    }
  }
  
  # STEP 4: Set raw_data for the model
  raw_data <<- combined_data  # Make it available globally
  
  cat("\n=== DATA COMBINATION COMPLETE ===\n")
  cat("Ready to run xwOBA model with", nrow(raw_data), "rows of data\n")
  
  return(combined_data)
}

# ===================================================================
# ===================================================================
# LOAD DATA FROM GOOGLE DRIVE (service account auth, no pipeline at startup)
# ===================================================================

live_rds_id <- "1IFO08F2YoO1qUXCw3GiU1k6FOXwt4Q0b"
if (file.exists("service_account.json")) {
  tmp_rds <- tempfile(fileext = ".rds")
  googledrive::drive_download(googledrive::as_id(live_rds_id), path = tmp_rds, overwrite = TRUE)
  raw_data <- readRDS(tmp_rds)
  unlink(tmp_rds)
} else {
  raw_data <- readRDS(url(paste0("https://drive.google.com/uc?export=download&id=", live_rds_id)))
}

# Model functions sourced from pipeline_functions.R
source("pipeline_functions.R")


# Run the complete model
# Load pre-trained model results from pipeline (avoids retraining on startup)
if (file.exists("xwoba_model.rds")) {
  cat("\nLoading pre-trained xwOBA model from xwoba_model.rds...\n")
  model_cache <- readRDS("xwoba_model.rds")
  ultimate_results   <- model_cache$ultimate_results
  full_xwoba_result  <- model_cache$full_xwoba_result
} else {
  cat("\nNo cached model found - training now (this will take a moment)...\n")
  ultimate_results   <- train_maximum_correlation_xwoba(raw_data)
  full_xwoba_result  <- calculate_full_xwoba(raw_data, ultimate_results)
  saveRDS(list(ultimate_results = ultimate_results,
               full_xwoba_result = full_xwoba_result),
          "xwoba_model.rds")
}

# Get key results
mean_xwobacon <- mean(ultimate_results$predictions, na.rm = TRUE)
correlation   <- ultimate_results$correlation

cat("\nFinal Results:\n")
cat("Mean xwOBAcon:", round(mean_xwobacon, 3), "\n")
cat("Full xwOBA:", round(full_xwoba_result, 3), "\n")
cat("Correlation:", round(correlation, 4), "\n")


# Enhanced xwOBA Dashboard - Complete R Shiny Integration
# SECTION 1: REQUIRED LIBRARIES
# ===================================================================

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rvest)
library(httr)
library(stringr)
library(xml2)
library(plotly)
# ===================================================================
# ===================================================================
# SECTION 2: LOAD PRE-COMPUTED PLAYER xwOBA SCORES
# ===================================================================

# Load cached scoring results from pipeline (pre-computed to avoid startup timeout)
if (file.exists("xwoba_model.rds")) {
  model_cache        <- readRDS("xwoba_model.rds")
  enhanced_data      <- model_cache$enhanced_data
  player_xwoba_full  <- model_cache$player_xwoba_full
  batted_balls       <- model_cache$batted_balls
  necbl_stats_cache  <- if (!is.null(model_cache$necbl_stats_cache)) model_cache$necbl_stats_cache else list()
  cat("Loaded pre-computed enhanced_data:", nrow(enhanced_data), "rows\n")
  cat("NECBL stats cached for seasons:", paste(names(necbl_stats_cache), collapse = ", "), "\n")
} else {
  cat("WARNING: xwoba_model.rds not found - scores may be missing\n")
  enhanced_data      <- data.frame()
  player_xwoba_full  <- data.frame()
  batted_balls       <- data.frame()
  necbl_stats_cache  <- list()
}

# ===================================================================
# SECTION 3: NECBL TEAM MAPPING & SEASON-SPECIFIC SCRAPING
# ===================================================================

# Complete team mapping with roster URLs and season IDs - FIXED VERSION
necbl_team_mapping_enhanced <- list(
  "BRI_B" = list(
    name = "Bristol Blues", 
    abbrev = "BRI",
    team_id = "89490"
  ),
  "DAN_WES" = list(
    name = "Danbury Westerners", 
    abbrev = "DAN",
    team_id = "6402"
  ),
  "KEE_SWA" = list(
    name = "Keene SwampBats", 
    abbrev = "KSB",
    team_id = "6401"
  ),
  "MAR_VIN" = list(
    name = "Martha's Vineyard Sharks", 
    abbrev = "MV",
    team_id = "142675"
  ),
  "MYS_SCH" = list(
    name = "Mystic Schooners", 
    abbrev = "MSC",
    team_id = "11912"
  ),
  "NEW_GUL" = list(
    name = "Newport Gulls", 
    abbrev = "NG",
    team_id = "6458"
  ),
  "NOR_ADA" = list(
    name = "North Adams Steeplecats", 
    abbrev = "NSC",
    team_id = "6404"
  ),
  # FIXED: Added NSH_N mapping
  "NSH_N" = list(
    name = "North Shore Navigators", 
    abbrev = "NSN",
    team_id = "154432"
  ),
  # Ocean State Waves - handles both OCE_STA and OCE_STA6
  "OCE_STA" = list(
    name = "Ocean State Waves", 
    abbrev = "OSW",
    team_id = "51489"
  ),
  "OCE_STA6" = list(
    name = "Ocean State Waves", 
    abbrev = "OSW",
    team_id = "51489"
  ),
  "SAN_MAI" = list(
    name = "Sanford Mainers", 
    abbrev = "SM",
    team_id = "6459"
  ),
  "UPP_VAL" = list(
    name = "Upper Valley Nighthawks", 
    abbrev = "UVNH",
    team_id = "104040"
  ),
  "VAL_BLU" = list(
    name = "Valley Blue Sox", 
    abbrev = "VAL",
    team_id = "6403"
  ),
  "VER_MOU" = list(
    name = "Vermont Mountaineers", 
    abbrev = "VM",
    team_id = "6405"
  ),
  # Additional mappings for unmapped teams
  "WIN_MUS" = list(
    name = "Winnipesaukee Muskrats", 
    abbrev = "WM",
    team_id = "6406"
  ),
  "NWL_WB" = list(
    name = "Newport Gulls", 
    abbrev = "NG",
    team_id = "6458"
  ),
  "NEC_EAS" = list(
    name = "NECBL East", 
    abbrev = "NE",
    team_id = "unknown"
  ),
  "NEC_WES" = list(
    name = "NECBL West", 
    abbrev = "NW", 
    team_id = "unknown"
  )
)

# NECBL URLs by season with proper season IDs
necbl_urls_by_season <- list(
  "2026" = list(seasonid = "34460"),
  "2025" = list(seasonid = "34029"),
  "2024" = list(seasonid = "33860"),
  "2023" = list(seasonid = "33589"),
  "2022" = list(seasonid = "33205"),
  "2021" = list(seasonid = "32746")
)

# Helper function to create composite keys
create_composite_key <- function(last_name, first_initial, team_abbrev, season) {
  paste(
    toupper(trimws(last_name)),
    toupper(trimws(first_initial)),
    toupper(trimws(team_abbrev)),
    season,
    sep = "_"
  )
}

# Helper function to extract team info from model team codes
get_team_info_from_model_code <- function(model_team_code) {
  team_info <- necbl_team_mapping_enhanced[[model_team_code]]
  if (!is.null(team_info)) {
    return(list(
      name = team_info$name,
      abbrev = team_info$abbrev,
      team_id = team_info$team_id
    ))
  } else {
    return(list(name = NA_character_, abbrev = "UNK", team_id = NA_character_))
  }
}
# ===================================================================
# SECTION 4: ENHANCED SCRAPER WITH COMPOSITE KEY GENERATION & FIXES
# ===================================================================

# PrestoSports scraper - static print template (no JavaScript required)
# URL: https://newenglandcollegiateleague.prestosports.com/sports/bsb/YEAR/teams/SLUG
#      ?tmpl=teaminfo-network-monospace-template&sort=ab&pos=h
get_necbl_woba_by_season <- function(season = "2026") {
  cat("=== SCRAPING NECBL", season, "SEASON (PrestoSports) ===\n")

  team_slugs <- list(
    "UPP_VAL" = "uppervalleynighthawks",
    "VAL_BLU" = "valleybluesox",
    "KEE_SWA" = "keeneswampbats",
    "BRI_B"   = "bristolblues",
    "MAR_VIN" = "marthasvineyardsharks",
    "OCE_STA" = "oceanstatewaves",
    "NOR_ADA" = "northadamssteeplecats",
    "MYS_SCH" = "mysticschooners",
    "NEW_GUL" = "newportgulls",
    "SAN_MAI" = "sanfordmainers",
    "VER_MOU" = "vermontmountaineers",
    "DAN_WES" = "danburywesterners",
    "NSN"     = "northshorenavigators"
  )

  all_woba <- data.frame()

  for (team_code in names(team_slugs)) {
    slug      <- team_slugs[[team_code]]
    team_info <- necbl_team_mapping_enhanced[[team_code]]
    if (is.null(team_info)) next
    team_name   <- team_info$name
    team_abbrev <- team_info$abbrev

    # Static print template - renders without JavaScript
    url <- paste0(
      "https://newenglandcollegiateleague.prestosports.com/sports/bsb/",
      season, "/teams/", slug,
      "?tmpl=teaminfo-network-monospace-template&sort=ab&pos=h"
    )

    cat("Scraping", team_name, "...\n")

    tryCatch({
      response <- httr::GET(
        url,
        httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"),
        httr::timeout(20)
      )

      if (httr::status_code(response) != 200) {
        cat("  HTTP", httr::status_code(response), "- skipping\n")
        next
      }

      page   <- rvest::read_html(httr::content(response, as = "text", encoding = "UTF-8"))
      tables <- rvest::html_table(page, fill = TRUE)

      # Find the hitting table - has AB and H columns and NAME/Player column
      hitting_tbl <- NULL
      for (tbl in tables) {
        col_upper <- toupper(names(tbl))
        has_ab    <- any(grepl("^AB", col_upper))
        has_h     <- any(col_upper == "H")
        has_name  <- any(grepl("PLAYER|NAME|^#$", col_upper))
        if (has_ab && has_h && has_name && nrow(tbl) > 2) {
          hitting_tbl <- tbl
          break
        }
      }

      if (is.null(hitting_tbl)) {
        cat("  No hitting table found - skipping\n")
        next
      }

      names(hitting_tbl) <- toupper(trimws(names(hitting_tbl)))

      get_col <- function(df, ...) {
        nms <- toupper(trimws(names(df)))
        for (nm in c(...)) {
          # Exact match first
          idx <- which(nms == nm)
          if (length(idx) > 0) return(idx[1])
          # Prefix match (e.g. "2B" matches "2B (DOUBLES)")
          idx <- which(startsWith(nms, nm))
          if (length(idx) > 0) return(idx[1])
        }
        NA_integer_
      }

      name_col <- get_col(hitting_tbl, "PLAYER", "NAME")
      ab_col   <- get_col(hitting_tbl, "AB", "AB (AT BATS)")
      h_col    <- get_col(hitting_tbl, "H", "H (HITS)")
      d_col    <- get_col(hitting_tbl, "2B", "2B (DOUBLES)")
      t_col    <- get_col(hitting_tbl, "3B", "3B (TRIPLES)")
      hr_col   <- get_col(hitting_tbl, "HR", "HR (HOME RUNS)")
      bb_col   <- get_col(hitting_tbl, "BB", "BB (BASE ON BALLS)")
      hbp_col  <- get_col(hitting_tbl, "HBP", "HBP (HIT BY PITCH)")
      so_col   <- get_col(hitting_tbl, "SO", "SO (STRIKEOUTS)", "K")

      if (is.na(name_col) || is.na(ab_col) || is.na(h_col)) {
        cat("  Missing required columns - skipping\n")
        next
      }

      team_stats <- data.frame()

      for (row_idx in seq_len(nrow(hitting_tbl))) {
        tryCatch({
          player_raw <- trimws(as.character(hitting_tbl[row_idx, name_col]))

          # Skip totals, opponents, header rows, separator rows
          if (is.na(player_raw) || nchar(player_raw) < 2) next
          if (grepl("^(Total|Opponent|Player|Name|---)", player_raw, ignore.case = TRUE)) next

          # Remove trailing dots used for monospace padding ("Hudson Ellis.......")
          player_clean <- trimws(gsub("\\.+$", "", player_raw))
          if (nchar(player_clean) < 2) next

          safe_num <- function(col_idx, default = 0) {
            if (is.na(col_idx)) return(default)
            v <- suppressWarnings(as.numeric(gsub("[^0-9.]", "",
                    as.character(hitting_tbl[row_idx, col_idx]))))
            if (is.na(v) || length(v) == 0) default else v
          }

          ab_val  <- safe_num(ab_col)
          h_val   <- safe_num(h_col)
          if (ab_val <= 0) next

          d_val   <- safe_num(d_col)
          t_val   <- safe_num(t_col)
          hr_val  <- safe_num(hr_col)
          bb_val  <- safe_num(bb_col)
          hbp_val <- safe_num(hbp_col)
          so_val  <- safe_num(so_col)

          singles_val        <- max(0, h_val - d_val - t_val - hr_val)
          PA                 <- ab_val + bb_val + hbp_val
          batted_balls_count <- max(1, ab_val - so_val)

          if (PA <= 0) next

          actual_wOBA <- (singles_val * 0.888 + d_val * 1.271 +
                          t_val * 1.616 + hr_val * 2.101 +
                          (bb_val + hbp_val) * 0.690) / PA

          actual_wOBACON <- (singles_val * 0.888 + d_val * 1.271 +
                             t_val * 1.616 + hr_val * 2.101) / batted_balls_count

          # Name parsing: "First Last" (trailing dots already stripped)
          words <- str_trim(str_split(player_clean, "\\s+")[[1]])
          words <- words[nchar(words) > 0]

          if (length(words) >= 2) {
            first_initial <- toupper(substr(words[1], 1, 1))
            last_name     <- toupper(paste(words[2:length(words)], collapse = " "))
          } else if (length(words) == 1) {
            first_initial <- "X"
            last_name     <- toupper(words[1])
          } else next

          composite_key <- create_composite_key(last_name, first_initial, team_abbrev, season)

          team_stats <- rbind(team_stats, data.frame(
            Player                 = player_clean,
            Last_Name              = last_name,
            First_Initial          = first_initial,
            Team                   = team_name,
            Team_Code              = team_code,
            Team_Abbrev            = team_abbrev,
            Season                 = season,
            AB                     = ab_val,
            H                      = h_val,
            Singles                = singles_val,
            Doubles                = d_val,
            Triples                = t_val,
            HR                     = hr_val,
            BB                     = bb_val,
            HBP                    = hbp_val,
            SO                     = so_val,
            PA                     = PA,
            Batted_Balls           = batted_balls_count,
            wOBA                   = round(actual_wOBA, 3),
            wOBACON                = round(actual_wOBACON, 3),
            Player_Team_Season_Key = composite_key,
            stringsAsFactors = FALSE
          ))

        }, error = function(e) invisible(NULL))
      }

      if (nrow(team_stats) > 0) {
        cat("  Got", nrow(team_stats), "players\n")
        all_woba <- rbind(all_woba, team_stats)
      } else {
        cat("  No valid player rows found\n")
      }

      Sys.sleep(0.5)

    }, error = function(e) {
      cat("  Error:", e$message, "\n")
    })
  }

  cat("Total players scraped:", nrow(all_woba), "\n")
  return(all_woba)
}
# Helper function to create composite keys
create_composite_key <- function(last_name, first_initial, team_abbrev, season) {
  paste(
    toupper(trimws(last_name)),
    toupper(trimws(first_initial)),
    toupper(trimws(team_abbrev)),
    season,
    sep = "_"
  )
}

# SIMPLIFIED MATCHING WITH ROBUST ERROR HANDLING
match_necbl_data_composite <- function(expected_data, necbl_data, selected_season = "2026") {
  cat("=== MATCHING WITH STANDARDIZED 'LASTNAME, F' FORMAT (BOTH SIDES) - SEASON-AWARE ===\n")
  cat("Expected records (PRIMARY):", nrow(expected_data), "\n")
  cat("NECBL records (OVERLAY):", nrow(necbl_data), "\n")
  cat("Selected season:", selected_season, "\n")
  
  # CRITICAL FIX: Filter expected data to selected season FIRST
  if ("Expected_Season" %in% names(expected_data)) {
    expected_season_data <- expected_data %>%
      filter(Expected_Season == selected_season)
    cat("Expected records filtered to", selected_season, ":", nrow(expected_season_data), "\n")
  } else if ("Date" %in% names(expected_data)) {
    expected_season_data <- expected_data %>%
      mutate(Expected_Season = as.character(year(as.Date(Date)))) %>%
      filter(Expected_Season == selected_season)
    cat("Expected records filtered to", selected_season, "via Date:", nrow(expected_season_data), "\n")
  } else {
    cat("WARNING: No season information in expected data - using all records\n")
    expected_season_data <- expected_data %>%
      mutate(Expected_Season = selected_season)
  }
  
  # Filter NECBL data to selected season ONLY
  necbl_season_data <- necbl_data %>%
    filter(Season == selected_season)
  
  cat("NECBL records for", selected_season, ":", nrow(necbl_season_data), "\n")
  
  # ===================================================================
  # MODEL SIDE: PERFECT "LASTNAME, F" STANDARDIZATION (MATCHING POINTSTREAK)
  # ===================================================================
  
  expected_with_keys <- expected_season_data %>%
    mutate(
      # Initialize with defaults
      Model_Last_Name = "UNKNOWN",
      Model_First_Initial = "X"
    )
  
  # Process each row safely to ensure perfect matching
  for (i in 1:nrow(expected_with_keys)) {
    tryCatch({
      batter_name <- as.character(expected_with_keys$Batter[i])
      
      if (!is.na(batter_name) && nchar(str_trim(batter_name)) > 0) {
        
        # CRITICAL FIX: Clean model names the same way as Pointstreak
        clean_batter <- iconv(batter_name, to = "ASCII//TRANSLIT")  # López → Lopez  
        clean_batter <- gsub("'", "", clean_batter)  # O'Neill → ONeill, O'Connor → OConnor
        clean_batter <- gsub("[^A-Za-z\\s,.-]", "", clean_batter)  # Remove special chars
        clean_batter <- str_trim(clean_batter)
        
        # Parse the cleaned name
        if (grepl(",", clean_batter)) {
          # "Last, First" format (like "Stang, Bobby" or "Stang, Robert")
          parts <- str_split(clean_batter, ",")[[1]]
          if (length(parts) >= 2) {
            # CRITICAL: Extract exactly what Pointstreak does
            expected_with_keys$Model_Last_Name[i] <- toupper(str_trim(parts[1]))
            first_part <- str_trim(parts[2])
            # CRITICAL: Take ONLY first letter, uppercase (Bobby→B, Robert→R, but both→same person!)
            expected_with_keys$Model_First_Initial[i] <- toupper(substr(first_part, 1, 1))
          }
        } else {
          # "First Last" format (like "Bobby Stang" or "Robert Stang")
          words <- str_trim(str_split(clean_batter, "\\s+")[[1]])
          if (length(words) >= 2) {
            # CRITICAL: Take ONLY first letter, uppercase (Bobby→B, Robert→R)
            expected_with_keys$Model_First_Initial[i] <- toupper(substr(words[1], 1, 1))
            expected_with_keys$Model_Last_Name[i] <- toupper(paste(words[2:length(words)], collapse = " "))
          } else if (length(words) == 1) {
            expected_with_keys$Model_Last_Name[i] <- toupper(words[1])
            expected_with_keys$Model_First_Initial[i] <- "X"
          }
        }
      }
      
      # CRITICAL: Double-check first initial is exactly one character
      current_initial <- expected_with_keys$Model_First_Initial[i]
      expected_with_keys$Model_First_Initial[i] <- toupper(substr(str_trim(current_initial), 1, 1))
      if (nchar(expected_with_keys$Model_First_Initial[i]) == 0) {
        expected_with_keys$Model_First_Initial[i] <- "X"
      }
      
      # CRITICAL: Double-check last name is uppercase and trimmed
      current_last <- expected_with_keys$Model_Last_Name[i]
      expected_with_keys$Model_Last_Name[i] <- toupper(str_trim(current_last))
      if (nchar(expected_with_keys$Model_Last_Name[i]) == 0) {
        expected_with_keys$Model_Last_Name[i] <- "UNKNOWN"
      }
      
    }, error = function(e) {
      # Keep defaults if parsing fails
    })
  }
  
  # Complete the processing
  expected_with_keys <- expected_with_keys %>%
    mutate(
      # CREATE IDENTICAL "LASTNAME, F" FORMAT AS POINTSTREAK
      Model_Standardized_Name = paste(Model_Last_Name, Model_First_Initial, sep = ", "),
      
      # Get team abbreviation from BatterTeam using enhanced mapping
      Team_Abbrev = sapply(BatterTeam, function(x) {
        team_info <- necbl_team_mapping_enhanced[[x]]
        if (!is.null(team_info)) team_info$abbrev else "UNK"
      }),
      
      # Generate composite key using IDENTICAL components as Pointstreak
      Expected_Composite_Key = create_composite_key(Model_Last_Name, Model_First_Initial, Team_Abbrev, selected_season)
    )
  
  cat("Generated", nrow(expected_with_keys), "composite keys for expected data in", selected_season, "\n")
  
  # Show sample expected keys for verification
  cat("Sample standardized model names for", selected_season, ":\n")
  sample_keys <- expected_with_keys %>% 
    distinct(Batter, Model_Standardized_Name, Expected_Composite_Key, BatterTeam) %>% 
    head(3)
  print(sample_keys)
  
  # If no NECBL data for selected season, return expected-only
  if (nrow(necbl_season_data) == 0) {
    cat("No NECBL data for", selected_season, "- returning expected data only\n")
    return(expected_with_keys %>%
             select(-Model_Last_Name, -Model_First_Initial, -Team_Abbrev, -Expected_Composite_Key) %>%
             mutate(
               actual_woba_final = NA_real_,
               actual_wobacon_final = NA_real_,
               data_source = "Expected Only"
             ))
  }
  
  # If no expected data for selected season, return empty
  if (nrow(expected_with_keys) == 0) {
    cat("No expected data for", selected_season, "- returning empty dataset\n")
    return(data.frame())
  }
  
  # Show sample NECBL names for comparison
  cat("Sample standardized NECBL names for", selected_season, ":\n")
  if (nrow(necbl_season_data) > 0) {
    sample_necbl <- necbl_season_data %>% 
      select(Player, Player_Team_Season_Key) %>% 
      head(3)
    print(sample_necbl)
  }
  
  # SIMPLE EXACT JOIN ON COMPOSITE KEYS
  joined_data <- expected_with_keys %>%
    left_join(
      necbl_season_data %>% 
        select(Player_Team_Season_Key, Player, wOBA, wOBACON, Team, Team_Abbrev, PA, Batted_Balls), 
      by = c("Expected_Composite_Key" = "Player_Team_Season_Key")
    )
  
  matched_data <- joined_data %>%
    mutate(
      # UPDATE BATTER NAME TO STANDARDIZED FORMAT FOR CONSISTENCY
      Batter = Model_Standardized_Name,
      
      # Overlay actual data where available
      actual_woba_final = wOBA,
      actual_wobacon_final = wOBACON,
      
      # Data source
      data_source = case_when(
        !is.na(wOBA) ~ paste0("Expected + Actual (", selected_season, ")"),
        TRUE ~ "Expected Only"
      )
    ) %>%
    # Clean up temporary columns
    select(-Model_Last_Name, -Model_First_Initial, -Expected_Composite_Key, -Model_Standardized_Name) %>%
    # Remove the joined columns that we've copied to new names
    select(-any_of(c("Player", "wOBA", "wOBACON", "Team", "Team_Abbrev", "PA", "Batted_Balls")))
  
  # MATCHING SUMMARY
  total_records <- nrow(matched_data)
  exact_matches <- sum(!is.na(matched_data$actual_woba_final))
  
  cat("\n=== STANDARDIZED NAME MATCHING SUMMARY ===\n")
  cat("🎯 SEASON FOCUS:", selected_season, "\n")
  cat("✅ EXPECTED RECORDS FOR", selected_season, ":", total_records, "(100%)\n")
  cat("🎯 EXACT MATCHES FOUND:", exact_matches, "\n")
  cat("📝 ALL NAMES NOW IN 'LASTNAME, F' FORMAT\n")
  
  return(matched_data)
}

# Helper function to analyze match quality - UPDATED FOR SEASON AWARENESS
analyze_match_quality <- function(matched_data) {
  cat("=== DETAILED MATCH QUALITY ANALYSIS (SEASON-AWARE) ===\n")
  
  # Overall statistics
  total <- nrow(matched_data)
  matched <- sum(!is.na(matched_data$actual_woba_final))
  
  cat("Total Expected Players:", total, "\n")
  cat("Successfully Matched:", matched, "\n")
  
  # Show season info if available
  if ("Expected_Season" %in% names(matched_data)) {
    season_info <- matched_data %>%
      count(Expected_Season, name = "Records") %>%
      arrange(Expected_Season)
    cat("Expected data by season:\n")
    print(season_info)
  }
  
  cat("\n")
  
  # By team analysis
  team_analysis <- matched_data %>%
    group_by(BatterTeam) %>%
    summarise(
      Total_Players = n(),
      Matched_Players = sum(!is.na(actual_woba_final)),
      .groups = "drop"
    ) %>%
    arrange(desc(Matched_Players))
  
  cat("PLAYERS BY TEAM:\n")
  print(team_analysis)
  
  return(team_analysis)
}
# ===================================================================
# SECTION 6: LOAD ALL SEASONS WITH COMPOSITE KEY TRACKING
# ===================================================================

# Load all seasons with progress tracking and composite key verification
get_all_necbl_seasons_composite <- function(progress_callback = NULL) {
  cat("=== LOADING ALL NECBL SEASONS (2021-2025) WITH COMPOSITE KEYS ===\n")
  
  all_seasons_data <- data.frame()
  seasons <- c("2021", "2022", "2023", "2024", "2025")
  total_composite_keys <- 0
  
  for (i in seq_along(seasons)) {
    if (!is.null(progress_callback)) {
      progress_callback(detail = paste("Loading", seasons[i], "season..."), 
                        value = i / length(seasons))
    }
    
    cat("\n📅 Scraping", seasons[i], "season...\n")
    season_data <- get_necbl_woba_by_season(seasons[i])
    
    if (nrow(season_data) > 0) {
      # Verify composite keys for this season
      season_keys <- unique(season_data$Player_Team_Season_Key)
      duplicate_check <- season_data %>%
        group_by(Player_Team_Season_Key) %>%
        filter(n() > 1)
      
      if (nrow(duplicate_check) > 0) {
        cat("⚠️  WARNING:", nrow(duplicate_check), "duplicate composite keys in", seasons[i], "\n")
        print(duplicate_check %>% select(Player, Player_Team_Season_Key, Team))
      } else {
        cat("✅ All", length(season_keys), "composite keys unique for", seasons[i], "\n")
      }
      
      all_seasons_data <- rbind(all_seasons_data, season_data)
      total_composite_keys <- total_composite_keys + length(season_keys)
      cat("📊 Added", nrow(season_data), "records for", seasons[i], "\n")
    } else {
      cat("❌ No data found for", seasons[i], "\n")
    }
    
    # Small delay between seasons
    Sys.sleep(2)
  }
  
  cat("\n=== FINAL COMPOSITE KEY VERIFICATION ===\n")
  
  if (nrow(all_seasons_data) > 0) {
    # Check for any cross-season duplicates (shouldn't happen with season in key)
    all_keys <- all_seasons_data$Player_Team_Season_Key
    duplicate_keys <- all_seasons_data %>%
      group_by(Player_Team_Season_Key) %>%
      filter(n() > 1)
    
    if (nrow(duplicate_keys) > 0) {
      cat("❌ CRITICAL ERROR:", nrow(duplicate_keys), "duplicate composite keys across all seasons!\n")
      print(duplicate_keys %>% select(Player, Player_Team_Season_Key, Season, Team))
    } else {
      cat("✅ ALL", length(unique(all_keys)), "COMPOSITE KEYS ARE GLOBALLY UNIQUE!\n")
    }
    
    # Summary statistics
    cat("\n📈 COMPOSITE KEY SUMMARY:\n")
    season_summary <- all_seasons_data %>%
      group_by(Season) %>%
      summarise(
        Players = n(),
        Unique_Keys = n_distinct(Player_Team_Season_Key),
        Teams = n_distinct(Team_Abbrev),
        Avg_wOBA = round(mean(wOBA, na.rm = TRUE), 3),
        Avg_wOBACON = round(mean(wOBACON, na.rm = TRUE), 3),
        .groups = "drop"
      ) %>%
      arrange(Season)
    
    print(season_summary)
    
    # Show sample composite keys from each season
    cat("\n🔍 SAMPLE COMPOSITE KEYS BY SEASON:\n")
    sample_keys <- all_seasons_data %>%
      group_by(Season) %>%
      slice_head(n = 2) %>%
      select(Season, Player, Player_Team_Season_Key, Team_Abbrev) %>%
      ungroup()
    
    print(sample_keys)
    
  } else {
    cat("❌ NO DATA LOADED FROM ANY SEASON\n")
  }
  
  cat("\n=== TOTAL NECBL DATA LOADED ===\n")
  cat("📊 Total records across all seasons:", nrow(all_seasons_data), "\n")
  cat("🔑 Total unique composite keys:", length(unique(all_seasons_data$Player_Team_Season_Key)), "\n")
  cat("📅 Seasons loaded:", paste(unique(all_seasons_data$Season), collapse = ", "), "\n")
  cat("🏟️  Teams represented:", length(unique(all_seasons_data$Team_Abbrev)), "\n")
  
  return(all_seasons_data)
}

# Enhanced initialization with composite key support - FIXED VERSION
initialize_enhanced_data_composite <- function() {
  cat("=== INITIALIZING ENHANCED DATA WITH COMPOSITE KEY SUPPORT ===\n")
  
  # Try to use your existing data - check multiple possible names
  if (exists("pipeline_results") && is.data.frame(pipeline_results) && nrow(pipeline_results) > 0) {
    cat("✅ Using existing pipeline_results data\n")
    base_data <- pipeline_results
    
    # Ensure proper xwOBA/xwOBACON separation
    if (!"predicted_xwobacon_final" %in% names(base_data)) {
      cat("🔄 Recalculating xwOBA/xwOBACON separation...\n")
      base_data <- calculate_expected_xwoba_and_full(base_data, if(exists("expected")) expected else NULL)
    }
    
  } else if (exists("combined_df") && is.data.frame(combined_df) && nrow(combined_df) > 0) {
    cat("✅ Using existing combined_df data\n")
    base_data <- combined_df
    
    # Ensure proper xwOBA/xwOBACON separation
    if (!"predicted_xwobacon_final" %in% names(base_data)) {
      cat("🔄 Recalculating xwOBA/xwOBACON separation...\n")
      base_data <- calculate_expected_xwoba_and_full(base_data, if(exists("expected")) expected else NULL)
    }
    
  } else if (exists("raw_data") && is.data.frame(raw_data) && nrow(raw_data) > 0) {
    cat("✅ Using raw_data and applying expected calculations\n")
    base_data <- calculate_expected_xwoba_and_full(raw_data, if(exists("expected")) expected else NULL)
    
  } else {
    cat("❌ No existing data found - create sample data first\n")
    return(NULL)
  }
  
  # Verify required columns exist
  required_cols <- c("Batter", "BatterTeam")
  missing_cols <- required_cols[!required_cols %in% names(base_data)]
  
  if (length(missing_cols) > 0) {
    cat("❌ MISSING REQUIRED COLUMNS:", paste(missing_cols, collapse = ", "), "\n")
    return(NULL)
  }
  
  # Check BatterTeam values against mapping
  unique_teams <- unique(base_data$BatterTeam)
  mapped_teams <- names(necbl_team_mapping_enhanced)
  unmapped_teams <- unique_teams[!unique_teams %in% mapped_teams]
  
  cat("🏟️  TEAM CODE VERIFICATION:\n")
  cat("   Expected teams found:", paste(intersect(unique_teams, mapped_teams), collapse = ", "), "\n")
  
  if (length(unmapped_teams) > 0) {
    cat("⚠️  UNMAPPED TEAM CODES:", paste(unmapped_teams, collapse = ", "), "\n")
    cat("   These will get 'UNK' abbreviation in composite keys\n")
  }
  
  # Ensure all required columns exist with proper defaults
  final_data <- base_data %>%
    mutate(
      predicted_xwoba_final = if("predicted_xwoba_final" %in% names(.)) predicted_xwoba_final else 0.350,
      predicted_xwobacon_final = if("predicted_xwobacon_final" %in% names(.)) predicted_xwobacon_final else 0.400,
      
      # Initialize actual overlay columns for composite key matching
      actual_woba_final = NA_real_,
      actual_wobacon_final = NA_real_,
      data_source = "Expected Only"
    )
  
  cat("\n✅ Enhanced data initialized with", nrow(final_data), "total records\n")
  
  return(final_data)
}
# ===================================================================
# SECTION 7: ENHANCED SHINY UI
# ===================================================================

ui <- dashboardPage(
  dashboardHeader(title = "xwOBA/xwOBACON Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("xwOBA vs. wOBA", tabName = "performance", icon = icon("chart-line")),
      menuItem("Underperformers", tabName = "underperform", icon = icon("arrow-down")),
      menuItem("Overperformers", tabName = "overperform", icon = icon("arrow-up")),
      menuItem("Player Comparison", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("Team Browser", tabName = "browser", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .season-highlight {
          background-color: #fff3cd !important;
        }
        .exact-match {
          background-color: #d4edda !important;
        }
        .no-match {
          background-color: #f8d7da !important;
        }
        .box-solid > .box-header {
          color: #fff;
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        }
        .progress-bar {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        }
      "))
    ),
    
    tabItems(
      # Performance Analysis Tab
      tabItem(tabName = "performance",
              fluidRow(
                box(
                  title = "Season & Filter Controls", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(3, 
                           selectInput("primary_season", "Primary Season:", 
                                       choices = c("2026", "2025", "2024", "2023", "2022", "2021"), 
                                       selected = "2026")
                    ),
                    column(3, 
                           selectInput("season_scope", "Show Players:", 
                                       choices = c("Selected Season Only" = "selected", 
                                                   "All Available Seasons" = "all"), 
                                       selected = "selected")
                    ),
                    column(3, 
                           selectInput("pa_threshold", "Minimum PAs:", 
                                       choices = c("All Players" = 0, "10+" = 10, "20+" = 20, "30+" = 30),
                                       selected = 20)
                    ),
                    column(3, 
                           selectInput("metric_type", "Primary Metric:", 
                                       choices = c("xwOBA vs wOBA" = "xwoba", "xwOBACON vs wOBACON" = "xwobacon"), 
                                       selected = "xwoba")
                    )
                  ),
                  hr(),
                  fluidRow(
                    column(3,
                           actionButton("load_season", "Load Selected Season", 
                                        class = "btn btn-primary", icon = icon("download"))
                    ),
                    column(3,
                           actionButton("load_all", "Load All Seasons (2021-2025)", 
                                        class = "btn btn-info", icon = icon("database"))
                    ),
                    column(6,
                           div(id = "data_status", 
                               textOutput("current_data_status", inline = TRUE))
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Expected vs Actual", status = "primary", solidHeader = TRUE, width = 12,
                  plotlyOutput("performance_plotly", height = "600px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Player Selection", status = "primary", solidHeader = TRUE, width = 4,
                  selectInput("focus_player", "Focus on Player:", 
                              choices = c("All Players" = ""), selected = "")
                ),
                box(
                  title = "Summary", status = "info", solidHeader = TRUE, width = 8,
                  DT::dataTableOutput("performance_summary")
                )
              )
      ),
      
      # Underperformers Tab
      tabItem(tabName = "underperform",
              fluidRow(
                box(
                  title = "Players Getting Unlucky (Expected > Actual)", 
                  status = "success", solidHeader = TRUE, width = 12,
                  p(paste("Players whose expected predictions exceed their actual results.",
                          "These players may be due for positive regression.")),
                  DT::dataTableOutput("underperformers_table")
                )
              )
      ),
      
      # Overperformers Tab
      tabItem(tabName = "overperform",
              fluidRow(
                box(
                  title = "Players Getting Lucky (Actual > Expected)", 
                  status = "warning", solidHeader = TRUE, width = 12,
                  p(paste("Players whose actual results exceed their expected predictions.",
                          "These players may be due for negative regression.")),
                  DT::dataTableOutput("overperformers_table")
                )
              )
      ),
      
      # Player Comparison Tab
      tabItem(tabName = "comparison",
              fluidRow(
                box(
                  title = "Player Selection", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(4, selectInput("comp_player1", "Player 1:", choices = c("Choose Player" = ""))),
                    column(4, selectInput("comp_player2", "Player 2:", choices = c("Choose Player" = ""))),
                    column(4, dateRangeInput("comp_date_range", "Date Range:", 
                                             start = Sys.Date() - 90, end = Sys.Date()))
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Comparison", status = "primary", solidHeader = TRUE, width = 12,
                  plotOutput("comparison_plot", height = "500px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Comparison Summary", status = "info", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("comparison_summary_table")
                )
              )
      ),
      
      # Team Browser Tab
      tabItem(tabName = "browser",
              fluidRow(
                box(
                  title = "Browser Controls", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(3, selectInput("browser_team", "Filter by Team:", 
                                          choices = c("All Teams" = "All"))),
                    column(3, selectInput("browser_player", "Filter by Player:", 
                                          choices = c("All Players" = "All"))),
                    column(3, selectInput("browser_sort", "Sort by:", 
                                          choices = c("xwOBA" = "Avg_xwOBA", "wOBA" = "Avg_wOBA", 
                                                      "xwOBACON" = "Avg_xwOBACON", "wOBACON" = "Avg_wOBACON", 
                                                      "PAs" = "PA_Count"),
                                          selected = "Avg_xwOBA")),
                    column(3, checkboxInput("show_only_matched", "Show Only Matched Players", value = TRUE))
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Team & Player Browser", status = "primary", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("browser_table")
                )
              )
      )
    )
  )
)
# ===================================================================
# SECTION 8 PART 1: ENHANCED SHINY SERVER - SEASON-AWARE VERSION (Reactive Values & Data Loading)
# ===================================================================

server <- function(input, output, session) {
  
  # Enhanced reactive values
  values <- reactiveValues(
    expected_data = NULL,
    necbl_data_all = NULL,
    necbl_data_current = NULL,
    enhanced_data = NULL,
    available_seasons = character(0),
    last_update = NULL
  )
  
  # Initialize with composite key support
  observe({
    if (is.null(values$expected_data)) {
      values$expected_data <- initialize_enhanced_data_composite()
      if (!is.null(values$expected_data)) {
        values$enhanced_data <- values$expected_data
        cat("✅ Initialized with", nrow(values$enhanced_data), "expected records\n")
      }
    }
  })
  
  # CRITICAL FIX: Helper function for filtered data with MATCHED PLAYERS ONLY
  get_filtered_data <- function() {
    req(values$enhanced_data)
    
    current_data <- values$enhanced_data
    
    if (is.null(current_data) || nrow(current_data) == 0) {
      return(data.frame())
    }
    
    # CRITICAL: FILTER TO MATCHED PLAYERS ONLY (those with actual_woba_final)
    current_data <- current_data %>%
      filter(!is.na(actual_woba_final))
    
    # Apply season filter to matched data only
    if (!is.null(input$season_scope) && input$season_scope == "matched" && 
        !is.null(input$primary_season)) {
      
      # Filter matched data to selected season
      if ("Expected_Season" %in% names(current_data)) {
        # Use existing Expected_Season column
        current_data <- current_data %>%
          filter(Expected_Season == input$primary_season)
        cat("Filtered players to", input$primary_season, "using Expected_Season:", nrow(current_data), "records\n")
      } else if ("Date" %in% names(current_data)) {
        # Extract year from Date column and filter to selected season
        current_data <- current_data %>%
          mutate(
            Expected_Season = as.character(year(as.Date(Date)))
          ) %>%
          filter(Expected_Season == input$primary_season)
        cat("Filtered players to", input$primary_season, "using Date extraction:", nrow(current_data), "records\n")
      } else {
        # If no season info, warn but continue
        cat("WARNING: No season information available - showing all matched data\n")
      }
    }
    
    # Apply PA threshold AFTER season and match filtering
    pa_threshold <- as.numeric(input$pa_threshold %||% 0)
    if (pa_threshold > 0) {
      current_data <- current_data %>%
        group_by(Batter) %>%
        filter(n() >= pa_threshold) %>%
        ungroup()
    }
    
    return(current_data)
  }
  
  # Update UI choices dynamically - MATCHED PLAYERS ONLY
  observe({
    current_data <- get_filtered_data()
    
    if (!is.null(current_data) && nrow(current_data) > 0) {
      player_choices <- c("All Players" = "", sort(unique(current_data$Batter)))
      
      updateSelectInput(session, "focus_player", choices = player_choices)
      updateSelectInput(session, "comp_player1", choices = c("Choose Player" = "", sort(unique(current_data$Batter))))
      updateSelectInput(session, "comp_player2", choices = c("Choose Player" = "", sort(unique(current_data$Batter))))
      updateSelectInput(session, "browser_player", choices = c("All Players" = "All", sort(unique(current_data$Batter))))
      
      if ("BatterTeam" %in% names(current_data)) {
        team_choices <- c("All Teams" = "All", sort(unique(current_data$BatterTeam)))
        updateSelectInput(session, "browser_team", choices = team_choices)
      }
    }
  })
  
  # Enhanced data status - show nothing after loading
  output$current_data_status <- renderText({
    ""
  })
  
  # Load selected season - SEASON-AWARE
  observeEvent(input$load_season, {
    withProgress(message = paste('Loading', input$primary_season, 'season...'), value = 0, {
      
      incProgress(0.3, detail = "Scraping actual data...")
      
      tryCatch({
        # Load from pre-scraped cache (pipeline scrapes daily via GitHub Actions)
        season_data <- necbl_stats_cache[[input$primary_season]]
        
        if (!is.null(season_data) && nrow(season_data) > 0) {
          values$necbl_data_current <- season_data
          
          # Update all seasons cache
          if (!is.null(values$necbl_data_all)) {
            values$necbl_data_all <- values$necbl_data_all %>%
              filter(Season != input$primary_season) %>%
              rbind(season_data)
          } else {
            values$necbl_data_all <- season_data
          }
          
          values$available_seasons <- unique(values$necbl_data_all$Season)
          values$last_update <- Sys.time()
          
          incProgress(0.6, detail = "Matching data...")
          
          # CRITICAL FIX: Re-match with season-aware matching
          if (!is.null(values$expected_data)) {
            values$enhanced_data <- match_necbl_data_composite(
              values$expected_data, 
              values$necbl_data_all, 
              input$primary_season  # This ensures season-specific matching
            )
          }
          
          incProgress(1.0, detail = "Complete!")
          
        } else {
          showNotification(
            paste("❌ No data found for", input$primary_season, "season"),
            type = "error", duration = 10
          )
        }
      }, error = function(e) {
        showNotification(
          paste("❌ Error loading", input$primary_season, ":", e$message),
          type = "error", duration = 15
        )
      })
    })
  })
  
  # Load all seasons - SEASON-AWARE
  observeEvent(input$load_all, {
    withProgress(message = 'Loading all NECBL seasons...', value = 0, {
      
      tryCatch({
        # Use progress callback
        progress_func <- function(detail = "", value = 0) {
          incProgress(value * 0.8, detail = detail)
        }
        
        values$necbl_data_all <- get_all_necbl_seasons_composite(progress_func)
        
        if (!is.null(values$necbl_data_all) && nrow(values$necbl_data_all) > 0) {
          values$necbl_data_current <- values$necbl_data_all %>%
            filter(Season == input$primary_season)
          
          values$available_seasons <- unique(values$necbl_data_all$Season)
          values$last_update <- Sys.time()
          
          incProgress(0.9, detail = "Matching data...")
          
          # CRITICAL FIX: Re-match with season-aware matching
          if (!is.null(values$expected_data)) {
            values$enhanced_data <- match_necbl_data_composite(
              values$expected_data, 
              values$necbl_data_all, 
              input$primary_season  # This ensures season-specific matching
            )
          }
          
          incProgress(1.0, detail = "All seasons loaded!")
          
        } else {
          showNotification("❌ Error loading season data", type = "error", duration = 10)
        }
      }, error = function(e) {
        showNotification(paste("❌ Error loading all seasons:", e$message), type = "error", duration = 15)
      })
    })
  })
  # Enhanced performance plot - MATCHED PLAYERS ONLY VERSION
  output$performance_plotly <- renderPlotly({
    current_data <- get_filtered_data()  # This now returns matched players only
    
    if (nrow(current_data) == 0) {
      p <- ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5, label = paste0("No players for ", input$primary_season, "\nLoad season data first")), size = 6) +
        theme_void()
      return(ggplotly(p))
    }
    
    # Filter for specific player if selected
    if (!is.null(input$focus_player) && input$focus_player != "") {
      current_data <- current_data %>% filter(Batter == input$focus_player)
      plot_title <- paste("xwOBA vs. wOBA:", input$focus_player, "(", input$primary_season, ")")
    } else {
      plot_title <- paste("xwOBA vs. wOBA:", input$primary_season, "Season - All")
    }
    
    # Calculate player-level performance metrics - MATCHED PLAYERS ONLY
    player_performance <- current_data %>%
      group_by(Batter) %>%
      summarise(
        xwOBA = mean(predicted_xwoba_final, na.rm = TRUE),
        wOBA = mean(actual_woba_final, na.rm = TRUE),
        xwOBACON = mean(predicted_xwobacon_final, na.rm = TRUE),
        wOBACON = mean(actual_wobacon_final, na.rm = TRUE),
        PA_Count = n(),
        Team = first(BatterTeam),
        Season = if("Expected_Season" %in% names(current_data)) first(Expected_Season) else input$primary_season,
        .groups = "drop"
      )
    
    if (nrow(player_performance) == 0) {
      p <- ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5, 
                      label = paste0("No players for ", input$primary_season, "\n\nClick 'Load Selected Season' to get NECBL data")), 
                  size = 6, color = "#667eea") +
        theme_void()
      return(ggplotly(p))
    }
    
    # Select metrics based on input
    if (input$metric_type == "xwoba") {
      plot_data <- player_performance %>%
        select(Batter, xwOBA, wOBA, PA_Count, Team, Season) %>%
        rename(Expected = xwOBA, Actual = wOBA)
      x_label <- paste("wOBA (Actual", input$primary_season, ")")
      y_label <- paste("xwOBA (Expected", input$primary_season, ")")
    } else {
      plot_data <- player_performance %>%
        select(Batter, xwOBACON, wOBACON, PA_Count, Team, Season) %>%
        rename(Expected = xwOBACON, Actual = wOBACON)
      x_label <- paste("wOBACON (Actual", input$primary_season, ")")
      y_label <- paste("xwOBACON (Expected", input$primary_season, ")")
    }
    
    # Create enhanced scatter plot - MATCHED PLAYERS ONLY
    p <- ggplot(plot_data, aes(x = Actual, y = Expected, text = paste(
      "Player:", Batter,
      "<br>Team:", Team,
      "<br>Season:", Season,
      "<br>PAs:", PA_Count,
      "<br>Expected:", round(Expected, 3),
      "<br>Actual:", round(Actual, 3),
      "<br>Difference:", round(Expected - Actual, 3)
    ))) +
      geom_point(aes(color = Season), size = 4, alpha = 0.8) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#dc3545", size = 1) +
      scale_color_manual(values = c("2025" = "#28a745", 
                                    "2024" = "#20c997", 
                                    "2023" = "#fd7e14", 
                                    "2022" = "#ffc107",
                                    "2021" = "#6f42c1")) +
      labs(title = plot_title,
           x = x_label, y = y_label, color = "Season") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "right"
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        title = list(text = plot_title, font = list(size = 16)),
        showlegend = TRUE
      )
  })
  
  # Performance summary table - MATCHED PLAYERS ONLY
  output$performance_summary <- DT::renderDataTable({
    current_data <- get_filtered_data()  # This now returns matched players only
    
    if (nrow(current_data) == 0) return(data.frame())
    
    summary_data <- current_data %>%
      group_by(Batter) %>%
      summarise(
        PA = n(),
        xwOBA = round(mean(predicted_xwoba_final, na.rm = TRUE), 3),
        wOBA = round(mean(actual_woba_final, na.rm = TRUE), 3),
        xwOBACON = round(mean(predicted_xwobacon_final, na.rm = TRUE), 3),
        wOBACON = round(mean(actual_wobacon_final, na.rm = TRUE), 3),
        Team = first(BatterTeam),
        Season = if("Expected_Season" %in% names(current_data)) first(Expected_Season) else input$primary_season,
        .groups = "drop"
      ) %>%
      mutate(
        wOBA_Diff = round(xwOBA - wOBA, 3),
        wOBACON_Diff = round(xwOBACON - wOBACON, 3)
      ) %>%
      arrange(desc(abs(wOBA_Diff)))
    
    DT::datatable(
      summary_data,
      options = list(pageLength = 10, scrollX = TRUE),
      caption = paste("Summary -", input$primary_season, "Season Only"),
      rownames = FALSE
    ) %>%
      formatRound(columns = c('xwOBA', 'wOBA', 'xwOBACON', 'wOBACON', 'wOBA_Diff', 'wOBACON_Diff'), digits = 3) %>%
      formatStyle(columns = c('wOBA_Diff', 'wOBACON_Diff'), 
                  backgroundColor = styleInterval(c(-0.025, 0.025), c('#f8d7da', '#ffffff', '#d4edda')))
  })
  
  # Underperformers table - MATCHED PLAYERS ONLY
  output$underperformers_table <- DT::renderDataTable({
    current_data <- get_filtered_data()  # This now returns matched players only
    pa_threshold <- max(20, as.numeric(input$pa_threshold %||% 0))
    
    underperformers <- current_data %>%
      group_by(Batter) %>%
      summarise(
        PA = n(),
        xwOBA = round(mean(predicted_xwoba_final, na.rm = TRUE), 3),
        wOBA = round(mean(actual_woba_final, na.rm = TRUE), 3),
        xwOBACON = round(mean(predicted_xwobacon_final, na.rm = TRUE), 3),
        wOBACON = round(mean(actual_wobacon_final, na.rm = TRUE), 3),
        Team = first(BatterTeam),
        Season = if("Expected_Season" %in% names(current_data)) first(Expected_Season) else input$primary_season,
        .groups = "drop"
      ) %>%
      mutate(
        wOBA_Diff = round(xwOBA - wOBA, 3),
        wOBACON_Diff = round(xwOBACON - wOBACON, 3)
      ) %>%
      filter(PA >= pa_threshold, (wOBA_Diff > 0.025 | wOBACON_Diff > 0.025)) %>%
      arrange(desc(wOBA_Diff))
    
    DT::datatable(
      underperformers,
      options = list(pageLength = 15, scrollX = TRUE),
      caption = paste("Players Getting Unlucky -", input$primary_season, "Season"),
      rownames = FALSE
    ) %>%
      formatRound(columns = c('xwOBA', 'wOBA', 'xwOBACON', 'wOBACON', 'wOBA_Diff', 'wOBACON_Diff'), digits = 3) %>%
      formatStyle(c('wOBA_Diff', 'wOBACON_Diff'), backgroundColor = '#d4edda')
  })
  
  # Overperformers table - MATCHED PLAYERS ONLY
  output$overperformers_table <- DT::renderDataTable({
    current_data <- get_filtered_data()  # This now returns matched players only
    pa_threshold <- max(20, as.numeric(input$pa_threshold %||% 0))
    
    overperformers <- current_data %>%
      group_by(Batter) %>%
      summarise(
        PA = n(),
        xwOBA = round(mean(predicted_xwoba_final, na.rm = TRUE), 3),
        wOBA = round(mean(actual_woba_final, na.rm = TRUE), 3),
        xwOBACON = round(mean(predicted_xwobacon_final, na.rm = TRUE), 3),
        wOBACON = round(mean(actual_wobacon_final, na.rm = TRUE), 3),
        Team = first(BatterTeam),
        Season = if("Expected_Season" %in% names(current_data)) first(Expected_Season) else input$primary_season,
        .groups = "drop"
      ) %>%
      mutate(
        wOBA_Diff = round(wOBA - xwOBA, 3),
        wOBACON_Diff = round(wOBACON - xwOBACON, 3)
      ) %>%
      filter(PA >= pa_threshold, (wOBA_Diff > 0.025 | wOBACON_Diff > 0.025)) %>%
      arrange(desc(wOBA_Diff))
    
    DT::datatable(
      overperformers,
      options = list(pageLength = 15, scrollX = TRUE),
      caption = paste("Players Getting Lucky -", input$primary_season, "Season"),
      rownames = FALSE
    ) %>%
      formatRound(columns = c('xwOBA', 'wOBA', 'xwOBACON', 'wOBACON', 'wOBA_Diff', 'wOBACON_Diff'), digits = 3) %>%
      formatStyle(c('wOBA_Diff', 'wOBACON_Diff'), backgroundColor = '#f8d7da')
  })
  # Player comparison plot - MATCHED PLAYERS ONLY
  output$comparison_plot <- renderPlot({
    req(input$comp_player1, input$comp_player2)
    
    current_data <- get_filtered_data()  # This now returns matched players only
    
    if ("Date" %in% names(current_data)) {
      current_data <- current_data %>%
        filter(Date >= input$comp_date_range[1], Date <= input$comp_date_range[2])
    }
    
    comparison_data <- current_data %>%
      filter(Batter %in% c(input$comp_player1, input$comp_player2)) %>%
      arrange(if("Date" %in% names(.)) Date else row_number()) %>%
      group_by(Batter) %>%
      mutate(
        PA_Number = row_number(),
        Cumulative_xwOBA = cummean(predicted_xwoba_final),
        Cumulative_wOBA = cummean(actual_woba_final),
        Cumulative_xwOBACON = cummean(predicted_xwobacon_final),
        Cumulative_wOBACON = cummean(actual_wobacon_final)
      ) %>%
      ungroup()
    
    if (nrow(comparison_data) > 0) {
      line_data <- comparison_data %>%
        select(Batter, PA_Number, Cumulative_xwOBA, Cumulative_wOBA, Cumulative_xwOBACON, Cumulative_wOBACON) %>%
        pivot_longer(cols = starts_with("Cumulative"), names_to = "Metric", values_to = "Value") %>%
        mutate(
          Metric = gsub("Cumulative_", "", Metric),
          Line_Type = ifelse(grepl("x", Metric), "Expected", paste("Actual", input$primary_season)),
          Metric_Type = ifelse(grepl("OBACON", Metric), "wOBACON (Batted Balls)", "wOBA (All PAs)")
        ) %>%
        filter(!is.na(Value))
      
      ggplot(line_data, aes(x = PA_Number, y = Value, color = Batter, linetype = Line_Type)) +
        geom_line(size = 1.2, alpha = 0.8) +
        facet_wrap(~Metric_Type, scales = "free_y") +
        scale_color_manual(values = c("#667eea", "#764ba2")) +
        scale_linetype_manual(values = setNames(
          c("solid", "dashed"),
          c("Expected", paste0("Actual ", input$primary_season))
        )) +
        labs(title = paste("Player Comparison (", input$primary_season, " Season):", input$comp_player1, "vs", input$comp_player2),
             subtitle = paste("Cumulative performance -", input$primary_season, "season players only"),
             x = "Plate Appearance Number", y = "Cumulative Value",
             color = "Player", linetype = "Type") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          legend.position = "top"
        )
    } else {
      ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5, label = paste0("No matched data for selected players in ", input$primary_season)), size = 6) +
        theme_void()
    }
  })
  
  # Comparison summary table - MATCHED PLAYERS ONLY
  output$comparison_summary_table <- DT::renderDataTable({
    req(input$comp_player1, input$comp_player2)
    
    current_data <- get_filtered_data()  # This now returns matched players only
    
    if ("Date" %in% names(current_data)) {
      current_data <- current_data %>%
        filter(Date >= input$comp_date_range[1], Date <= input$comp_date_range[2])
    }
    
    comparison_summary <- current_data %>%
      filter(Batter %in% c(input$comp_player1, input$comp_player2)) %>%
      group_by(Batter) %>%
      summarise(
        PA = n(),
        Avg_xwOBA = round(mean(predicted_xwoba_final, na.rm = TRUE), 3),
        Avg_wOBA = round(mean(actual_woba_final, na.rm = TRUE), 3),
        Avg_xwOBACON = round(mean(predicted_xwobacon_final, na.rm = TRUE), 3),
        Avg_wOBACON = round(mean(actual_wobacon_final, na.rm = TRUE), 3),
        wOBA_Gap = round(mean(predicted_xwoba_final, na.rm = TRUE) - mean(actual_woba_final, na.rm = TRUE), 3),
        wOBACON_Gap = round(mean(predicted_xwobacon_final, na.rm = TRUE) - mean(actual_wobacon_final, na.rm = TRUE), 3),
        Team = first(BatterTeam),
        Season = if("Expected_Season" %in% names(current_data)) first(Expected_Season) else input$primary_season,
        .groups = "drop"
      )
    
    DT::datatable(
      comparison_summary,
      options = list(pageLength = 15, scrollX = TRUE),
      caption = paste("Player Comparison Summary -", input$primary_season, "Season"),
      rownames = FALSE
    ) %>%
      formatRound(columns = c('Avg_xwOBA', 'Avg_wOBA', 'Avg_xwOBACON', 'Avg_wOBACON', 'wOBA_Gap', 'wOBACON_Gap'), digits = 3)
  })
  
  # Enhanced team browser - MATCHED PLAYERS ONLY
  output$browser_table <- DT::renderDataTable({
    current_data <- get_filtered_data()  # This now returns matched players only
    
    if (!is.null(input$browser_team) && input$browser_team != "All") {
      if ("BatterTeam" %in% names(current_data)) {
        current_data <- current_data %>% filter(BatterTeam == input$browser_team)
      }
    }
    
    if (!is.null(input$browser_player) && input$browser_player != "All") {
      current_data <- current_data %>% filter(Batter == input$browser_player)
    }
    
    browser_data <- current_data %>%
      group_by(Batter) %>%
      summarise(
        PA_Count = n(),
        Avg_xwOBA = round(mean(predicted_xwoba_final, na.rm = TRUE), 3),
        Avg_wOBA = round(mean(actual_woba_final, na.rm = TRUE), 3),
        Avg_xwOBACON = round(mean(predicted_xwobacon_final, na.rm = TRUE), 3),
        Avg_wOBACON = round(mean(actual_wobacon_final, na.rm = TRUE), 3),
        wOBA_Gap = round(mean(predicted_xwoba_final, na.rm = TRUE) - mean(actual_woba_final, na.rm = TRUE), 3),
        wOBACON_Gap = round(mean(predicted_xwobacon_final, na.rm = TRUE) - mean(actual_wobacon_final, na.rm = TRUE), 3),
        Team = first(BatterTeam),
        Season = if("Expected_Season" %in% names(current_data)) first(Expected_Season) else input$primary_season,
        .groups = "drop"
      ) %>%
      arrange(desc(!!sym(input$browser_sort)))
    
    DT::datatable(
      browser_data,
      options = list(pageLength = 25, scrollX = TRUE),
      caption = paste("Browser -", input$primary_season, "Season Focus"),
      rownames = FALSE
    ) %>%
      formatRound(columns = c('Avg_xwOBA', 'Avg_wOBA', 'Avg_xwOBACON', 'Avg_wOBACON', 'wOBA_Gap', 'wOBACON_Gap'), digits = 3)
  })
  
}
# ===================================================================
# SECTION 9: FINAL LAUNCH FUNCTIONS AND USAGE INSTRUCTIONS - MATCHED PLAYERS ONLY VERSION
# ===================================================================

# Function to launch the matched players only enhanced dashboard
launch_xwoba_dashboard_composite <- function() {
  # Check if required data exists - check multiple possible names
  data_exists <- exists("pipeline_results") || exists("combined_df") || exists("raw_data")
  
  if (!data_exists) {
    stop("❌ No expected data found. Please ensure 'pipeline_results', 'combined_df', or 'raw_data' exists in your environment.")
  }
  
  cat("🎯 Launching Enhanced xwOBA Dashboard - Matched Players Only Edition...\n")
  cat("✅ Season-aware matching system\n")
  cat("✅ Expected data filtered by season BEFORE matching\n")
  cat("✅ MATCHED PLAYERS ONLY - no unmatched players shown\n") 
  cat("✅ OCE_STA and OCE_STA6 both map to Ocean State Waves\n")
  cat("✅ NSH_N maps to North Shore Navigators\n")
  cat("✅ 'X' character removal from NECBL player names\n")
  cat("✅ FIXED: Multi-season players properly separated by season\n")
  cat("✅ Interactive Plotly visualizations\n")
  cat("✅ Season-specific filtering and analysis throughout\n")
  
  shinyApp(ui = ui, server = server)
}

# Helper function to verify data readiness - MATCHED PLAYERS ONLY VERSION
verify_data_readiness <- function() {
  cat("=== MATCHED PLAYERS ONLY DATA READINESS VERIFICATION ===\n")
  
  # Check for required data
  data_sources <- c()
  if (exists("pipeline_results")) data_sources <- c(data_sources, "pipeline_results")
  if (exists("combined_df")) data_sources <- c(data_sources, "combined_df")
  if (exists("raw_data")) data_sources <- c(data_sources, "raw_data")
  
  if (length(data_sources) == 0) {
    cat("❌ NO DATA FOUND\n")
    cat("   Please ensure 'pipeline_results', 'combined_df', or 'raw_data' exists in your environment\n")
    return(FALSE)
  }
  
  cat("✅ DATA SOURCES FOUND:", paste(data_sources, collapse = ", "), "\n")
  
  # Get the primary data source
  if (exists("pipeline_results")) {
    primary_data <- pipeline_results
    data_name <- "pipeline_results"
  } else if (exists("combined_df")) {
    primary_data <- combined_df
    data_name <- "combined_df"
  } else {
    primary_data <- raw_data
    data_name <- "raw_data"
  }
  
  cat("📊 PRIMARY DATA SOURCE:", data_name, "with", nrow(primary_data), "records\n")
  
  # Check required columns
  required_cols <- c("Batter", "BatterTeam")
  missing_cols <- required_cols[!required_cols %in% names(primary_data)]
  
  if (length(missing_cols) > 0) {
    cat("❌ MISSING REQUIRED COLUMNS:", paste(missing_cols, collapse = ", "), "\n")
    return(FALSE)
  }
  
  cat("✅ REQUIRED COLUMNS FOUND:", paste(required_cols, collapse = ", "), "\n")
  
  # Check for season information
  if ("Date" %in% names(primary_data)) {
    seasons_found <- primary_data %>%
      mutate(Season = as.character(year(as.Date(Date)))) %>%
      count(Season) %>%
      arrange(Season)
    
    cat("🗓️  SEASONS FOUND IN DATA:\n")
    print(seasons_found)
  } else if ("Expected_Season" %in% names(primary_data)) {
    seasons_found <- primary_data %>%
      count(Expected_Season) %>%
      arrange(Expected_Season)
    
    cat("🗓️  SEASONS FOUND IN DATA:\n")
    print(seasons_found)
  } else {
    cat("⚠️  NO SEASON INFORMATION FOUND - will default to 2025\n")
  }
  
  # Check team codes
  unique_teams <- unique(primary_data$BatterTeam)
  mapped_teams <- names(necbl_team_mapping_enhanced)
  unmapped_teams <- unique_teams[!unique_teams %in% mapped_teams]
  
  cat("🏟️  TEAM CODE STATUS:\n")
  cat("   Total unique team codes:", length(unique_teams), "\n")
  cat("   Mapped team codes:", length(intersect(unique_teams, mapped_teams)), "\n")
  
  if (length(unmapped_teams) > 0) {
    cat("⚠️  UNMAPPED TEAM CODES:", paste(unmapped_teams, collapse = ", "), "\n")
    cat("   These will get 'UNK' abbreviation\n")
  }
  
  # Show sample data with season info
  cat("\n🔍 SAMPLE DATA PREVIEW:\n")
  if ("Date" %in% names(primary_data)) {
    sample_data <- primary_data %>%
      mutate(Season = as.character(year(as.Date(Date)))) %>%
      select(any_of(c("Batter", "BatterTeam", "Date", "Season", "Inning"))) %>%
      head(3)
  } else {
    sample_data <- primary_data %>%
      select(any_of(c("Batter", "BatterTeam", "Expected_Season", "Date", "Inning"))) %>%
      head(3)
  }
  print(sample_data)
  
  cat("\n✅ MATCHED PLAYERS ONLY DATA VERIFICATION COMPLETE - READY FOR LAUNCH!\n")
  return(TRUE)
}

# Function to test matched players only system
test_composite_key_system <- function() {
  cat("=== TESTING MATCHED PLAYERS ONLY SYSTEM ===\n")
  
  # Test season filtering logic
  cat("\n🗓️  TESTING SEASON FILTERING LOGIC:\n")
  
  if (exists("pipeline_results")) {
    if ("Date" %in% names(pipeline_results)) {
      season_summary <- pipeline_results %>%
        mutate(Season = as.character(year(as.Date(Date)))) %>%
        count(Season, name = "Records") %>%
        arrange(Season)
      
      cat("   Seasons available in your data:\n")
      print(season_summary)
      
      # Test filtering to 2025
      test_2025 <- pipeline_results %>%
        mutate(Season = as.character(year(as.Date(Date)))) %>%
        filter(Season == "2025")
      
      cat("   Records when filtered to 2025:", nrow(test_2025), "\n")
      
      if (nrow(test_2025) > 0) {
        unique_players_2025 <- length(unique(test_2025$Batter))
        cat("   Unique players in 2025:", unique_players_2025, "\n")
      }
    } else {
      cat("   No Date column found - season filtering may not work optimally\n")
    }
  } else {
    cat("   No pipeline_results found - cannot test season filtering\n")
  }
  
  cat("\n✅ MATCHED PLAYERS ONLY SYSTEM TEST COMPLETE!\n")
}

# Enhanced usage instructions - MATCHED PLAYERS ONLY VERSION
print_usage_instructions <- function() {
  cat("=== ENHANCED xwOBA DASHBOARD - MATCHED PLAYERS ONLY EDITION ===\n\n")
  
  cat("🚀 QUICK START:\n")
  cat("   1. verify_data_readiness()              # Check if your data is ready\n")
  cat("   2. test_composite_key_system()          # Test the matched players only system\n")
  cat("   3. launch_xwoba_dashboard_composite()   # Launch the dashboard\n\n")
  
  cat("🔧 KEY FIXES IN THIS VERSION:\n")
  cat("   ✅ FIXED: Only matched players shown throughout dashboard\n")
  cat("   ✅ FIXED: No unmatched or expected-only players displayed\n")
  cat("   ✅ FIXED: All plots, tables, and analysis show actual vs expected only\n")
  cat("   ✅ FIXED: Season-aware matching prevents data contamination\n")
  cat("   ✅ FIXED: Clean interface focusing on players with both metrics\n\n")
  
  cat("📋 KEY FEATURES:\n")
  cat("   ✅ Matched players only (no expected-only players shown)\n")
  cat("   ✅ Season-aware matching with proper filtering\n")
  cat("   ✅ Interactive Plotly plots with matched player tooltips\n")
  cat("   ✅ All analysis restricted to players with actual NECBL data\n")
  cat("   ✅ Enhanced verification and debugging tools\n")
  cat("   ✅ OCE_STA/OCE_STA6 both map correctly to Ocean State Waves\n")
  cat("   ✅ NSH_N maps correctly to North Shore Navigators\n")
  cat("   ✅ 'X' character removal from NECBL player names\n\n")
  
  cat("📖 DASHBOARD USAGE:\n")
  cat("   1. Launch dashboard and select your desired season (2025, 2024, etc.)\n")
  cat("   2. Click 'Load Selected Season' to get NECBL data for that season only\n")
  cat("   3. Dashboard automatically shows only matched players\n")
  cat("   4. All analysis (plots, tables) shows only players with both expected and actual data\n")
  cat("   5. Multi-season players properly separated by season\n\n")
  
  cat("⚠️  REQUIREMENTS:\n")
  cat("   • Your data must have 'Batter', 'BatterTeam', and 'Date' columns\n")
  cat("   • Date column used to extract season information\n")
  cat("   • Internet connection for NECBL data scraping\n")
  cat("   • R packages: shiny, shinydashboard, DT, ggplot2, dplyr, tidyr, rvest, httr, stringr, xml2, plotly\n\n")
  
  cat("🎯 TROUBLESHOOTING:\n")
  cat("   • No players showing? Load NECBL data first to get matches\n")
  cat("   • Player showing in multiple seasons? Check if Date column exists and is formatted correctly\n")
  cat("   • No data loading? Check internet connection and NECBL website status\n\n")
}

# Quick test function to verify matched players only system works
quick_composite_test <- function() {
  cat("🔥 QUICK MATCHED PLAYERS ONLY TEST\n")
  
  # Test with actual data if available
  if (exists("pipeline_results")) {
    cat("✅ Testing with pipeline_results...\n")
    
    # Check for season information
    if ("Date" %in% names(pipeline_results)) {
      seasons <- pipeline_results %>%
        mutate(Season = as.character(year(as.Date(Date)))) %>%
        count(Season) %>%
        arrange(Season)
      
      cat("✅ Season information found:\n")
      print(seasons)
      
      # Test season filtering
      test_2025 <- pipeline_results %>%
        mutate(Season = as.character(year(as.Date(Date)))) %>%
        filter(Season == "2025")
      
      if (nrow(test_2025) > 0) {
        cat("✅ Season filtering works: 2025 has", nrow(test_2025), "records\n")
        
        # Test a small scrape and match for 2025
        small_test <- tryCatch({
          get_necbl_woba_by_season("2025")
        }, error = function(e) {
          cat("❌ Scraping test failed:", e$message, "\n")
          return(data.frame())
        })
        
        if (nrow(small_test) > 0) {
          cat("✅ Scraping works:", nrow(small_test), "players for 2025\n")
          
          # Test matched players only matching
          small_match <- match_necbl_data_composite(pipeline_results, small_test, "2025")
          matches <- sum(!is.na(small_match$actual_woba_final))
          cat("✅ Matched players only matching works:", matches, "matches for 2025\n")
          
          if (matches > 0) {
            cat("🎉 MATCHED PLAYERS ONLY SYSTEM IS WORKING! Ready to launch dashboard.\n")
            return(TRUE)
          }
        }
      } else {
        cat("⚠️  No 2025 data found - try different season\n")
      }
    } else {
      cat("⚠️  No Date column found - season awareness may not work\n")
    }
  }
  
  cat("⚠️  Test inconclusive - run verify_data_readiness() for details\n")
  return(FALSE)
}

# ===================================================================
# FINAL SETUP AND INSTRUCTIONS
# ===================================================================

# Display instructions immediately when code is loaded
cat("=== ENHANCED xwOBA DASHBOARD - MATCHED PLAYERS ONLY EDITION LOADED! ===\n\n")

print_usage_instructions()

cat("🔥 READY TO LAUNCH WITH MATCHED PLAYERS ONLY! Run: launch_xwoba_dashboard_composite()\n\n")

# Make main functions available in global environment
if (!exists("launch_xwoba_dashboard")) {
  assign("launch_xwoba_dashboard", launch_xwoba_dashboard_composite, envir = .GlobalEnv)
}

assign("launch_xwoba_dashboard_composite", launch_xwoba_dashboard_composite, envir = .GlobalEnv)
assign("verify_data_readiness", verify_data_readiness, envir = .GlobalEnv)
assign("test_composite_key_system", test_composite_key_system, envir = .GlobalEnv)
assign("print_usage_instructions", print_usage_instructions, envir = .GlobalEnv)

assign("quick_composite_test", quick_composite_test, envir = .GlobalEnv)

# Return shinyApp object for shinyapps.io
shinyApp(ui = ui, server = server)
