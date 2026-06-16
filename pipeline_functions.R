# ============================================================================
#  pipeline_functions.R
#  Data ingestion functions (CSV combine, clean/standardize) AND
#  xwOBA model functions (create_ultimate_features,
#  train_maximum_correlation_xwoba, calculate_full_xwoba).
#  Sourced by both pipeline_daily.R (training) and app.R (loading).
# ============================================================================

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


# Incremental combine: only processes files new/modified since last run.
# Maintains a manifest (file_manifest.rds) of processed file IDs + modified
# times. On first run (no manifest), every file in the folder is treated as
# new - but files already represented in the seeded existing dataset will be
# deduped out downstream in pipeline_daily.R, so this is safe to re-run.
combine_navs_csvs_incremental <- function(folder_id, manifest_path = "file_manifest.rds") {
  
  cat("INCREMENTAL CSV COMBINE\n")
  cat("===========================\n")
  
  folder <- googledrive::drive_get(googledrive::as_id(folder_id))
  cat("Found folder:", folder$name, "\n")
  
  all_files <- googledrive::drive_ls(folder, pattern = "\\.csv$")
  
  if (nrow(all_files) == 0) {
    cat("No CSV files found in folder\n")
    return(list(new_data = NULL, manifest = NULL, new_count = 0))
  }
  
  # Extract modified time from drive_resource metadata
  all_files$modified_time <- sapply(all_files$drive_resource, function(x) x$modifiedTime)
  
  cat("Found", nrow(all_files), "total CSV files in Drive\n")
  
  # Load existing manifest if present
  if (file.exists(manifest_path)) {
    manifest <- readRDS(manifest_path)
    cat("Loaded existing manifest with", nrow(manifest), "previously processed files\n")
  } else {
    manifest <- data.frame(id = character(0), name = character(0),
                            modified_time = character(0), stringsAsFactors = FALSE)
    cat("No existing manifest - this is a first/full run\n")
  }
  
  # Determine which files are new or modified
  to_process <- all_files %>%
    dplyr::left_join(manifest %>% dplyr::select(id, prev_modified = modified_time),
                      by = "id") %>%
    dplyr::filter(is.na(prev_modified) | modified_time != prev_modified)
  
  cat("Files to process (new or changed):", nrow(to_process), "\n")
  
  if (nrow(to_process) == 0) {
    cat("Nothing new to process.\n")
    return(list(new_data = NULL,
                 manifest = all_files %>% dplyr::select(id, name, modified_time),
                 new_count = 0))
  }
  
  combined_data <- data.frame()
  
  for (i in seq_len(nrow(to_process))) {
    file_name <- to_process$name[i]
    cat("Processing:", file_name, "(", i, "/", nrow(to_process), ")\n")
    
    tryCatch({
      temp_file <- tempfile(fileext = ".csv")
      googledrive::drive_download(to_process$id[i], path = temp_file, overwrite = TRUE, verbose = FALSE)
      
      current_data <- read_csv(temp_file, show_col_types = FALSE)
      
      essential_columns <- c("Batter", "BatterTeam", "Date", "Inning", "PAofInning",
                             "ExitSpeed", "Angle", "PlayResult", "Bearing", "KorBB", "PitchCall")
      available_columns <- intersect(essential_columns, names(current_data))
      current_data <- current_data %>% select(all_of(available_columns))
      
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
  
  cat("Combined", nrow(combined_data), "new rows from", nrow(to_process), "files\n")
  
  updated_manifest <- all_files %>% dplyr::select(id, name, modified_time)
  
  return(list(new_data = combined_data, manifest = updated_manifest, new_count = nrow(to_process)))
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


create_ultimate_features <- function(raw_data) {
  
  cat("\nUltimate Feature Engineering\n")
  cat("Creating 500+ features with emphasis on top performers\n\n")
  
  # Filter to batted balls with complete data
  pa_data <- raw_data %>%
    filter(
      PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", 
                        "FieldersChoice", "Error", "Sacrifice") &
        !is.na(ExitSpeed) & ExitSpeed > 0 &
        !is.na(Angle)
    )
  
  cat("Total batted balls:", nrow(pa_data), "\n")
  
  # Outcome classification
  pa_data$outcome <- case_when(
    pa_data$PlayResult %in% c("Single") ~ "single",
    pa_data$PlayResult %in% c("Double") ~ "double",
    pa_data$PlayResult %in% c("Triple") ~ "triple", 
    pa_data$PlayResult %in% c("HomeRun") ~ "home_run",
    pa_data$PlayResult %in% c("Out", "FieldersChoice", "Error", "Sacrifice") ~ "out",
    TRUE ~ "other"
  )
  
  pa_data <- pa_data[pa_data$outcome != "other", ]
  
  # Core measurements (keeping bearing for compatibility)
  ev <- pa_data$ExitSpeed
  la <- pa_data$Angle
  bearing <- ifelse(is.na(pa_data$Bearing), 0, pa_data$Bearing)
  
  # 1. Polynomial Features - PRIORITIZING TOP PERFORMERS
  extreme_polynomials <- data.frame(
    ev = ev,
    ev_2 = ev^2,
    ev_3 = ev^3,
    ev_4 = pmin(ev^4, 1e10),
    ev_5 = pmin(ev^5, 1e12),
    ev_6 = pmin(ev^6, 1e14),
    
    la = la,
    la_2 = la^2,
    la_3 = la^3,
    la_4 = pmin(la^4, 1e8),
    la_5 = pmin(la^5, 1e10),
    la_6 = pmin(la^6, 1e12),
    
    bearing = bearing,
    bearing_2 = bearing^2,
    bearing_3 = bearing^3,
    bearing_4 = bearing^4,
    bearing_abs = abs(bearing),
    bearing_abs_2 = abs(bearing)^2,
    bearing_abs_3 = abs(bearing)^3,
    
    ev_2_la_2 = ev^2 * la^2,
    ev_3_la_3 = pmin(ev^3 * la^3, 1e12),
    ev_4_la_4 = pmin(ev^4 * la^4, 1e15)
  )
  
  # 2. EV-LA Interactions - EMPHASIZING TOP FEATURES
  ev_la_features <- data.frame(
    # TOP PERFORMER #1: ev_2_la - multiple variations
    ev_2_la = ev^2 * la,
    
    ev_la = ev * la,
    ev_la_2 = ev * la^2,
    ev_la_3 = ev * la^3,
    ev_la_4 = ev * la^4,
    ev_la_5 = ev * la^5,
    
    ev_2_la_2 = ev^2 * la^2,
    ev_2_la_3 = ev^2 * la^3,
    ev_2_la_4 = ev^2 * la^4,
    
    # TOP PERFORMER #6: ev_3_la
    ev_3_la = ev^3 * la,
    ev_3_la_2 = ev^3 * la^2,
    ev_3_la_3 = pmin(ev^3 * la^3, 1e12),
    
    # TOP PERFORMER #5: ev_4_la_2
    ev_4_la = pmin(ev^4 * la, 1e10),
    ev_4_la_2 = pmin(ev^4 * la^2, 1e12),
    
    # TOP PERFORMER #7: ev_5_la and #4: ev_6_la
    ev_5_la = pmin(ev^5 * la, 1e12),
    ev_6_la = pmin(ev^6 * la, 1e14),
    
    ev_div_la = ifelse(abs(la) > 0.1, ev / abs(la), 0),
    la_div_ev = ifelse(ev > 0, la / ev, 0),
    
    ev_exp_la = ev * exp(pmin(abs(la) / 30, 3)),
    la_exp_ev = la * exp(pmin(ev / 100, 3))
  )
  
  # 3. Bearing and Field Position Features
  bearing_features <- data.frame(
    bearing_ev = abs(bearing) * ev,
    bearing_2_ev = bearing^2 * ev,
    bearing_ev_2 = abs(bearing) * ev^2,
    
    # TOP PERFORMER #8: bearing_3_ev
    bearing_3_ev = abs(bearing)^3 * ev,
    bearing_ev_3 = abs(bearing) * ev^3,
    
    bearing_la = abs(bearing) * abs(la),
    bearing_2_la = bearing^2 * abs(la),
    bearing_la_2 = abs(bearing) * la^2,
    
    bearing_ev_la = abs(bearing) * ev * abs(la) / 1000,
    bearing_2_ev_la = bearing^2 * ev * abs(la) / 10000,
    bearing_ev_2_la = abs(bearing) * ev^2 * abs(la) / 100000,
    
    # Field direction indicators
    field_direction_x = sin(bearing * pi / 180),
    field_direction_y = cos(bearing * pi / 180),
    field_direction_x_2 = (sin(bearing * pi / 180))^2,
    field_direction_y_2 = (cos(bearing * pi / 180))^2,
    field_direction_x_3 = (sin(bearing * pi / 180))^3,
    field_direction_y_3 = (cos(bearing * pi / 180))^3,
    
    center_field_bonus = (30 - pmin(abs(bearing), 30)) / 30,
    gap_penalty = pmax(0, (abs(bearing) - 20)) / 25,
    extreme_pull_penalty = pmax(0, (abs(bearing) - 40)) / 20
  )
  
  # 4. Outcome-Specific Zones
  outcome_zones <- data.frame(
    hr_zone_1 = as.numeric(ev >= 95 & la >= 20 & la <= 40),
    hr_zone_2 = as.numeric(ev >= 100 & la >= 15 & la <= 45),
    hr_zone_3 = as.numeric(ev >= 105 & la >= 10),
    hr_zone_4 = as.numeric(ev >= 98 & la >= 25 & la <= 35),
    hr_zone_5 = as.numeric(ev >= 92 & la >= 28 & la <= 32),
    hr_zone_6 = as.numeric(ev >= 110 & la >= 8),
    hr_zone_7 = as.numeric(ev >= 90 & la >= 30 & la <= 38),
    
    double_zone_1 = as.numeric(abs(bearing) > 20 & ev >= 80),
    double_zone_2 = as.numeric(la >= 5 & la <= 22 & ev >= 85),
    double_zone_3 = as.numeric(ev >= 90 & la >= 8 & la <= 25),
    double_zone_4 = as.numeric(abs(bearing) > 15 & ev >= 85),
    double_zone_5 = as.numeric(la >= 10 & la <= 30 & ev >= 88),
    
    single_zone_1 = as.numeric(la >= 0 & la <= 25 & ev >= 70 & ev < 95),
    single_zone_2 = as.numeric(ev >= 75 & la >= -5 & la <= 30),
    single_zone_3 = as.numeric(la >= -5 & la <= 30 & ev >= 65),
    single_zone_4 = as.numeric(ev >= 80 & la >= 0 & la <= 35),
    
    barrel_1 = as.numeric(ev >= 98 & la >= 26 & la <= 30),
    barrel_2 = as.numeric(ev >= 95 & la >= 24 & la <= 32),
    quality_1 = as.numeric(ev >= 85 & la >= 8 & la <= 32),
    quality_2 = as.numeric(ev >= 80 & la >= 10 & la <= 30),
    elite_1 = as.numeric(ev >= 100 & la >= 15 & la <= 35),
    elite_2 = as.numeric(ev >= 95 & la >= 20 & la <= 40)
  )
  
  # 5. Interaction Combinations - EMPHASIZING TOP FEATURES
  mega_combos <- data.frame(
    ev_la_bearing = ev * la * abs(bearing) / 1000,
    
    power_score_1 = ev^2 * pmax(0, la) / 1000,
    power_score_2 = ev^2 * pmax(0, (la - 10)) / 1000,
    power_score_3 = ev^2 * pmax(0, (la - 15)) / 1000,
    power_score_4 = ev^3 * pmax(0, la) / 100000,
    
    contact_score_1 = ev * (1 / (1 + abs(la - 20))) * (1 / (1 + abs(bearing) / 30)) / 100,
    contact_score_2 = ev * (1 / (1 + abs(la - 25))) * (1 / (1 + abs(bearing) / 40)) / 100,
    contact_score_3 = ev * (1 / (1 + abs(la - 22))) / 10,
    
    ev_percentile = rank(ev) / length(ev),
    bearing_percentile = (rank(bearing) + length(bearing)) / (2 * length(bearing)),
    
    optimal_combination_1 = ev * (40 - abs(la - 20)) * (45 - pmin(abs(bearing), 45)) / 10000,
    # TOP PERFORMER #3: optimal_combination_2
    optimal_combination_2 = ev^2 * (35 - abs(la - 25)) / 1000,
    optimal_combination_3 = ev * la * (1 / (1 + abs(bearing) / 30)) / 100,
    
    # TOP PERFORMER #2: ev_la_optimal
    ev_la_optimal = ev * pmax(0, 30 - abs(la - 20)) / 100,
    ev_bearing_optimal = ev * pmax(0, 40 - abs(bearing)) / 100,
    la_bearing_combo = abs(la) * abs(bearing) / 100,
    
    # Trajectory quality indicators
    trajectory_quality_1 = ev * (1 / (1 + abs(la - 20))) / 10,
    trajectory_quality_2 = ev * (1 / (1 + abs(la - 25))) / 10,
    trajectory_quality_3 = ev * (1 / (1 + abs(la - 30))) / 10,
    
    # Advanced angle interactions
    optimal_la_factor = pmax(0, 35 - abs(la - 22)) / 35,
    launch_efficiency = ev * pmax(0, 30 - abs(la - 20)) / 1000,
    bearing_efficiency = ev * pmax(0, 35 - abs(bearing)) / 1000,
    
    # Additional variations of top performers
    ev_2_la_alt = ev^2 * pmax(0, la) / 1000,
    ev_la_optimal_alt = ev * pmax(0, 25 - abs(la - 22)) / 100,
    optimal_combination_2_alt = ev^2 * (30 - abs(la - 20)) / 1000,
    bearing_3_ev_alt = abs(bearing)^3 * ev / 1000,
    ev_4_la_2_alt = pmin(ev^4 * la^2, 1e12) / 1000000,
    ev_3_la_alt = ev^3 * pmax(0, la) / 100000,
    ev_5_la_alt = pmin(ev^5 * pmax(0, la), 1e12) / 1000000000,
    ev_6_la_alt = pmin(ev^6 * pmax(0, la), 1e14) / 1000000000000
  )
  
  # Combine all features
  all_features <- cbind(
    extreme_polynomials,
    ev_la_features,
    bearing_features,
    outcome_zones,
    mega_combos
  )
  
  # Clean extreme values
  all_features[is.infinite(as.matrix(all_features))] <- 0
  all_features[abs(as.matrix(all_features)) > 1e15] <- 0
  all_features[is.na(all_features)] <- 0
  
  cat("\nFeature engineering complete\n")
  cat("Total features created:", ncol(all_features), "\n")
  cat("Total batted balls processed:", nrow(all_features), "\n")
  
  return(list(
    features = all_features,
    outcomes = pa_data$outcome,
    pa_data = pa_data
  ))
}

train_maximum_correlation_xwoba <- function(raw_data) {
  
  cat("\nMaximum Correlation xwOBA Model Training\n")
  cat("Goal: Push correlation above 80% with emphasis on top features\n\n")
  
  # Create features
  feature_result <- create_ultimate_features(raw_data)
  features <- feature_result$features
  outcomes <- feature_result$outcomes
  
  # Prepare targets
  outcome_mapping <- c("out" = 0, "single" = 1, "double" = 2, "triple" = 3, "home_run" = 4)
  y_class <- outcome_mapping[outcomes]
  
  # Calculate actual wOBA
  woba_weights <- c("out" = 0.000, "single" = 0.888, "double" = 1.271, 
                    "triple" = 1.616, "home_run" = 2.101)
  actual_woba <- woba_weights[outcomes]
  
  cat("Model summary:\n")
  cat("Features:", ncol(features), "\n")
  cat("Samples:", nrow(features), "\n")
  cat("Mean wOBA:", round(mean(actual_woba), 3), "\n")
  
  # Train/test split
  set.seed(42)
  train_idx <- createDataPartition(outcomes, p = 0.7, list = FALSE)
  remaining_idx <- setdiff(1:length(outcomes), train_idx)
  
  val_idx <- createDataPartition(outcomes[remaining_idx], p = 0.5, list = FALSE)
  test_idx <- setdiff(1:length(remaining_idx), val_idx)
  
  final_val_idx <- remaining_idx[val_idx]
  final_test_idx <- remaining_idx[test_idx]
  
  # Split data
  X_train <- as.matrix(features[train_idx, ])
  X_val <- as.matrix(features[final_val_idx, ])
  X_test <- as.matrix(features[final_test_idx, ])
  
  y_train <- y_class[train_idx]
  y_val <- y_class[final_val_idx]
  y_test <- y_class[final_test_idx]
  
  woba_train <- actual_woba[train_idx]
  woba_val <- actual_woba[final_val_idx]
  woba_test <- actual_woba[final_test_idx]
  
  # Create DMatrix
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dval <- xgb.DMatrix(data = X_val, label = y_val)
  dtest <- xgb.DMatrix(data = X_test, label = y_test)
  
  # Model parameters
  params <- list(
    objective = "multi:softprob",
    eval_metric = "mlogloss",
    num_class = 5,
    
    max_depth = 10,
    eta = 0.02,
    subsample = 0.9,
    colsample_bytree = 0.7,
    min_child_weight = 3,
    reg_alpha = 0.01,
    reg_lambda = 0.1,
    gamma = 0.01,
    
    seed = 42
  )
  
  cat("Training model...\n")
  
  # Train model
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 3000,
    watchlist = list(train = dtrain, val = dval),
    early_stopping_rounds = 150,
    verbose = 1,
    print_every_n = 300
  )
  
  # Get predictions
  test_probs <- predict(model, dtest, reshape = TRUE)
  colnames(test_probs) <- c("P_out", "P_single", "P_double", "P_triple", "P_home_run")
  
  # Calculate xwOBA
  woba_weights_vector <- c(0.000, 0.888, 1.271, 1.616, 2.101)
  predicted_xwoba <- as.vector(test_probs %*% woba_weights_vector)
  
  # Evaluation
  correlation <- cor(predicted_xwoba, woba_test)
  mae <- mean(abs(predicted_xwoba - woba_test))
  rmse <- sqrt(mean((predicted_xwoba - woba_test)^2))
  
  pred_class <- apply(test_probs, 1, which.max) - 1
  class_accuracy <- mean(pred_class == y_test)
  
  t_test_result <- t.test(predicted_xwoba, woba_test, paired = TRUE)
  
  brier_scores <- numeric(5)
  for(i in 1:5) {
    actual_binary <- as.numeric(y_test == (i-1))
    predicted_prob <- test_probs[, i]
    brier_scores[i] <- mean((predicted_prob - actual_binary)^2)
  }
  overall_brier <- mean(brier_scores)
  
  cat("\nModel Results:\n")
  cat("Correlation:", round(correlation, 4), "\n")
  cat("MAE:", round(mae, 4), "\n")
  cat("RMSE:", round(rmse, 4), "\n")
  cat("Classification Accuracy:", round(class_accuracy, 4), "\n")
  cat("t-test p-value:", round(t_test_result$p.value, 6), "\n")
  cat("Brier Score:", round(overall_brier, 4), "\n")
  
  if(t_test_result$p.value > 0.05) {
    cat("Well-calibrated probabilities maintained\n")
  } else {
    cat("Some calibration trade-off for correlation\n")
  }
  
  importance <- xgb.importance(model = model)
  cat("\nTop 25 Most Important Features:\n")
  print(head(importance, 25))
  
  return(list(
    model = model,
    correlation = correlation,
    mae = mae,
    rmse = rmse,
    class_accuracy = class_accuracy,
    t_test_p = t_test_result$p.value,
    brier_score = overall_brier,
    predictions = predicted_xwoba,
    probabilities = test_probs,
    actual = woba_test,
    importance = importance,
    feature_count = ncol(X_train)
  ))
}

calculate_full_xwoba <- function(raw_data, model_results) {
  
  all_pa <- raw_data %>% filter(!is.na(Batter))
  
  all_pa$outcome <- case_when(
    all_pa$PlayResult %in% c("Single", "Double", "Triple", "HomeRun") ~ "hit",
    all_pa$PlayResult %in% c("Out", "FieldersChoice", "Error") ~ "out",
    all_pa$PlayResult %in% c("Sacrifice") ~ "sacrifice_fly",
    all_pa$KorBB == "Walk" ~ "walk",
    all_pa$KorBB == "IntentionalWalk" ~ "intentional_walk",
    all_pa$PitchCall == "HitByPitch" ~ "hit_by_pitch",
    all_pa$KorBB %in% c("Strikeout", "StrikeoutLooking", "StrikeoutSwinging") ~ "strikeout",
    TRUE ~ "other"
  )
  
  classified_pa <- all_pa[all_pa$outcome != "other", ]
  
  hits <- sum(classified_pa$outcome == "hit")
  outs <- sum(classified_pa$outcome == "out")
  strikeouts <- sum(classified_pa$outcome == "strikeout")
  BB <- sum(classified_pa$outcome == "walk")
  IBB <- sum(classified_pa$outcome == "intentional_walk")
  SF <- sum(classified_pa$outcome == "sacrifice_fly")
  HBP <- sum(classified_pa$outcome == "hit_by_pitch")
  
  batted_balls <- hits + outs
  AB <- hits + outs + strikeouts
  denominator <- AB + BB - IBB + SF + HBP
  
  mean_xwoba_con <- mean(model_results$predictions, na.rm = TRUE)
  
  wBB_HBP <- 0.690
  
  numerator <- (mean_xwoba_con * batted_balls) + (wBB_HBP * (BB - IBB + HBP))
  full_xwoba <- numerator / denominator
  
  cat("Full xwOBA calculation:\n")
  cat("Batted balls:", batted_balls, "\n")
  cat("Walks (BB):", BB, "\n")
  cat("HBP:", HBP, "\n")
  cat("Mean xwOBA (contact):", round(mean_xwoba_con, 3), "\n")
  cat("Full xwOBA:", round(full_xwoba, 3), "\n")
  
  return(full_xwoba)
}
# ============================================================================
#  calculate_enhanced_data: runs the full player-level xwOBA scoring block
#  that app.R previously ran at startup. Called by pipeline_daily.R and
#  cached in xwoba_model.rds so app.R just loads it instantly.
# ============================================================================
calculate_enhanced_data <- function(raw_data, ultimate_results) {
  
  library(dplyr)
  library(lubridate)
  
  cat("\nCalculating player-level xwOBA scores...\n")
  
  # Season extraction
  if ("Date" %in% names(raw_data)) {
    raw_data <- raw_data %>%
      mutate(Expected_Season = as.character(year(as.Date(Date))))
    cat("Seasons found:", paste(unique(raw_data$Expected_Season), collapse = ", "), "\n")
  } else {
    raw_data <- raw_data %>% mutate(Expected_Season = "2025")
  }
  
  # Filter to batted balls with tracking data
  batted_balls <- raw_data %>%
    filter(
      PlayResult %in% c("Single","Double","Triple","HomeRun","Out","FieldersChoice","Error","Sacrifice") &
        !is.na(ExitSpeed) & ExitSpeed > 0 & !is.na(Angle) & !is.na(Batter)
    )
  
  # Outcome classification
  batted_balls$outcome <- case_when(
    batted_balls$PlayResult == "Single"   ~ "single",
    batted_balls$PlayResult == "Double"   ~ "double",
    batted_balls$PlayResult == "Triple"   ~ "triple",
    batted_balls$PlayResult == "HomeRun"  ~ "home_run",
    batted_balls$PlayResult %in% c("Out","FieldersChoice","Error","Sacrifice") ~ "out",
    TRUE ~ "other"
  )
  batted_balls <- batted_balls[batted_balls$outcome != "other", ]
  
  # Use model predictions if available, otherwise EV/angle fallback
  if (!is.null(ultimate_results) && !is.null(ultimate_results$model)) {
    tryCatch({
      model_env_funcs <- create_ultimate_features(batted_balls)
      X_all <- model_env_funcs$features
      dall  <- xgboost::xgb.DMatrix(data = as.matrix(X_all))
      probs <- predict(ultimate_results$model, dall, reshape = TRUE)
      woba_weights_vec <- c(0.000, 0.888, 1.271, 1.616, 2.101)
      batted_balls$predicted_xwobacon <- as.vector(probs %*% woba_weights_vec)
      cat("Used xgboost model predictions for", nrow(batted_balls), "batted balls\n")
    }, error = function(e) {
      cat("Model prediction failed (", e$message, ") - using EV/angle fallback\n")
      batted_balls$predicted_xwobacon <<- pmin(2.5, pmax(0,
        0.1 + (batted_balls$ExitSpeed - 60) * 0.01 +
        pmax(0, 30 - abs(batted_balls$Angle - 20)) * 0.005
      ))
    })
  } else {
    batted_balls$predicted_xwobacon <- pmin(2.5, pmax(0,
      0.1 + (batted_balls$ExitSpeed - 60) * 0.01 +
      pmax(0, 30 - abs(batted_balls$Angle - 20)) * 0.005
    ))
  }
  
  cat("xwOBACON calculated for", nrow(batted_balls), "batted balls\n")
  
  # All plate appearances (includes walks, Ks, HBP)
  all_pa <- raw_data %>%
    filter(!is.na(Batter)) %>%
    mutate(
      pa_outcome = case_when(
        PlayResult %in% c("Single","Double","Triple","HomeRun") ~ "hit",
        PlayResult %in% c("Out","FieldersChoice","Error")       ~ "out",
        PlayResult == "Sacrifice"                               ~ "sacrifice_fly",
        KorBB == "Walk"                                         ~ "walk",
        KorBB == "IntentionalWalk"                              ~ "intentional_walk",
        PitchCall == "HitByPitch"                               ~ "hit_by_pitch",
        KorBB %in% c("Strikeout","StrikeoutLooking","StrikeoutSwinging") ~ "strikeout",
        TRUE ~ "other"
      )
    ) %>%
    filter(pa_outcome != "other")
  
  cat("Total plate appearances:", nrow(all_pa), "\n")
  
  # Player-level xwOBA by season
  player_xwoba_full <- all_pa %>%
    left_join(
      batted_balls %>% select(Batter, Date, Inning, PAofInning, predicted_xwobacon, Expected_Season),
      by = c("Batter","Date","Inning","PAofInning")
    ) %>%
    mutate(Expected_Season = coalesce(Expected_Season.x, Expected_Season.y)) %>%
    group_by(Batter, Expected_Season) %>%
    summarise(
      hits             = sum(pa_outcome == "hit"),
      outs             = sum(pa_outcome == "out"),
      strikeouts       = sum(pa_outcome == "strikeout"),
      BB               = sum(pa_outcome == "walk"),
      IBB              = sum(pa_outcome == "intentional_walk"),
      SF               = sum(pa_outcome == "sacrifice_fly"),
      HBP              = sum(pa_outcome == "hit_by_pitch"),
      batted_balls_count = hits + outs,
      AB               = hits + outs + strikeouts,
      total_pa         = AB + BB - IBB + SF + HBP,
      mean_xwobacon    = mean(predicted_xwobacon, na.rm = TRUE),
      wBB_HBP          = 0.690,
      xwoba_numerator  = (mean_xwobacon * batted_balls_count) + (wBB_HBP * (BB - IBB + HBP)),
      predicted_xwoba_full = ifelse(total_pa > 0, xwoba_numerator / total_pa, NA_real_),
      .groups = "drop"
    )
  
  cat("xwOBA calculated for", nrow(player_xwoba_full), "player-season combinations\n")
  cat("Walk check - total BB across all players:", sum(player_xwoba_full$BB), "\n")
  
  # Merge back to PA level
  enhanced_data <- all_pa %>%
    left_join(
      batted_balls %>% select(Batter, Date, Inning, PAofInning, predicted_xwobacon),
      by = c("Batter","Date","Inning","PAofInning")
    ) %>%
    left_join(
      player_xwoba_full %>% select(Batter, Expected_Season, predicted_xwoba_full, mean_xwobacon),
      by = c("Batter","Expected_Season")
    ) %>%
    mutate(
      predicted_xwoba_final    = predicted_xwoba_full,
      predicted_xwobacon_final = ifelse(!is.na(predicted_xwobacon), predicted_xwobacon, mean_xwobacon)
    )
  
  cat("Enhanced data rows:", nrow(enhanced_data), "\n")
  cat("Records with xwOBA:", sum(!is.na(enhanced_data$predicted_xwoba_final)), "\n")
  
  list(
    enhanced_data      = enhanced_data,
    player_xwoba_full  = player_xwoba_full,
    batted_balls       = batted_balls
  )
}

# ============================================================================
#  NECBL TEAM MAPPING + COMPOSITE KEY + PRESTOSPORTS SCRAPER
#  Used by pipeline_daily.R to pre-scrape NECBL stats for caching
#  so app.R doesn't need to make live HTTP requests (which get 403'd
#  from shinyapps.io IP ranges).
# ============================================================================

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
