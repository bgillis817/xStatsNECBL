# ============================================================================
#  xStatsNECBL - Daily Data Refresh Pipeline (incremental)
#  NECBL actual stats scraped via Python/Selenium (see scrape_necbl.py)
#  which runs before this script in GitHub Actions.
# ============================================================================

suppressPackageStartupMessages({
  if (!require(pacman)) install.packages("pacman")
  pacman::p_load(dplyr, readr, data.table, tidyr, lubridate, stringr,
                 googledrive, xgboost, caret)
})

sa_path <- Sys.getenv("GDRIVE_SA_PATH", "service_account.json")
if (!file.exists(sa_path)) stop("Service account JSON not found at: ", sa_path)
googledrive::drive_auth(path = sa_path)
cat("Authenticated to Google Drive via service account\n")

source("pipeline_functions.R")

GDRIVE_FOLDER_ID <- Sys.getenv("GDRIVE_FOLDER_ID", "1haJdctNyLTx81GXlvCdBRDSWKrDAlKAw")
LIVE_RDS_FILE_ID <- Sys.getenv("LIVE_RDS_FILE_ID", "1IFO08F2YoO1qUXCw3GiU1k6FOXwt4Q0b")

# ===================================================================
# STEP 1: Incremental combine
# ===================================================================
cat("\n=== STEP 1: INCREMENTAL COMBINE FROM GOOGLE DRIVE ===\n")
result <- combine_navs_csvs_incremental(
  folder_id     = GDRIVE_FOLDER_ID,
  manifest_path = "file_manifest.rds"
)
if (is.null(result$manifest)) stop("Pipeline aborted: could not list files")
if (result$new_count == 0) {
  cat("No new files - retraining model on existing data.\n")
  new_data <- NULL
} else {
  new_data <- result$new_data
}

# ===================================================================
# STEP 2: Merge
# ===================================================================
cat("\n=== STEP 2: MERGING WITH EXISTING DATA ===\n")
if (file.exists("navs_all_data.rds")) {
  existing_data <- readRDS("navs_all_data.rds")
  cat("Loaded existing navs_all_data.rds with", nrow(existing_data), "rows\n")
} else {
  cat("Seeding from live Drive file...\n")
  existing_data <- tryCatch(
    readRDS(url(paste0("https://drive.google.com/uc?export=download&id=", LIVE_RDS_FILE_ID))),
    error = function(e) { cat("Error:", e$message, "\n"); data.frame() }
  )
  cat("Seeded with", nrow(existing_data), "rows\n")
}
if (!is.null(new_data) && nrow(new_data) > 0) {
  if (nrow(existing_data) > 0) {
    all_cols <- union(names(existing_data), names(new_data))
    for (col in setdiff(all_cols, names(existing_data))) existing_data[[col]] <- NA
    for (col in setdiff(all_cols, names(new_data)))      new_data[[col]] <- NA
  }
  combined_data <- bind_rows(existing_data, new_data)
  # Keep only outcome rows — the final pitch of each PA
  # Outcome rows: batted balls (have ExitSpeed), walks, strikeouts, HBP
  # This ensures each PA is represented exactly once with its result
  if ("PlayResult" %in% names(combined_data)) {
    before <- nrow(combined_data)
    combined_data <- combined_data %>%
      filter(
        # Batted balls
        (!is.na(ExitSpeed) & ExitSpeed > 0) |
        # Walks and intentional walks
        (!is.na(KorBB) & KorBB %in% c("Walk", "IntentionalWalk")) |
        # Strikeouts
        (!is.na(KorBB) & KorBB %in% c("Strikeout", "StrikeoutLooking", "StrikeoutSwinging")) |
        # HBP
        (!is.na(PitchCall) & PitchCall == "HitByPitch") |
        # PlayResult already set (from Step 2b)
        (!is.na(PlayResult) & !PlayResult %in% c("", "Undefined"))
      )
    cat("Filtered to outcome rows:", before, "->", nrow(combined_data), "rows\n")
  }
} else {
  combined_data <- existing_data
}
cat("Total combined rows:", nrow(combined_data), "\n")

# ===================================================================
# STEP 2b: Shift KorBB walks/strikeouts into PlayResult
# Walk rows in Trackman have KorBB="Walk" but PlayResult is blank/NA
# This ensures calculate_expected_xwoba_and_full counts them correctly
# ===================================================================
cat("\n=== STEP 2b: SHIFTING KorBB TO PlayResult ===\n")
if ("KorBB" %in% names(combined_data) && "PlayResult" %in% names(combined_data)) {
  before_walks <- sum(combined_data$KorBB == "Walk", na.rm = TRUE)
  combined_data <- combined_data %>%
    mutate(
      PlayResult = case_when(
        KorBB == "Walk" & (is.na(PlayResult) | PlayResult == "" | PlayResult == "Undefined") ~ "Walk",
        KorBB == "IntentionalWalk" & (is.na(PlayResult) | PlayResult == "" | PlayResult == "Undefined") ~ "Walk",
        KorBB %in% c("Strikeout","StrikeoutLooking","StrikeoutSwinging") & 
          (is.na(PlayResult) | PlayResult == "" | PlayResult == "Undefined") ~ "Strikeout",
        PitchCall == "HitByPitch" & (is.na(PlayResult) | PlayResult == "" | PlayResult == "Undefined") ~ "HitByPitch",
        TRUE ~ PlayResult
      )
    )
  after_walks <- sum(combined_data$PlayResult == "Walk", na.rm = TRUE)
  after_ks <- sum(combined_data$PlayResult == "Strikeout", na.rm = TRUE)
  cat("Walks in PlayResult:", after_walks, "\n")
  cat("Strikeouts in PlayResult:", after_ks, "\n")
}

# ===================================================================
# STEP 3: Save navs_all_data.rds + manifest
# ===================================================================
cat("\n=== STEP 3: SAVING navs_all_data.rds ===\n")
saveRDS(combined_data, "navs_all_data.rds")
saveRDS(result$manifest, "file_manifest.rds")
cat("Saved navs_all_data.rds (", nrow(combined_data), "rows)\n")

# ===================================================================
# STEP 4: Train model + score players
# ===================================================================
cat("\n=== STEP 4: TRAINING MODEL + SCORING PLAYERS ===\n")
ultimate_results  <- NULL
full_xwoba_result <- NULL
scoring_result    <- NULL
tryCatch({
  cat("Training xwOBA model...\n")
  ultimate_results  <- train_maximum_correlation_xwoba(combined_data)
  full_xwoba_result <- calculate_full_xwoba(combined_data, ultimate_results)
  cat("Model trained - correlation:", round(ultimate_results$correlation, 4), "\n")
  cat("Scoring player-level xwOBA...\n")
  scoring_result <- calculate_enhanced_data(combined_data, ultimate_results)
  cat("Players scored:", nrow(scoring_result$player_xwoba_full), "combinations\n")
}, error = function(e) {
  cat("WARNING: Model training/scoring failed:", e$message, "\n")
})

# ===================================================================
# STEP 5: Load NECBL actual stats from Google Drive
#         You run scrape_necbl_local.py locally, upload necbl_stats.csv
#         to the NECBL Stats Drive folder, pipeline picks it up here.
# ===================================================================
cat("\n=== STEP 5: LOADING NECBL STATS FROM GOOGLE DRIVE ===\n")
necbl_stats_cache <- list()

necbl_folder_id <- Sys.getenv("NECBL_STATS_FOLDER_ID", "")

if (nchar(necbl_folder_id) > 0) {
  tryCatch({
    files <- googledrive::drive_ls(googledrive::as_id(necbl_folder_id))
    csv_files <- files[grepl("necbl_stats.*\\.csv$", files$name, ignore.case = TRUE), ]
    
    if (nrow(csv_files) > 0) {
      tmp_csv <- tempfile(fileext = ".csv")
      googledrive::drive_download(
        googledrive::as_id(csv_files$id[1]),
        path = tmp_csv,
        overwrite = TRUE
      )
      necbl_df <- readr::read_csv(tmp_csv, show_col_types = FALSE)
      unlink(tmp_csv)
      
      if (!is.null(necbl_df) && nrow(necbl_df) > 0) {
        for (season in unique(necbl_df$Season)) {
          season_data <- necbl_df %>% filter(Season == as.character(season))
          necbl_stats_cache[[as.character(season)]] <- season_data
          cat("Cached", nrow(season_data), "players for", season, "\n")
        }
        cat("NECBL stats loaded for seasons:", paste(names(necbl_stats_cache), collapse = ", "), "\n")
      }
    } else {
      cat("No necbl_stats.csv found in NECBL Stats Drive folder\n")
    }
  }, error = function(e) {
    cat("Error loading NECBL stats from Drive:", e$message, "\n")
  })
} else {
  cat("NECBL_STATS_FOLDER_ID not set - skipping NECBL stats\n")
}

# ===================================================================
# STEP 6: Push updated navs_all_data.rds to live Drive
# ===================================================================
cat("\n=== STEP 6: UPDATING SHARED GOOGLE DRIVE RDS ===\n")
tryCatch({
  googledrive::drive_update(file = googledrive::as_id(LIVE_RDS_FILE_ID), media = "navs_all_data.rds")
  cat("Updated live Drive file\n")
}, error = function(e) cat("WARNING:", e$message, "\n"))

# ===================================================================
# STEP 7: Save everything to xwoba_model.rds
# ===================================================================
cat("\n=== STEP 7: SAVING xwoba_model.rds ===\n")
saveRDS(
  list(
    ultimate_results   = ultimate_results,
    full_xwoba_result  = full_xwoba_result,
    enhanced_data      = if (!is.null(scoring_result)) scoring_result$enhanced_data      else data.frame(),
    player_xwoba_full  = if (!is.null(scoring_result)) scoring_result$player_xwoba_full  else data.frame(),
    batted_balls       = if (!is.null(scoring_result)) scoring_result$batted_balls        else data.frame(),
    necbl_stats_cache  = necbl_stats_cache
  ),
  "xwoba_model.rds"
)
cat("Saved xwoba_model.rds\n")
cat("NECBL seasons cached:", paste(names(necbl_stats_cache), collapse = ", "), "\n")

# ===================================================================
# STEP 8: Clean / standardize
# ===================================================================
cat("\n=== STEP 8: CLEANING / STANDARDIZING ===\n")
clean_data <- clean_and_standardize_data(combined_data)
if (!is.null(clean_data) && nrow(clean_data) > 0) {
  saveRDS(clean_data, "xwoba_clean_data.rds")
  cat("Saved xwoba_clean_data.rds with", nrow(clean_data), "rows\n")
}

cat("\n=== PIPELINE COMPLETE ===\n")
cat("New files processed:", result$new_count, "\n")
cat("Total rows:", nrow(combined_data), "\n")
