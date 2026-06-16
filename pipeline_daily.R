# ============================================================================
#  xStatsNECBL - Daily Data Refresh Pipeline (incremental)
#  Pulls new/changed CSVs from Google Drive, appends to navs_all_data.rds,
#  trains xwOBA model, scores all players, caches everything in
#  xwoba_model.rds so app.R loads instantly on startup.
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
if (is.null(result$manifest)) stop("Pipeline aborted: could not list files in Drive folder")
if (result$new_count == 0) {
  cat("\nNo new files - retraining model on existing data.\n")
  new_data <- NULL
} else {
  new_data <- result$new_data
}

# ===================================================================
# STEP 2: Merge with existing data
# ===================================================================
cat("\n=== STEP 2: MERGING WITH EXISTING DATA ===\n")
if (file.exists("navs_all_data.rds")) {
  existing_data <- readRDS("navs_all_data.rds")
  cat("Loaded existing navs_all_data.rds with", nrow(existing_data), "rows\n")
} else {
  cat("Seeding from live Drive file (one-time)...\n")
  existing_data <- tryCatch(
    readRDS(url(paste0("https://drive.google.com/uc?export=download&id=", LIVE_RDS_FILE_ID))),
    error = function(e) { cat("Could not load:", e$message, "\n"); data.frame() }
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
  dedupe_cols <- intersect(c("source_file","Batter","Date","Inning","PAofInning","PitchNo"), names(combined_data))
  if (length(dedupe_cols) > 0) {
    before <- nrow(combined_data)
    combined_data <- combined_data %>% distinct(across(all_of(dedupe_cols)), .keep_all = TRUE)
    cat("Deduped:", before, "->", nrow(combined_data), "rows\n")
  }
} else {
  combined_data <- existing_data
}
cat("Total combined rows:", nrow(combined_data), "\n")

# ===================================================================
# STEP 3: Save navs_all_data.rds + manifest
# ===================================================================
cat("\n=== STEP 3: SAVING navs_all_data.rds ===\n")
saveRDS(combined_data, "navs_all_data.rds")
saveRDS(result$manifest, "file_manifest.rds")
cat("Saved navs_all_data.rds (", nrow(combined_data), "rows)\n")

# ===================================================================
# STEP 4: Train model + score all players + cache everything
# ===================================================================
cat("\n=== STEP 4: TRAINING MODEL + SCORING PLAYERS ===\n")
tryCatch({
  # Train xwOBA model
  cat("Training xwOBA model on", nrow(combined_data), "rows...\n")
  ultimate_results  <- train_maximum_correlation_xwoba(combined_data)
  full_xwoba_result <- calculate_full_xwoba(combined_data, ultimate_results)
  cat("Model trained - correlation:", round(ultimate_results$correlation, 4), "\n")

  # Score all players (this is what was timing out on shinyapps.io startup)
  cat("Scoring player-level xwOBA...\n")
  scoring_result <- calculate_enhanced_data(combined_data, ultimate_results)

  # Save everything to xwoba_model.rds - app.R loads this on startup
  saveRDS(
    list(
      ultimate_results  = ultimate_results,
      full_xwoba_result = full_xwoba_result,
      enhanced_data     = scoring_result$enhanced_data,
      player_xwoba_full = scoring_result$player_xwoba_full,
      batted_balls      = scoring_result$batted_balls
    ),
    "xwoba_model.rds"
  )
  cat("Saved xwoba_model.rds\n")
  cat("Players scored:", nrow(scoring_result$player_xwoba_full), "player-season combinations\n")
  cat("Total BB across all players:", sum(scoring_result$player_xwoba_full$BB), "\n")

}, error = function(e) {
  cat("WARNING: Model training/scoring failed:", e$message, "\n")
  cat("app.R will attempt retraining on startup.\n")
})

# ===================================================================
# STEP 5: Push to live Drive RDS
# ===================================================================
cat("\n=== STEP 5: UPDATING SHARED GOOGLE DRIVE RDS ===\n")
tryCatch({
  googledrive::drive_update(file = googledrive::as_id(LIVE_RDS_FILE_ID), media = "navs_all_data.rds")
  cat("Updated live Drive file\n")
}, error = function(e) {
  cat("WARNING: Failed to update live Drive file:", e$message, "\n")
})

# ===================================================================
# STEP 6: Clean / standardize
# ===================================================================
cat("\n=== STEP 6: CLEANING / STANDARDIZING ===\n")
clean_data <- clean_and_standardize_data(combined_data)
if (!is.null(clean_data) && nrow(clean_data) > 0) {
  saveRDS(clean_data, "xwoba_clean_data.rds")
  cat("Saved xwoba_clean_data.rds with", nrow(clean_data), "rows\n")
}

cat("\n=== PIPELINE COMPLETE ===\n")
cat("New files processed:", result$new_count, "\n")
cat("Total rows:", nrow(combined_data), "\n")
