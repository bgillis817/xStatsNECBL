# ============================================================================
#  xStatsNECBL - Daily Data Refresh Pipeline
#  Runs headless (no Shiny). Pulls raw CSVs from Google Drive, combines and
#  cleans them, and writes navs_all_data.rds + xwoba_clean_data.rds back
#  into the repo. app.R reads navs_all_data.rds on startup and retrains the
#  model on whatever is in that file - so this script just keeps the file
#  fresh. Designed to run via GitHub Actions cron.
# ============================================================================

suppressPackageStartupMessages({
  if (!require(pacman)) install.packages("pacman")
  pacman::p_load(dplyr, readr, data.table, tidyr, lubridate, stringr, googledrive)
})

# Google Drive auth via service account (CI-safe, no browser)
sa_path <- Sys.getenv("GDRIVE_SA_PATH", "service_account.json")
if (!file.exists(sa_path)) {
  stop("Service account JSON not found at: ", sa_path,
       " — set GDRIVE_SA_PATH or write the secret to service_account.json")
}
googledrive::drive_auth(path = sa_path)
cat("Authenticated to Google Drive via service account\n")

# Load pipeline functions (combine_navs_csvs_by_id, clean_and_standardize_data)
source("pipeline_functions.R")

# ===================================================================
# STEP 1: Combine raw CSVs from Google Drive (by folder ID)
# ===================================================================
cat("\n=== STEP 1: COMBINING CSVs FROM GOOGLE DRIVE ===\n")

GDRIVE_FOLDER_ID <- Sys.getenv("GDRIVE_FOLDER_ID", "1haJdctNyLTx81GXlvCdBRDSWKrDAlKAw")

combined_data <- combine_navs_csvs_by_id(folder_id = GDRIVE_FOLDER_ID)

if (is.null(combined_data) || nrow(combined_data) == 0) {
  stop("Pipeline aborted: no data combined from Google Drive folder ID '", GDRIVE_FOLDER_ID, "'")
}

cat("Combined", nrow(combined_data), "rows from Drive\n")

# ===================================================================
# STEP 2: Save raw combined data (this is what app.R reads as raw_data)
# ===================================================================
cat("\n=== STEP 2: SAVING navs_all_data.rds ===\n")
saveRDS(combined_data, "navs_all_data.rds")
cat("Saved navs_all_data.rds with", nrow(combined_data), "rows\n")

# ===================================================================
# STEP 3: Clean / standardize for the xwOBA feature pipeline
# ===================================================================
cat("\n=== STEP 3: CLEANING / STANDARDIZING ===\n")
clean_data <- clean_and_standardize_data(combined_data)

if (is.null(clean_data) || nrow(clean_data) == 0) {
  stop("Pipeline aborted: clean_and_standardize_data() returned no rows")
}

saveRDS(clean_data, "xwoba_clean_data.rds")
cat("Saved xwoba_clean_data.rds with", nrow(clean_data), "rows\n")

# ===================================================================
# STEP 4: Data quality summary (for Actions log)
# ===================================================================
cat("\n=== STEP 4: DATA QUALITY SUMMARY ===\n")
cat("Dataset dimensions:", nrow(combined_data), "rows x", ncol(combined_data), "columns\n")

required_cols <- c("ExitSpeed", "Angle", "PlayResult")
missing_cols <- setdiff(required_cols, names(combined_data))
if (length(missing_cols) > 0) {
  cat("WARNING: Missing required columns:", paste(missing_cols, collapse = ", "), "\n")
} else {
  cat("SUCCESS: All required columns present\n")
}
for (col in required_cols) {
  if (col %in% names(combined_data)) {
    complete_pct <- sum(!is.na(combined_data[[col]])) / nrow(combined_data) * 100
    cat(col, ":", round(complete_pct, 1), "% complete\n")
  }
}

cat("\n=== PIPELINE COMPLETE ===\n")
cat("Files updated:\n")
cat("  - navs_all_data.rds  (", nrow(combined_data), "rows )\n")
cat("  - xwoba_clean_data.rds  (", nrow(clean_data), "rows )\n")
cat("\napp.R will retrain the xwOBA model from navs_all_data.rds on its next start.\n")
