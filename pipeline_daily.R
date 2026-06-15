# ============================================================================
#  xStatsNECBL - Daily Data Refresh Pipeline (incremental)
#  Only processes CSVs that are new/modified since the last run (tracked via
#  file_manifest.rds). Appends new rows to the existing navs_all_data.rds,
#  re-cleans, pushes the updated file to the live Google Drive RDS that
#  app.R reads on startup, commits results, and the workflow then redeploys
#  to shinyapps.io.
#
#  First run: no manifest exists, so every file in "Navs CSVs" is treated as
#  new. The script seeds existing_data from the current live Drive RDS
#  (2023-2025), combines it with everything pulled from the folder, dedupes,
#  and writes the merged result. This first run is slow (processes ~1000
#  files). Every run after that only touches files added/changed since the
#  last run - typically a handful per day during the season.
# ============================================================================

suppressPackageStartupMessages({
  if (!require(pacman)) install.packages("pacman")
  pacman::p_load(dplyr, readr, data.table, tidyr, lubridate, stringr, googledrive)
})

sa_path <- Sys.getenv("GDRIVE_SA_PATH", "service_account.json")
if (!file.exists(sa_path)) {
  stop("Service account JSON not found at: ", sa_path)
}
googledrive::drive_auth(path = sa_path)
cat("Authenticated to Google Drive via service account\n")

source("pipeline_functions.R")

GDRIVE_FOLDER_ID <- Sys.getenv("GDRIVE_FOLDER_ID", "1haJdctNyLTx81GXlvCdBRDSWKrDAlKAw")
LIVE_RDS_FILE_ID <- Sys.getenv("LIVE_RDS_FILE_ID", "1IFO08F2YoO1qUXCw3GiU1k6FOXwt4Q0b")

# ===================================================================
# STEP 1: Incremental combine - only new/changed files since last run
# ===================================================================
cat("\n=== STEP 1: INCREMENTAL COMBINE FROM GOOGLE DRIVE ===\n")

result <- combine_navs_csvs_incremental(folder_id = GDRIVE_FOLDER_ID,
                                          manifest_path = "file_manifest.rds")

if (is.null(result$manifest)) {
  stop("Pipeline aborted: could not list files in Drive folder '", GDRIVE_FOLDER_ID, "'")
}

if (result$new_count == 0) {
  cat("\nNo new/changed files since last run. Nothing to do.\n")
  saveRDS(result$manifest, "file_manifest.rds")
  quit(save = "no", status = 0)
}

new_data <- result$new_data

# ===================================================================
# STEP 2: Merge with existing navs_all_data.rds (or seed from live Drive
# file on first run)
# ===================================================================
cat("\n=== STEP 2: MERGING WITH EXISTING DATA ===\n")

if (file.exists("navs_all_data.rds")) {
  existing_data <- readRDS("navs_all_data.rds")
  cat("Loaded existing navs_all_data.rds with", nrow(existing_data), "rows\n")
} else {
  cat("No local navs_all_data.rds - seeding from live Drive file (one-time)\n")
  existing_data <- tryCatch(
    readRDS(url(paste0("https://drive.google.com/uc?export=download&id=", LIVE_RDS_FILE_ID))),
    error = function(e) {
      cat("Could not load live Drive file (", e$message, ") - starting fresh\n")
      data.frame()
    }
  )
  cat("Seeded with", nrow(existing_data), "rows from live Drive file\n")
}

# Align columns (in case new files have slightly different column sets)
if (nrow(existing_data) > 0 && nrow(new_data) > 0) {
  all_cols <- union(names(existing_data), names(new_data))
  for (col in setdiff(all_cols, names(existing_data))) existing_data[[col]] <- NA
  for (col in setdiff(all_cols, names(new_data)))      new_data[[col]] <- NA
}

combined_data <- bind_rows(existing_data, new_data)

# Dedupe: handles re-processing a modified file or seed/new overlap
dedupe_cols <- intersect(c("source_file","Batter","Date","Inning","PAofInning","PitchNo"),
                          names(combined_data))
if (length(dedupe_cols) > 0) {
  before <- nrow(combined_data)
  combined_data <- combined_data %>% distinct(across(all_of(dedupe_cols)), .keep_all = TRUE)
  cat("Deduped:", before, "->", nrow(combined_data), "rows\n")
}

cat("Total combined rows:", nrow(combined_data), "\n")

# ===================================================================
# STEP 3: Save updated navs_all_data.rds + manifest
# ===================================================================
cat("\n=== STEP 3: SAVING navs_all_data.rds ===\n")
saveRDS(combined_data, "navs_all_data.rds")
saveRDS(result$manifest, "file_manifest.rds")
cat("Saved navs_all_data.rds (", nrow(combined_data), "rows ) and file_manifest.rds (",
    nrow(result$manifest), "files tracked )\n")

# ===================================================================
# STEP 4: Push to live Google Drive RDS that app.R reads on startup
# ===================================================================
cat("\n=== STEP 4: UPDATING SHARED GOOGLE DRIVE RDS ===\n")
tryCatch({
  googledrive::drive_update(
    file  = googledrive::as_id(LIVE_RDS_FILE_ID),
    media = "navs_all_data.rds"
  )
  cat("Updated live Drive file (id:", LIVE_RDS_FILE_ID, ")\n")
}, error = function(e) {
  cat("WARNING: Failed to update live Drive file:", e$message, "\n")
  cat("app.R will keep serving the previous version until this succeeds.\n")
})

# ===================================================================
# STEP 5: Clean / standardize for the xwOBA feature pipeline
# ===================================================================
cat("\n=== STEP 5: CLEANING / STANDARDIZING ===\n")
clean_data <- clean_and_standardize_data(combined_data)

if (!is.null(clean_data) && nrow(clean_data) > 0) {
  saveRDS(clean_data, "xwoba_clean_data.rds")
  cat("Saved xwoba_clean_data.rds with", nrow(clean_data), "rows\n")
} else {
  cat("WARNING: clean_and_standardize_data() returned no rows - skipping xwoba_clean_data.rds\n")
}

cat("\n=== PIPELINE COMPLETE ===\n")
cat("New files processed this run:", result$new_count, "\n")
cat("Total rows in navs_all_data.rds:", nrow(combined_data), "\n")
