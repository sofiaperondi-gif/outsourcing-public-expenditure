#############################################
# diagnostics_biddername_filter.R
# Purpose: Inspect bidder_name and evaluate filtering
#############################################

library(data.table)
library(dplyr)
library(stringr)

# === 1. Set working directory ===
setwd("/Users/sofiaperondi/Desktop/Tesina IV /Data selection and summary analysis /Downloaded data")

# Folder where you manually placed EU-only CSVs
folder <- "data GDDP"   

# Create outputs folder (inside working directory)
output_dir <- "outputs"
dir.create(output_dir, showWarnings = FALSE)

# === 2. Variables to keep ===
cols_to_keep <- c(
  "buyer_country","buyer_nuts","buyer_buyertype","buyer_mainactivities",
  "tender_year","tender_digiwhist_price","tender_finalpriceUsd","bid_priceUsd",
  "tender_supplytype","tender_maincpv",
  "bidder_masterid","lot_smebidscount","bidder_country",
  "bid_issubcontracted","bid_subcontractedproportion",
  "corr_singleb","lot_bidscount","tender_proceduretype","tender_selectionmethod",
  "lot_updateddurationdays","tender_isframeworkagreement","tender_isdps",
  "filter_ok","tender_isawarded","bid_iswinning","bidder_name"
)

# === 3. Processing function (no bidder_name filter yet) ===
process_gppd_file <- function(file_path) {
  cat("Processing:", basename(file_path), "\n")
  tryCatch({
    data <- fread(file_path, select = cols_to_keep) %>%
      mutate(
        tender_isawarded = tender_isawarded == "t",
        bid_iswinning   = bid_iswinning == "t",
        filter_ok       = filter_ok == TRUE
      ) %>%
      filter(filter_ok, tender_isawarded, bid_iswinning)
    return(data)
  }, error = function(e) {
    cat("✗ Error with", basename(file_path), "-", conditionMessage(e), "\n")
    return(NULL)
  })
}

# === 4. Load all EU files ===
files <- list.files(folder, full.names = TRUE, pattern = "\\.csv(\\.gz)?$")
cat("Total files found:", length(files), "\n")

gppd_complete <- rbindlist(lapply(files, process_gppd_file), use.names = TRUE, fill = TRUE)
cat("Final dataset loaded:", nrow(gppd_complete), "contracts\n")

# === 5. Diagnostics on bidder_name ===
# Identify suspicious bidders (likely public entities)
suspicious <- gppd_complete %>%
  filter(grepl("ministry|municipality|government|council|university|hospital",
               bidder_name, ignore.case = TRUE))

cat("\n--- BIDDER_NAME DIAGNOSTICS ---\n")
cat("Total contracts:", nrow(gppd_complete), "\n")
cat("Suspicious contracts:", nrow(suspicious), "\n")
cat("Share suspicious:", round(100 * nrow(suspicious)/nrow(gppd_complete), 2), "%\n")

# By sector
susp_sector <- suspicious %>%
  group_by(tender_supplytype) %>%
  summarise(n_contracts = n(),
            total_value = sum(tender_digiwhist_price, na.rm = TRUE),
            .groups = "drop")
print(susp_sector)

# Top suspicious bidders
top_suspicious <- suspicious %>%
  group_by(bidder_name) %>%
  summarise(n_contracts = n(),
            total_value = sum(tender_digiwhist_price, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(desc(total_value)) %>%
  head(20)
print(top_suspicious)

# === 7. Before vs After Filter Comparison ===

# Define suspicious filter (likely public entities)
suspicious_flag <- grepl("ministry|municipality|government|council|university|hospital",
                         gppd_complete$bidder_name, ignore.case = TRUE)

# Counts before filtering
total_contracts <- nrow(gppd_complete)
total_value <- sum(gppd_complete$tender_digiwhist_price, na.rm = TRUE)

# Counts for suspicious bidders
susp_contracts <- sum(suspicious_flag)
susp_value <- sum(gppd_complete$tender_digiwhist_price[suspicious_flag], na.rm = TRUE)

# Counts after filtering (dropping suspicious)
clean_contracts <- total_contracts - susp_contracts
clean_value <- total_value - susp_value

cat("\n--- BEFORE vs AFTER FILTER (dropping public bidders) ---\n")
cat("Total contracts:", total_contracts, "\n")
cat("Suspicious contracts:", susp_contracts, "(", 
    round(100 * susp_contracts / total_contracts, 2), "% )\n")
cat("Remaining contracts:", clean_contracts, "\n\n")

cat("Total value:", round(total_value/1e9, 2), "B EUR\n")
cat("Suspicious value:", round(susp_value/1e9, 2), "B EUR (", 
    round(100 * susp_value / total_value, 2), "% )\n")
cat("Remaining value:", round(clean_value/1e9, 2), "B EUR\n")

# Export quick summary
comparison <- data.frame(
  Metric = c("Contracts", "Contract Value (B EUR)"),
  Total = c(total_contracts, round(total_value/1e9,2)),
  Suspicious = c(susp_contracts, round(susp_value/1e9,2)),
  Remaining = c(clean_contracts, round(clean_value/1e9,2)),
  Suspicious_SharePct = c(round(100*susp_contracts/total_contracts,2),
                          round(100*susp_value/total_value,2))
)

fwrite(comparison, file.path(output_dir, "before_after_bidder_filter.csv"))
print(comparison)

# === Suspicious keyword diagnostics ===

# Define keywords you want to check
keywords <- c("ministry", "municipality", "government", "council", "university", "hospital")

# Build frequency table
keyword_stats <- lapply(keywords, function(k) {
  subset <- gppd_complete %>%
    filter(grepl(k, bidder_name, ignore.case = TRUE))
  
  data.frame(
    keyword = k,
    n_contracts = nrow(subset),
    total_value_billion = sum(subset$tender_digiwhist_price, na.rm = TRUE) / 1e9,
    share_contracts_pct = round(100 * nrow(subset) / nrow(gppd_complete), 2),
    share_value_pct = round(100 * sum(subset$tender_digiwhist_price, na.rm = TRUE) /
                              sum(gppd_complete$tender_digiwhist_price, na.rm = TRUE), 2)
  )
})

keyword_stats <- do.call(rbind, keyword_stats)

print(keyword_stats)

# Export to CSV
fwrite(keyword_stats, file.path(output_dir, "suspicious_keywords_summary.csv"))

# === 6. Export diagnostics ===
fwrite(susp_sector, file.path(output_dir, "suspicious_sector.csv"))
fwrite(top_suspicious, file.path(output_dir, "top_suspicious_bidders.csv"))

# Save cleaned dataset with bidder_name still inside
saveRDS(gppd_complete, file.path(output_dir, "gppd_awarded_with_biddername.rds"))