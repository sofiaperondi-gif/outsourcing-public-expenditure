
# Set your working directory - CHANGE THIS PATH to where your CSV files are
setwd("/Users/sofiaperondi/Desktop/Tesina IV /Data selection and summary analysis /Downloaded data")  # Change this to your actual folder

# Clear memory and restart
rm(list = ls())
gc()

# Load packages
library(dplyr)
library(tidyr)
library(data.table)
install.packages("R.utils") 

# Read the files
gov_main <- read.csv("gov_10a_main.csv", colClasses = "character")
cofog <- read.csv("gov_10a_exp.csv", colClasses = "character")


# CORRECTED EU COUNTRIES LIST WITH BOTH GREECE CODES
eu_countries <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", 
                            "FR", "GR", "EL", "HR", "HU", "IE", "IT", "LT", "LU", "LV", 
                            "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")

# Filter main government data - USING ONLY VARIABLES THAT EXIST
gov_clean <- gov_main %>%
  mutate(
    TIME_PERIOD = as.numeric(TIME_PERIOD),
    OBS_VALUE = as.numeric(OBS_VALUE)
  ) %>%
  filter(
    na_item %in% c("P3", "TE", "B1G", "P2", "P5", "D1PAY"), 
    sector == "S13",
    geo %in% eu_countries,
    TIME_PERIOD >= 2006 & TIME_PERIOD <= 2021
  ) %>%
  distinct(geo, TIME_PERIOD, na_item, .keep_all = TRUE) %>%
  select(geo, TIME_PERIOD, na_item, OBS_VALUE) %>%
  pivot_wider(
    names_from = na_item, 
    values_from = OBS_VALUE
  ) %>%
  rename(
    final_consumption = P3,
    total_expenditure = TE,
    gdp = B1G,
    intermediate_consumption = P2,
    gross_fixed_capital = P5,
    time = TIME_PERIOD,
    compensation_employees = D1PAY
  )

# Filter COFOG data
cofog_clean <- cofog %>%
  mutate(
    TIME_PERIOD = as.numeric(TIME_PERIOD),
    OBS_VALUE = as.numeric(OBS_VALUE)
  ) %>%
  filter(
    cofog99 %in% c("GF01", "GF03", "GF04", "GF07", "GF09", "GF10"),
    sector == "S13",
    geo %in% eu_countries, 
    TIME_PERIOD >= 2006 & TIME_PERIOD <= 2021
  ) %>%
  distinct(geo, TIME_PERIOD, cofog99, .keep_all = TRUE) %>%
  select(geo, TIME_PERIOD, cofog99, OBS_VALUE) %>%
  pivot_wider(
    names_from = cofog99, 
    values_from = OBS_VALUE
  ) %>%
  rename(
    public_services = GF01,
    public_order = GF03,
    economic_affairs = GF04,
    health = GF07,
    education = GF09,
    social_protection = GF10,
    time = TIME_PERIOD
  )

# Merge both datasets
final_data <- gov_clean %>%
  left_join(cofog_clean, by = c("geo", "time"))

# Add country names
final_data <- final_data %>%
  mutate(country_name = case_when(
    geo == "AT" ~ "Austria", geo == "BE" ~ "Belgium", geo == "BG" ~ "Bulgaria",
    geo == "CY" ~ "Cyprus", geo == "CZ" ~ "Czechia", geo == "DE" ~ "Germany",
    geo == "DK" ~ "Denmark", geo == "EE" ~ "Estonia", geo == "ES" ~ "Spain",
    geo == "FI" ~ "Finland", geo == "FR" ~ "France", geo == "EL" ~ "Greece",
    geo == "HR" ~ "Croatia", geo == "HU" ~ "Hungary", geo == "IE" ~ "Ireland",
    geo == "IT" ~ "Italy", geo == "LT" ~ "Lithuania", geo == "LU" ~ "Luxembourg",
    geo == "LV" ~ "Latvia", geo == "MT" ~ "Malta", geo == "NL" ~ "Netherlands",
    geo == "PL" ~ "Poland", geo == "PT" ~ "Portugal", geo == "RO" ~ "Romania",
    geo == "SE" ~ "Sweden", geo == "SI" ~ "Slovenia", geo == "SK" ~ "Slovakia"
  ))

# Save the final data
write.csv(final_data, "eurostat_clean_data.csv", row.names = FALSE)

cat("Done! Final dataset has", nrow(final_data), "rows\n")
print(head(final_data))

dim(final_data)
table(final_data$geo)
colSums(is.na(final_data))
unique(gov_main$na_item)

# Upload cleaned dataset
country_data <- read.csv("eurostat_clean_data.csv")

# Download regional data
regional_gdp <- read.csv("nama_10r_2gdp.csv") 

# Explore the structure of regional data
cat("Struttura dati regionali:\n")
print(head(regional_gdp))
cat("Nomi colonne regionali:", names(regional_gdp), "\n")
cat("Anni disponibili:", unique(regional_gdp$TIME_PERIOD), "\n")

# Control geographical codes
cat("Esempio codici NUTS2:", head(unique(regional_gdp$geo)), "\n")

# Cleand data at regional level (NUTS 2)
regional_gdp_clean <- regional_gdp %>%
  filter(
    nchar(geo) == 4,  # Solo codici NUTS2 (4 caratteri: ES61, ITC4, etc.)
    substr(geo, 1, 2) %in% c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", "FR", 
                             "HR", "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT", 
                             "PL", "PT", "RO", "SI", "SK", "FI", "SE")
  ) %>%
  mutate(
    time = as.numeric(TIME_PERIOD),
    regional_gdp = as.numeric(OBS_VALUE),
    country_code = substr(geo, 1, 2)  # Estrae il codice paese per unire dopo
  ) %>%
  select(geo, country_code, time, regional_gdp)

# Verify
cat("Dati regionali NUTS2 puliti:\n")
print(head(regional_gdp_clean))
cat("Numero di regioni NUTS2:", n_distinct(regional_gdp_clean$geo), "\n")
cat("Osservazioni totali:", nrow(regional_gdp_clean), "\n")

# Merge dataset keeping regional level 
final_dataset <- country_data %>%
  inner_join(regional_gdp_clean, by = c("geo" = "country_code", "time")) %>%
  rename(
    country_code = geo,          # Codice paese
    nuts2_region = geo.y,        # Codice regione NUTS2
    gdp_national = gdp,          # GDP nazionale (dal dataset country)
    gdp_regional = regional_gdp  # GDP regionale (nuova variabile)
  )

# Verify union
cat("=== DATASET FINALE MULTI-LIVELLO ===\n")
cat("Osservazioni totali:", nrow(final_dataset), "\n")
cat("Numero di paesi:", n_distinct(final_dataset$country_code), "\n")
cat("Numero di regioni NUTS2:", n_distinct(final_dataset$nuts2_region), "\n")
cat("Anni coperti:", toString(unique(final_dataset$time)), "\n")
cat("Struttura finale:\n")
print(head(final_dataset))

# Save dataset 
write.csv(final_dataset, "thesis_multilevel_dataset.csv", row.names = FALSE)
final_dataset <- read.csv( "thesis_multilevel_dataset.csv")

cat("✅ Dataset multi-livello salvato con successo!\n")
cat("📊 Struttura finale:", nrow(final_dataset), "osservazioni regione-anno\n")
cat("🇪🇺 Paesi:", n_distinct(final_dataset$country_code), "\n")
cat("🗺️ Regioni NUTS2:", n_distinct(final_dataset$nuts2_region), "\n")

cat("Number of countries after:", n_distinct(country_data$geo), "\n")

#loading GDDP dataset

# === 1. Unzip files ===
unzip("GTI Global Public Procurement Dataset (GPPD) 12.zip", exdir = "data1/GPPD")
unzip("GTI Global Public Procurement Dataset (GPPD) 22.zip", exdir = "data2/GPPD")

# === 2. Define EU countries (EL for Greece to match Eurostat) ===
eu_countries <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", 
                  "FR", "GR", "EL", "HR", "HU", "IE", "IT", "LT", "LU", "LV", 
                  "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")

# === 3. Columns to keep (only necessary vars) ===
cols_to_keep <- c(
  "buyer_country", "buyer_nuts", "buyer_buyertype", "buyer_mainactivities",
  "tender_year", "tender_digiwhist_price", "tender_finalpriceUsd", "bid_priceUsd",
  "tender_supplytype", "tender_maincpv",
  "bidder_masterid", "lot_smebidscount", "bidder_country",
  "bid_issubcontracted", "bid_subcontractedproportion",
  "corr_singleb", "lot_bidscount", "tender_proceduretype", "tender_selectionmethod",
  "lot_updateddurationdays", "tender_isframeworkagreement", "tender_isdps",
  # filters needed for cleaning
  "filter_ok", "tender_isawarded", "bid_iswinning", "bidder_name"
)

# === 4. Safe file processing function ===
process_gppd_file_safe <- function(file_path) {
  cat("Processing:", basename(file_path), "\n")
  
  tryCatch({
    data <- fread(file_path, select = cols_to_keep) %>%
      mutate(
        tender_isawarded = tender_isawarded == "t",
        bid_iswinning = bid_iswinning == "t",
        filter_ok = filter_ok == TRUE
      )
    
    filtered_data <- data %>%
      filter(
        filter_ok,
        tender_isawarded,
        bid_iswinning,
        !(grepl("ministry|municipality|government", bidder_name, ignore.case = TRUE))
      ) %>%
      select(-filter_ok, -tender_isawarded, -bid_iswinning, -bidder_name)
    
    # 🔍 Add this check BEFORE returning
    if (all(filtered_data$tender_maincpv == "t")) {
      cat("⚠️ CPV codes corrupted in:", basename(file_path), "\n")
    } else {
      cat("✓ CPV codes look OK in:", basename(file_path), "\n")
    }
    
    return(filtered_data)
    
  }, error = function(e) {
    cat("✗ ERROR with", basename(file_path), "-", conditionMessage(e), "\n")
    return(NULL)
  })
}

# === 5. Batch processing function ===
process_batch_safe <- function(file_list, label) {
  cat("=== Processing batch:", label, "(", length(file_list), "files ) ===\n")
  results <- lapply(file_list, process_gppd_file_safe)
  return(rbindlist(results, use.names = TRUE, fill = TRUE))
}

# === 6. File lists (filter EU27 only) ===
folder1 <- "data1/GPPD/GTI Global Public Procurement Dataset (GPPD) 12"
folder2 <- "data2/GPPD/GTI Global Public Procurement Dataset (GPPD) 22"

files1 <- list.files(folder1, full.names = TRUE, pattern = "\\.csv.gz$")
files2 <- list.files(folder2, full.names = TRUE, pattern = "\\.csv.gz$")
files1 <- files1[substr(basename(files1), 1, 2) %in% eu_countries]
files2 <- files2[substr(basename(files2), 1, 2) %in% eu_countries]

cat("Total files to process:", length(files1) + length(files2), "\n")

# Load one country without filtering
test_data <- fread(files1[1], select = cols_to_keep)

# Check column names
names(test_data)

# Check values in key filters
table(test_data$filter_ok, useNA = "ifany")
table(test_data$tender_isawarded, useNA = "ifany")
table(test_data$bid_iswinning, useNA = "ifany")

# === 7. Process both folders ===
gppd_folder1 <- process_batch_safe(files1, "Folder1")
gppd_folder2 <- process_batch_safe(files2, "Folder2")

# === 8. Combine all parts ===
gppd_complete <- rbindlist(list(gppd_folder1, gppd_folder2), use.names = TRUE, fill = TRUE)

cat("🎉 PROCESSING COMPLETE!\n")
cat("Final dataset:", nrow(gppd_complete), "observations\n")
cat("Countries included:", toString(unique(gppd_complete$buyer_country)), "\n")

# === 9. Save final dataset ===
write.csv(gppd_complete, "gppd_outsourcing_data_FINAL.csv", row.names = FALSE)
save(gppd_complete, file = "gppd_outsourcing_data_FINAL.RData")

cat("✅ Final dataset saved: gppd_outsourcing_data_FINAL.csv & .RData\n")

#open GPPD
gppd_complete <- read.csv("gppd_outsourcing_data_FINAL.csv")

# Check countries
unique(gppd_complete$buyer_country)
table(gppd_complete$buyer_country)

#Fix countries
# Keep only EU-27 countries
gppd_complete <- gppd_complete %>%
  filter(buyer_country %in% eu_countries)

cat("✅ After filtering:", length(unique(gppd_complete$buyer_country)), "countries\n")
cat("Countries kept:", toString(sort(unique(gppd_complete$buyer_country))), "\n")

#Change Greece to be consistent with Eurostat
gppd_complete <- gppd_complete %>%
  mutate(buyer_country = ifelse(buyer_country == "GR", "EL", buyer_country))

# Check variables
names(gppd_complete)
setdiff(cols_to_keep, names(gppd_complete))

# Check missing values
missingness <- sapply(gppd_complete, function(x) mean(is.na(x)) * 100)
missingness <- sort(missingness, decreasing = TRUE)

# Show top 15 variables with most missing data
head(missingness, 15)

# Sanity check 
stopifnot(!all(gppd_complete$tender_maincpv == "t"))

# Overwrite the old CSV
write.csv(gppd_complete, "gppd_outsourcing_data_FINAL.csv", row.names = FALSE)

# Overwrite the old RData
save(gppd_complete, file = "gppd_outsourcing_data_FINAL.RData")

cat("✅ Old dataset successfully replaced with the cleaned version\n")

head(gppd_complete$tender_maincpv, 10)
table(substr(gppd_complete$tender_maincpv, 1, 2))


## MERGING EUROSTAT AND GDDP 
# Aggregate by country (for merging with Eurostat)
gppd_country <- gppd_complete %>%
  group_by(buyer_country, tender_year) %>%
  summarise(
    total_value_ppp = sum(tender_digiwhist_price, na.rm = TRUE),
    avg_bids = mean(lot_bidscount, na.rm = TRUE),
    single_bid_rate = mean(corr_singleb, na.rm = TRUE),
    share_framework = mean(tender_isframeworkagreement, na.rm = TRUE),
    .groups = "drop"
  )

# Load your Eurostat data
eurostat_country <- read.csv("eurostat_clean_data_corrected.csv")

# Merge with Eurostat data
panel_df <- eurostat_country %>%
  rename(year = time) %>%
  left_join(gppd_country, by = c("geo" = "buyer_country", "year" = "tender_year"))

# Save the merged panel dataset
write.csv(panel_df, "panel_data_merged.csv", row.names = FALSE)

cat("Panel dataset created with", nrow(panel_df), "observations\n")


