# === 0. Libraries ===
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

# === 1. Set working directory ===
setwd("/Users/sofiaperondi/Desktop/Tesina IV /Data selection and summary analysis /Downloaded data")

# === 2. EU countries (Eurostat codes) ===
eu_countries <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", 
                  "FR","GR", "EL", "HR", "HU", "IE", "IT", "LT", "LU", "LV", 
                  "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")

# === 3. Variables to keep (final restricted set) ===
cols_to_keep <- c(
  "buyer_country", "buyer_nuts", "buyer_mainactivities", "buyer_buyertype",
  "tender_year", "tender_digiwhist_price", "tender_supplytype",
  "bidder_country", "bid_issubcontracted", "bid_subcontractedproportion",
  "tender_selectionmethod", "lot_updateddurationdays",
  "corr_singleb",   # example corruption indicator, can be replaced/expanded later
  # filters for cleaning
  "filter_ok", "tender_isawarded", "bid_iswinning"
)

# === 4. File list (all EU CSVs already in same folder) ===
data_dir <- "data GDDP"   # <- folder where you placed the csv.gz files
files <- list.files(data_dir, full.names = TRUE, pattern = "\\.csv(\\.gz)?$")
files <- files[substr(basename(files), 1, 2) %in% eu_countries]

cat("Total EU files found:", length(files), "\n")

# === 5. Processing function ===
process_gppd_file <- function(file_path) {
  cat("Processing:", basename(file_path), "\n")
  tryCatch({
    data <- fread(file_path, select = cols_to_keep) %>%
      mutate(
        tender_isawarded = tender_isawarded == "t",
        bid_iswinning = bid_iswinning == "t",
        filter_ok = filter_ok == TRUE
      ) %>%
      filter(
        filter_ok,
        tender_isawarded,
        bid_iswinning
        # ⚠️ NO filter on bidder_name
      ) %>%
      select(-filter_ok, -tender_isawarded, -bid_iswinning)
    return(data)
  }, error = function(e) {
    cat("✗ ERROR:", basename(file_path), "-", conditionMessage(e), "\n")
    return(NULL)
  })
}

# === 6. Batch processing ===
gppd_list <- lapply(files, process_gppd_file)
gppd_complete <- rbindlist(gppd_list, use.names = TRUE, fill = TRUE)

cat("✅ Processing complete\n")
cat("Final dataset:", nrow(gppd_complete), "contracts\n")
cat("Countries included:", toString(unique(gppd_complete$buyer_country)), "\n")

# === 7. Harmonize country codes ===
gppd_complete <- gppd_complete %>%
  filter(buyer_country %in% eu_countries) %>%
  mutate(buyer_country = ifelse(buyer_country == "GR", "EL", buyer_country))

cat("✅ After filtering:", length(unique(gppd_complete$buyer_country)), "countries\n")
cat("Countries kept:", toString(sort(unique(gppd_complete$buyer_country))), "\n")

# === 10. Save final dataset ===
write.csv(gppd_complete, "gppd_clean_selected.csv", row.names = FALSE)
save(gppd_complete, file = "gppd_clean_selected.RData")

cat("✅ Final dataset saved: gppd_clean_selected.csv & .RData\n")

# TRANSFORM SPECIFIED VARIABLES TO DUMMIES
library(dplyr)

# Load your dataset
gppd_complete <- read.csv("gppd_clean_selected.csv")

# ALTERNATIVE METHOD WITHOUT fastDummies PACKAGE
gppd_with_dummies <- gppd_complete %>%
  # Convert bid_issubcontracted to binary (0/1)
  mutate(bid_issubcontracted = as.integer(bid_issubcontracted == "t" | bid_issubcontracted == TRUE)) %>%
  # Create dummies manually for categorical variables
  
  # For tender_supplytype
  mutate(tender_supplytype_works = as.integer(tender_supplytype == "WORKS"),
         tender_supplytype_services = as.integer(tender_supplytype == "SERVICES"),
         tender_supplytype_supplies = as.integer(tender_supplytype == "SUPPLIES")) %>%
  
  # For tender_selectionmethod (add the most common methods)
  mutate(tender_selectionmethod_open = as.integer(tender_selectionmethod == "OPEN"),
         tender_selectionmethod_restricted = as.integer(tender_selectionmethod == "RESTRICTED"),
         tender_selectionmethod_negotiated = as.integer(tender_selectionmethod == "NEGOTIATED")) %>%
  
  # For bidder_country (create dummies for EU countries)
  mutate(bidder_country_local = as.integer(bidder_country == buyer_country),
         bidder_country_eu = as.integer(bidder_country %in% eu_countries & bidder_country != buyer_country))

# Check results
cat("Dummy variables created:\n")
cat("- tender_supplytype_works:", sum(gppd_with_dummies$tender_supplytype_works, na.rm = TRUE), "\n")
cat("- tender_supplytype_services:", sum(gppd_with_dummies$tender_supplytype_services, na.rm = TRUE), "\n")
cat("- tender_supplytype_supplies:", sum(gppd_with_dummies$tender_supplytype_supplies, na.rm = TRUE), "\n")
cat("- bidder_country_local:", sum(gppd_with_dummies$bidder_country_local, na.rm = TRUE), "\n")
cat("- bidder_country_eu:", sum(gppd_with_dummies$bidder_country_eu, na.rm = TRUE), "\n")

# Controlla se ci sono overlap
check_overlap <- gppd_with_dummies %>%
  summarise(
    both_local_eu = sum(bidder_country_local == 1 & bidder_country_eu == 1, na.rm = TRUE),
    neither_local_eu = sum(bidder_country_local == 0 & bidder_country_eu == 0, na.rm = TRUE)
  )

cat("Bidder country categories:\n")
cat("Sovrapposizione locale-EU:", check_overlap$both_local_eu, "\n")
cat("Né locale né EU:", check_overlap$neither_local_eu, "\n")

# Mantieni la variabile "bidder_origin" con tre categorie:
# - "local" (stesso paese del buyer)
# - "eu_other" (altro paese EU)  
# - "non_eu" (paese extra-UE)

gppd_final <- gppd_with_dummies %>%
  mutate(
    bidder_origin = case_when(
      bidder_country == buyer_country ~ "local",
      bidder_country %in% eu_countries ~ "eu_other", 
      TRUE ~ "non_eu"
    )
  )

# Analizza come varia la spesa per tipo di fornitore
spesa_per_origine <- gppd_final %>%
  group_by(bidder_origin) %>%
  summarise(
    n_contratti = n(),
    spesa_media = mean(tender_digiwhist_price, na.rm = TRUE),
    spesa_totale = sum(tender_digiwhist_price, na.rm = TRUE)
  )

print(spesa_per_origine)

# DUMMY VARIABLES FINALI CON REFERENCE CATEGORIES
gppd_final <- gppd_with_dummies %>%
  mutate(
    # 1. BIDDER ORIGIN (3 categorie)
    bidder_origin_local = as.integer(bidder_country == buyer_country),
    bidder_origin_eu = as.integer(bidder_country %in% eu_countries & bidder_country != buyer_country),
    # REFERENCE: bidder_origin_non_eu (quando entrambe = 0)
    
    # 2. TENDER SUPPLY TYPE (3 categorie principali)
    tender_supplytype_works = as.integer(tender_supplytype == "WORKS"),
    tender_supplytype_services = as.integer(tender_supplytype == "SERVICES"), 
    tender_supplytype_supplies = as.integer(tender_supplytype == "SUPPLIES"),
    # REFERENCE: altre categorie (OTHER, missing, etc.)
    
    # 3. TENDER SELECTION METHOD (3 metodi principali)
    tender_selectionmethod_open = as.integer(tender_selectionmethod == "OPEN"),
    tender_selectionmethod_restricted = as.integer(tender_selectionmethod == "RESTRICTED"),
    tender_selectionmethod_negotiated = as.integer(tender_selectionmethod == "NEGOTIATED"),
    # REFERENCE: altri metodi (COMPETITIVE_DIALOGUE, etc.)
    
    # 4. BINARY VARIABLES (già reference = 0)
    bid_issubcontracted = as.integer(bid_issubcontracted == "t" | bid_issubcontracted == TRUE)
    # REFERENCE: bid_issubcontracted = 0 (non subappaltato)
  )

# CONTINUOUS VARIABLES (no reference category)
# - bid_subcontractedproportion (0-1)
# - lot_updateddurationdays (giorni)

# Save the dataset
write.csv(gppd_with_dummies, "gppd_with_dummies.csv", row.names = FALSE)
cat("💾 Saved: gppd_with_dummies.csv\n")

#Questo dataset va bene, solo che vorrei usare delle reference category piu appropriate per l'analisi
# Per cui ho creato il nuovo script con analisi definitiva 

#Versione finale del dataset: "gppd_final_corrected_dummies.csv"