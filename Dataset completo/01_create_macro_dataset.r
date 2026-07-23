
setwd("/Users/sofiaperondi/Desktop/Tesina IV /Data selection and summary analysis /Downloaded data")

# 1. Pulisci memoria
rm(list = ls())
gc()

# 01_create_macro_dataset.R
library(tidyverse)

# Carica il dataset merged
final_dataset_2011 <- read_csv("FINAL_ANALYSIS_DATASET_2006_2021.csv") %>%
  filter(tender_year >= 2011 & tender_year <= 2021)

# Crea dataset macro
macro_dataset <- final_dataset_2011 %>%
  group_by(buyer_country, tender_year) %>%
  summarise(
    # VARIABILE DIPENDENTE
    total_expenditure_growth_real = first(total_expenditure_growth_real),
    
    # CONTROLLI MACRO
    unemp_rate = first(unemp_rate),
    recession = first(recession),
    old_dep = first(old_dep),
    pop_density = first(pop_density),
    gov_debt_pct_gdp = first(gov_debt_pct_gdp),
    corruption_control = first(corruption_control),
    gov_state_market = first(gov_state_market),
    
    # VARIABILI PROCUREMENT AGGREGATE
    prop_eu_bidders = mean(bidder_origin_eu, na.rm = TRUE),
    prop_non_eu_bidders = mean(bidder_origin_non_eu, na.rm = TRUE),
    prop_works = mean(tender_supplytype_works, na.rm = TRUE),
    prop_services = mean(tender_supplytype_services, na.rm = TRUE),
    prop_meat = mean(tender_selectionmethod_meat, na.rm = TRUE),
    prop_subcontracted = mean(bid_issubcontracted, na.rm = TRUE),
    avg_contract_price = mean(tender_digiwhist_price, na.rm = TRUE),
    total_contract_value = sum(tender_digiwhist_price, na.rm = TRUE),
    n_contracts = n(),
    
    .groups = 'drop'
  ) %>%
  arrange(buyer_country, tender_year)

# Salva
write_csv(macro_dataset, "macro_analysis_dataset_2011_2021.csv")

cat("✅ DATASET MACRO CREATO:\n")
cat("Osservazioni:", nrow(macro_dataset), "\n")
cat("Paesi:", n_distinct(macro_dataset$buyer_country), "\n")
cat("Anni:", n_distinct(macro_dataset$tender_year), "\n")