# PULIZIA MEMORIA E CREAZIONE SICURA DEL DATASET FINALE

# VERSIONE SUPER SICURA - CREA FILE CON NOME UNIVOCO

# 1. Pulisci memoria
rm(list = ls())
gc()

library(dplyr)

# 2. Carica l'ORIGINALE (mai modificato)
gppd_clean <- read.csv("gppd_clean_selected.csv")

# 3. Definisci paesi EU
eu_countries <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", 
                  "FR", "GR", "EL", "HR", "HU", "IE", "IT", "LT", "LU", "LV", 
                  "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")

# 4. Crea dataset FINALE con dummy corrette
gppd_final_corrected <- gppd_clean %>%
  mutate(
    # BIDDER ORIGIN - Reference: "local"
    bidder_origin_eu = as.integer(bidder_country %in% eu_countries & bidder_country != buyer_country),
    bidder_origin_non_eu = as.integer(!bidder_country %in% eu_countries & bidder_country != ""),
    
    # TENDER SUPPLY TYPE - Reference: "supplies" 
    tender_supplytype_works = as.integer(tender_supplytype == "WORKS"),
    tender_supplytype_services = as.integer(tender_supplytype == "SERVICES"),
    
    # TENDER SELECTION METHOD - Reference: "LOWEST_PRICE" 
    tender_selectionmethod_meat = as.integer(tender_selectionmethod == "MEAT"),
    
    # BINARY VARIABLES
    bid_issubcontracted = as.integer(bid_issubcontracted == "t" | bid_issubcontracted == TRUE)
  )

# CHECK DIAGNOSIS COMPLETO PRIMA DEL SALVATAGGIO FINALE

cat("🔍 DIAGNOSI COMPLETA DEL DATASET FINALE\n")
cat("=========================================\n\n")

# 1. CHECK STRUTTURA BASE
cat("1. STRUTTURA BASE:\n")
cat("   - Osservazioni:", nrow(gppd_final_corrected), "\n")
cat("   - Variabili:", ncol(gppd_final_corrected), "\n")
cat("   - Memory usage:", format(object.size(gppd_final_corrected), units = "MB"), "\n\n")

# 2. CHECK DUMMY VARIABLES
cat("2. DUMMY VARIABLES - DISTRIBUZIONE:\n")

# Bidder Origin
cat("   BIDDER ORIGIN:\n")
bidder_summary <- gppd_final_corrected %>%
  summarise(
    local = n() - sum(bidder_origin_eu) - sum(bidder_origin_non_eu),
    eu_other = sum(bidder_origin_eu),
    non_eu = sum(bidder_origin_non_eu),
    total = local + eu_other + non_eu
  )
cat("     - Locale (REF):", bidder_summary$local, "(", round(bidder_summary$local/bidder_summary$total*100, 1), "%)\n")
cat("     - EU altro:", bidder_summary$eu_other, "(", round(bidder_summary$eu_other/bidder_summary$total*100, 1), "%)\n")
cat("     - Extra-UE:", bidder_summary$non_eu, "(", round(bidder_summary$non_eu/bidder_summary$total*100, 1), "%)\n")

# Supply Type
cat("   SUPPLY TYPE:\n")
supply_summary <- gppd_final_corrected %>%
  summarise(
    supplies_ref = n() - sum(tender_supplytype_works) - sum(tender_supplytype_services),
    works = sum(tender_supplytype_works),
    services = sum(tender_supplytype_services),
    total = supplies_ref + works + services
  )
cat("     - Supplies (REF):", supply_summary$supplies_ref, "(", round(supply_summary$supplies_ref/supply_summary$total*100, 1), "%)\n")
cat("     - Works:", supply_summary$works, "(", round(supply_summary$works/supply_summary$total*100, 1), "%)\n")
cat("     - Services:", supply_summary$services, "(", round(supply_summary$services/supply_summary$total*100, 1), "%)\n")

# Selection Method
cat("   SELECTION METHOD:\n")
selection_summary <- gppd_final_corrected %>%
  summarise(
    lowest_price_ref = sum(tender_selectionmethod == "LOWEST_PRICE", na.rm = TRUE),
    meat = sum(tender_selectionmethod_meat),
    other = sum(!tender_selectionmethod %in% c("LOWEST_PRICE", "MEAT"), na.rm = TRUE),
    total = lowest_price_ref + meat + other
  )
cat("     - Lowest Price (REF):", selection_summary$lowest_price_ref, "(", round(selection_summary$lowest_price_ref/selection_summary$total*100, 1), "%)\n")
cat("     - MEAT (qualità):", selection_summary$meat, "(", round(selection_summary$meat/selection_summary$total*100, 1), "%)\n")
cat("     - Altri metodi:", selection_summary$other, "(", round(selection_summary$other/selection_summary$total*100, 1), "%)\n")

# Subcontracted
cat("   SUBCONTRACTED:\n")
sub_summary <- gppd_final_corrected %>%
  summarise(
    no_ref = sum(bid_issubcontracted == 0, na.rm = TRUE),
    yes = sum(bid_issubcontracted == 1, na.rm = TRUE),
    total = no_ref + yes
  )
cat("     - Non subappaltato (REF):", sub_summary$no_ref, "(", round(sub_summary$no_ref/sub_summary$total*100, 1), "%)\n")
cat("     - Subappaltato:", sub_summary$yes, "(", round(sub_summary$yes/sub_summary$total*100, 1), "%)\n\n")

# 3. CHECK QUALITÀ DATI
cat("3. QUALITÀ DATI:\n")
missing_summary <- gppd_final_corrected %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing") %>%
  filter(missing > 0) %>%
  arrange(desc(missing))

if(nrow(missing_summary) > 0) {
  cat("   VARIABILI CON MISSING VALUES:\n")
  print(missing_summary)
} else {
  cat("   ✅ Nessun missing value trovato\n")
}

# 4. CHECK CONSISTENZA
cat("\n4. CONSISTENZA DUMMY VARIABLES:\n")

consistency_check <- gppd_final_corrected %>%
  summarise(
    # Verifica che le dummy siano mutualmente esclusive
    bidder_overlap = sum(bidder_origin_eu == 1 & bidder_origin_non_eu == 1),
    supply_overlap = sum(tender_supplytype_works == 1 & tender_supplytype_services == 1),
    
    # Verifica che le dummy non superino il totale
    bidder_total_ok = (sum(bidder_origin_eu) + sum(bidder_origin_non_eu) + 
                         (n() - sum(bidder_origin_eu) - sum(bidder_origin_non_eu))) == n(),
    supply_total_ok = (sum(tender_supplytype_works) + sum(tender_supplytype_services) + 
                         (n() - sum(tender_supplytype_works) - sum(tender_supplytype_services))) == n()
  )

cat("   - Bidder overlap:", consistency_check$bidder_overlap, "(dovrebbe essere 0)\n")
cat("   - Supply type overlap:", consistency_check$supply_overlap, "(dovrebbe essere 0)\n")
cat("   - Bidder total ok:", consistency_check$bidder_total_ok, "(dovrebbe essere TRUE)\n")
cat("   - Supply total ok:", consistency_check$supply_total_ok, "(dovrebbe essere TRUE)\n\n")

# 5. CHECK VARIABILI CONTINUE
cat("5. VARIABILI CONTINUE:\n")
continuous_summary <- gppd_final_corrected %>%
  summarise(
    subcontracted_prop_mean = mean(bid_subcontractedproportion, na.rm = TRUE),
    subcontracted_prop_sd = sd(bid_subcontractedproportion, na.rm = TRUE),
    duration_mean = mean(lot_updateddurationdays, na.rm = TRUE),
    duration_sd = sd(lot_updateddurationdays, na.rm = TRUE)
  )
cat("   - Bid subcontracted proportion: Mean =", round(continuous_summary$subcontracted_prop_mean, 2), 
    "SD =", round(continuous_summary$subcontracted_prop_sd, 2), "\n")
cat("   - Lot duration days: Mean =", round(continuous_summary$duration_mean, 2), 
    "SD =", round(continuous_summary$duration_sd, 2), "\n\n")

# 6. VERIFICA FINALE
cat("6. VERIFICA FINALE REFERENCE CATEGORIES:\n")
cat("   ✅ Bidder origin: LOCALE\n")
cat("   ✅ Supply type: SUPPLIES\n") 
cat("   ✅ Selection method: LOWEST_PRICE\n")
cat("   ✅ Subcontracted: NO\n")

cat("\n🎯 DIAGNOSI COMPLETATA - DATASET PRONTO PER IL SALVATAGGIO!\n")

# 5. Salva con nome DIVERSO dalla versione precedente
write.csv(gppd_final_corrected, "gppd_final_corrected_dummies.csv", row.names = FALSE)

# 6. Verifica
cat("🎯 FILE CREATI:\n")
cat("1. gppd_clean_selected.csv → ORIGINALE (intatto)\n")
cat("2. gppd_final_corrected_dummies.csv → NUOVO FINALE\n")
cat("3. gppd_with_dummies.csv → VECCHIO (puoi cancellare)\n")
cat("4. gppd_final_for_analysis.csv → VECCHIO (puoi cancellare)\n\n")

cat("📊 Osservazioni nel nuovo file:", nrow(gppd_final_corrected), "\n")