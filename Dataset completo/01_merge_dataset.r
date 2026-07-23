# MERGE FINALE CON CONSISTENZA TEMPORALE

setwd("/Users/sofiaperondi/Desktop/Tesina IV /Data selection and summary analysis /Downloaded data")

# MERGE FINALE CON VARIABILE DIPENDENTE CORRETTA: total_expenditure_growth_real
library(tidyverse)

# Carica dataset
dependent <- read_csv("eurostat_data_real_with_growth_CORRECTED.csv")
independent <- read_csv("gppd_final_corrected_dummies.csv")
controls <- read_csv("all_controls_complete_2006_2021.csv")

# 1. Filtra anni indipendente (2006-2021)
independent_filtered <- independent %>%
  filter(tender_year >= 2006 & tender_year <= 2021)

cat("📅 ALLINEAMENTO TEMPORALE:\n")
cat("Gare dopo filtro anni:", nrow(independent_filtered), "/", nrow(independent), 
    "(", round(nrow(independent_filtered)/nrow(independent)*100, 1), "% mantenute)\n")

# 2. Merge finale
final_dataset <- independent_filtered %>%
  left_join(dependent, by = c("buyer_country" = "geo", "tender_year" = "year")) %>%
  left_join(controls, by = c("buyer_country" = "geo", "tender_year" = "year"))

# 3. Diagnosi finale con variabile dipendente CORRETTA
cat("\n🎯 DIAGNOSI FINALE MERGE:\n")
cat("Osservazioni finali:", nrow(final_dataset), "\n")
cat("Variabili finali:", ncol(final_dataset), "\n")
cat("📊 VARIABILE DIPENDENTE: total_expenditure_growth_real\n")

merge_missing <- final_dataset %>%
  summarise(
    missing_dependent = sum(is.na(total_expenditure_growth_real)),
    missing_controls = sum(is.na(gdp_growth)),
    total_obs = n()
  )

cat("\n📊 MISSING VALUES NEL MERGE:\n")
cat("Crescita spesa pubblica (total_expenditure_growth_real) missing:", merge_missing$missing_dependent, 
    "(", round(merge_missing$missing_dependent/merge_missing$total_obs*100, 1), "%)\n")
cat("Controlli (gdp_growth) missing:", merge_missing$missing_controls, 
    "(", round(merge_missing$missing_controls/merge_missing$total_obs*100, 1), "%)\n")

# 4. Statistiche variabile dipendente
dep_var_stats <- final_dataset %>%
  filter(!is.na(total_expenditure_growth_real)) %>%
  summarise(
    n_obs = n(),
    mean_growth = mean(total_expenditure_growth_real, na.rm = TRUE),
    sd_growth = sd(total_expenditure_growth_real, na.rm = TRUE),
    min_growth = min(total_expenditure_growth_real, na.rm = TRUE),
    max_growth = max(total_expenditure_growth_real, na.rm = TRUE)
  )

cat("\n📈 STATISTICHE VARIABILE DIPENDENTE:\n")
cat("Osservazioni con crescita spesa:", dep_var_stats$n_obs, "\n")
cat("Crescita media:", round(dep_var_stats$mean_growth, 2), "%\n")
cat("Deviazione standard:", round(dep_var_stats$sd_growth, 2), "%\n")
cat("Range:", round(dep_var_stats$min_growth, 2), "% a", round(dep_var_stats$max_growth, 2), "%\n")

# 5. Salva dataset finale
write_csv(final_dataset, "FINAL_ANALYSIS_DATASET_2006_2021.csv")

cat("\n💾 DATASET FINALE SALVATO: FINAL_ANALYSIS_DATASET_2006_2021.csv\n")