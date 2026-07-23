# =========================================================
# CREAZIONE DATASET MACRO CON PRIMARY EXPENDITURE + CONTROLLI DINAMICI
# =========================================================

# 0) Setup ambiente ------------------------------------------------------

rm(list = ls())
gc()

library(tidyverse)
library(fixest)

# Imposta la directory di lavoro (modifica il percorso se necessario)
setwd("~/Desktop/Tesina IV /Data analysis /Downloaded data")

# 1) Carica i dataset principali ----------------------------------------

macro <- read_csv("macro_analysis_dataset_2011_2021.csv") %>%
  mutate(
    buyer_country = as.character(buyer_country),
    tender_year = as.numeric(tender_year)
  )

exp_primary <- read_csv("eurostat_primary_expenditure_real.csv") %>%
  mutate(
    geo = as.character(geo),
    year = as.numeric(year)
  )

# 2) Merge coerente con variabile dipendente primary_growth_real ---------

macro_primary <- macro %>%
  left_join(
    exp_primary %>% select(geo, year, primary_growth_real),
    by = c("buyer_country" = "geo", "tender_year" = "year")
  ) %>%
  select(-total_expenditure_growth_real)   # rimuove la vecchia dipendente

# 3) Crea controlli dinamici (unemp_growth, pop_growth) ------------------

macro_primary <- macro_primary %>%
  group_by(buyer_country) %>%
  arrange(tender_year, .by_group = TRUE) %>%
  mutate(
    # Crescita tasso di disoccupazione (%)
    unemp_growth = ifelse(
      !is.na(lag(unemp_rate)) & lag(unemp_rate) > 0,
      100 * (unemp_rate / lag(unemp_rate) - 1),
      NA_real_
    ),
    # Crescita densità di popolazione (%)
    pop_growth = ifelse(
      !is.na(lag(pop_density)) & lag(pop_density) > 0,
      100 * (pop_density / lag(pop_density) - 1),
      NA_real_
    )
  ) %>%
  ungroup()

# 4) Diagnostica base ---------------------------------------------------

cat("\nDIAGNOSTICA: Variabili di crescita\n")

diagnostics <- macro_primary %>%
  summarise(
    mean_unemp_growth = mean(unemp_growth, na.rm = TRUE),
    sd_unemp_growth = sd(unemp_growth, na.rm = TRUE),
    mean_pop_growth = mean(pop_growth, na.rm = TRUE),
    sd_pop_growth = sd(pop_growth, na.rm = TRUE)
  )

print(round(diagnostics, 3))

cat("\nRange unemp_growth:\n")
print(summary(macro_primary$unemp_growth))

cat("\nRange pop_growth:\n")
print(summary(macro_primary$pop_growth))

# 5) Check coerenza temporale e variabile dipendente --------------------

cat("\nPeriodo:",
    min(macro_primary$tender_year, na.rm = TRUE), "-",
    max(macro_primary$tender_year, na.rm = TRUE), "\n")

cat("Primary growth real disponibile:",
    sum(!is.na(macro_primary$primary_growth_real)), "/",
    nrow(macro_primary), "osservazioni\n")

# 6) Salvataggio finale -------------------------------------------------

write_csv(macro_primary, "macro_primary_expenditure_with_growth_controls.csv")

cat("\nFile salvato: macro_primary_expenditure_with_growth_controls.csv\n")

# =========================================================
# DIAGNOSTICA VARIABILI INDIPENDENTI E DUMMIES
# =========================================================

macro_primary <- read_csv("macro_primary_expenditure_with_growth_controls.csv")

cat("\n=============================================================\n")
cat("DIAGNOSTICA VARIABILI INDIPENDENTI (OUTSOURCING + CONTROLLI)\n")
cat("=============================================================\n\n")

# 1) Lista completa delle variabili presenti
cat("Variabili presenti nel dataset:\n")
print(names(macro_primary))

# 2) Verifica dummies e variabili chiave
cat("\n\n--- DUMMY / PROXY DI OUTSOURCING ---\n")

outsourcing_vars <- c(
  "prop_services", "prop_works",
  "prop_meat", "prop_subcontracted",
  "prop_eu_bidders", "prop_non_eu_bidders"
)

# Seleziona solo quelle effettivamente presenti
outsourcing_vars <- outsourcing_vars[outsourcing_vars %in% names(macro_primary)]

summary_outsourcing <- macro_primary %>%
  summarise(across(all_of(outsourcing_vars), list(mean = mean, sd = sd, min = min, max = max), na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value")

print(summary_outsourcing)

# 3) Reference categories note dal dataset originale
cat("\nREFERENCE CATEGORIES (definite in fase di creazione):\n")
cat("-------------------------------------------------------------\n")
cat("Supply type:     Reference = SUPPLIES (dummy: prop_services, prop_works)\n")
cat("Selection method: Reference = LOWEST_PRICE (dummy: prop_meat)\n")
cat("Subcontracted:   Reference = NO (dummy: prop_subcontracted = 1 se subappaltato)\n")
cat("Bidder origin:   Reference = LOCALE (dummy: prop_eu_bidders, prop_non_eu_bidders)\n")

# 4) Check range e coerenza valori percentuali (0–1)
cat("\n\n--- RANGE CHECK SU DUMMIES / PROPORZIONI ---\n")
for (v in outsourcing_vars) {
  rng <- range(macro_primary[[v]], na.rm = TRUE)
  cat(sprintf("%s: range %.3f – %.3f\n", v, rng[1], rng[2]))
}

# 5) Statistiche sintetiche sui controlli macro
cat("\n\n--- CONTROLLI MACRO ---\n")
macro_controls <- c(
  "unemp_rate", "unemp_growth",
  "pop_density", "pop_growth",
  "gdp_growth", "gov_debt_pct_gdp",
  "corruption_control", "gov_state_market"
)
macro_controls <- macro_controls[macro_controls %in% names(macro_primary)]

summary_controls <- macro_primary %>%
  summarise(across(all_of(macro_controls), list(mean = mean, sd = sd, min = min, max = max), na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value")

print(summary_controls)

cat("\n\n--- CONFERMA STRUTTURA ---\n")
cat("Osservazioni:", nrow(macro_primary), "\n")
cat("Paesi:", n_distinct(macro_primary$buyer_country), "\n")
cat("Anni:", paste(range(macro_primary$tender_year), collapse = " – "), "\n")

# Introduzione variabile indipendente 

df <- read_csv("macro_primary_expenditure_with_growth_controls.csv")
nrow(df); length(unique(df$buyer_country)); range(df$tender_year)

# Analisi variabile principale 

library(tidyverse)

# Distribuzione generale
summary(df$avg_contract_price)
quantile(df$avg_contract_price, c(0.01, 0.05, 0.5, 0.95, 0.99), na.rm = TRUE)

# Istogramma generale
ggplot(df, aes(x = avg_contract_price)) +
  geom_histogram(bins = 40, fill = "#4E79A7", color = "white") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Distribuzione del valore medio dei contratti (avg_contract_price)",
       x = "Valore medio per paese-anno (PPP, €)",
       y = "Frequenza")

# Boxplot per Paese
ggplot(df, aes(x = reorder(buyer_country, avg_contract_price, na.rm = TRUE, median),
               y = avg_contract_price)) +
  geom_boxplot(outlier.shape = 21, fill = "#A0CBE8") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  labs(title = "Distribuzione del valore medio dei contratti per Paese",
       x = "Paese", y = "Valore medio contratto (PPP, €)")

# Evoluzione temporale
ggplot(df, aes(x = tender_year, y = avg_contract_price, group = buyer_country)) +
  geom_line(alpha = 0.4) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Andamento del valore medio dei contratti nel tempo",
       x = "Anno", y = "Valore medio contratto (PPP, €)")

micro <- read_csv("FINAL_ANALYSIS_DATASET_2006_2021.csv") %>%
  filter(!is.na(tender_digiwhist_price)) %>%
  select(buyer_country, tender_year, tender_digiwhist_price)

summary(micro$tender_digiwhist_price)
quantile(micro$tender_digiwhist_price, c(0.01, 0.05, 0.5, 0.95, 0.99), na.rm = TRUE)

micro_country <- micro %>%
  group_by(buyer_country) %>%
  summarise(
    mean_price = mean(tender_digiwhist_price, na.rm = TRUE),
    median_price = median(tender_digiwhist_price, na.rm = TRUE),
    n_contracts = n()
  ) %>%
  arrange(mean_price)

print(micro_country, n = 10)

micro %>%
  group_by(buyer_country, tender_year) %>%
  summarise(contracts = n()) %>%
  summarise(min_n = min(contracts), max_n = max(contracts))

