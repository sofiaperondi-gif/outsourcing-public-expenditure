
############################################################
# 0️⃣ SETUP AMBIENTE
############################################################

rm(list = ls())
gc()

library(tidyverse)
library(psych)        # per skewness/kurtosis (se vuoi usarlo in alternativa a moments)
library(moments)      # skewness / kurtosis
library(plm)          # panel data + variance decomposition
library(ggplot2)
library(janitor)
library(patchwork)    # per combinare grafici

############################################################
# 1️⃣ CARICAMENTO DATI
############################################################

setwd("~/Desktop/Tesina IV /Data analysis /Downloaded data")

# Macro dataset (controlli + avg_contract_price + growth)
df <- read_csv("macro_primary_expenditure_with_growth_controls.csv")

# Eurostat real expenditure + deflator
expprim <- read_csv("eurostat_primary_expenditure_real.csv") %>%
  select(geo, year, primary_real, gdp_deflator)

# Micro dataset completo (GPPD arricchito)
final_raw <- read_csv("FINAL_ANALYSIS_DATASET_2006_2021.csv")


############################################################
# 2️⃣ PULIZIA MICRODATI GPPD E OUTLIER IT–2016
############################################################

# 2.1 — Rimuovo l’osservazione IT–2016 dal micro-dataset
final_raw_clean <- final_raw %>%
  filter(!(buyer_country == "IT" & tender_year == 2016))

# 2.2 — Skewness & kurtosis sui valori di contratto (ripuliti da IT–2016)
contracts_clean <- final_raw_clean %>%
  filter(!is.na(tender_digiwhist_price),
         tender_digiwhist_price > 0) %>%
  pull(tender_digiwhist_price)

# Distribuzione grezza
skew_raw  <- skewness(contracts_clean)
kurt_raw  <- kurtosis(contracts_clean)
kurt_excess <- kurt_raw - 3

# Distribuzione in log
contracts_log <- log(contracts_clean)
skew_log  <- skewness(contracts_log)
kurt_log  <- kurtosis(contracts_log)
kurt_log_excess <- kurt_log - 3

cat("\n=== Skewness & Kurtosis (IT–2016 removed) ===\n")
cat("Skewness (raw):         ", round(skew_raw, 3), "\n")
cat("Kurtosis (raw):         ", round(kurt_raw, 3), "\n")
cat("Excess Kurtosis (raw):  ", round(kurt_excess, 3), "\n\n")
cat("Skewness (log):         ", round(skew_log, 3), "\n")
cat("Kurtosis (log):         ", round(kurt_log, 3), "\n")
cat("Excess Kurtosis (log):  ", round(kurt_log_excess, 3), "\n")
cat("===========================================\n\n")


############################################################
# 3️⃣ MERGE MACRO + EUROSTAT + REAL GDP
############################################################

# 3.1 — Merge df + expprim (aggiungo primary_real e gdp_deflator)
df <- df %>%
  left_join(
    expprim,
    by = c("buyer_country" = "geo",
           "tender_year"   = "year"),
    suffix = c("", "_exp")
  )

# Se il join ha creato primary_real_exp (per conflitto), la rinomino in primary_real
if (!"primary_real" %in% names(df) && "primary_real_exp" %in% names(df)) {
  df <- df %>% rename(primary_real = primary_real_exp)
}
if (!"gdp_deflator" %in% names(df) && "gdp_deflator_exp" %in% names(df)) {
  df <- df %>% rename(gdp_deflator = gdp_deflator_exp)
}

# 3.2 — Costruisco GDP reale a livello country–year dai microdati puliti
gdp_country <- final_raw_clean %>%
  filter(!is.na(gdp_real)) %>%
  group_by(buyer_country, tender_year) %>%
  summarise(gdp_real = mean(gdp_real, na.rm = TRUE),
            .groups = "drop")

# 3.3 — Merge macro + real GDP
df <- df %>%
  left_join(gdp_country, by = c("buyer_country", "tender_year"))

# 3.4 — Filtro osservazioni valide e costruisco log-variabili
df <- df %>%
  # rimuovo IT–2016 anche qui, per coerenza
  filter(!(buyer_country == "IT" & tender_year == 2016)) %>%
  filter(
    !is.na(primary_real), primary_real > 0,
    !is.na(avg_contract_price), avg_contract_price > 0,
    !is.na(unemp_rate), unemp_rate > 0,
    !is.na(pop_density), pop_density > 0,
    !is.na(gov_debt_pct_gdp), gov_debt_pct_gdp > 0,
    !is.na(gdp_real), gdp_real > 0
  ) %>%
  mutate(
    ln_spesa       = log(primary_real),
    ln_valmean     = log(avg_contract_price),
    ln_unemp_rate  = log(unemp_rate),
    ln_pop_density = log(pop_density),
    ln_gov_debt    = log(gov_debt_pct_gdp),
    ln_gdp_real    = log(gdp_real)
  ) %>%
  group_by(buyer_country) %>%
  mutate(
    X = ln_valmean - mean(ln_valmean, na.rm = TRUE)   # demeaned outsourcing
  ) %>%
  ungroup()

df <- df %>%
  filter(tender_year >= 2011,
         tender_year <= 2021)


############################################################
# 4️⃣ DATASET A LIVELLO COUNTRY–YEAR
############################################################

df_country <- df %>%
  group_by(buyer_country, tender_year) %>%
  summarise(
    ln_spesa       = mean(ln_spesa, na.rm = TRUE),
    ln_valmean     = mean(ln_valmean, na.rm = TRUE),
    ln_unemp_rate  = mean(ln_unemp_rate, na.rm = TRUE),
    ln_pop_density = mean(ln_pop_density, na.rm = TRUE),
    ln_gov_debt    = mean(ln_gov_debt, na.rm = TRUE),
    corruption_control = mean(corruption_control, na.rm = TRUE),
    gov_state_market   = mean(gov_state_market, na.rm = TRUE),
    recession          = mean(recession, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(buyer_country) %>%
  mutate(
    X = ln_valmean - mean(ln_valmean, na.rm = TRUE)   # demeaned a questo livello
  ) %>%
  ungroup()

cat("Dimensioni df_country (panel):\n")
print(dim(df_country))
cat("Paesi unici:", length(unique(df_country$buyer_country)), "\n")
cat("Anni unici:", length(unique(df_country$tender_year)), "\n\n")


############################################################
# 5️⃣ SUMMARY STATISTICS (Sezione 3.3.1)
############################################################

vars_baseline <- df_country %>%
  select(
    ln_spesa, ln_valmean, X,
    ln_unemp_rate, ln_pop_density, ln_gov_debt,
    corruption_control, gov_state_market, recession
  )

summary_stats <- vars_baseline %>%
  summarise(across(
    everything(),
    list(
      mean   = ~ mean(.x, na.rm = TRUE),
      median = ~ median(.x, na.rm = TRUE),
      sd     = ~ sd(.x, na.rm = TRUE),
      min    = ~ min(.x, na.rm = TRUE),
      max    = ~ max(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(everything(), names_to = "name", values_to = "value") %>%
  separate(name, into = c("variable", "stat"), sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = stat, values_from = value)

cat("===== Summary statistics (country–year) =====\n")
print(summary_stats)
cat("\n")

# Parto dal df_country che hai già costruito:
# buyer_country, tender_year, ln_spesa, ln_valmean, ..., X

df_country <- df_country %>%
  group_by(buyer_country) %>%
  mutate(
    ln_spesa_dm = ln_spesa - mean(ln_spesa, na.rm = TRUE)  # Y demeaned
    # X è già demeaned by construction: ln_valmean - mean(ln_valmean)
  ) %>%
  ungroup()

vars_baseline <- df_country %>%
  select(
    ln_spesa_dm,      # nuova Y demeaned
    ln_valmean,       # livello log del valore medio (se vuoi tenerla)
    X,                # outsourcing demeaned
    ln_unemp_rate,
    ln_pop_density,
    ln_gov_debt,
    corruption_control,
    gov_state_market,
    recession
  )

summary_stats_dm <- vars_baseline %>%
  summarise(across(
    everything(),
    list(
      mean   = ~ mean(.x, na.rm = TRUE),
      median = ~ median(.x, na.rm = TRUE),
      sd     = ~ sd(.x, na.rm = TRUE),
      min    = ~ min(.x, na.rm = TRUE),
      max    = ~ max(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(everything(), names_to = "name", values_to = "value") %>%
  separate(name, into = c("variable", "stat"), sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = stat, values_from = value)

print(summary_stats_dm)

############################################################
# 1. Costruiamo Y demeaned = ln_spesa_dm
############################################################

df_country <- df_country %>%
  group_by(buyer_country) %>%
  mutate(
    ln_spesa_dm = ln_spesa - mean(ln_spesa, na.rm = TRUE)   # demeaned dependent
  ) %>%
  ungroup()

############################################################
# 2. Variance decomposition (between/within)
############################################################

library(plm)
pdata <- pdata.frame(df_country, index = c("buyer_country", "tender_year"))

# Lista variabili da decomporre (aggiornata!)
vars <- c(
  "ln_spesa_dm",      # NEW demeaned dependent
  "X",                # demeaned outsourcing
  "ln_unemp_rate",
  "ln_pop_density",
  "ln_gov_debt",
  "corruption_control",
  "gov_state_market",
  "recession"
)

var_decomp <- function(vname) {
  
  v <- pdata[[vname]]
  
  # between: varianza delle medie paese
  means_cty <- tapply(v, pdata$buyer_country, mean, na.rm = TRUE)
  var_between <- var(means_cty, na.rm = TRUE)
  
  # within: media delle varianze intra-paese
  var_within <- mean(
    tapply(v, pdata$buyer_country, var, na.rm = TRUE),
    na.rm = TRUE
  )
  
  tibble(
    variable         = vname,
    total_variance   = var_between + var_within,
    between_variance = var_between,
    within_variance  = var_within,
    between_share    = var_between / (var_between + var_within),
    within_share     = var_within  / (var_between + var_within)
  )
}

variance_table <- bind_rows(lapply(vars, var_decomp)) %>%
  arrange(desc(within_share))

print(variance_table)




############################################################
# 7️⃣ TEMPORAL PATTERNS (Sezione 3.3.3 – versione corretta con ln_spesa_dm)
############################################################

temporal_df <- df_country %>%
  group_by(tender_year) %>%
  summarise(
    ln_spesa_dm_mean = mean(ln_spesa_dm, na.rm = TRUE),
    ln_spesa_dm_sd   = sd(ln_spesa_dm, na.rm = TRUE),
    X_mean           = mean(X, na.rm = TRUE),
    X_sd             = sd(X, na.rm = TRUE),
    n_countries      = n(),
    .groups = "drop"
  ) %>%
  arrange(tender_year)

cat("===== Temporal patterns (EU-27 averages, demeaned) =====\n")
print(temporal_df)
cat("\n")

# Δ anno su anno (solo per commento testuale)
temporal_df <- temporal_df %>%
  mutate(
    d_ln_spesa_dm = ln_spesa_dm_mean - lag(ln_spesa_dm_mean),
    d_X           = X_mean           - lag(X_mean)
  )

# Preparazione dei dati per i grafici
temporal_long <- df_country %>%
  select(tender_year, ln_spesa_dm, X) %>%
  pivot_longer(cols = c(ln_spesa_dm, X),
               names_to = "variable",
               values_to = "value") %>%
  group_by(tender_year, variable) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

# === FIGURE A: demeaned ln(primary expenditure) ===
fig_spesa_dm <- temporal_long %>%
  filter(variable == "ln_spesa_dm") %>%
  ggplot(aes(x = tender_year, y = mean_value)) +
  geom_line(linewidth = 1.3, color = "#1B4F72") +
  geom_point(size = 3, color = "#1B4F72") +
  labs(title = "Demeaned Real Primary Expenditure (ln), EU-27 Average",
       x = "Year", y = "Mean demeaned ln(primary expenditure)") +
  theme_minimal(base_size = 14)

print(fig_spesa_dm)

# === FIGURE B: Outsourcing Intensity (X) ===
fig_outsourcing <- temporal_long %>%
  filter(variable == "X") %>%
  ggplot(aes(x = tender_year, y = mean_value)) +
  geom_line(linewidth = 1.3, color = "#6C3483") +
  geom_point(size = 3, color = "#6C3483") +
  labs(title = "Outsourcing Intensity (X), EU-27 Average",
       x = "Year", y = "Mean demeaned ln(contract value)") +
  theme_minimal(base_size = 14)

print(fig_outsourcing)

library(ggplot2)
library(dplyr)
library(tidyr)

# === Recompute temporal_long to be safe ===
temporal_long <- df_country %>%
  select(tender_year, ln_spesa_dm, X) %>%
  pivot_longer(cols = c(ln_spesa_dm, X),
               names_to = "variable",
               values_to = "value") %>%
  group_by(tender_year, variable) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

# === Tema estetico per LaTeX ===
theme_pdf <- function() {
  theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 13),
      plot.margin = unit(c(10, 10, 10, 10), "pt"),
      panel.grid.minor = element_blank()
    )
}

# ================================
# FIGURE 1 — Demeaned Expenditure
# ================================
fig_spesa <- temporal_long %>%
  filter(variable == "ln_spesa_dm") %>%
  ggplot(aes(x = tender_year, y = mean_value)) +
  geom_line(linewidth = 1.2, color = "#1B4F72") +
  geom_point(size = 3, color = "#1B4F72") +
  labs(title = "Demeaned Real Primary Expenditure (EU-27 Average)",
       x = "Year",
       y = "Mean demeaned ln(primary expenditure)") +
  theme_pdf()

ggsave("fig_spesa_dm.pdf", fig_spesa,
       width = 7, height = 4.5, device = "pdf")

print(fig_spesa)

# ================================
# FIGURE 2 — Demeaned Outsourcing X
# ================================
fig_X <- temporal_long %>%
  filter(variable == "X") %>%
  ggplot(aes(x = tender_year, y = mean_value)) +
  geom_line(linewidth = 1.2, color = "#6C3483") +
  geom_point(size = 3, color = "#6C3483") +
  labs(title = "Demeaned Outsourcing Intensity (EU-27 Average)",
       x = "Year",
       y = "Mean demeaned ln(contract value)") +
  theme_pdf()

ggsave("fig_X_dm.pdf", fig_X,
       width = 7, height = 4.5, device = "pdf")


############################################################
# 1️⃣ CLASSIFICAZIONE GEOGRAFICA (region_map)
############################################################

region_map <- tribble(
  ~buyer_country, ~region,
  "AT","Western", "BE","Western", "DE","Western", "DK","Northern",
  "ES","Southern", "FI","Northern", "FR","Western", "EL","Southern",
  "IE","Northern", "IT","Southern", "NL","Western", "PT","Southern",
  "SE","Northern","CY","Southern","CZ","Eastern","EE","Eastern",
  "HU","Eastern","LT","Eastern","LV","Eastern","PL","Eastern",
  "RO","Eastern","SI","Eastern","SK","Eastern","BG","Eastern",
  "LU","Western","MT","Southern","HR","Eastern"
)

############################################################
# 2️⃣ RICOSTRUZIONE df_country CON ln_spesa_dm (se già fatto, si può saltare)
############################################################

df_country <- df %>%
  group_by(buyer_country, tender_year) %>%
  summarise(
    ln_spesa       = mean(ln_spesa, na.rm = TRUE),
    ln_valmean     = mean(ln_valmean, na.rm = TRUE),
    ln_unemp_rate  = mean(ln_unemp_rate, na.rm = TRUE),
    ln_pop_density = mean(ln_pop_density, na.rm = TRUE),
    ln_gov_debt    = mean(ln_gov_debt, na.rm = TRUE),
    corruption_control = mean(corruption_control, na.rm = TRUE),
    gov_state_market   = mean(gov_state_market, na.rm = TRUE),
    recession          = mean(recession, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(buyer_country) %>%
  mutate(
    ln_spesa_dm = ln_spesa - mean(ln_spesa, na.rm = TRUE),
    X           = ln_valmean - mean(ln_valmean, na.rm = TRUE)
  ) %>%
  ungroup()

############################################################
# 3️⃣ MERGE df_country + region_map
############################################################

df_country_region <- df_country %>%
  left_join(region_map, by = "buyer_country")

############################################################
# 4️⃣ COSTRUZIONE DELLA TABELLA DI ETEROGENEITÀ (Y demeaned)
############################################################

cross_table_dm <- df_country_region %>%
  group_by(region, buyer_country) %>%
  summarise(
    ln_spesa_dm_mean = mean(ln_spesa_dm, na.rm = TRUE),
    ln_spesa_dm_sd   = sd(ln_spesa_dm, na.rm = TRUE),
    ln_spesa_dm_cv   = ln_spesa_dm_sd / abs(ln_spesa_dm_mean),
    
    X_mean = mean(X, na.rm = TRUE),
    X_sd   = sd(X, na.rm = TRUE),
    outsourcing_volatility = X_sd,
    
    .groups = "drop"
  ) %>%
  arrange(region, desc(ln_spesa_dm_mean))

############################################################
# 5️⃣ PULIZIA ESTETICA (per LaTeX)
############################################################

cross_table_dm_clean <- cross_table_dm %>%
  mutate(
    ln_spesa_dm_mean = round(ln_spesa_dm_mean, 3),
    ln_spesa_dm_sd   = round(ln_spesa_dm_sd, 3),
    ln_spesa_dm_cv   = round(ln_spesa_dm_cv, 3),
    X_mean           = round(X_mean, 3),
    X_sd             = round(X_sd, 3),
    outsourcing_volatility = round(outsourcing_volatility, 3),
    
    region = case_when(
      region == "Western"  ~ "Western Europe",
      region == "Northern" ~ "Northern Europe",
      region == "Southern" ~ "Southern Europe",
      region == "Eastern"  ~ "Eastern Europe",
      TRUE ~ region
    )
  ) %>%
  arrange(region, desc(ln_spesa_dm_mean))

############################################################
# 6️⃣ STAMPA TABELLA FINALE
############################################################

print(cross_table_dm_clean, n = 40)




############################################################
# 9️⃣ PANEL STRUCTURE SUMMARY (per testo in 3.3)
############################################################

panel_summary <- df_country %>%
  group_by(buyer_country) %>%
  summarise(
    n_years  = n(),
    min_year = min(tender_year),
    max_year = max(tender_year),
    .groups = "drop"
  )

cat("===== Panel structure by country =====\n")
print(panel_summary)
cat("\n")

cat("Years covered in panel:", paste(range(df_country$tender_year), collapse = "–"), "\n")
cat("Total country–years:", nrow(df_country), "\n")
cat("Countries with full 2011–2021 coverage:",
    sum(panel_summary$n_years == length(unique(df_country$tender_year))), "out of",
    length(unique(df_country$buyer_country)), "\n")

############################################################
# 1️⃣ VERIFICO CHE Y E X SIANO GIÀ COSTRUITI
############################################################

# df_country deve contenere:
# buyer_country, tender_year, ln_spesa_dm, X

stopifnot("ln_spesa_dm" %in% names(df_country))
stopifnot("X" %in% names(df_country))


############################################################
# 2️⃣ WITHIN-COUNTRY CORRELATIONS (core della sezione 3.4)
############################################################

corr_within_country <- df_country %>%
  group_by(buyer_country) %>%
  summarise(
    cor_YX = cor(ln_spesa_dm, X, use = "pairwise.complete.obs"),
    n      = sum(!is.na(ln_spesa_dm) & !is.na(X)),
    .groups = "drop"
  ) %>%
  arrange(desc(cor_YX))

cat("===== Within-country correlation between Y and X =====\n")
print(corr_within_country, n = 27)


############################################################
# 3️⃣ OPTIONAL: WITHIN-YEAR (cross-sectional) CORRELATIONS
############################################################

corr_within_year <- df_country %>%
  group_by(tender_year) %>%
  summarise(
    cor_YX = cor(ln_spesa_dm, X, use = "pairwise.complete.obs"),
    n      = sum(!is.na(ln_spesa_dm) & !is.na(X)),
    .groups = "drop"
  ) %>%
  arrange(tender_year)

cat("\n===== Within-year (cross-sectional) correlation =====\n")
print(corr_within_year, n = 27)


############################################################
# 4️⃣ TABELLA PULITA PER LaTeX (versione elegante)
############################################################

corr_country_clean <- corr_within_country %>%
  mutate(
    cor_YX = round(cor_YX, 3),
    n      = as.integer(n)
  )

cat("\n===== LaTeX-ready country correlation table =====\n")
print(corr_country_clean, n = 27)


############################################################
# 5️⃣ FIGURA (FACOLTATIVA): BARPLOT ORDINATO PER PAESE
############################################################

library(ggplot2)

fig_corr <- corr_country_clean %>%
  ggplot(aes(x = reorder(buyer_country, cor_YX), y = cor_YX)) +
  geom_col(fill = "#4A235A") +
  coord_flip() +
  labs(title = "Within-country correlation between Y and X",
       x = "Country", y = "Correlation") +
  theme_minimal(base_size = 14)

ggsave("corr_within_country.pdf", fig_corr,
       width = 6.5, height = 5, device = "pdf")

print(fig_corr, n = 27)


############################################################
# 6️⃣ PREPARAZIONE TABELLA PER LA TESI (OUTPUT CONSIGLIATO)
############################################################

corr_latex <- corr_country_clean %>%
  mutate(
    buyer_country = as.character(buyer_country)
  )

# Se vuoi anche la regione, aggiungila qui:
# corr_latex <- corr_latex %>% left_join(region_map, by = "buyer_country")

write_csv(corr_latex, "table_corr_within_country.csv")

cat("\nFile 'table_corr_within_country.csv' created.\n")

