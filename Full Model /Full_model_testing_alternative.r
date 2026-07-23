

# =========================================================
# TWFE MODELLO FINALE — Log Primary Expenditure ~ Outsourcing + Controls + log(GDP_real)
# =========================================================

# 0️⃣ Setup ambiente -----------------------------------------------------
rm(list = ls())
gc()

library(tidyverse)
library(fixest)
library(modelsummary)

# Imposta la directory dove hai i dati
setwd("~/Desktop/Tesina IV /Data analysis /Downloaded data")

# =========================================================
# 1️⃣ Caricamento dati macro e spesa pubblica reale
# =========================================================

# Macro dataset (contiene controlli e avg_contract_price)
df <- read_csv("macro_primary_expenditure_with_growth_controls.csv")

# Dataset Eurostat (per aggiungere i livelli reali della spesa e deflatore)
expprim <- read_csv("eurostat_primary_expenditure_real.csv") %>%
  select(geo, year, primary_real, gdp_deflator)

# Dataset micro (per PIL nominale)
final <- read_csv("FINAL_ANALYSIS_DATASET_2006_2021.csv") %>%
  select(buyer_country, tender_year, gdp_nominal) %>%
  distinct()

# =========================================================
# 2️⃣ Merge coerente per avere tutti i denominatori
# =========================================================

df <- df %>%
  left_join(expprim, by = c("buyer_country" = "geo", "tender_year" = "year")) %>%
  left_join(final, by = c("buyer_country", "tender_year")) %>%
  mutate(
    gdp_real = if_else(!is.na(gdp_nominal) & !is.na(gdp_deflator) & gdp_deflator > 0,
                       gdp_nominal / (gdp_deflator / 100), NA_real_)
  )

# =========================================================
# 3️⃣ Costruzione variabili logaritmiche e indicatore di outsourcing
# =========================================================

df <- df %>%
  filter(!is.na(primary_real), primary_real > 0,
         !is.na(avg_contract_price), avg_contract_price > 0,
         !is.na(unemp_rate), unemp_rate > 0,
         !is.na(pop_density), pop_density > 0,
         !is.na(gov_debt_pct_gdp), gov_debt_pct_gdp > 0,
         !is.na(gdp_real), gdp_real > 0) %>%
  mutate(
    # Variabile dipendente
    ln_spesa = log(primary_real),
    
    # Indicatore di outsourcing
    ln_valmean = log(avg_contract_price),
    
    # Controlli in log
    ln_unemp_rate = log(unemp_rate),
    ln_pop_density = log(pop_density),
    ln_gov_debt = log(gov_debt_pct_gdp),
    ln_gdp_real = log(gdp_real)
  ) %>%
  group_by(buyer_country) %>%
  mutate(
    X = ln_valmean - mean(ln_valmean, na.rm = TRUE)   # deviazione log rispetto alla media del paese
  ) %>%
  ungroup()

# =========================================================
# 4️⃣ Stima TWFE (Fixed Effects: paese + anno, SE clusterizzati per paese)
# =========================================================

m_twfe <- feols(
  ln_spesa ~ X +
    ln_unemp_rate + ln_pop_density + ln_gov_debt +
    corruption_control + gov_state_market + ln_gdp_real |
    buyer_country + tender_year,
  data = df,
  cluster = ~ buyer_country
)

# =========================================================
# 5️⃣ Output e interpretazione
# =========================================================

etable(
  m_twfe,
  se = "cluster",
  cluster = "buyer_country",
  dict = c(
    "ln_spesa" = "log(Primary Expenditure, real)",
    "X" = "Log-diff(mean contract value)",
    "ln_unemp_rate" = "log(Unemployment rate)",
    "ln_pop_density" = "log(Population density)",
    "ln_gov_debt" = "log(Gov. debt % GDP)",
    "corruption_control" = "Corruption control index",
    "gov_state_market" = "Gov. state–market index",
    "ln_gdp_real" = "log(Real GDP)"
  ),
  fitstat = c("n", "r2")
)

# =========================================================
# 6️⃣ Esporta tabella per la tesina
# =========================================================

msummary(
  list("TWFE - final model with log(GDP_real)" = m_twfe),
  stars = TRUE,
  gof_omit = "IC|Log|Adj|AIC|BIC",
  output = "TWFE_final_model_with_logGDP.html"
)

cat("\n✅ Modello TWFE stimato e salvato come 'TWFE_final_model_with_logGDP.html'\n")
cat("   Effetti fissi: paese + anno; SE clusterizzati per buyer_country.\n")

m_twfe_rec <- feols(
  ln_spesa ~ X +
    ln_unemp_rate + ln_pop_density + ln_gov_debt +
    corruption_control + gov_state_market +
    ln_gdp_real + recession |
    buyer_country + tender_year,
  data = df,
  cluster = ~ buyer_country
)

etable(
  m_twfe_rec,
  se = "cluster",
  cluster = "buyer_country",
  dict = c(
    "ln_spesa" = "log(Primary Expenditure, real)",
    "X" = "Log-diff(mean contract value)",
    "ln_unemp_rate" = "log(Unemployment rate)",
    "ln_pop_density" = "log(Population density)",
    "ln_gov_debt" = "log(Gov. debt % GDP)",
    "corruption_control" = "Corruption control index",
    "gov_state_market" = "Gov. state–market index",
    "ln_gdp_real" = "log(Real GDP)",
    "recession" = "Recession dummy"
  ),
  fitstat = c("n", "r2")
)

# Modello preferito (con log GDP)
m_with_gdp <- feols(
  ln_spesa ~ X +
    ln_unemp_rate + ln_pop_density + ln_gov_debt +
    corruption_control + gov_state_market + ln_gdp_real |
    buyer_country + tender_year,
  data = df, cluster = ~ buyer_country
)

# Modello di confronto (senza log GDP)
m_no_gdp <- feols(
  ln_spesa ~ X +
    ln_unemp_rate + ln_pop_density + ln_gov_debt +
    corruption_control + gov_state_market |
    buyer_country + tender_year,
  data = df, cluster = ~ buyer_country
)

etable(
  list("With log(GDP_real)" = m_with_gdp, "Without log(GDP_real)" = m_no_gdp),
  se = "cluster", cluster = "buyer_country",
  dict = c("X"="Log-diff(mean contract value)"),
  fitstat = c("n","r2")
)

library(car)

# Modello OLS semplificato (solo regressori, senza FE)
m_ols <- lm(
  ln_spesa ~ X + ln_unemp_rate + ln_pop_density + ln_gov_debt +
    corruption_control + gov_state_market + recession,
  data = df
)

summary(m_ols)

vif(m_ols)

# Install 'car' if needed
# install.packages("car")

library(car)

# Ricrea la formula senza FE (per ottenere la matrice dei regressori)
form_noFE <- as.formula("ln_spesa ~ X + ln_unemp_rate + ln_pop_density +
                        ln_gov_debt + corruption_control + gov_state_market + ln_gdp_real")

# Refit senza FE per VIF (solo diagnostica, non per interpretazione)
m_noFE <- lm(form_noFE, data = df)

# Calculate VIF
vif_values <- vif(m_noFE)
print(round(vif_values, 3))

# Optional: interpret quickly
cat("\nInterpretation guide:\n",
    "VIF < 5  → very safe\n",
    "5 ≤ VIF < 10 → moderate collinearity (keep an eye)\n",
    "VIF ≥ 10 → problematic (consider removing one variable)\n")

# A) Pooled OLS (no FE)
m_pool <- feols(
  ln_spesa ~ X + ln_unemp_rate + ln_pop_density + ln_gov_debt +
    corruption_control + gov_state_market + ln_gdp_real,
  data = df, cluster = ~buyer_country
)

# B) Country FE only
m_country <- feols(
  ln_spesa ~ X + ln_unemp_rate + ln_pop_density + ln_gov_debt +
    corruption_control + gov_state_market + ln_gdp_real | buyer_country,
  data = df, cluster = ~buyer_country
)

# C) Year FE only
m_year <- feols(
  ln_spesa ~ X + ln_unemp_rate + ln_pop_density + ln_gov_debt +
    corruption_control + gov_state_market + ln_gdp_real | tender_year,
  data = df, cluster = ~buyer_country
)

# D) Two-way FE (your main model)
m_twfe <- feols(
  ln_spesa ~ X + ln_unemp_rate + ln_pop_density + ln_gov_debt +
    corruption_control + gov_state_market + ln_gdp_real |
    buyer_country + tender_year,
  data = df, cluster = ~buyer_country
)

# Compare results side by side
etable(
  list("Pooled OLS" = m_pool,
       "Country FE" = m_country,
       "Year FE" = m_year,
       "Two-way FE" = m_twfe),
  se = "cluster", cluster = "buyer_country",
  fitstat = ~ n + r2 
)

library(plm)
pdata <- pdata.frame(df, index = c("buyer_country", "tender_year"))

summary(pdata$ln_spesa)
pvar <- plm::pvar(pdata$ln_spesa)
pvar

library(plm)
library(dplyr)

# Assicura che le colonne abbiano i nomi giusti
names(df)

# Definisci il dataset come panel
pdata <- pdata.frame(df, index = c("buyer_country", "tender_year"))

# Lista delle variabili che vuoi analizzare
vars <- c("ln_spesa", "X", "ln_unemp_rate", "ln_pop_density",
          "ln_gov_debt", "corruption_control",
          "gov_state_market", "ln_gdp_real")

# Funzione per calcolare varianza within e between
get_var_components <- function(varname) {
  # Estrai la variabile come vettore numerico
  v <- pdata[[varname]]
  
  # Calcola le varianze between e within manualmente
  mean_overall <- mean(v, na.rm = TRUE)
  
  # Between variance: varianza delle medie di ciascun paese
  means_country <- tapply(v, pdata$buyer_country, mean, na.rm = TRUE)
  between_var <- var(means_country, na.rm = TRUE)
  
  # Within variance: media della varianza interna a ciascun paese
  within_var <- mean(tapply(v, pdata$buyer_country, var, na.rm = TRUE), na.rm = TRUE)
  
  total_var <- between_var + within_var
  
  tibble(
    variable = varname,
    total_var = total_var,
    between_var = between_var,
    within_var = within_var,
    between_share = between_var / total_var,
    within_share = within_var / total_var
  )
}

# Applica la funzione a tutte le variabili
var_summary <- bind_rows(lapply(vars, get_var_components))

# Ordina per quota within crescente
var_summary <- var_summary %>%
  arrange(within_share)

print(var_summary, n = Inf)

library(dplyr)

# Calcolo covarianza within per ciascun paese
cov_within_country <- df %>%
  group_by(buyer_country) %>%
  summarise(
    cov_within = cov(ln_spesa, X, use = "pairwise.complete.obs"),
    cor_within = cor(ln_spesa, X, use = "pairwise.complete.obs"),
    .groups = "drop"
  )

# Media (ponderata) delle covarianze across countries
cov_overall <- mean(cov_within_country$cov_within, na.rm = TRUE)
cor_overall <- mean(cov_within_country$cor_within, na.rm = TRUE)

cat("Average within-country covariance (ln_spesa, X):", round(cov_overall, 5), "\n")
cat("Average within-country correlation (ln_spesa, X):", round(cor_overall, 3), "\n")

# Distribuzione per paese
print(cov_within_country)

# Carichiamo gdp_growth dal file dei controlli completi
ctrl_gdp <- read_csv("all_controls_complete_2006_2021.csv", show_col_types = FALSE) %>%
  transmute(
    buyer_country = geo,
    tender_year   = year,
    gdp_growth    = gdp_growth
  ) %>%
  filter(tender_year >= 2011, tender_year <= 2021)

# Join con il dataset principale
df <- df %>%
  left_join(ctrl_gdp, by = c("buyer_country", "tender_year"))

# Controllo rapido
sum(is.na(df$gdp_growth))

controls_growth <- ~ unemp_growth +
  pop_growth +
  gdp_growth +
  gov_debt_pct_gdp +
  corruption_control +
  gov_state_market

fe_spec <- ~ buyer_country + tender_year

fml_growth <- as.formula(paste(
  "ln_spesa ~ X +",
  paste(all.vars(controls_growth), collapse = " + "),
  "| buyer_country + tender_year"
))

m_twfe_growth <- feols(
  fml_growth,
  data = df,
  cluster = ~ buyer_country
)

etable(m_twfe_growth, se = "cluster", cluster = "buyer_country")

