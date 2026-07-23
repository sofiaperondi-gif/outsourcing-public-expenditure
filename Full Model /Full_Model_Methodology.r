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

library(plm)
library(lmtest)
library(sandwich)

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


############################################################
# 4️⃣ DATASET A LIVELLO COUNTRY–YEAR
###########################################################

df_country <- df %>%
  group_by(buyer_country, tender_year) %>%
  summarise(
    ln_spesa       = mean(ln_spesa, na.rm = TRUE),
    ln_valmean     = mean(ln_valmean, na.rm = TRUE),
    ln_unemp_rate  = mean(ln_unemp_rate, na.rm = TRUE),
    ln_pop_density = mean(ln_pop_density, na.rm = TRUE),
    ln_gov_debt    = mean(ln_gov_debt, na.rm = TRUE),
    ln_gdp_real    = mean(ln_gdp_real, na.rm = TRUE),
    corruption_control = mean(corruption_control, na.rm = TRUE),
    gov_state_market   = mean(gov_state_market, na.rm = TRUE),
    recession          = mean(recession, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(buyer_country) %>%
  mutate(
    X = ln_valmean - mean(ln_valmean, na.rm = TRUE),
    ln_spesa_dm = ln_spesa - mean(ln_spesa, na.rm = TRUE)
  ) %>%
  ungroup()

cat("Dimensioni df_country (panel):\n")
print(dim(df_country))
cat("Paesi unici:", length(unique(df_country$buyer_country)), "\n")
cat("Anni unici:", length(unique(df_country$tender_year)), "\n\n")

############################################################
# COSTRUZIONE DF COUNTRY NON-DEMEANED (PER HAUSMAN)
############################################################

df_country_raw <- df %>%
  group_by(buyer_country, tender_year) %>%
  summarise(
    ln_spesa       = mean(ln_spesa, na.rm = TRUE),
    ln_valmean     = mean(ln_valmean, na.rm = TRUE),
    ln_unemp_rate  = mean(ln_unemp_rate, na.rm = TRUE),
    ln_pop_density = mean(ln_pop_density, na.rm = TRUE),
    ln_gov_debt    = mean(ln_gov_debt, na.rm = TRUE),
    ln_gdp_real    = mean(ln_gdp_real, na.rm = TRUE),
    corruption_control = mean(corruption_control, na.rm = TRUE),
    gov_state_market   = mean(gov_state_market, na.rm = TRUE),
    recession          = mean(recession, na.rm = TRUE),
    .groups = "drop"
  )

# FE (two-ways) in livelli
fe_test_model <- plm(
  ln_spesa ~ ln_valmean + ln_unemp_rate + ln_pop_density +
    ln_gov_debt + corruption_control + gov_state_market + ln_gdp_real,
  data = df_country_raw,
  index = c("buyer_country", "tender_year"),
  model = "within",
  effect = "twoways"
)

# RE (two-ways)
re_test_model <- plm(
  ln_spesa ~ ln_valmean + ln_unemp_rate + ln_pop_density +
    ln_gov_debt + corruption_control + gov_state_market + ln_gdp_real,
  data = df_country_raw,
  index = c("buyer_country", "tender_year"),
  model = "random",
  effect = "twoways"
)

hausman_test <- phtest(
  plm(ln_spesa ~ ln_valmean + ln_unemp_rate + ln_pop_density +
        ln_gov_debt + corruption_control + gov_state_market + ln_gdp_real,
      data = df_country_raw,
      index = c("buyer_country", "tender_year"),
      model = "within"),
  
  plm(ln_spesa ~ ln_valmean + ln_unemp_rate + ln_pop_density +
        ln_gov_debt + corruption_control + gov_state_market + ln_gdp_real,
      data = df_country_raw,
      index = c("buyer_country", "tender_year"),
      model = "random")
)

print(hausman_test)

############################################################
# 🔎 1️⃣ F-TEST PER FIXED EFFECTS (COUNTRY)
############################################################

fe_country_test <- pFtest(
  plm(
    ln_spesa_dm ~ X + ln_unemp_rate + ln_pop_density +
      ln_gov_debt + corruption_control + gov_state_market + ln_gdp_real,
    data = df_country, index = c("buyer_country", "tender_year"),
    model = "within", effect = "individual"
  ),
  plm(
    ln_spesa_dm ~ X + ln_unemp_rate + ln_pop_density +
      ln_gov_debt + corruption_control + gov_state_market + ln_gdp_real,
    data = df_country, index = c("buyer_country", "tender_year"),
    model = "pooling"
  )
)

print(fe_country_test)



############################################################
# 🔎 2️⃣ F-TEST PER TIME FIXED EFFECTS
############################################################

fe_time_test <- pFtest(
  plm(
    ln_spesa_dm ~ X + ln_unemp_rate + ln_pop_density +
      ln_gov_debt + corruption_control + gov_state_market + ln_gdp_real,
    data = df_country, index = c("buyer_country", "tender_year"),
    model = "within", effect = "time"
  ),
  plm(
    ln_spesa_dm ~ X + ln_unemp_rate + ln_pop_density +
      ln_gov_debt + corruption_control + gov_state_market + ln_gdp_real,
    data = df_country, index = c("buyer_country", "tender_year"),
    model = "pooling"
  )
)

print(fe_time_test)



############################################################
# 🔎 3️⃣ LM TEST PER RANDOM EFFECTS (BREUSCH–PAGAN)
############################################################

bp_test <- plmtest(
  ln_spesa_dm ~ X + ln_unemp_rate + ln_pop_density +
    ln_gov_debt + corruption_control + gov_state_market + ln_gdp_real,
  data = df_country,
  index = c("buyer_country", "tender_year"),
  type = "bp"
)

print(bp_test)



############################################################
# 🔎 4️⃣ AUTOCORRELAZIONE LAG-1 DELLA DEPENDENT
############################################################

ac_y <- df_country %>%
  arrange(buyer_country, tender_year) %>%
  group_by(buyer_country) %>%
  summarise(
    ac1_ln_spesa_dm = {
      y <- ln_spesa_dm
      if (sum(!is.na(y)) >= 3) {
        stats::acf(y, lag.max = 1, plot = FALSE)$acf[2]
      } else {
        NA_real_
      }
    },
    n_obs = sum(!is.na(ln_spesa_dm)),
    .groups = "drop"
  )

print(ac_y, n = 27)
summary(ac_y$ac1_ln_spesa_dm, n = 27)

library(plm)
library(tseries)

# Variabili da testare
vars_to_test <- c("ln_spesa_dm", "X", "ln_unemp_rate", "ln_pop_density",
                  "ln_gov_debt", "corruption_control", "gov_state_market")

# Funzione per LLC, IPS e ADF-Fisher
run_stationarity_tests <- function(varname) {
  cat("\n===================================\n")
  cat("Unit root tests for:", varname, "\n")
  cat("===================================\n")
  
  series <- df_country[[varname]]
  pdata   <- pdata.frame(df_country, index = c("buyer_country", "tender_year"))
  
  print(purtest(pdata[[varname]], test = "levinlin"))
  print(purtest(pdata[[varname]], test = "ips"))
  print(purtest(pdata[[varname]], test = "madwu"))   # Fisher-ADF
}

# Loop
for (v in vars_to_test) {
  run_stationarity_tests(v)
}



############################################################
# 🔎 5️⃣ MODELLO DINAMICO (TWFE + LAG DI Y)
############################################################

df_dyn <- df_country %>%
  arrange(buyer_country, tender_year) %>%
  group_by(buyer_country) %>%
  mutate(
    ln_spesa_dm_lag1 = dplyr::lag(ln_spesa_dm, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(ln_spesa_dm_lag1))   # rimuove il primo anno di ogni paese

dyn_fe_model <- plm(
  ln_spesa_dm ~ ln_spesa_dm_lag1 + X + ln_unemp_rate + ln_pop_density +
    ln_gov_debt + corruption_control + gov_state_market + ln_gdp_real,
  data  = df_dyn,
  index = c("buyer_country", "tender_year"),
  model = "within",
  effect = "twoways"
)

summary(dyn_fe_model)

library(plm)

# Creo pdata
pdata <- pdata.frame(df_country, index = c("buyer_country", "tender_year"))

vars_for_stationarity <- c(
  "ln_spesa",          # livello originale
  "ln_valmean",
  "ln_unemp_rate",
  "ln_pop_density",
  "ln_gov_debt",
  "corruption_control",
  "gov_state_market"
)

run_unitroot_tests <- function(v) {
  cat("\n===================================\n")
  cat("Unit root tests for:", v, "\n")
  cat("===================================\n")
  
  # LLC
  print(purtest(pdata[[v]], test = "levinlin"))
  # IPS
  print(purtest(pdata[[v]], test = "ips"))
  # Fisher-ADF
  print(purtest(pdata[[v]], test = "madwu"))
}

for (v in vars_for_stationarity) {
  run_unitroot_tests(v)
}


############################################################
# RICOSTRUZIONE CORRETTA DEL DATASET COUNTRY–YEAR
############################################################

df_country <- df %>%
  group_by(buyer_country, tender_year) %>%
  summarise(
    ln_spesa       = mean(ln_spesa, na.rm = TRUE),
    ln_valmean     = mean(ln_valmean, na.rm = TRUE),
    ln_unemp_rate  = mean(ln_unemp_rate, na.rm = TRUE),
    ln_pop_density = mean(ln_pop_density, na.rm = TRUE),
    ln_gov_debt    = mean(ln_gov_debt, na.rm = TRUE),
    ln_gdp_real    = mean(ln_gdp_real, na.rm = TRUE),
    corruption_control = mean(corruption_control, na.rm = TRUE),
    gov_state_market   = mean(gov_state_market, na.rm = TRUE),
    recession          = mean(recession, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(buyer_country) %>%
  mutate(
    X = ln_valmean - mean(ln_valmean, na.rm = TRUE),
    ln_spesa_dm = ln_spesa - mean(ln_spesa, na.rm = TRUE)
  ) %>%
  ungroup()

############################################################
# 1️⃣ Costruzione del dataset dinamico
############################################################

df_dyn <- df_country %>%
  arrange(buyer_country, tender_year) %>%
  group_by(buyer_country) %>%
  mutate(
    ln_spesa_dm_lag1 = dplyr::lag(ln_spesa_dm, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(ln_spesa_dm_lag1))

############################################################
# 2️⃣ Modello TWFE dinamico
############################################################

dyn_fe_model <- plm(
  ln_spesa_dm ~ ln_spesa_dm_lag1 +
    X + ln_unemp_rate + ln_pop_density +
    ln_gov_debt + corruption_control +
    gov_state_market + ln_gdp_real,
  data  = df_dyn,
  index = c("buyer_country", "tender_year"),
  model = "within",
  effect = "twoways"
)

summary(dyn_fe_model)

library(lmtest)
library(plm)

fe_model <- plm(
  ln_spesa_dm ~ X + ln_unemp_rate + ln_pop_density +
    ln_gov_debt + corruption_control + gov_state_market + ln_gdp_real,
  data = df_country, 
  index = c("buyer_country", "tender_year"),
  model = "within", effect = "twoways"
)

# Test di eteroschedasticità (Breusch–Pagan)
bp_hetero <- bptest(fe_model)

print(bp_hetero)

library(plm)

wooldridge_test <- pbgtest(fe_model)

print(wooldridge_test)

library(plm)

cd_test <- pcdtest(fe_model, test = "cd")   # Pesaran CD test

print(cd_test)

m1 <- plm(ln_spesa_dm ~ lag(ln_spesa_dm, 1),
          data=df_country, model="within", effect="twoways")

m2 <- plm(ln_spesa_dm ~ lag(ln_spesa_dm, 1) + X,
          data=df_country, model="within", effect="twoways")

m3 <- plm(ln_spesa_dm ~ lag(ln_spesa_dm, 1) + X + ln_unemp_rate + ln_pop_density + ln_gov_debt,
          data=df_country, model="within", effect="twoways")

m4 <- plm(ln_spesa_dm ~ lag(ln_spesa_dm, 1) + X + ln_unemp_rate + ln_pop_density +
            ln_gov_debt + corruption_control + gov_state_market,
          data=df_country, model="within", effect="twoways")

summary(m1); summary(m2); summary(m3); summary(m4)

# Assicurati di avere df_country
# Controlla che esista:
ls()

# Se df_country esiste, allora crea df_dyn così:

df_dyn <- df_country %>%
  arrange(buyer_country, tender_year) %>%
  group_by(buyer_country) %>%
  mutate(
    ln_spesa_dm_lag1 = lag(ln_spesa_dm)
  ) %>%
  ungroup() %>%
  filter(!is.na(ln_spesa_dm_lag1))


library(car)
vif(lm(ln_spesa_dm ~ lag(ln_spesa_dm,1) + X + ln_unemp_rate +
         ln_pop_density + ln_gov_debt + corruption_control + gov_state_market,
       data=df_dyn))

# modello statico
static <- plm(ln_spesa_dm ~ X + ln_unemp_rate + ln_pop_density +
                ln_gov_debt + corruption_control + gov_state_market,
              data=df_country, model="within", effect="twoways")

# modello dinamico
dynamic <- plm(ln_spesa_dm ~ lag(ln_spesa_dm,1) + X + ln_unemp_rate +
                 ln_pop_density + ln_gov_debt + corruption_control + gov_state_market,
               data=df_dyn, model="within", effect="twoways")

summary(static)$r.squared$within
summary(dynamic)$r.squared$within

model_fe <- plm(ln_spesa_dm ~ lag(ln_spesa_dm,1) + X,
                data=df_dyn, model="within", effect="individual")

model_iv <- plm(ln_spesa_dm ~ lag(ln_spesa_dm,1) + X |
                  lag(ln_spesa_dm,2) + X,
                data=df_dyn, model="within", effect="individual")

hausman <- phtest(model_fe, model_iv)
hausman

static_resid <- residuals(static)
acf(static_resid)



