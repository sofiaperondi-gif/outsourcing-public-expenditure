###############################################################
# 0. PACKAGES
###############################################################
rm(list = ls())
gc()

library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)
library(janitor)

setwd("~/Desktop/Tesina IV /Data analysis /Downloaded data")


###############################################################
# 1. LOAD DATA
###############################################################

# Macro dataset (già contiene primary_real, prop_* e controlli macro)
df <- read_csv("macro_primary_expenditure_with_growth_controls.csv")

# Eurostat primary_real + gdp_deflator (usato per sicurezza/coerenza)
expprim <- read_csv("eurostat_primary_expenditure_real.csv") %>%
  select(geo, year, primary_real, gdp_deflator)

# Micro dataset completo (per gdp_real + decomposizione spesa)
final_raw <- read_csv("FINAL_ANALYSIS_DATASET_2006_2021.csv")

# Rimuovo IT–2016 dai microdati
final_raw_clean <- final_raw %>%
  filter(!(buyer_country == "IT" & tender_year == 2016))


###############################################################
# 2. MERGE MACRO DF + EUROSTAT PRIMARY_REAL + REAL GDP
###############################################################

# 2.1 Aggiungo primary_real e gdp_deflator da expprim, se servono
df <- df %>%
  left_join(
    expprim,
    by = c("buyer_country" = "geo",
           "tender_year"   = "year"),
    suffix = c("", "_exp")
  )

# Se il join ha creato variabili "_exp", uso quelle
if (!"primary_real" %in% names(df) & "primary_real_exp" %in% names(df)) {
  df <- df %>% rename(primary_real = primary_real_exp)
}
if (!"gdp_deflator" %in% names(df) & "gdp_deflator_exp" %in% names(df)) {
  df <- df %>% rename(gdp_deflator = gdp_deflator_exp)
}

# 2.2 Costruisco gdp_real a livello country–year dai microdati
gdp_country <- final_raw_clean %>%
  filter(!is.na(gdp_real)) %>%
  group_by(buyer_country, tender_year) %>%
  summarise(
    gdp_real = mean(gdp_real, na.rm = TRUE),
    .groups = "drop"
  )

# 2.3 Merge nel macro df
df <- df %>%
  left_join(gdp_country, by = c("buyer_country", "tender_year"))


###############################################################
# 3. FILTRI DI BASE + LOG-TRANSFORM (2011–2021)
###############################################################

df <- df %>%
  # rimuovo IT–2016 a livello macro per coerenza
  filter(!(buyer_country == "IT" & tender_year == 2016)) %>%
  # finestra temporale della tesina
  filter(tender_year >= 2011, tender_year <= 2021) %>%
  # condizioni di positività per il log
  filter(
    primary_real > 0,
    avg_contract_price > 0,
    unemp_rate > 0,
    pop_density > 0,
    gov_debt_pct_gdp > 0,
    gdp_real > 0
  ) %>%
  mutate(
    ln_spesa       = log(primary_real),
    ln_valmean     = log(avg_contract_price),
    ln_unemp_rate  = log(unemp_rate),
    ln_pop_density = log(pop_density),
    ln_gov_debt    = log(gov_debt_pct_gdp),
    ln_gdp_real    = log(gdp_real)
  )

cat("N. righe df dopo filtri baseline:", nrow(df), "\n")
cat("Paesi unici:", length(unique(df$buyer_country)), "\n")
cat("Anni unici:", length(unique(df$tender_year)), "\n\n")


###############################################################
# 4. PROCUREMENT COMPOSITION VARIABLES (demeaned per paese)
###############################################################
# NB: qui NON dondiamo a livello micro; lavoriamo sul df macro (già country–year)

df <- df %>%
  group_by(buyer_country) %>%
  mutate(
    prop_services_dm       = prop_services       - mean(prop_services,       na.rm = TRUE),
    prop_works_dm          = prop_works          - mean(prop_works,          na.rm = TRUE),
    prop_meat_dm           = prop_meat           - mean(prop_meat,           na.rm = TRUE),
    prop_subcontracted_dm  = prop_subcontracted  - mean(prop_subcontracted,  na.rm = TRUE),
    prop_eu_bidders_dm     = prop_eu_bidders     - mean(prop_eu_bidders,     na.rm = TRUE),
    prop_non_eu_bidders_dm = prop_non_eu_bidders - mean(prop_non_eu_bidders, na.rm = TRUE)
  ) %>%
  ungroup()


###############################################################
# 5. COSTRUZIONE ECONOMIC + COFOG REAL EXPENDITURE (da micro)
###############################################################
# ATTENZIONE: qui usiamo i micro per decomporre la spesa
#            e deflazioniamo con gdp_deflator (media paese-anno)

df_exp <- final_raw_clean %>%
  filter(tender_year >= 2011, tender_year <= 2021) %>%
  group_by(buyer_country, tender_year) %>%
  summarise(
    # ECONOMIC COMPOSITION (nominale)
    P2_nom = sum(intermediate_consumption,    na.rm = TRUE),
    D1_nom = sum(compensation_employees,      na.rm = TRUE),
    P5_nom = sum(gross_fixed_capital,         na.rm = TRUE),
    
    # COFOG (nominale)
    COFOG_gen_pub = sum(public_services,      na.rm = TRUE),
    COFOG_order   = sum(public_order,         na.rm = TRUE),
    COFOG_econ    = sum(economic_affairs,     na.rm = TRUE),
    COFOG_health  = sum(health,               na.rm = TRUE),
    COFOG_edu     = sum(education,            na.rm = TRUE),
    COFOG_socprot = sum(social_protection,    na.rm = TRUE),
    
    # deflatore medio (dovrebbe essere lo stesso di Eurostat)
    gdp_deflator = mean(gdp_deflator, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Deflaziono (base 2010 = 100)
  mutate(
    P2_real = P2_nom / (gdp_deflator / 100),
    D1_real = D1_nom / (gdp_deflator / 100),
    P5_real = P5_nom / (gdp_deflator / 100),
    
    gen_pub_real = COFOG_gen_pub / (gdp_deflator / 100),
    order_real   = COFOG_order   / (gdp_deflator / 100),
    econ_real    = COFOG_econ    / (gdp_deflator / 100),
    health_real  = COFOG_health  / (gdp_deflator / 100),
    edu_real     = COFOG_edu     / (gdp_deflator / 100),
    socprot_real = COFOG_socprot / (gdp_deflator / 100)
  ) %>%
  # Log (dove > 0; gli zeri genereranno -Inf → verranno poi trattati)
  mutate(
    ln_P2  = if_else(P2_real  > 0, log(P2_real),  NA_real_),
    ln_D1  = if_else(D1_real  > 0, log(D1_real),  NA_real_),
    ln_P5  = if_else(P5_real  > 0, log(P5_real),  NA_real_),
    
    ln_gen_pub = if_else(gen_pub_real  > 0, log(gen_pub_real),  NA_real_),
    ln_order   = if_else(order_real    > 0, log(order_real),    NA_real_),
    ln_econ    = if_else(econ_real     > 0, log(econ_real),     NA_real_),
    ln_health  = if_else(health_real   > 0, log(health_real),   NA_real_),
    ln_edu     = if_else(edu_real      > 0, log(edu_real),      NA_real_),
    ln_socprot = if_else(socprot_real  > 0, log(socprot_real),  NA_real_)
  ) %>%
  # Demeaning delle log variabili di spesa per paese (solo ora!)
  group_by(buyer_country) %>%
  mutate(
    ln_P2_dm      = ln_P2      - mean(ln_P2,      na.rm = TRUE),
    ln_D1_dm      = ln_D1      - mean(ln_D1,      na.rm = TRUE),
    ln_P5_dm      = ln_P5      - mean(ln_P5,      na.rm = TRUE),
    
    ln_gen_pub_dm = ln_gen_pub - mean(ln_gen_pub, na.rm = TRUE),
    ln_order_dm   = ln_order   - mean(ln_order,   na.rm = TRUE),
    ln_econ_dm    = ln_econ    - mean(ln_econ,    na.rm = TRUE),
    ln_health_dm  = ln_health  - mean(ln_health,  na.rm = TRUE),
    ln_edu_dm     = ln_edu     - mean(ln_edu,     na.rm = TRUE),
    ln_socprot_dm = ln_socprot - mean(ln_socprot, na.rm = TRUE)
  ) %>%
  ungroup()

cat("N. righe df_exp (decomposition):", nrow(df_exp), "\n")


###############################################################
# 6. COSTRUZIONE PANEL COUNTRY–YEAR (BASELINE + PROCUREMENT + DECOMP)
###############################################################

df_country <- df %>%
  group_by(buyer_country, tender_year) %>%
  summarise(
    ln_spesa       = mean(ln_spesa,       na.rm = TRUE),
    ln_valmean     = mean(ln_valmean,     na.rm = TRUE),
    ln_unemp_rate  = mean(ln_unemp_rate,  na.rm = TRUE),
    ln_pop_density = mean(ln_pop_density, na.rm = TRUE),
    ln_gov_debt    = mean(ln_gov_debt,    na.rm = TRUE),
    ln_gdp_real    = mean(ln_gdp_real,    na.rm = TRUE),
    corruption_control = mean(corruption_control, na.rm = TRUE),
    gov_state_market   = mean(gov_state_market,   na.rm = TRUE),
    recession          = mean(recession,          na.rm = TRUE),
    
    # procurement controls (già DM a livello macro)
    prop_services_dm       = mean(prop_services_dm,       na.rm = TRUE),
    prop_works_dm          = mean(prop_works_dm,          na.rm = TRUE),
    prop_meat_dm           = mean(prop_meat_dm,           na.rm = TRUE),
    prop_subcontracted_dm  = mean(prop_subcontracted_dm,  na.rm = TRUE),
    prop_eu_bidders_dm     = mean(prop_eu_bidders_dm,     na.rm = TRUE),
    prop_non_eu_bidders_dm = mean(prop_non_eu_bidders_dm, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  # join con decomposizione di spesa
  left_join(df_exp, by = c("buyer_country", "tender_year")) %>%
  # demean principale Y e X a livello paese
  group_by(buyer_country) %>%
  mutate(
    X           = ln_valmean - mean(ln_valmean, na.rm = TRUE),
    ln_spesa_dm = ln_spesa    - mean(ln_spesa,  na.rm = TRUE)
  ) %>%
  ungroup()

cat("DIM df_country:", dim(df_country), "\n")
cat("Paesi:", length(unique(df_country$buyer_country)),
    " — Anni:", length(unique(df_country$tender_year)), "\n\n")

# Qui ti puoi aspettare n ≈ 231 se il macro file ha 232 righe e togli solo IT–2016.
# Se vedi meno → significa che qualche filtro sopra (log, >0, ecc.) cancella ulteriori osservazioni.


###############################################################
# 7. PREPARO pdata PER plm
###############################################################

pdata <- pdata.frame(df_country, index = c("buyer_country","tender_year"))


###############################################################
# 8. MODELLI
###############################################################

## (A) BASELINE MODEL ----------------------------------------

mod_baseline <- plm(
  ln_spesa_dm ~ X +
    ln_unemp_rate + ln_pop_density + ln_gov_debt +
    corruption_control + gov_state_market + recession,
  data  = pdata,
  model = "within",
  effect = "twoways"
)

res_baseline <- coeftest(
  mod_baseline,
  vcov = vcovHC(mod_baseline, type = "HC1", cluster = "group")
)


## (B) PROCUREMENT CONTROLS EXTENDED MODEL --------------------

mod_proc <- plm(
  ln_spesa_dm ~ X +
    prop_services_dm + prop_works_dm +
    prop_meat_dm + prop_subcontracted_dm +
    prop_eu_bidders_dm + prop_non_eu_bidders_dm +
    ln_unemp_rate + ln_pop_density + ln_gov_debt +
    corruption_control + gov_state_market + recession,
  data  = pdata,
  model = "within",
  effect = "twoways"
)

res_proc <- coeftest(
  mod_proc,
  vcov = vcovHC(mod_proc, type = "HC1", cluster = "group")
)


## (C) FUNZIONE AUSILIARIA PER SPECIFICHE ALTERNATIVE ---------

run_alt <- function(dep) {
  form <- as.formula(
    paste0(dep,
           " ~ X + ln_unemp_rate + ln_pop_density + ln_gov_debt + ",
           "corruption_control + gov_state_market + recession")
  )
  m <- plm(form, data = pdata, model = "within", effect = "twoways")
  coeftest(m, vcov = vcovHC(m, type = "HC1", cluster = "group"))
}

## (D) ECONOMIC COMPOSITION (P2, D1, P5) ----------------------

res_P2 <- run_alt("ln_P2_dm")
res_D1 <- run_alt("ln_D1_dm")
res_P5 <- run_alt("ln_P5_dm")


## (E) COFOG COMPOSITION (gen_pub, order, econ, health, edu, socprot) ----

res_genpub  <- run_alt("ln_gen_pub_dm")
res_order   <- run_alt("ln_order_dm")
res_econ    <- run_alt("ln_econ_dm")
res_health  <- run_alt("ln_health_dm")
res_edu     <- run_alt("ln_edu_dm")
res_socprot <- run_alt("ln_socprot_dm")


###############################################################
# 9. PRINT RISULTATI
###############################################################

cat("\n=== BASELINE ===\n")
print(res_baseline)
# Residui del modello TWFE
res <- residuals(mod_baseline)

# Valore predetto (within) del modello 
fit <- as.numeric(fitted(mod_baseline))

# R2 overall
y <- df_country$ln_spesa_dm   # tua variabile dipendente
r2_overall <- 1 - var(res, na.rm=TRUE) / var(y, na.rm=TRUE)
cat("R² overall:", r2_overall, "\n")

# Within transformation
y_within  <- y - ave(y, df_country$buyer_country, FUN=mean)
fit_within <- fit - ave(fit, df_country$buyer_country, FUN=mean)

r2_within <- 1 - sum( (y_within - fit_within)^2 ) / sum( y_within^2 )
cat("R² within:", r2_within, "\n")

# Between transformation
y_between <- ave(y, df_country$buyer_country, FUN=mean)
fit_between <- ave(fit, df_country$buyer_country, FUN=mean)

r2_between <- 1 - sum( (y_between - fit_between)^2 ) / sum( y_between^2 )
cat("R² between:", r2_between, "\n")

k <- length(coef(mod_baseline))      # number of regressors
n <- nobs(mod_baseline)              # number of observations

adj_r2_overall <- 1 - (1 - r2_overall)*(n - 1)/(n - k - 1)
cat("Adjusted R² overall:", adj_r2_overall, "\n")


cat("\n=== PROCUREMENT CONTROLS ===\n")
print(res_proc)

# Residui del modello TWFE (procurement controls)
res_proc_res <- residuals(mod_proc)

# Valori predetti (within)
fit_proc <- as.numeric(fitted(mod_proc))

# Variabile dipendente
y_proc <- df_country$ln_spesa_dm

# 1. R² overall
r2_overall_proc <- 1 - var(res_proc_res, na.rm = TRUE) / var(y_proc, na.rm = TRUE)
cat("R² overall (proc):", r2_overall_proc, "\n")

# 2. R² within
y_within_proc  <- y_proc - ave(y_proc, df_country$buyer_country, FUN = mean)
fit_within_proc <- fit_proc - ave(fit_proc, df_country$buyer_country, FUN = mean)
r2_within_proc <- 1 - sum( (y_within_proc - fit_within_proc)^2 ) / sum( y_within_proc^2 )
cat("R² within (proc):", r2_within_proc, "\n")

# 3. R² between
y_between_proc <- ave(y_proc, df_country$buyer_country, FUN = mean)
fit_between_proc <- ave(fit_proc, df_country$buyer_country, FUN = mean)
r2_between_proc <- 1 - sum( (y_between_proc - fit_between_proc)^2 ) / sum( y_between_proc^2 )
cat("R² between (proc):", r2_between_proc, "\n")

# 4. Adjusted R² overall
k_proc <- length(coef(mod_proc))      # number of regressors
n_proc <- nobs(mod_proc)              # number of observations
adj_r2_overall_proc <- 1 - (1 - r2_overall_proc)*(n_proc - 1)/(n_proc - k_proc - 1)
cat("Adjusted R² overall (proc):", adj_r2_overall_proc, "\n")


cat("\n=== ECONOMIC COMPOSITION (P2, D1, P5) ===\n")
print(res_P2)
print(res_D1)
print(res_P5)

cat("\n=== COFOG (gen_pub, order, econ, health, edu, socprot) ===\n")
print(res_genpub)
print(res_order)
print(res_econ)
print(res_health)
print(res_edu)
print(res_socprot)

###############################################################
# COUNTRY-SPECIFIC ESTIMATES OF THE EFFECT OF X
# (HETEROGENEITY DIAGNOSTICS FOR TWFE)
###############################################################
library(tidyverse)
library(fixest)
library(broom)
library(kableExtra)

# Dataset country-year già costruito (come nel report principale)
df <- read_csv("final_countryyear_2011_2021.csv", show_col_types = FALSE) %>%
  # togli IT-2016
  filter(!(buyer_country == "IT" & tender_year == 2016)) %>%
  mutate(
    buyer_country = factor(buyer_country),
    tender_year   = as.factor(tender_year)
  )

# 1) TWFE con eterogeneità per paese: i(buyer_country, X, ref="IT")
m_countryhet <- feols(
  ln_spesa_dm ~ i(buyer_country, X, ref = "IT") +
    ln_unemp_rate + ln_pop_density + ln_gov_debt +
    corruption_control + gov_state_market + recession
  | buyer_country + tender_year,
  data    = df,
  cluster = ~ buyer_country
)

# 2) Estrai coefficienti X×country con SE clustered e CI
coefs_cty <- tidy(m_countryhet, conf.int = TRUE) %>%
  filter(str_detect(term, "buyer_country::")) %>%
  mutate(
    buyer_country = str_extract(term,
                                "(AT|BE|BG|CY|CZ|DE|DK|EE|EL|ES|FI|FR|HR|HU|IE|IT|LT|LU|LV|MT|NL|PL|PT|RO|SE|SI|SK)"
    )
  ) %>%
  select(buyer_country, estimate, std.error, statistic, p.value, conf.low, conf.high)

# Aggiungi IT come riferimento (effetto = 0)
coefs_cty <- bind_rows(
  coefs_cty,
  tibble(
    buyer_country = "IT",
    estimate = 0,
    std.error = NA_real_,
    statistic = NA_real_,
    p.value = NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_
  )
)

# 3) Categorie segno × significatività (utile per il testo)
coefs_cty <- coefs_cty %>%
  mutate(
    signif = case_when(
      is.na(p.value)        ~ "Reference",
      p.value < 0.05        ~ "p<0.05",
      p.value < 0.10        ~ "p<0.10",
      TRUE                  ~ "n.s."
    ),
    sign = case_when(
      estimate >  0 ~ "Positive",
      estimate <  0 ~ "Negative",
      TRUE          ~ "Zero"
    ),
    group = case_when(
      buyer_country == "IT"          ~ "Reference (IT)",
      p.value < 0.05 & estimate > 0  ~ "Positive-Significant",
      p.value < 0.05 & estimate < 0  ~ "Negative-Significant",
      estimate > 0                   ~ "Positive-Not Sig",
      estimate < 0                   ~ "Negative-Not Sig",
      TRUE                           ~ "Zero/Other"
    )
  )

# 4) Tabella compatta per appendice (ordinata per coefficiente)
tab_coef <- coefs_cty %>%
  arrange(desc(estimate)) %>%
  mutate(across(c(estimate, std.error, conf.low, conf.high, p.value),
                ~ round(., 4)))

print(table_coef)

kable(
  tab_coef,
  booktabs = TRUE,
  caption = "Country-specific effect of outsourcing (X) on demeaned public expenditure"
) %>%
  kable_styling(latex_options = c("HOLD_position", "scale_down"),
                font_size = 8)

# 5) Conteggio paesi per categoria (riassunto che userai nel testo)
tab_counts <- coefs_cty %>%
  filter(buyer_country != "IT") %>%
  count(group) %>%
  arrange(desc(n))

kable(
  tab_counts,
  booktabs = TRUE,
  caption = "Countries by effect category (relative to IT = reference)"
) %>%
  kable_styling(latex_options = c("HOLD_position"),
                font_size = 8)

library(tidyverse)
library(fixest)
library(broom)

setwd("~/Desktop/Tesina IV /Data analysis /Downloaded data")

df <- read_csv("final_countryyear_2011_2021.csv", show_col_types = FALSE) %>%
  filter(!(buyer_country == "IT" & tender_year == 2016)) %>%
  mutate(
    buyer_country = factor(buyer_country),
    tender_year   = factor(tender_year)
  )

coefs_cty <- tidy(m_countryhet, conf.int = TRUE) %>%
  filter(str_detect(term, "buyer_country::")) %>%
  mutate(
    buyer_country = str_extract(term,
                                "(AT|BE|BG|CY|CZ|DE|DK|EE|EL|ES|FI|FR|HR|HU|IE|IT|LT|LU|LV|MT|NL|PL|PT|RO|SE|SI|SK)"
    )
  ) %>%
  select(buyer_country, estimate, std.error, p.value, conf.low, conf.high)

# aggiungi IT come riferimento
coefs_cty <- bind_rows(
  coefs_cty,
  tibble(
    buyer_country = "IT",
    estimate = 0,
    std.error = NA_real_,
    p.value = NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_
  )
)

print(coefs_cty %>% arrange(buyer_country), n = 27)

coefs_groups <- coefs_cty %>%
  mutate(
    group = case_when(
      buyer_country == "IT" ~ "Reference (IT)",
      p.value < 0.05 & estimate > 0  ~ "Positive-Significant",
      p.value < 0.05 & estimate < 0  ~ "Negative-Significant",
      p.value >= 0.05 & estimate > 0 ~ "Positive-Not Significant",
      p.value >= 0.05 & estimate < 0 ~ "Negative-Not Significant",
      TRUE ~ "Zero"
    )
  )

print(coefs_groups %>% arrange(group), n = 27)

summary_stats <- coefs_cty %>%
  summarise(
    min = min(estimate, na.rm=TRUE),
    q1  = quantile(estimate, 0.25, na.rm=TRUE),
    median = median(estimate, na.rm=TRUE),
    mean = mean(estimate, na.rm=TRUE),
    q3  = quantile(estimate, 0.75, na.rm=TRUE),
    max = max(estimate, na.rm=TRUE)
  )

print(summary_stats, n = 27)

###############################################################
# 8bis. STANDARDIZZAZIONE SOLO DI X
###############################################################

df_stdX <- df_country %>%
  mutate(
    X_z = as.numeric(scale(X))   # standardizzazione di X
  )

pdata_stdX <- pdata.frame(df_stdX, index = c("buyer_country", "tender_year"))

mod_baseline_Xstd <- plm(
  ln_spesa_dm ~ 
    X_z +
    ln_unemp_rate + ln_pop_density + ln_gov_debt +
    corruption_control + gov_state_market + recession,
  data  = pdata_stdX,
  model = "within",
  effect = "twoways"
)

res_baseline_Xstd <- coeftest(
  mod_baseline_Xstd,
  vcov = vcovHC(mod_baseline_Xstd, type = "HC1", cluster = "group")
)

cat("\n=== BASELINE con X STANDARDIZZATO ===\n")
print(res_baseline_Xstd)

sd_X <- sd(df_country$X, na.rm = TRUE)
mean_X <- mean(df_country$X, na.rm = TRUE)
cat("Media di X:", mean_X, "\n")
cat("Deviazione standard di X:", sd_X, "\n")

###############################################################
# 8ter. STANDARDIZZAZIONE DI ENTRAMBE LE VARIABILI (Y e X)
# Per ottenere il "beta standardizzato"
###############################################################

# Calcola deviazioni standard di Y (ln_spesa_dm) e X
sd_Y <- sd(df_country$ln_spesa_dm, na.rm = TRUE)
sd_X <- sd(df_country$X, na.rm = TRUE)

cat("\n=== DEVIAZIONI STANDARD PER STANDARDIZZAZIONE ===\n")
cat("Deviazione standard di Y (ln_spesa_dm):", sd_Y, "\n")
cat("Deviazione standard di X:", sd_X, "\n")
cat("Rapporto sd_X/sd_Y:", sd_X/sd_Y, "\n\n")

# 1. Calcolo MANUALE del beta standardizzato
# Estrai il coefficiente X da res_baseline (che è una matrice)
beta_original <- res_baseline["X", "Estimate"]  # CORREZIONE QUI
beta_std_manual <- beta_original * (sd_X / sd_Y)

cat("=== BETA STANDARDIZZATO (calcolo manuale) ===\n")
cat("Coefficiente originale (β):", beta_original, "\n")
cat("Beta standardizzato (β_std = β * sd_X / sd_Y):", beta_std_manual, "\n")
cat("Interpretazione: 1 deviazione standard in X →", 
    round(beta_std_manual, 4), "deviazioni standard in Y\n\n")

# 2. Verifica con regressione diretta (standardizzando entrambe le variabili)
df_stdYX <- df_country %>%
  mutate(
    Y_z = ln_spesa_dm / sd_Y,  # standardizza Y
    X_z = X / sd_X             # standardizza X
  )

# Prepara il pdata frame
pdata_stdYX <- pdata.frame(df_stdYX, index = c("buyer_country", "tender_year"))

# Stima il modello con entrambe standardizzate
mod_stdYX <- plm(
  Y_z ~ 
    X_z +
    ln_unemp_rate + ln_pop_density + ln_gov_debt +
    corruption_control + gov_state_market + recession,
  data  = pdata_stdYX,
  model = "within",
  effect = "twoways"
)

res_stdYX <- coeftest(
  mod_stdYX,
  vcov = vcovHC(mod_stdYX, type = "HC1", cluster = "group")
)

cat("\n=== MODELLO CON Y E X ENTRAMBE STANDARDIZZATE ===\n")
print(res_stdYX)

# 3. Verifica che sia uguale al calcolo manuale
beta_std_reg <- res_stdYX["X_z", "Estimate"]  # CORREZIONE QUI
cat("\n=== CONFRONTO ===\n")
cat("Beta standardizzato (calcolo manuale):", beta_std_manual, "\n")
cat("Beta standardizzato (regressione diretta):", beta_std_reg, "\n")
cat("Differenza:", abs(beta_std_manual - beta_std_reg), "\n")

# 4. Interpretazione chiara
cat("\n=== INTERPRETAZIONE DEL BETA STANDARDIZZATO ===\n")
cat("Il beta standardizzato di", round(beta_std_manual, 3), "significa che:\n")
cat("- Un aumento di 1 deviazione standard nell'intensità di outsourcing (X)\n")
cat("- È associato a un aumento di", round(beta_std_manual, 3), 
    "deviazioni standard nella spesa primaria reale (Y)\n")
cat("\nPoiché una deviazione standard di Y è", round(sd_Y, 3), 
    "in unità logaritmiche demeaned,\n")
cat("ciò equivale a un aumento di", round(beta_std_manual * sd_Y, 5), 
    "in log(Y), cioè circa", 
    round(100 * (exp(beta_std_manual * sd_Y) - 1), 3), "% in termini reali.\n")

# 5. Rappresentazione grafica per visualizzare la "modestezza"
library(ggplot2)

# Crea un grafico che mostra la relazione
plot_data <- df_country %>%
  mutate(
    X_sd = X / sd_X,  # X in unità di deviazioni standard
    Y_sd = ln_spesa_dm / sd_Y  # Y in unità di deviazioni standard
  )

# Calcola la regressione semplice per la linea di tendenza
lm_simple <- lm(Y_sd ~ X_sd, data = plot_data)
slope_simple <- coef(lm_simple)["X_sd"]

ggplot(plot_data, aes(x = X_sd, y = Y_sd)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              color = "darkred", size = 0.8) +
  geom_abline(slope = beta_std_manual, intercept = coef(lm_simple)[1], 
              color = "blue", linetype = "dashed", size = 0.8) +
  labs(
    x = "Intensità di outsourcing (deviazioni standard)",
    y = "Spesa primaria reale (deviazioni standard)",
    title = "Relazione tra outsourcing e spesa pubblica",
    subtitle = paste("Beta standardizzato =", round(beta_std_manual, 3), 
                     "(pendenza blu tratteggiata)")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  annotate("text", x = min(plot_data$X_sd, na.rm = TRUE) + 1, 
           y = max(plot_data$Y_sd, na.rm = TRUE) - 0.5,
           label = paste("Pendenza:", round(beta_std_manual, 3)),
           color = "blue", size = 4)

# Salva il grafico
ggsave("beta_standardizzato.png", width = 8, height = 6, dpi = 300)

# 6. Opzionale: confronto con altri regressori
cat("\n=== BETA STANDARDIZZATI - RIGA PER RIGA ===\n")
cat("Variabile X (outsourcing):", round(beta_std_manual, 4), "\n")

# Calcola per le altre variabili di controllo
control_vars <- c("ln_unemp_rate", "ln_pop_density", "ln_gov_debt", 
                  "corruption_control", "gov_state_market", "recession")

for (var in control_vars) {
  if (var %in% names(df_country)) {
    beta_var <- res_baseline[var, "Estimate"]
    sd_var <- sd(df_country[[var]], na.rm = TRUE)
    beta_std_var <- beta_var * (sd_var / sd_Y)
    cat(var, ":", round(beta_std_var, 4), "\n")
  }
}
