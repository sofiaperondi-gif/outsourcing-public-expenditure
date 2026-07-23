############################################################
# 03_alternative_specification_TWFE.R
# Testing alternative model specification with primary expenditure
# and dynamic population/unemployment variables
############################################################

rm(list = ls())
gc()

# === LIBRARIES ===
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(fixest)
  library(modelsummary)
})

# === STEP 1: Load merged expenditure dataset ===
# (created previously after cleaning TE and D41PAY)
merged <- read_csv("eurostat_primary_expenditure_real.csv")

# Quick sanity check
cat("✅ Dataset loaded with", nrow(merged), "rows\n")
cat("Countries:", length(unique(merged$geo)), "\nYears:",
    min(merged$year), "-", max(merged$year), "\n\n")

# === STEP 2: Load macro dataset (controls, governance, etc.) ===
df_controls <- read_csv("FINAL_ANALYSIS_DATASET_2006_2021.csv") %>%
  rename(country = buyer_country, year = tender_year) %>%
  group_by(country, year) %>%
  summarise(across(c(total_expenditure_growth_real, unemp_rate, old_dep,
                     pop_density, gov_debt_pct_gdp, corruption_control,
                     gov_state_market, gdp_growth_real),
                   ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

# === STEP 3: Harmonise keys and merge ===
df_panel <- df_controls %>%
  rename(geo = country) %>%
  left_join(merged %>%
              select(geo, year, primary_growth_real),
            by = c("geo", "year")) %>%
  rename(primary_expenditure_growth_real = primary_growth_real)

# === STEP 4: Construct alternative variables ===
df_panel <- df_panel %>%
  group_by(geo) %>%
  arrange(year) %>%
  mutate(
    # Unemployment growth rate (%)
    unemp_growth = 100 * (unemp_rate / lag(unemp_rate) - 1),
    # Population growth rate (%)
    pop_growth = 100 * (pop_density / lag(pop_density) - 1)
  ) %>%
  ungroup()

# === STEP 5: Structural Dummies (same as before) ===
df_panel <- df_panel %>%
  mutate(
    d_EL = as.integer(geo == "EL"),
    d_IE = as.integer(geo == "IE"),
    d_MT = as.integer(geo == "MT"),
    EL_debt  = d_EL * gov_debt_pct_gdp,
    EL_unemp = d_EL * unemp_rate,
    IE_gdp   = d_IE * gdp_growth_real,
    MT_gdp   = d_MT * gdp_growth_real
  )

# === STEP 6: Define model specifications ===

# Baseline with total expenditure
f_base <- total_expenditure_growth_real ~ unemp_rate + old_dep + pop_density +
  gov_debt_pct_gdp + corruption_control + gov_state_market + gdp_growth_real

# Model with structural dummies
f_struct <- update(f_base, . ~ . + EL_debt + EL_unemp + IE_gdp + MT_gdp)

# Alternative model with primary expenditure and dynamic controls
f_alt <- primary_expenditure_growth_real ~ unemp_growth + old_dep + pop_growth +
  gov_debt_pct_gdp + corruption_control + gov_state_market + gdp_growth_real +
  EL_debt + EL_unemp + IE_gdp + MT_gdp

# === STEP 7: Estimate TWFE models (clustered SEs) ===
mod_base    <- feols(f_base, data = df_panel, fe = ~ geo + year, cluster = ~ geo)
mod_struct  <- feols(f_struct, data = df_panel, fe = ~ geo + year, cluster = ~ geo)
mod_alt     <- feols(f_alt, data = df_panel, fe = ~ geo + year, cluster = ~ geo)

# === STEP 8: Summary Table ===
msummary(
  list(
    "Baseline (Total Exp.)" = mod_base,
    "Structural Dummies"    = mod_struct,
    "Alternative (Primary Exp.)" = mod_alt
  ),
  stars = TRUE,
  gof_omit = "AIC|BIC|Log|F|RMSE|Adj|DF",
  coef_rename = c(
    unemp_rate = "Unemployment rate",
    unemp_growth = "Unemployment growth",
    old_dep = "Old-age dependency",
    pop_density = "Population density",
    pop_growth = "Population growth",
    gov_debt_pct_gdp = "Government debt (% GDP)",
    corruption_control = "Corruption control",
    gov_state_market = "State–market index",
    gdp_growth_real = "Real GDP growth",
    EL_debt = "EL × Debt",
    EL_unemp = "EL × Unemployment",
    IE_gdp = "IE × GDP growth",
    MT_gdp = "MT × GDP growth"
  ),
  output = "markdown"
)

# === STEP 9: Quick diagnostics ===
etable(mod_alt)

cat("\n✅ Alternative specification estimated successfully.\n")

############################################################
# Alternative specification — WITHOUT structural dummies
############################################################

library(fixest)
library(modelsummary)
library(dplyr)

# === 1️⃣ Formula: alternative specification, no dummies ===
f_alt_nodum <- primary_expenditure_growth_real ~ 
  unemp_growth + old_dep + pop_growth +
  gov_debt_pct_gdp + corruption_control + 
  gov_state_market + gdp_growth_real

# === 2️⃣ Stima modello TWFE con cluster robusti ===
mod_alt_nodum <- feols(f_alt_nodum, 
                       data = df_panel, 
                       fe = ~ geo + year, 
                       cluster = ~ geo)

# === 3️⃣ Visualizza risultati singoli ===
etable(mod_alt_nodum)

# === 4️⃣ Confronto diretto con modello con dummies strutturali ===
msummary(
  list(
    "Alternative (no dummies)" = mod_alt_nodum,
    "Alternative (+structural dummies)" = mod_alt
  ),
  stars = TRUE,
  gof_omit = "AIC|BIC|Log|F|RMSE|Adj|DF",
  coef_rename = c(
    unemp_growth = "Unemployment growth",
    old_dep = "Old-age dependency",
    pop_growth = "Population growth",
    gov_debt_pct_gdp = "Government debt (% GDP)",
    corruption_control = "Corruption control",
    gov_state_market = "State–market index",
    gdp_growth_real = "Real GDP growth",
    EL_debt = "EL × Debt",
    EL_unemp = "EL × Unemployment",
    IE_gdp = "IE × GDP growth",
    MT_gdp = "MT × GDP growth"
  ),
  output = "markdown"
)

# === 5️⃣ Breve riassunto numerico ===
cat("\n📊 Comparison summary:\n")
cat("R² (no dummies):       ", round(r2(mod_alt_nodum), 3), "\n")
cat("R² (with dummies):     ", round(r2(mod_alt), 3), "\n")
cat("Δ R² = ", round(r2(mod_alt) - r2(mod_alt_nodum), 3), "\n\n")
cat("✅ Done — alternative model estimated without structural dummies.\n")

cat("Within R2 (no dummies): ", r2(mod_alt_nodum, type="within"), "\n")
cat("Within R2 (+struct.):   ", r2(mod_alt,      type="within"), "\n")

alt_pre <- feols(
  primary_expenditure_growth_real ~ unemp_growth + old_dep + pop_growth +
    gov_debt_pct_gdp + corruption_control + gov_state_market + gdp_growth_real,
  data = df_panel %>% filter(year <= 2019),
  fe = ~ geo + year, cluster = ~ geo
)
etable(alt_pre)

df_panel <- df_panel %>%
  group_by(geo) %>% arrange(year) %>%
  mutate(gdp_growth_real_l1 = dplyr::lag(gdp_growth_real, 1)) %>% ungroup()

alt_lag <- feols(
  primary_expenditure_growth_real ~ unemp_growth + old_dep + pop_growth +
    gov_debt_pct_gdp + corruption_control + gov_state_market +
    gdp_growth_real + gdp_growth_real_l1,
  data = df_panel, fe = ~ geo + year, cluster = ~ geo
)
etable(alt_lag)

df_tmp <- df_panel %>% 
  select(unemp_growth, pop_growth, gdp_growth_real, gov_debt_pct_gdp,
         corruption_control, gov_state_market) %>%
  mutate(across(everything(), as.numeric))

round(cor(df_tmp, use="pairwise.complete.obs"), 2)

# modello senza il PIL
alt_no_gdp <- feols(
  primary_expenditure_growth_real ~ unemp_growth + old_dep + pop_growth +
    gov_debt_pct_gdp + corruption_control + gov_state_market,
  data = df_panel, fe = ~ geo + year, cluster = ~ geo
)

# === Helper per R² within compatibile con tutte le versioni ===
get_within_r2 <- function(model) {
  types <- tryCatch(fitstat(show_types = TRUE), error = function(e) NULL)
  if (!is.null(types) && any(grepl("r2_w", types))) {
    return(as.numeric(fitstat(model, "r2_w")))
  } else if (!is.null(types) && any(grepl("r2_within", types))) {
    return(as.numeric(fitstat(model, "r2_within")))
  } else {
    # fallback: usa l'R2 standard
    return(as.numeric(r2(model)))
  }
}

# === Calcola R² within per ciascun modello ===
r2_nodum_within <- get_within_r2(mod_alt_nodum)
r2_dum_within   <- get_within_r2(mod_alt)
r2_nogdp_within <- get_within_r2(alt_no_gdp)

cat("\n📊 Within R² values:\n")
cat("Alternative (no dummies):       ", round(r2_nodum_within, 3), "\n")
cat("Alternative (+struct dummies):  ", round(r2_dum_within, 3), "\n")
cat("Alternative (no GDP variable):  ", round(r2_nogdp_within, 3), "\n")

# === Partial contribution of GDP growth ===
partial_r2_gdp <- r2_nodum_within - r2_nogdp_within
cat("\nPartial within R² (gdp_growth_real):", round(partial_r2_gdp, 4), "\n")

# --- 0️⃣ Setup ---------------------------------------------------------------
rm(list = ls())
gc()

library(dplyr)
library(data.table)

# --- 1️⃣ Importa il dataset già pulito e salvato ----------------------------
merged <- fread("eurostat_primary_expenditure_real.csv")

# Controllo rapido sul contenuto
cat("✅ Dataset loaded:", nrow(merged), "rows\n")
cat("Countries:", length(unique(merged$geo)), "\n")
cat("Years:", min(merged$year), "-", max(merged$year), "\n\n")

# --- 2️⃣ Restringi il periodo di analisi (2011–2021) ------------------------
merged <- merged %>%
  filter(year >= 2011 & year <= 2021)

# Controllo post-filtro
cat("✅ Filtered to 2011–2021\n")
cat("Countries:", length(unique(merged$geo)), "\n")
cat("Years:", min(merged$year), "-", max(merged$year), "\n")
cat("Observations:", nrow(merged), "\n\n")

# --- 3️⃣ Controllo su variabili chiave --------------------------------------
summary(merged$primary_growth_real)

# Verifica che la spesa primaria < spesa totale in tutti i casi
check_primary <- merged %>%
  summarise(
    n_violation = sum(primary_real > te_real, na.rm = TRUE),
    share_violation = mean(primary_real > te_real, na.rm = TRUE)
  )
cat("⚙️  Check primary < total:", check_primary$n_violation,
    "violations (", round(100 * check_primary$share_violation, 2), "%)\n\n")

# --- 4️⃣ Salva la versione filtrata se vuoi tenerla separata ----------------
fwrite(merged, "eurostat_primary_expenditure_real_2011_2021.csv")
cat("✅ File saved: eurostat_primary_expenditure_real_2011_2021.csv\n\n")

# --- 5️⃣ Calcoli descrittivi per la tabella ---------------------------------
mean_val <- mean(merged$primary_growth_real, na.rm = TRUE)
sd_val   <- sd(merged$primary_growth_real, na.rm = TRUE)
min_val  <- min(merged$primary_growth_real, na.rm = TRUE)
max_val  <- max(merged$primary_growth_real, na.rm = TRUE)

cat("📊 Primary expenditure growth (2011–2021):\n")
cat("Mean:", round(mean_val, 2), "\n")
cat("SD:", round(sd_val, 2), "\n")
cat("Min:", round(min_val, 2), "\n")
cat("Max:", round(max_val, 2), "\n\n")

# --- 6️⃣ Within / between variation (facoltativo) ---------------------------
merged %>%
  group_by(geo) %>%
  summarise(mean_country = mean(primary_growth_real, na.rm = TRUE)) -> country_means

within_sd <- sd(merged$primary_growth_real - ave(merged$primary_growth_real, merged$geo, FUN=mean, na.rm=TRUE), na.rm = TRUE)
between_sd <- sd(country_means$mean_country, na.rm = TRUE)

cat("Within SD:", round(within_sd, 2), "\n")
cat("Between SD:", round(between_sd, 2), "\n")

