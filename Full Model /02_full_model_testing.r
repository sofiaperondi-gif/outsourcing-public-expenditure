# --- 0) Setup --------------------------------------------------------------
library(tidyverse)
library(fixest)

setwd("~/Desktop/Tesina IV /Data analysis /Downloaded data")

# Dati già validati
df_macro  <- read_csv("macro_primary_expenditure_with_growth_controls.csv")  # contiene primary_growth_real + controlli + TCV/n_contracts/avg...
exp_prim  <- read_csv("eurostat_primary_expenditure_real.csv")               # contiene primary_real (livelli) per anno-paese

# Merge per avere i denominatori (PrimaryReal, GDP) nello stesso df
# Nota: in macro hai buyer_country/tender_year; in exp_prim hai geo/year
df <- df_macro %>%
  left_join(
    exp_prim %>% select(geo, year, primary_real, te_real), 
    by = c("buyer_country" = "geo", "tender_year" = "year")
  )

# ================================
# Outsourcing Indicators: Step 1 (Diagnostics only)
# ================================

# 0) Setup ---------------------------------------------------------------
rm(list = ls()); gc()
library(tidyverse)
library(fixest)

# 1) Caricamento dati -----------------------------------------------------
final   <- read_csv("FINAL_ANALYSIS_DATASET_2006_2021.csv")
expprim <- read_csv("eurostat_primary_expenditure_real.csv")
macro   <- read_csv("macro_primary_expenditure_with_growth_controls.csv")

# 2) Selezione finestre e variabili minime -------------------------------
# Usiamo 2011-2021 per coerenza con macro; puoi cambiare se necessario
final_s <- final %>%
  filter(tender_year >= 2011, tender_year <= 2021) %>%
  select(buyer_country, tender_year, tender_digiwhist_price)

# GDP nominale (dal FINAL) per normalizzazioni su PIL
gdp_panel <- final %>%
  filter(tender_year >= 2011, tender_year <= 2021) %>%
  select(buyer_country, tender_year, gdp_nominal) %>%
  filter(!is.na(gdp_nominal)) %>%
  group_by(buyer_country, tender_year) %>%
  summarise(gdp_nominal = mean(gdp_nominal, na.rm = TRUE), .groups = "drop")

# Primary_real e deflatore dal file eurostat
prim_panel <- expprim %>%
  filter(year >= 2011, year <= 2021) %>%
  transmute(buyer_country = geo,
            tender_year   = year,
            primary_real,
            gdp_deflator)

# Controllo coperture
stopifnot(nrow(gdp_panel) > 0, nrow(prim_panel) > 0)

# 3) Costruzione indicatori base da micro --------------------------------
# A) Aggregati per Paese-anno: Totale valore (TCV) e N contratti
agg_micro <- final_s %>%
  filter(!is.na(tender_digiwhist_price)) %>%
  group_by(buyer_country, tender_year) %>%
  summarise(
    TCV   = sum(tender_digiwhist_price, na.rm = TRUE),
    N     = n(),
    AVG   = mean(tender_digiwhist_price, na.rm = TRUE),
    MED   = median(tender_digiwhist_price, na.rm = TRUE),
    .groups = "drop"
  )

# B) Variante mediana "pulita" (opzionale): escludi importi < 1,000 se serve
agg_micro_clean <- final_s %>%
  filter(!is.na(tender_digiwhist_price)) %>%
  mutate(price_ge1k = if_else(tender_digiwhist_price >= 1000, tender_digiwhist_price, NA_real_)) %>%
  group_by(buyer_country, tender_year) %>%
  summarise(
    MED_clean = median(price_ge1k, na.rm = TRUE),
    share_lt1k = mean(tender_digiwhist_price < 1000, na.rm = TRUE),
    .groups = "drop"
  )

# 4) Merge denominatori e costruzione PIL reale --------------------------
base_panel <- agg_micro %>%
  left_join(agg_micro_clean, by = c("buyer_country","tender_year")) %>%
  left_join(prim_panel,      by = c("buyer_country","tender_year")) %>%
  left_join(gdp_panel,       by = c("buyer_country","tender_year"))

# GDP reale: gdp_nominal / (gdp_deflator/100)
base_panel <- base_panel %>%
  mutate(
    gdp_real = if_else(!is.na(gdp_nominal) & !is.na(gdp_deflator) & gdp_deflator > 0,
                       gdp_nominal / (gdp_deflator/100), NA_real_)
  )

# 5) Costruzione famiglie A, B, C (con lag per normalizzate) -------------
# A) Valori assoluti
#    - A1: TCV (totale valore contratti)
#    - A2: N   (numero contratti)
# B) Statistiche di taglia
#    - B1: AVG (media valore contratto)
#    - B2: MED_clean (mediana pulita; fallback su MED se tutti NA)
# C) Normalizzate (intensità) con lag t-1
#    - C1: TCV / primary_real (t-1)
#    - C2: TCV / gdp_real     (t-1)

panel <- base_panel %>%
  group_by(buyer_country) %>%
  arrange(tender_year, .by_group = TRUE) %>%
  mutate(
    # A
    A1_TCV = TCV,
    A2_N   = N,
    
    # B
    B1_AVG = AVG,
    B2_MED = if_else(!is.na(MED_clean), MED_clean, MED),
    
    # Lag dei denominatori
    primary_real_l1 = lag(primary_real, 1),
    gdp_real_l1     = lag(gdp_real, 1),
    
    # C
    C1_TCV_over_PRIMARY = if_else(!is.na(TCV) & !is.na(primary_real_l1) & primary_real_l1 > 0,
                                  TCV / primary_real_l1, NA_real_),
    C2_TCV_over_GDP     = if_else(!is.na(TCV) & !is.na(gdp_real_l1) & gdp_real_l1 > 0,
                                  TCV / gdp_real_l1,     NA_real_)
  ) %>%
  ungroup()

# 6) Funzioni diagnostiche ------------------------------------------------
diag_vec <- function(x){
  list(
    n_obs  = sum(!is.na(x)),
    n_zero = sum(x == 0, na.rm = TRUE),
    q      = as.numeric(quantile(x, c(.01,.05,.25,.5,.75,.95,.99), na.rm = TRUE))
  )
}

fe_r2 <- function(df, vname){
  m <- feols(as.formula(paste0(vname, " ~ 1 | buyer_country + tender_year")), data = df)
  as.numeric(fitstat(m, "r2"))
}

# 7) Diagnostica: distribuzioni, coperture, FE-R2, correlazioni -----------
cand_vars <- c("A1_TCV","A2_N","B1_AVG","B2_MED","C1_TCV_over_PRIMARY","C2_TCV_over_GDP")

# Distribuzioni
diag_list <- lapply(cand_vars, function(v){
  out <- diag_vec(panel[[v]])
  tibble(
    variable = v,
    n_obs  = out$n_obs,
    n_zero = out$n_zero,
    q01 = out$q[1], q05 = out$q[2], q25 = out$q[3], q50 = out$q[4],
    q75 = out$q[5], q95 = out$q[6], q99 = out$q[7]
  )
})
diagnostics_summary <- bind_rows(diag_list)

# Varianza assorbita da FE
r2_list <- tibble(
  variable = cand_vars,
  r2_fe = sapply(cand_vars, function(v) fe_r2(panel, v))
)

# Copertura per paese (dopo lag dove rileva)
coverage <- panel %>%
  summarise(
    across(all_of(cand_vars), ~ mean(!is.na(.)), .names = "cover_{.col}")
  )

# Correlazioni di "senso comune" con denominatori laggati
corr_safe <- function(a,b) if(all(is.na(a)) || all(is.na(b))) NA_real_ else cor(a,b,use="complete.obs")
corr_table <- tibble(
  variable = cand_vars,
  corr_with_primary_l1 = sapply(cand_vars, function(v) corr_safe(panel[[v]], panel$primary_real_l1)),
  corr_with_gdp_l1     = sapply(cand_vars, function(v) corr_safe(panel[[v]], panel$gdp_real_l1))
)

# 8) Output file di lavoro -----------------------------------------------
write_csv(panel %>% select(buyer_country, tender_year, all_of(cand_vars),
                           primary_real, primary_real_l1, gdp_real, gdp_real_l1),
          "outsourcing_indicators_panel.csv")

write_csv(diagnostics_summary, "outsourcing_indicators_diagnostics_summary.csv")
write_csv(r2_list,            "outsourcing_indicators_fe_r2.csv")
write_csv(corr_table,         "outsourcing_indicators_correlations.csv")

# 9) Stampa breve in console ---------------------------------------------
cat("\n=== DIAGNOSTICA INDICATORI (Step 1) ===\n")
print(diagnostics_summary, n = Inf)
cat("\n--- FE-only R2 (indicatore ~ FE paese + FE anno) ---\n")
print(r2_list, n = Inf)
cat("\n--- Correlazioni con denominatori laggati ---\n")
print(corr_table, n = Inf)

cat("\nFile creati:\n",
    "- outsourcing_indicators_panel.csv\n",
    "- outsourcing_indicators_diagnostics_summary.csv\n",
    "- outsourcing_indicators_fe_r2.csv\n",
    "- outsourcing_indicators_correlations.csv\n")

# ============================================================
# STEP 2 – Diagnostica indicatori di outsourcing (senza IT2016)
# Versione sicura con creazione lag variabili mancanti
# ============================================================

setwd("/Users/sofiaperondi/Desktop/Tesina IV /Data selection and summary analysis /Downloaded data")

library(tidyverse)
library(fixest)

cat("=== 🧮 Diagnostica indicatori di outsourcing - SENZA IT2016 ===\n\n")

# 1️⃣ Carica dataset generato nello step 1
panel <- read_csv("outsourcing_indicators_panel.csv")
cat("✅ Dataset caricato:", nrow(panel), "osservazioni totali\n\n")

# 2️⃣ Escludi IT2016
panel_noIT2016 <- panel %>%
  filter(!(buyer_country == "IT" & tender_year == 2016))

cat("📅 Osservazioni dopo esclusione IT2016:", nrow(panel_noIT2016), "\n")
cat("   Paesi:", n_distinct(panel_noIT2016$buyer_country),
    " | Anni:", paste(range(panel_noIT2016$tender_year), collapse = "–"), "\n\n")

# 3️⃣ Crea variabili lag se mancanti
panel_noIT2016 <- panel_noIT2016 %>%
  group_by(buyer_country) %>%
  arrange(tender_year, .by_group = TRUE) %>%
  mutate(
    primary_real_l1 = if ("primary_real" %in% names(.)) lag(primary_real, 1) else NA_real_,
    gdp_nominal_l1  = if ("gdp_nominal" %in% names(.)) lag(gdp_nominal, 1) else NA_real_
  ) %>%
  ungroup()

cat("✅ Variabili lag create (se mancanti): primary_real_l1, gdp_nominal_l1\n\n")

# 4️⃣ Lista indicatori da analizzare
indicators <- c("A1_TCV", "A2_N", "B1_AVG", "B2_MED",
                "C1_TCV_over_PRIMARY", "C2_TCV_over_GDP")

# 5️⃣ Diagnostica descrittiva completa
cat("=== 📊 Statistiche descrittive e quantili ===\n\n")

diagnostics_summary_noIT <- indicators %>%
  map_dfr(~{
    x <- panel_noIT2016[[.x]]
    tibble(
      variable = .x,
      n_obs = sum(!is.na(x)),
      n_zero = sum(x == 0, na.rm = TRUE),
      q01 = quantile(x, 0.01, na.rm = TRUE),
      q05 = quantile(x, 0.05, na.rm = TRUE),
      q25 = quantile(x, 0.25, na.rm = TRUE),
      q50 = quantile(x, 0.5, na.rm = TRUE),
      q75 = quantile(x, 0.75, na.rm = TRUE),
      q95 = quantile(x, 0.95, na.rm = TRUE),
      q99 = quantile(x, 0.99, na.rm = TRUE)
    )
  })

print(diagnostics_summary_noIT, n = Inf)

# 6️⃣ R² con soli effetti fissi paese + anno
cat("\n=== ⚙️ R² (Fixed Effects only) ===\n\n")

r2_list_noIT <- map_dfr(indicators, function(var) {
  model <- feols(as.formula(paste0(var, " ~ 1 | buyer_country + tender_year")),
                 data = panel_noIT2016)
  tibble(
    variable = var,
    r2_fe = as.numeric(fitstat(model, "r2"))
  )
})

print(r2_list_noIT, n = Inf)

# 7️⃣ Correlazioni con denominatori laggati
cat("\n=== 🔗 Correlazioni con variabili denominatore (lag) ===\n\n")

corr_table_noIT <- indicators %>%
  map_dfr(~{
    x <- panel_noIT2016[[.x]]
    tibble(
      variable = .x,
      corr_with_primary_l1 = if ("primary_real_l1" %in% names(panel_noIT2016))
        cor(x, panel_noIT2016$primary_real_l1, use = "pairwise.complete.obs") else NA_real_,
      corr_with_gdp_l1 = if ("gdp_nominal_l1" %in% names(panel_noIT2016))
        cor(x, panel_noIT2016$gdp_nominal_l1, use = "pairwise.complete.obs") else NA_real_
    )
  })

print(corr_table_noIT, n = Inf)

# 8️⃣ Salvataggio risultati
cat("\n=== 💾 File CSV salvati ===\n")
write_csv(diagnostics_summary_noIT, "outsourcing_indicators_diagnostics_noIT2016.csv")
write_csv(r2_list_noIT, "outsourcing_indicators_fe_r2_noIT2016.csv")
write_csv(corr_table_noIT, "outsourcing_indicators_correlations_noIT2016.csv")

cat("- outsourcing_indicators_diagnostics_noIT2016.csv\n")
cat("- outsourcing_indicators_fe_r2_noIT2016.csv\n")
cat("- outsourcing_indicators_correlations_noIT2016.csv\n")

# 9️⃣ Riassunto finale sintetico
cat("\n=== ✅ Diagnostica completata ===\n")
cat("• Indicatori analizzati:", paste(indicators, collapse = ", "), "\n")
cat("• Osservazioni analizzate:", nrow(panel_noIT2016), "\n")
cat("• IT2016 escluso per valori anomali in tender_digiwhist_price\n\n")
cat("Ora puoi confrontare i quantili e gli R² con la versione completa per capire l’impatto dell’esclusione di IT2016.\n")

# ============================================================
# STEP 2 – Diagnostica grafica e confronto indicatori outsourcing
# Focus: A2_N, B2_MED, C1_TCV_over_PRIMARY, C2_TCV_over_GDP
# ============================================================

rm(list = ls()); gc()
library(tidyverse)
library(scales)

# >>>> IMPOSTA LA TUA CARTELLA <<<<

cat("=== STEP 2: Diagnostica grafica indicatori outsourcing ===\n\n")

# --------------------------
# 0) Parametri
# --------------------------
exclude_it2016 <- TRUE                 # <-- metti FALSE se vuoi includerlo
sample_countries <- c("DE","FR","IT","ES","SE")  # paesi per i trend

# --------------------------
# 1) Caricamento dati
# --------------------------
panel <- read_csv("outsourcing_indicators_panel.csv",
                  show_col_types = FALSE)
stopifnot(all(c("buyer_country","tender_year") %in% names(panel)))

cat("✅ Caricato: outsourcing_indicators_panel.csv\n")
cat("   Righe:", nrow(panel), " | Colonne:", ncol(panel), "\n\n")

if (exclude_it2016) {
  panel <- panel %>% filter(!(buyer_country == "IT" & tender_year == 2016))
  cat("➡️  Escluso IT-2016. Nuove righe:", nrow(panel), "\n\n")
}

# Indicatori da analizzare
vars <- c("A2_N", "B2_MED", "C1_TCV_over_PRIMARY", "C2_TCV_over_GDP")
stopifnot(all(vars %in% names(panel)))

# --------------------------
# 2) Riassunti numerici
# --------------------------
cat("=== 📊 STATISTICHE DESCRITTIVE ===\n\n")
summ_tbl <- map_dfr(vars, function(v){
  x <- panel[[v]]
  tibble(
    variable = v,
    n_obs = sum(!is.na(x)),
    n_zero = sum(x == 0, na.rm = TRUE),
    q01 = quantile(x, 0.01, na.rm = TRUE),
    q05 = quantile(x, 0.05, na.rm = TRUE),
    q25 = quantile(x, 0.25, na.rm = TRUE),
    q50 = quantile(x, 0.50, na.rm = TRUE),
    q75 = quantile(x, 0.75, na.rm = TRUE),
    q95 = quantile(x, 0.95, na.rm = TRUE),
    q99 = quantile(x, 0.99, na.rm = TRUE)
  )
})
print(summ_tbl, n = Inf)

write_csv(summ_tbl, "STEP2_summary_indicators.csv")
cat("\n💾 Salvato: STEP2_summary_indicators.csv\n\n")

# --------------------------
# 3) Correlazioni pairwise
# --------------------------
cat("=== 🔗 CORRELAZIONI TRA INDICATORI (pairwise complete) ===\n\n")
corr_mat <- panel %>%
  select(all_of(vars)) %>%
  cor(use = "pairwise.complete.obs")
print(round(corr_mat, 3))

write_csv(
  as_tibble(corr_mat, rownames = "variable"),
  "STEP2_correlations_indicators.csv"
)
cat("\n💾 Salvato: STEP2_correlations_indicators.csv\n\n")

# ============================================================
# STEP 3 – TWFE cluster-robust
# Baseline: C1_TCV_over_PRIMARY lag t-2
# Robustness: C2_TCV_over_GDP (t-1, t-2), A2_N, B2_MED
# ============================================================

rm(list = ls()); gc()
library(tidyverse)
library(fixest)
library(modelsummary)

cat("=== STEP 3: TWFE cluster-robust (Paese) ===\n\n")

# --------------------------
# 1) Carica dataset
# --------------------------
# DV + controlli dinamici
macro <- read_csv("macro_primary_expenditure_with_growth_controls.csv",
                  show_col_types = FALSE)

# Indicatori outsourcing (A2, B2, C1, C2)
panel <- read_csv("outsourcing_indicators_panel.csv",
                  show_col_types = FALSE)

# Verifiche minime
stopifnot(all(c("buyer_country","tender_year","primary_growth_real") %in% names(macro)))
stopifnot(all(c("buyer_country","tender_year",
                "A2_N","B2_MED","C1_TCV_over_PRIMARY","C2_TCV_over_GDP") %in% names(panel)))

cat("✅ Caricati:\n")
cat("  - macro_primary_expenditure_with_growth_controls.csv (", nrow(macro), ")\n", sep = "")
cat("  - outsourcing_indicators_panel.csv (", nrow(panel), ")\n\n", sep = "")

# --------------------------
# 2) Merge
# --------------------------
df <- macro %>%
  select(
    buyer_country, tender_year, primary_growth_real,
    # controlli (dinamici + livelli usati fin qui)
    unemp_rate, unemp_growth,
    pop_density, pop_growth,
    gov_debt_pct_gdp,
    corruption_control, gov_state_market,
    recession, old_dep
  ) %>%
  left_join(
    panel %>% select(
      buyer_country, tender_year,
      A2_N, B2_MED, C1_TCV_over_PRIMARY, C2_TCV_over_GDP
    ),
    by = c("buyer_country","tender_year")
  )

cat("🔗 Merge completato. Righe:", nrow(df), "\n\n")

# --------------------------
# 3) Esclusione IT-2016 (outlier)
# --------------------------
df <- df %>% filter(!(buyer_country == "IT" & tender_year == 2016))
cat("➡️  IT-2016 escluso. Righe:", nrow(df), "\n\n")

# --------------------------
# 4) Crea lag t−1 e t−2 per C1 e C2
# --------------------------
df <- df %>%
  group_by(buyer_country) %>%
  arrange(tender_year, .by_group = TRUE) %>%
  mutate(
    C1_l1 = lag(C1_TCV_over_PRIMARY, 1),
    C1_l2 = lag(C1_TCV_over_PRIMARY, 2),
    C2_l1 = lag(C2_TCV_over_GDP, 1),
    C2_l2 = lag(C2_TCV_over_GDP, 2)
  ) %>%
  ungroup()

cat("✅ Lag creati: C1_l1, C1_l2, C2_l1, C2_l2\n")
cat("   N validi C1_l1:", sum(!is.na(df$C1_l1)), 
    " | C1_l2:", sum(!is.na(df$C1_l2)), "\n")
cat("   N validi C2_l1:", sum(!is.na(df$C2_l1)), 
    " | C2_l2:", sum(!is.na(df$C2_l2)), "\n\n")

# --------------------------
# 5) Definisci formula controlli + FE
# --------------------------
controls <- ~ unemp_rate + unemp_growth +
  pop_density + pop_growth +
  gov_debt_pct_gdp +
  corruption_control + gov_state_market +
  recession + old_dep

fe_spec <- ~ buyer_country + tender_year

# Helper stampa N (post NA omission)
count_complete <- function(formula_iv, data){
  f <- reformulate(termlabels = c(all.vars(formula_iv), 
                                  all.vars(controls),
                                  "buyer_country","tender_year",
                                  "primary_growth_real"),
                   response = NULL)
  sum(complete.cases(model.frame(f, data = data)))
}

# === Patch corretta: unisci gdp_growth dai controlli macro ===

ctrl_long <- read_csv("all_controls_complete_2006_2021.csv", show_col_types = FALSE) %>%
  transmute(
    buyer_country = geo,
    tender_year   = year,
    gdp_growth    = gdp_growth   # nel file esiste solo questa colonna
  ) %>%
  filter(tender_year >= 2011, tender_year <= 2021)

# Controllo duplicati (ci deve essere una riga per paese-anno)
dup_ctrl <- ctrl_long %>% count(buyer_country, tender_year) %>% filter(n > 1)
if (nrow(dup_ctrl) > 0) {
  print(dup_ctrl)
  stop("Duplicati in all_controls_complete_2006_2021.csv per (paese, anno). Risolvi e riprova.")
}

# Join con il dataset principale
df <- df %>%
  left_join(ctrl_long, by = c("buyer_country","tender_year"))

# Check rapido
cat("🔎 gdp_growth presente nel df? ", "gdp_growth" %in% names(df), "\n")
cat("   Missing gdp_growth dopo il join:", sum(is.na(df$gdp_growth)), "\n\n")



# =======================================================
# 5) Definizione controlli coerenti con modello testato
# =======================================================
controls <- ~ unemp_growth +
  pop_growth +
  gov_debt_pct_gdp +
  corruption_control +
  gov_state_market +
  gdp_growth

fe_spec <- ~ buyer_country + tender_year


# =======================================================
# 6) Funzione helper per costruire la formula completa
# =======================================================
make_formula <- function(dep, iv, controls, fe) {
  as.formula(paste0(
    dep, " ~ ", iv, " + ",
    paste(all.vars(controls), collapse = " + "),
    " | ", paste(all.vars(fe), collapse = " + ")
  ))
}

cat("📋 Formula di test:\n")
print(make_formula("primary_growth_real", "C1_l2", controls, fe_spec))


# =======================================================
# 7) Stime TWFE (cluster su buyer_country)
# =======================================================
m_base <- feols(
  fml = make_formula("primary_growth_real", "C1_l2", controls, fe_spec),
  data = df,
  cluster = ~ buyer_country
)

m_c1_l1 <- feols(fml = make_formula("primary_growth_real", "C1_l1", controls, fe_spec), data = df, cluster = ~ buyer_country)
m_c2_l1 <- feols(fml = make_formula("primary_growth_real", "C2_l1", controls, fe_spec), data = df, cluster = ~ buyer_country)
m_c2_l2 <- feols(fml = make_formula("primary_growth_real", "C2_l2", controls, fe_spec), data = df, cluster = ~ buyer_country)
m_a2    <- feols(fml = make_formula("primary_growth_real", "A2_N",   controls, fe_spec), data = df, cluster = ~ buyer_country)
m_b2    <- feols(fml = make_formula("primary_growth_real", "B2_MED", controls, fe_spec), data = df, cluster = ~ buyer_country)


# =======================================================
# 8) Tabella dei risultati (cluster-robust)
# =======================================================
cat("\n=== RISULTATI TWFE (coefficiente IV) ===\n\n")

etable(
  list(
    "Baseline: C1_l2" = m_base,
    "C1_l1"           = m_c1_l1,
    "C2_l1"           = m_c2_l1,
    "C2_l2"           = m_c2_l2,
    "A2_N"            = m_a2,
    "B2_MED"          = m_b2
  ),
  se = "cluster",
  cluster = "buyer_country",
  dict = c(
    "primary_growth_real" = "Δ Primary Expenditure (real) %",
    "C1_l2" = "C1 (TCV/Primary, t-2)",
    "C1_l1" = "C1 (TCV/Primary, t-1)",
    "C2_l1" = "C2 (TCV/GDP, t-1)",
    "C2_l2" = "C2 (TCV/GDP, t-2)",
    "A2_N"  = "A2 (Number of contracts)",
    "B2_MED"= "B2 (Median contract value)"
  ),
  fitstat = c("n", "r2"),  # <— usa solo R2 totale
  tex = FALSE
)


# =======================================================
# 9) Salvataggio tabella riassuntiva
# =======================================================
msummary(
  list(
    "Baseline: C1_l2" = m_base,
    "C1_l1"          = m_c1_l1,
    "C2_l1"          = m_c2_l1,
    "C2_l2"          = m_c2_l2,
    "A2_N"           = m_a2,
    "B2_MED"         = m_b2
  ),
  stars = TRUE,
  gof_omit = "IC|Log|Adj|AIC|BIC",
  output = "STEP3_TWFE_results.html"
)

cat("\n💾 Tabella risultati salvata: STEP3_TWFE_results.html\n")
cat("   (SE clusterizzati per buyer_country; FE: paese + anno)\n\n")
cat("=== ✅ STEP 3 completato ===\n")

# =======================================================
# 🔧 0) Setup di base
# =======================================================
library(tidyverse)
library(fixest)
library(modelsummary)

# Assicurati che 'df' contenga tutte le variabili già create
cat("✅ Dataset caricato:", nrow(df), "osservazioni\n\n")

# =======================================================
# 1️⃣ Crea lag a 3 anni per gli indicatori C1 e C2
# =======================================================
df <- df %>%
  group_by(buyer_country) %>%
  arrange(tender_year, .by_group = TRUE) %>%
  mutate(
    C1_l3 = lag(C1_TCV_over_PRIMARY, 3),
    C2_l3 = lag(C2_TCV_over_GDP, 3)
  ) %>%
  ungroup()

cat("📅 Lag a 3 anni creati: C1_l3, C2_l3\n")

# =======================================================
# 2️⃣ Definizione dei controlli coerenti
# =======================================================
controls <- ~ unemp_growth +
  pop_growth +
  gov_debt_pct_gdp +
  corruption_control +
  gov_state_market +
  gdp_growth

fe_spec <- ~ buyer_country + tender_year

# =======================================================
# 3️⃣ Funzione helper per creare la formula
# =======================================================
make_formula <- function(dep, iv, controls, fe) {
  as.formula(paste0(
    dep, " ~ ", iv, " + ",
    paste(all.vars(controls), collapse = " + "),
    " | ", paste(all.vars(fe), collapse = " + ")
  ))
}

cat("📋 Formula di test:\n")
print(make_formula("primary_growth_real", "C1_l3", controls, fe_spec))

# =======================================================
# 4️⃣ Stima TWFE cluster-robust (lag 3)
# =======================================================
m_c1_l3 <- feols(
  fml = make_formula("primary_growth_real", "C1_l3", controls, fe_spec),
  data = df,
  cluster = ~ buyer_country
)

m_c2_l3 <- feols(
  fml = make_formula("primary_growth_real", "C2_l3", controls, fe_spec),
  data = df,
  cluster = ~ buyer_country
)

cat("\n✅ Modelli stimati con successo\n")

# =======================================================
# 5️⃣ Tabella di confronto risultati (cluster-robust)
# =======================================================
etable(
  list(
    "C1_l3" = m_c1_l3,
    "C2_l3" = m_c2_l3
  ),
  se = "cluster",
  cluster = "buyer_country",
  dict = c(
    "primary_growth_real" = "Δ Primary Expenditure (real) %",
    "C1_l3" = "C1 (TCV/Primary, t-3)",
    "C2_l3" = "C2 (TCV/GDP, t-3)"
  ),
  fitstat = c("n", "r2"),
  tex = FALSE
)

# =======================================================
# 6️⃣ Esporta risultati HTML
# =======================================================
msummary(
  list(
    "C1_l3" = m_c1_l3,
    "C2_l3" = m_c2_l3
  ),
  stars = TRUE,
  gof_omit = "IC|Log|Adj|AIC|BIC",
  output = "STEP4_TWFE_LAG3_results.html"
)

cat("\n💾 Tabella salvata: STEP4_TWFE_LAG3_results.html\n")
cat("   (SE clusterizzati per buyer_country; FE: paese + anno)\n\n")

# =======================================================
# STEP – MEDIA MOBILE BIENNALE E TWFE CLUSTER-ROBUST
# =======================================================

library(dplyr)
library(fixest)
library(modelsummary)

# 1️⃣ Crea variabili media mobile biennale
df <- df %>%
  group_by(buyer_country) %>%
  arrange(tender_year, .by_group = TRUE) %>%
  mutate(
    C1_avg2 = (lag(C1_TCV_over_PRIMARY, 1) + lag(C1_TCV_over_PRIMARY, 2)) / 2,
    C2_avg2 = (lag(C2_TCV_over_GDP, 1) + lag(C2_TCV_over_GDP, 2)) / 2
  ) %>%
  ungroup()

cat("✅ Variabili media mobile biennale create (C1_avg2, C2_avg2)\n")
cat("   Osservazioni totali:", nrow(df), "\n\n")

# 2️⃣ Controllo base
cat("📋 Missing C1_avg2:", sum(is.na(df$C1_avg2)), "\n")
cat("📋 Missing C2_avg2:", sum(is.na(df$C2_avg2)), "\n\n")

# 3️⃣ Definizione dei controlli e FE
controls <- ~ unemp_growth +
  pop_growth +
  gov_debt_pct_gdp +
  corruption_control +
  gov_state_market +
  gdp_growth

fe_spec <- ~ buyer_country + tender_year

# 4️⃣ Funzione helper per la formula
make_formula <- function(dep, iv, controls, fe) {
  as.formula(paste0(
    dep, " ~ ", iv, " + ",
    paste(all.vars(controls), collapse = " + "),
    " | ", paste(all.vars(fe), collapse = " + ")
  ))
}

cat("📋 Formula di test:\n")
print(make_formula("primary_growth_real", "C1_avg2", controls, fe_spec))

# 5️⃣ Stime TWFE cluster-robust
m_c1_avg2 <- feols(
  fml = make_formula("primary_growth_real", "C1_avg2", controls, fe_spec),
  data = df,
  cluster = ~ buyer_country
)

m_c2_avg2 <- feols(
  fml = make_formula("primary_growth_real", "C2_avg2", controls, fe_spec),
  data = df,
  cluster = ~ buyer_country
)

cat("\n✅ Modelli stimati con successo\n")

# 6️⃣ Tabella di confronto
etable(
  list(
    "C1_avg2" = m_c1_avg2,
    "C2_avg2" = m_c2_avg2
  ),
  se = "cluster",
  cluster = "buyer_country",
  dict = c(
    "primary_growth_real" = "Δ Primary Expenditure (real) %",
    "C1_avg2" = "C1 (media mobile biennale, TCV/Primary)",
    "C2_avg2" = "C2 (media mobile biennale, TCV/GDP)"
  ),
  fitstat = c("n", "r2"),
  tex = FALSE
)

# 7️⃣ Esporta risultati in HTML per archivio
msummary(
  list(
    "C1_avg2" = m_c1_avg2,
    "C2_avg2" = m_c2_avg2
  ),
  stars = TRUE,
  gof_omit = "IC|Log|Adj|AIC|BIC",
  output = "STEP5_TWFE_moving_average_2years.html"
)

cat("\n💾 Risultati salvati: STEP5_TWFE_moving_average_2years.html\n")
cat("   (SE clusterizzati per buyer_country; FE: paese + anno)\n\n")









