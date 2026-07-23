
# 1. Pulisci memoria
rm(list = ls())
gc()

install.packages("kableExtra", repos = "https://cloud.r-project.org")
library(tidyverse)
library(plm)
library(car)

setwd("~/Desktop/Tesina IV /Data analysis /Downloaded data")

# 1A) Carica e prepara il dataset macro (solo controlli)

final_dataset <- read_csv("FINAL_ANALYSIS_DATASET_2006_2021.csv") %>%
  filter(tender_year >= 2011)

macro <- final_dataset %>%
  group_by(buyer_country, tender_year) %>%
  summarise(
    total_expenditure_growth_real = first(total_expenditure_growth_real),
    unemp_rate = first(unemp_rate),
    recession = first(recession),
    old_dep = first(old_dep),
    pop_density = first(pop_density),
    gov_debt_pct_gdp = first(gov_debt_pct_gdp),
    corruption_control = first(corruption_control),
    gov_state_market = first(gov_state_market),
    .groups = "drop"
  )

cat("Paesi:", n_distinct(macro$buyer_country), 
    "| Anni:", length(unique(macro$tender_year)),
    "| Osservazioni:", nrow(macro), "\n")

summary(macro$total_expenditure_growth_real)
cat("Min growth:", min(macro$total_expenditure_growth_real, na.rm=TRUE), "\n")
cat("% valori negativi:", mean(macro$total_expenditure_growth_real < 0, na.rm=TRUE)*100, "%\n")

table(macro$tender_year)
table(table(macro$buyer_country))   # utile per capire se panel bilanciato

# 1B) Struttura generale del panel
cat("Paesi:", n_distinct(macro$buyer_country), "\n")
cat("Anni:", length(unique(macro$tender_year)), "\n")
cat("Osservazioni totali:", nrow(macro), "\n")

# 1C) Distribuzione della variabile dipendente
summary(macro$total_expenditure_growth_real)
cat("Valori negativi (%):",
    mean(macro$total_expenditure_growth_real < 0, na.rm = TRUE) * 100, "\n")

# 1D) Variazione within vs between
# media per paese e scarto tipo within
macro_var <- macro %>%
  group_by(buyer_country) %>%
  summarise(
    var_within_unemp = sd(unemp_rate - mean(unemp_rate, na.rm = TRUE), na.rm = TRUE),
    var_total_unemp  = sd(unemp_rate, na.rm = TRUE),
    var_within_popdens = sd(pop_density - mean(pop_density, na.rm = TRUE), na.rm = TRUE),
    var_total_popdens  = sd(pop_density, na.rm = TRUE)
  ) %>%
  summarise(across(starts_with("var_"), mean, na.rm = TRUE))

print(macro_var)
# Se var_within << var_total → variabile quasi fissa nel tempo

# 1E) Correlazioni e collinearità di base
cor_matrix <- macro %>%
  select(unemp_rate, recession, old_dep, pop_density,
         gov_debt_pct_gdp, corruption_control, gov_state_market) %>%
  cor(use = "pairwise.complete.obs")
round(cor_matrix, 2)

# (opzionale) VIF su un modello pooled "di servizio"
model_temp <- lm(total_expenditure_growth_real ~ unemp_rate + recession + old_dep + pop_density +
                   gov_debt_pct_gdp + corruption_control + gov_state_market,
                 data = macro)
vif(model_temp)

# Controllo su pop_density
# andamento nel tempo per alcuni paesi
library(ggplot2)

ggplot(macro, aes(x = tender_year, y = pop_density, group = buyer_country)) +
  geom_line(alpha = 0.4) +
  labs(title = "Evoluzione della densità di popolazione nel tempo",
       x = "Anno", y = "Popolazione per km²") +
  theme_minimal()

# variazione percentuale anno-su-anno per ogni paese
macro_density_var <- macro %>%
  group_by(buyer_country) %>%
  arrange(tender_year) %>%
  mutate(
    diff_density = (pop_density - lag(pop_density)) / lag(pop_density) * 100
  )

summary(macro_density_var$diff_density)

# coefficiente di variazione within per paese
cv_density <- macro %>%
  group_by(buyer_country) %>%
  summarise(cv_popdens = sd(pop_density, na.rm = TRUE) / mean(pop_density, na.rm = TRUE))
summary(cv_density$cv_popdens)

cv_density %>% arrange(desc(cv_popdens)) %>% head(3)

# Step 2: diagnostica della forma funzionale 
controls <- c("unemp_rate", "gov_debt_pct_gdp",
              "corruption_control", "pop_density")

for (x in controls) {
  ggplot(macro, aes_string(x = x, y = "total_expenditure_growth_real")) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +
    labs(title = paste("Crescita spesa pubblica vs", x),
         x = x, y = "Crescita spesa pubblica (%)") +
    theme_minimal() -> p
  print(p)
}

for (x in controls) {
  p <- ggplot(macro, aes_string(x = x, y = "total_expenditure_growth_real")) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +
    labs(title = paste("Crescita spesa pubblica vs", x),
         x = x, y = "Crescita spesa pubblica (%)") +
    theme_minimal()
  print(p)
  Sys.sleep(1)  # permette di vederli tutti
}

# Verifica globale della linearità 
library(lmtest)

model_reset <- lm(
  total_expenditure_growth_real ~ unemp_rate + recession + old_dep + pop_density +
    gov_debt_pct_gdp + corruption_control + gov_state_market,
  data = macro
)

resettest(model_reset)

#Per vedere la parte residua 
avPlots(model_reset,
        terms = ~ unemp_rate + gov_debt_pct_gdp + corruption_control,
        ask = FALSE)

# Step 3: selezione e confronto modelli Panel 
install.packages("clubSandwich")
library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)
library(clubSandwich)   # per vcovCR (cluster-robust generico)
# Dati già pronti: macro (come da STEP 1–2)
mp <- pdata.frame(macro, index = c("buyer_country","tender_year"))

# Formula base (solo controlli)
fml <- total_expenditure_growth_real ~ unemp_rate + recession + old_dep + pop_density +
  gov_debt_pct_gdp + corruption_control + gov_state_market

# Pooled OLS
m_pool <- plm(fml, data = mp, model = "pooling")

# Between (long-run strutturale tra paesi)
m_between <- plm(fml, data = mp, model = "between")

# First differences (dinamica breve)
m_fd <- plm(fml, data = mp, model = "fd")

# Fixed Effects (within, one-way: paese)
m_fe <- plm(fml, data = mp, model = "within", effect = "individual")

# Random Effects (GLS)
m_re <- plm(fml, data = mp, model = "random")

# Two-Way Fixed Effects (paese + anno)
m_twfe <- plm(update(fml, . ~ . + factor(tender_year)), data = mp, model = "within")

# FE vs Pooled
test_F_FE_pool <- pFtest(m_fe, m_pool)

# RE vs Pooled (Breusch–Pagan Lagrange Multiplier)
test_BP_RE_pool <- plmtest(m_pool, type = "bp")

# FE vs RE (Hausman) — versione classica
test_Hausman <- phtest(m_fe, m_re)

# Necessità effetti temporali (test su time effects)
test_time_FE <- plmtest(m_fe, effect = "time", type = "bp")

cat("F(FE vs pooled) p =", test_F_FE_pool$p.value, "\n")
cat("BP(RE vs pooled) p =", test_BP_RE_pool$p.value, "\n")
cat("Hausman(FE vs RE) p =", test_Hausman$p.value, "\n")
cat("Time effects p =", test_time_FE$p.value, "\n")

# Autocorrelazione seriale (Wooldridge/Baltagi–Wu)
test_ar_fe <- pbgtest(m_fe)

# Eteroschedasticità (Breusch–Pagan su FE)
test_het_fe <- bptest(m_fe)

# (Opzionale) dipendenza cross-section (Pesaran CD)
# library(pcse) o plm::pcdtest; qui usiamo pcdtest su FE a due vie semplificato
test_csdep <- pcdtest(m_twfe, test = "cd")

cat("Serial corr (FE) p =", test_ar_fe$p.value, "\n")
cat("Heterosk (FE) p =", test_het_fe$p.value, "\n")
cat("Cross-sec dep (TWFE) p =", test_csdep$p.value, "\n")

# Cluster-robust su paese (consigliato come default)
vcov_cluster <- function(mod) vcovHC(mod, method = "arellano", type = "HC1", cluster = "group")

# Driscoll–Kraay per FE/TWFE (N grande, T moderato)
vcov_DK <- function(mod) vcovSCC(mod, type = "HC1", maxlag = 2)  # regola 'maxlag' se serve

# Esempio: coefficienti FE con cluster su paese
coeftest(m_fe, vcov = vcov_cluster(m_fe))

# Esempio: FE con Driscoll–Kraay
coeftest(m_fe, vcov = vcov_DK(m_fe))

# TWFE con cluster
coeftest(m_twfe, vcov = vcov_cluster(m_twfe))

mundlak_ols <- lm(total_expenditure_growth_real ~ 
                    unemp_rate + recession + old_dep + pop_density +
                    gov_debt_pct_gdp + corruption_control + gov_state_market +
                    unemp_rate_mean + recession_mean,
                  data = macro_mundlak)

coeftest(mundlak_ols, vcov = vcovCL, cluster = ~buyer_country)

extract_coefs <- function(model, model_name) {
  # Prova a capire il tipo di modello guardando all'oggetto 'model'
  model_type <- NA
  if ("plm" %in% class(model)) {
    if (!is.null(model$args$model)) {
      model_type <- model$args$model
    } else if (!is.null(attr(model, "args")$model)) {
      model_type <- attr(model, "args")$model
    } else {
      model_type <- "other"
    }
  }
  
  # Scegli il tipo di var-cov in base al modello
  if (model_type == "between") {
    vc <- tryCatch(vcov(model), error = function(e) NULL)
  } else {
    vc <- tryCatch(vcovHC(model, type = "HC1", cluster = "group"),
                   error = function(e) vcov(model))
  }
  
  if (is.null(vc)) {
    warning(paste("⚠️ Var-cov non calcolabile per", model_name))
    return(tibble(
      Modello = model_name,
      Variabile = names(coef(model)),
      Coefficiente = round(coef(model), 4),
      StdError = NA,
      t_value = NA,
      p_value = NA,
      Signif = "",
      Coeff_sig = round(coef(model), 4)
    ))
  }
  
  coefs <- coef(model)
  robust_se <- sqrt(diag(vc))
  tvals <- coefs / robust_se
  pvals <- 2 * pt(-abs(tvals), df = model$df.residual)
  
  tibble(
    Modello = model_name,
    Variabile = names(coefs),
    Coefficiente = round(coefs, 4),
    StdError = round(robust_se, 4),
    t_value = round(tvals, 2),
    p_value = round(pvals, 4),
    Signif = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.1  ~ "*",
      TRUE ~ ""
    ),
    Coeff_sig = paste0(Coefficiente, Signif)
  )
}
res_pool   <- extract_coefs(m_pool, "Pooled OLS")
res_between <- extract_coefs(m_between, "Between Effects")
res_fd     <- extract_coefs(m_fd, "First Differences")
res_fe     <- extract_coefs(m_fe, "Fixed Effects (within)")
res_re     <- extract_coefs(m_re, "Random Effects (GLS)")
res_twfe   <- extract_coefs(m_twfe, "Two-Way FE")

results_all <- bind_rows(res_pool, res_between, res_fd, res_fe, res_re, res_twfe)

results_summary <- results_all %>%
  select(Modello, Variabile, Coeff_sig, StdError, p_value)

print(results_summary, n = 30)

# Assicurati che mp esista - se non esiste, ricreiamolo
if (!exists("mp")) {
  mp <- pdata.frame(macro, index = c("buyer_country", "tender_year"))
}

# Correzione della funzione hausman_robust_test
hausman_robust_test <- function(fe_model, re_model, data) {
  # Coefficienti dai due modelli
  beta_fe <- coef(fe_model)
  beta_re <- coef(re_model)
  
  # Prendiamo solo i coefficienti in comune (escludiamo l'intercetta dal RE)
  common_vars <- names(beta_fe)[names(beta_fe) %in% names(beta_re)]
  beta_fe_common <- beta_fe[common_vars]
  beta_re_common <- beta_re[common_vars]
  
  # Differenza nei coefficienti
  diff_beta <- beta_fe_common - beta_re_common
  
  # Matrici di varianza robuste - correggiamo l'argomento cluster
  vcov_fe <- vcovCR(fe_model, cluster = data$buyer_country, type = "CR1")
  vcov_re <- vcovCR(re_model, cluster = data$buyer_country, type = "CR1")
  
  # Prendiamo le sottomatrici per le variabili in comune
  vcov_fe_common <- vcov_fe[common_vars, common_vars]
  vcov_re_common <- vcov_re[common_vars, common_vars]
  
  # Varianza della differenza
  vcov_diff <- vcov_fe_common - vcov_re_common
  
  # Statistica test
  hausman_stat <- t(diff_beta) %*% solve(vcov_diff) %*% diff_beta
  df <- length(diff_beta)
  p_value <- pchisq(hausman_stat, df = df, lower.tail = FALSE)
  
  return(list(
    statistic = as.numeric(hausman_stat),
    df = df,
    p_value = p_value,
    method = "Robust Hausman Test (CR1)"
  ))
}

# Ora eseguiamo il test passando i dati correttamente
hausman_robust_result <- hausman_robust_test(m_fe, m_re, macro)
print(hausman_robust_result)

# APPROCCIO SEMPLIFICATO E PIÙ AFFIDABILE
# Test di Hausman con correzione per eteroschedasticità
library(lmtest)

hausman_semi_robust <- phtest(m_fe, m_re, 
                              vcov = function(x) vcovHC(x, type = "HC1"))
print(hausman_semi_robust)

# APPROCCIO ALTERNATIVO: usa waldtest da lmtest (se wald_test non funziona)
# Controlla se le funzioni necessarie sono caricate
library(clubSandwich)

# Metodo diretto con clubSandwich
hausman_direct <- function(fe_model, re_model) {
  beta_fe <- coef(fe_model)
  beta_re <- coef(re_model)
  
  common_vars <- names(beta_fe)[names(beta_fe) %in% names(beta_re)]
  beta_fe_common <- beta_fe[common_vars]
  beta_re_common <- beta_re[common_vars]
  
  diff_beta <- beta_fe_common - beta_re_common
  
  # Usa i dati dal modello FE per ottenere i cluster
  model_data <- model.frame(fe_model)
  clusters <- model_data[[1]]  # Prima colonna dovrebbe essere l'identificatore del paese
  
  vcov_fe <- vcovCR(fe_model, type = "CR1")
  vcov_re <- vcovCR(re_model, type = "CR1")
  
  vcov_fe_common <- vcov_fe[common_vars, common_vars]
  vcov_re_common <- vcov_re[common_vars, common_vars]
  
  vcov_diff <- vcov_fe_common - vcov_re_common
  
  # Controlla che la matrice sia invertibile
  if (any(eigen(vcov_diff)$values <= 0)) {
    warning("Matrice vcov_diff non è definita positiva. Usando pseudoinversa.")
    hausman_stat <- t(diff_beta) %*% MASS::ginv(vcov_diff) %*% diff_beta
  } else {
    hausman_stat <- t(diff_beta) %*% solve(vcov_diff) %*% diff_beta
  }
  
  df <- length(diff_beta)
  p_value <- pchisq(hausman_stat, df = df, lower.tail = FALSE)
  
  return(list(
    statistic = as.numeric(hausman_stat),
    df = df,
    p_value = p_value,
    method = "Robust Hausman Test"
  ))
}

# Prova il metodo diretto
hausman_direct_result <- hausman_direct(m_fe, m_re)
print(hausman_direct_result)

# CONFRONTO FINALE
cat("\n=== CONFRONTO RISULTATI HAUSMAN ===\n")
cat("1. Hausman classico (omoschedastico):\n")
cat("   p-value =", hausman_standard$p.value, "\n")
cat("   Decisione:", ifelse(hausman_standard$p.value < 0.05, "FE", "RE"), "\n\n")

if (exists("hausman_robust_result")) {
  cat("2. Hausman robust (clubSandwich):\n")
  cat("   p-value =", hausman_robust_result$p_value, "\n")
  cat("   Decisione:", ifelse(hausman_robust_result$p_value < 0.05, "FE", "RE"), "\n\n")
}

cat("3. Hausman semi-robust (vcovHC):\n")
cat("   p-value =", hausman_semi_robust$p.value, "\n")
cat("   Decisione:", ifelse(hausman_semi_robust$p.value < 0.05, "FE", "RE"), "\n\n")

# RACCOMANDAZIONE FINALE BASATA SUI TEST ROBUSTI
final_p_value <- if (exists("hausman_robust_result")) {
  hausman_robust_result$p_value
} else {
  hausman_semi_robust$p.value
}

cat("=== RACCOMANDAZIONE FINALE ===\n")
if (final_p_value < 0.05) {
  cat("✅ TESTO ROBUSTI: p-value =", round(final_p_value, 4), "\n")
  cat("✅ SCELTA: Fixed Effects (FE) - effetti correlati con regressori\n")
} else {
  cat("❌ TESTO ROBUSTI: p-value =", round(final_p_value, 4), "\n")
  cat("✅ SCELTA: Random Effects (RE) - più efficiente\n")
  cat("   (Il test classico era probabilmente influenzato da eteroschedasticità)\n")
}




