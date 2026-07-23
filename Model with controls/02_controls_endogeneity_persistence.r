# CARICHIAMO TUTTI I PACCHETTI NECESSARI
install.packages("tseries")
library(plm)
library(lmtest)
library(sandwich)
library(car)
library(urca)  # Per test stazionarietà
library(tseries) # Per ADF test

cat("\n🔧 CORREZIONE PROBLEMA PDATA.FRAME:\n")
mp_regular <- as.data.frame(mp_updated)
cat("Convertito pdata.frame in data.frame normale\n")
cat("Dimensioni:", dim(mp_regular), "\n")

# TEST STAZIONARIETÀ - CON APPROCCIO SISTEMATICO
cat("\n=== RIPRENDIAMO CON APPROCCIO SISTEMATICO ===\n")

cat("1.1 TEST STAZIONARIETÀ - VARIABILE DIPENDENTE:\n")
cat("ADF test per total_expenditure_growth_real:\n")

# Facciamo il test con opzioni esplicite
adf_result <- adf.test(na.omit(mp_regular$total_expenditure_growth_real), 
                       k = 1)  # k = 1 lag per semplicità

cat("Statistica test:", adf_result$statistic, "\n")
cat("p-value:", adf_result$p.value, "\n")
cat("Lags:", adf_result$parameter, "\n")

# Interpretazione attenta
if(adf_result$p.value < 0.01) {
  cat("→ La serie è FORTEMENTE STAZIONARIA (p < 0.01)\n")
  cat("→ I modelli in livelli sono appropriati\n")
} else if(adf_result$p.value < 0.05) {
  cat("→ La serie è stazionaria (p < 0.05)\n")
} else if(adf_result$p.value < 0.1) {
  cat("→ La serie è debolmente stazionaria (p < 0.1)\n")
} else {
  cat("→ Possibile radice unitaria (p > 0.1)\n")
}

cat("=== FASE 3: ANALISI SERIA DELLA PERSISTENZA ===\n")
cat("Obiettivo: Capire se la spesa pubblica ha memoria nel tempo\n")

cat("3.1 MISURA DELLA PERSISTENZA - APPROCCIO ROBUSTO:\n")

# Calcoliamo autocorrelazioni per diversi lag
lags <- 1:3
autocorrelations <- sapply(lags, function(lag_val) {
  cor(mp_regular$total_expenditure_growth_real, 
      lag(mp_regular$total_expenditure_growth_real, lag_val), 
      use = "complete.obs", method = "pearson")
})

persistence_df <- data.frame(
  Lag = lags,
  Autocorrelation = round(autocorrelations, 3),
  Interpretation = c(
    ifelse(abs(autocorrelations[1]) > 0.5, "Forte persistenza annuale", 
           ifelse(abs(autocorrelations[1]) > 0.3, "Persistenza moderata", "Persistenza debole")),
    "Persistenza a 2 anni",
    "Persistenza a 3 anni"
  )
)

print(persistence_df)

# CORREZIONE PROBLEMA LAG
cat("=== CORREZIONE PROBLEMA TECNICO LAG ===\n")

# Approccio più robusto per creare il lag
mp_dynamic_prep <- mp_regular %>%
  arrange(buyer_country, tender_year) %>%  # Ordiniamo prima
  group_by(buyer_country) %>%
  mutate(
    lag1_growth = dplyr::lag(total_expenditure_growth_real, n = 1, order_by = tender_year)
  ) %>%
  ungroup() %>%
  filter(!is.na(lag1_growth))  # Rimuoviamo osservazioni senza lag

cat("Osservazioni per modello dinamico:", nrow(mp_dynamic_prep), "\n")
cat("Paesi nel modello dinamico:", length(unique(mp_dynamic_prep$buyer_country)), "\n")

# Verifichiamo che il lag sia stato creato correttamente
cat("\nVerifica lag - prime 6 osservazioni:\n")
print(head(mp_dynamic_prep[, c("buyer_country", "tender_year", "total_expenditure_growth_real", "lag1_growth")]))

# ANALISI PERSISTENZA SEMPLICE
cat("\n3.1 ANALISI PERSISTENZA BASE:\n")

# Calcoliamo autocorrelazione semplice
lag1_cor <- cor(mp_dynamic_prep$total_expenditure_growth_real, 
                mp_dynamic_prep$lag1_growth, 
                use = "complete.obs")

cat("Autocorrelazione lag-1:", round(lag1_cor, 3), "\n")

# Interpretazione
if(abs(lag1_cor) > 0.5) {
  cat("→ FORTE PERSISTENZA: autocorrelazione > 0.5\n")
} else if(abs(lag1_cor) > 0.3) {
  cat("→ PERSISTENZA MODERATA: autocorrelazione 0.3-0.5\n")
} else {
  cat("→ PERSISTENZA DEBOLE: autocorrelazione < 0.3\n")
}

# Distribuzione della persistenza per paese
cat("\n3.2 PERSISTENZA PER PAESE:\n")

persistence_by_country <- mp_dynamic_prep %>%
  group_by(buyer_country) %>%
  summarise(
    persistence = cor(total_expenditure_growth_real, lag1_growth, use = "complete.obs"),
    n_obs = n()
  ) %>%
  filter(n_obs >= 3)  # Almeno 3 osservazioni per calcolo sensato

print(persistence_by_country[order(abs(persistence_by_country$persistence), decreasing = TRUE), ])

# MODELLO DINAMICO SEMPLICE
cat("\n3.3 MODELLO DINAMICO SEMPLICE:\n")

# Usiamo plm invece di lm per essere consistenti
mp_dynamic_pdata <- pdata.frame(mp_dynamic_prep, 
                                index = c("buyer_country", "tender_year"))

m_dynamic_plm <- plm(total_expenditure_growth_real ~ 
                       lag1_growth +
                       gdp_growth_real + unemp_rate + old_dep +
                       gov_debt_pct_gdp + gov_state_market +
                       factor(tender_year),
                     data = mp_dynamic_pdata, 
                     model = "within")

coef_dynamic <- coeftest(m_dynamic_plm, 
                         vcov = vcovHC(m_dynamic_plm, type = "HC1", cluster = "group"))

cat("Modello dinamico - coefficiente persistenza:\n")
print(coef_dynamic["lag1_growth", ])

# Confronto con modello statico
cat("\n3.4 CONFRONTO CON MODELLO STATICO:\n")

# Ristimiamo modello statico sullo stesso campione per confronto equo
m_static_comparable <- plm(total_expenditure_growth_real ~ 
                             gdp_growth_real + unemp_rate + old_dep +
                             gov_debt_pct_gdp + gov_state_market +
                             factor(tender_year),
                           data = mp_dynamic_pdata, 
                           model = "within")

coef_static <- coeftest(m_static_comparable, 
                        vcov = vcovHC(m_static_comparable, type = "HC1", cluster = "group"))

comparison <- data.frame(
  Variable = c("gdp_growth_real", "unemp_rate", "old_dep", "gov_debt_pct_gdp", "gov_state_market"),
  Static = coef_static[c("gdp_growth_real", "unemp_rate", "old_dep", "gov_debt_pct_gdp", "gov_state_market"), 1],
  Dynamic = coef_dynamic[c("gdp_growth_real", "unemp_rate", "old_dep", "gov_debt_pct_gdp", "gov_state_market"), 1],
  Change_Pct = round((coef_dynamic[c("gdp_growth_real", "unemp_rate", "old_dep", "gov_debt_pct_gdp", "gov_state_market"), 1] - 
                        coef_static[c("gdp_growth_real", "unemp_rate", "old_dep", "gov_debt_pct_gdp", "gov_state_market"), 1]) / 
                       abs(coef_static[c("gdp_growth_real", "unemp_rate", "old_dep", "gov_debt_pct_gdp", "gov_state_market"), 1]) * 100, 1)
)

print(comparison)

library(tseries)

cat("=== TEST ADF PER VARIABILI ESPLICATIVE ===\n")

vars_to_test <- c("gdp_growth_real", "unemp_rate", "old_dep", 
                  "gov_debt_pct_gdp", "gov_state_market")

adf_results <- lapply(vars_to_test, function(v) {
  x <- na.omit(mp_regular[[v]])
  test <- adf.test(x, k = 1)
  data.frame(
    Variable = v,
    Statistic = round(test$statistic, 3),
    P_value = test$p.value,
    Lags = test$parameter
  )
})

adf_results_df <- do.call(rbind, adf_results)
print(adf_results_df)

# Interpretazione automatica
adf_results_df$Interpretation <- ifelse(
  adf_results_df$P_value < 0.01, "Fortemente stazionaria (p<0.01)",
  ifelse(adf_results_df$P_value < 0.05, "Stazionaria (p<0.05)",
         ifelse(adf_results_df$P_value < 0.1, "Debolmente stazionaria (p<0.1)",
                "Possibile radice unitaria (p>0.1)"))
)

print(adf_results_df)

