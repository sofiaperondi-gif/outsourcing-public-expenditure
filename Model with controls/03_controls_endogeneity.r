# ===============================================================
# ANALISI ENDOGENEITÀ - MODELLO BASE TWFE
# ===============================================================

library(plm)
library(lmtest)
library(sandwich)
library(dplyr)
library(ggplot2)
library(car)
library(tidyverse)

setwd("~/Desktop/Tesina IV /Data analysis /Downloaded data")

# 1A) Carica e prepara il dataset macro (solo controlli)

final_dataset <- read_csv("FINAL_ANALYSIS_DATASET_2006_2021.csv") %>%
  filter(tender_year >= 2011)

file.choose()

# 1️⃣ Ricrea dataset "macro_updated" includendo tutte le variabili chiave
macro_updated <- final_dataset %>%
  group_by(buyer_country, tender_year) %>%
  summarise(
    total_expenditure_growth_real = first(total_expenditure_growth_real),
    gdp_growth_real = first(gdp_growth_real),          # ✅ PIL reale (cruciale)
    unemp_rate = first(unemp_rate),
    old_dep = first(old_dep),
    pop_density = first(pop_density),
    gov_debt_pct_gdp = first(gov_debt_pct_gdp),
    corruption_control = first(corruption_control),
    gov_state_market = first(gov_state_market),
    .groups = "drop"
  )

# 2️⃣ Controlla che tutte le variabili siano presenti
cat("✅ Variabili presenti nel dataset macro_updated:\n")
print(names(macro_updated))

# 3️⃣ Verifica che non ci siano NA gravi
cat("\n📊 Valori mancanti per gdp_growth_real:\n")
sum(is.na(macro_updated$gdp_growth_real))

# 4️⃣ Crea il pdata.frame per analisi panel
mp_updated <- pdata.frame(macro_updated, index = c("buyer_country", "tender_year"))



# === 1. MODELLO BASE TWFE ===
fml_base <- total_expenditure_growth_real ~ 
  gdp_growth_real + unemp_rate + old_dep + pop_density +
  gov_debt_pct_gdp + corruption_control + gov_state_market +
  factor(tender_year)

m_twfe <- plm(fml_base, data = mp_updated, model = "within")
coef_twfe <- coeftest(m_twfe, vcov = vcovHC(m_twfe, type = "HC1", cluster = "group"))

names(macro_updated)

cat("\n=== MODELLO BASE TWFE (cluster-robust SE) ===\n")
print(coef_twfe)

# Variabili potenzialmente endogene (economicamente plausibili)
suspected_vars <- c("gdp_growth_real", "unemp_rate", "gov_debt_pct_gdp")

# === 2. ANALISI DIAGNOSTICA ===
cat("\n=== TEST DIAGNOSTICI PER ENDOGENEITÀ ===\n")

# A) Correlazione residui vs variabili sospette
residuals_twfe <- resid(m_twfe)
for (v in suspected_vars) {
  test <- cor.test(mp_updated[[v]], residuals_twfe, use = "complete.obs")
  cat(sprintf("%-20s: r = %.3f (p = %.3f)\n", v, test$estimate, test$p.value))
}

# RICALCOLO CORRETTO DELLE CORRELAZIONI
cat("=== RICALCOLO CORRETTO CORRELAZIONI RESIDUI ===\n")

for (v in suspected_vars) {
  # Prepariamo i dati correttamente
  complete_cases <- complete.cases(twfe_data[[v]], residuals_twfe)
  x_var <- twfe_data[[v]][complete_cases]
  y_var <- residuals_twfe[complete_cases]
  
  # Calcoliamo correlazione e test
  correlation <- cor(x_var, y_var)
  test_result <- cor.test(x_var, y_var)
  
  cat(sprintf("%-20s: r = %.6f, p = %.6f, n = %d\n", 
              v, correlation, test_result$p.value, length(x_var)))
}

# Verifichiamo anche la distribuzione
cat("\n=== VERIFICA DISTRIBUZIONE ===\n")
cat("Varianza residui:", var(residuals_twfe, na.rm = TRUE), "\n")
cat("SD residui:", sd(residuals_twfe, na.rm = TRUE), "\n")
for (v in suspected_vars) {
  cat(sprintf("%-20s: var = %.6f, sd = %.6f\n", 
              v, var(twfe_data[[v]], na.rm = TRUE), sd(twfe_data[[v]], na.rm = TRUE)))
}

# === B) TEST RESET CORRETTO PER PANEL DATA ===

# Convertiamo in data.frame normale per il test RESET
twfe_data <- as.data.frame(mp_updated)
twfe_data$tender_year <- as.factor(twfe_data$tender_year)

# Stima modello lm equivalente con fixed effects
fml_lm <- total_expenditure_growth_real ~ 
  gdp_growth_real + unemp_rate + old_dep + pop_density +
  gov_debt_pct_gdp + corruption_control + gov_state_market +
  tender_year

m_lm <- lm(fml_lm, data = twfe_data)

# Test RESET standard
reset_test <- resettest(m_lm, power = 2:3, type = "fitted")
cat("RESET test risultato:\n")
print(reset_test)

# Test RESET manuale per controllo
twfe_data$fitted <- fitted(m_lm)
twfe_data$fitted2 <- twfe_data$fitted^2
twfe_data$fitted3 <- twfe_data$fitted^3

m_reset_manual <- lm(update(fml_lm, . ~ . + fitted2 + fitted3), data = twfe_data)
reset_manual_test <- anova(m_lm, m_reset_manual)
cat("\nRESET test manuale (F-test):\n")
print(reset_manual_test)

# Estrai p-value
p_value_reset <- reset_manual_test$`Pr(>F)`[2]
cat(sprintf("\nP-value RESET test: %.3f\n", p_value_reset))

if (p_value_reset < 0.05) {
  cat("→ RESET significativo: possibile misspecificazione\n")
} else {
  cat("→ RESET non significativo: forma funzionale adeguata\n")
}

# C) Test di correlazione seriale (PBG)
cat("\n=== TEST CORRELAZIONE SERIALE ===\n")
pbg_result <- pbgtest(m_twfe)
print(pbg_result)
if (pbg_result$p.value < 0.05) {
  cat("→ Presente correlazione seriale → possibile dinamica omessa\n")
} else {
  cat("→ Nessuna correlazione seriale\n")
}

# === 3. MODELLO DI ROBUSTEZZA SENZA VARIABILI SOSPETTE ===
cat("\n=== MODELLO SENZA VARIABILI POTENZIALMENTE ENDOGENE ===\n")

fml_no_endo <- total_expenditure_growth_real ~ 
  old_dep + pop_density + corruption_control + gov_state_market +
  factor(tender_year)

m_no_endo <- plm(fml_no_endo, data = mp_updated, model = "within")
coef_no_endo <- coeftest(m_no_endo, vcov = vcovHC(m_no_endo, type = "HC1", cluster = "group"))

print(coef_no_endo)

# === 4. CONFRONTO TRA MODELLI ===
cat("\n=== CONFRONTO COEFFICIENTI: MODELLO COMPLETO vs SENZA VARIABILI SOSPETTE ===\n")

common_vars <- intersect(names(coef(m_twfe)), names(coef(m_no_endo)))
comparison <- data.frame(
  Variable = common_vars,
  Full_Model = coef(m_twfe)[common_vars],
  No_Endo = coef(m_no_endo)[common_vars],
  Change_Pct = round((coef(m_no_endo)[common_vars] - coef(m_twfe)[common_vars]) / 
                       coef(m_twfe)[common_vars] * 100, 1)
)
print(comparison)

# === 5. GRAFICI DIAGNOSTICI ===
cat("\n=== GRAFICI DIAGNOSTICI: RESIDUI vs VARIABILI SOSPETTE ===\n")

for (v in suspected_vars) {
  ggplot(mp_updated, aes_string(x = v, y = residuals_twfe)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = paste("Residui TWFE vs", v),
         subtitle = "Pattern lineare = endogeneità potenziale",
         x = v, y = "Residui") +
    theme_minimal() -> p
  print(p)
}

# === 6. SINTESI INTERPRETATIVA ===
cat("\n=== SINTESI ===\n")

cat("1️⃣ Correlazione residui-variabili: valori di r > 0.2 o p < 0.05 suggeriscono endogeneità\n")
cat("2️⃣ RESET significativo → possibile specificazione errata\n")
cat("3️⃣ PBG significativo → autocorrelazione residui → dinamica omessa\n")
cat("4️⃣ Confronto tra modelli → variazioni > 20% indicano sensibilità\n")

cat("\nConclusione per la tesi:\n")
cat("• Le variabili gdp_growth_real, unemp_rate e gov_debt_pct_gdp sono economicamente plausibili candidati endogeni.\n")
cat("• Le evidenze empiriche (correlazioni, RESET, PBG) permettono di motivare l’uso di strumenti o GMM.\n")
cat("• Nel report, interpretare questi risultati come segnale di simultaneità potenziale, non di bias certo.\n")

