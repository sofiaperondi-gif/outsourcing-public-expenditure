# === MODELLO DINAMICO CON SOLI CONTROLLI ===

library(plm)
library(lmtest)
library(sandwich)
library(dplyr)
library(tidyverse)

setwd("~/Desktop/Tesina IV /Data analysis /Downloaded data")

# 1A) Carica e prepara il dataset macro (solo controlli)

final_dataset <- read_csv("FINAL_ANALYSIS_DATASET_2006_2021.csv") %>%
  filter(tender_year >= 2011)


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

# 1️⃣ Crea il lag della variabile dipendente per ciascun paese
mp_dynamic <- macro_updated %>%
  group_by(buyer_country) %>%
  arrange(tender_year) %>%
  mutate(
    lag_growth = lag(total_expenditure_growth_real, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(lag_growth))  # rimuovi le prime osservazioni senza lag

# 2️⃣ Crea pdata.frame
mp_dynamic_pdata <- pdata.frame(mp_dynamic, index = c("buyer_country", "tender_year"))

# 3️⃣ Stima modello dinamico
fml_dynamic <- total_expenditure_growth_real ~ lag_growth + gdp_growth_real + 
  unemp_rate + old_dep + gov_debt_pct_gdp + gov_state_market + 
  factor(tender_year)

m_dynamic <- plm(fml_dynamic, data = mp_dynamic_pdata, model = "within")
coef_dynamic <- coeftest(m_dynamic, vcov = vcovHC(m_dynamic, type = "HC1", cluster = "group"))

cat("\n=== MODELLO DINAMICO TWFE ===\n")
print(coef_dynamic)

# 4️⃣ Interpreta il coefficiente del lag
lag_coef <- coef_dynamic["lag_growth", 1]
lag_pval <- coef_dynamic["lag_growth", 4]
cat("\nCoeff. lag =", round(lag_coef, 3), "p =", round(lag_pval, 3), "\n")

if (lag_pval < 0.05) {
  if (lag_coef > 0.5) {
    cat("→ FORTE persistenza: comportamento altamente inerziale\n")
  } else if (lag_coef > 0.2) {
    cat("→ PERSISTENZA moderata: inerzia significativa ma limitata\n")
  } else {
    cat("→ PERSISTENZA debole: comportamento mean-reverting\n")
  }
} else {
  cat("→ Nessuna persistenza significativa\n")
}

# 5️⃣ Confronto con modello statico
m_static <- plm(
  total_expenditure_growth_real ~ gdp_growth_real + unemp_rate + old_dep + 
    gov_debt_pct_gdp + gov_state_market + factor(tender_year),
  data = mp_dynamic_pdata, model = "within"
)
coef_static <- coeftest(m_static, vcov = vcovHC(m_static, type = "HC1", cluster = "group"))

cat("\n=== CONFRONTO COEFFICIENTI STATICO vs DINAMICO ===\n")
comparison <- data.frame(
  Variable = intersect(names(coef(m_static)), names(coef(m_dynamic))),
  Static = coef(m_static)[intersect(names(coef(m_static)), names(coef(m_dynamic)))],
  Dynamic = coef(m_dynamic)[intersect(names(coef(m_static)), names(coef(m_dynamic)))]
)
print(comparison)

# === 6️⃣ TEST DIAGNOSTICI SUL MODELLO DINAMICO ===

library(lmtest)

cat("\n=== TEST DIAGNOSTICI SUL MODELLO DINAMICO ===\n")

# === TEST RAMSEY RESET (corretto) ===

# 1️⃣ Estrai i dati del modello dinamico
dynamic_data <- mp_dynamic %>%
  mutate(
    tender_year = as.factor(tender_year)  # assicurati che sia fattore
  )

# 2️⃣ Crea un modello lm() equivalente (con effetti fissi "espliciti")
lm_dynamic_equiv <- lm(
  total_expenditure_growth_real ~ lag_growth + gdp_growth_real + unemp_rate +
    old_dep + gov_debt_pct_gdp + gov_state_market + tender_year,
  data = dynamic_data
)

# 3️⃣ Esegui il test RESET
library(lmtest)
reset_result <- resettest(lm_dynamic_equiv)

print(reset_result)

if (reset_result$p.value < 0.05) {
  cat("→ Possibile forma funzionale errata o variabili omesse\n")
} else {
  cat("→ Forma funzionale appropriata\n")
}

# B) Test PBG (Breusch-Godfrey) per autocorrelazione seriale
cat("\n🔸 TEST PBG (correlazione seriale)\n")

pbg_dyn <- pbgtest(m_dynamic)
print(pbg_dyn)

if (pbg_dyn$p.value < 0.05) {
  cat("→ Correlazione seriale ancora presente → potenziale dinamica più lunga (lag > 1)\n")
} else {
  cat("→ Nessuna correlazione seriale residua → modello dinamico ben specificato\n")
}

# C) Sintesi finale
cat("\n=== SINTESI MODELLO DINAMICO ===\n")
cat("Lag coefficiente:", round(lag_coef, 3), "(p =", round(lag_pval, 3), ")\n")
cat("RESET p-value:", round(p_reset, 4), "\n")
cat("PBG p-value:", round(pbg_dyn$p.value, 4), "\n")

if (lag_pval < 0.05 & pbg_dyn$p.value > 0.05 & p_reset > 0.05) {
  cat("✅ Modello dinamico solido: nessuna evidenza di specificazione errata o autocorrelazione.\n")
} else {
  cat("⚠️ Modello dinamico valido ma con segnali di complessità residua (endogeneità o non linearità).\n")
}

library(plm)

library(plm)

# 1️⃣ Modello GMM più parsimonioso (senza factor(tender_year))
m_ab <- pgmm(
  total_expenditure_growth_real ~ lag(total_expenditure_growth_real, 1) +
    gdp_growth_real + unemp_rate + old_dep + gov_debt_pct_gdp + gov_state_market |
    lag(total_expenditure_growth_real, 2:3) +  # strumenti per il lag endogeno
    lag(gdp_growth_real, 2:3) +                # strumenti per variabile potenzialmente endogena
    old_dep + gov_state_market,                # variabili esogene
  data = mp_updated,
  effect = "individual",
  model = "twosteps",
  transformation = "d"
)

# 2️⃣ Sommario dei risultati
summary(m_ab)

# 3️⃣ Test Arellano–Bond per autocorrelazione (AR1 e AR2)
mtest(m_ab, order = 1)
mtest(m_ab, order = 2)

# 4️⃣ Test Hansen/Sargan per validità strumenti
sargan(m_ab)



