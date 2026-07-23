# RICREAZIONE DATASET PULITI E MODELLI CORRETTI
library(tidyverse)
library(plm)

# 1. CARICA DATASET ORIGINALE E PULISCI
final_dataset <- read_csv("FINAL_ANALYSIS_DATASET_2006_2021.csv")

# Rimuovi variabili con missing >99%
final_dataset_clean <- final_dataset %>%
  select(-lot_updateddurationdays, -bid_subcontractedproportion)

# 2. CREA DATASET TEMPORALI
final_dataset_full <- final_dataset_clean %>%
  filter(tender_year >= 2006 & tender_year <= 2021)

final_dataset_restricted <- final_dataset_clean %>%
  filter(tender_year >= 2011 & tender_year <= 2021)

cat("📅 DATASET CREATI:\n")
cat("Full (2006-2021):", nrow(final_dataset_full), "osservazioni\n")
cat("Restricted (2011-2021):", nrow(final_dataset_restricted), "osservazioni\n")

# 3. SALVA DATASET PULITI
write_csv(final_dataset_full, "final_dataset_full_clean.csv")
write_csv(final_dataset_restricted, "final_dataset_restricted_clean.csv")

# 4. MODELLI CORRETTI - SOLO CONTROLLI CONCORDATI
model_full_corrected <- plm(total_expenditure_growth_real ~ 
                              unemp_rate + recession + old_dep + pop_density +
                              gov_debt_pct_gdp + corruption_control + gov_state_market,
                            data = final_dataset_full,
                            index = c("buyer_country", "tender_year"),
                            model = "within",
                            effect = "twoways")

cat("\n📊 MODELLO FULL CORRETTO (2006-2021) - SOLO CONTROLLI CONCORDATI:\n")
summary(model_full_corrected)

# 5. MODELLO RESTRICTED (2011-2021)
model_restricted_corrected <- plm(total_expenditure_growth_real ~ 
                                    unemp_rate + recession + old_dep + pop_density +
                                    gov_debt_pct_gdp + corruption_control + gov_state_market,
                                  data = final_dataset_restricted,
                                  index = c("buyer_country", "tender_year"),
                                  model = "within",
                                  effect = "twoways")

cat("\n📊 MODELLO RESTRICTED CORRETTO (2011-2021) - SOLO CONTROLLI CONCORDATI:\n")
summary(model_restricted_corrected)

# 6. ANALISI SEGNI COEFFICIENTI
cat("\n🔍 ANALISI SEGNI COEFFICIENTI - ATTESI vs OSSERVATI:\n")

expected_signs <- tibble(
  variable = c("unemp_rate", "recession", "old_dep", "pop_density", 
               "gov_debt_pct_gdp", "corruption_control", "gov_state_market"),
  expected_sign = c("+", "+/-", "+", "-", "-", "+", "-"),
  economic_rationale = c(
    "Counter-cyclical social spending",
    "Mixed: counter-cyclical vs austerity", 
    "Aging population pressure",
    "Economies of scale",
    "Debt sustainability constraints",
    "Better institutions → efficient spending",
    "Pro-market → smaller government"
  )
)

observed_signs <- broom::tidy(model_full_corrected) %>%
  filter(term != "(Intercept)") %>%
  select(variable = term, observed_sign = estimate) %>%
  mutate(observed_sign = ifelse(observed_sign > 0, "+", "-"))

sign_analysis <- expected_signs %>%
  left_join(observed_signs, by = "variable") %>%
  mutate(consistency = ifelse(expected_sign == "+/-", "Mixed", 
                              ifelse(substr(expected_sign, 1, 1) == observed_sign, "Consistent", "Inconsistent")))

print(sign_analysis, n = 10)

# 7. SALVA TUTTO
write_csv(broom::tidy(model_full_corrected), "model_full_corrected_results.csv")
write_csv(broom::tidy(model_restricted_corrected), "model_restricted_corrected_results.csv")
write_csv(sign_analysis, "coefficient_sign_analysis.csv")

cat("\n💾 TUTTO SALVATO! Analisi completata.\n")

# 1. VERIFICA COLLINEARITÀ
correlation_matrix <- final_dataset_full %>%
  select(unemp_rate, recession, old_dep, pop_density, 
         gov_debt_pct_gdp, corruption_control, gov_state_market) %>%
  cor(use = "complete.obs")

print("Matrice di correlazione:")
print(correlation_matrix)

# 2. PROVA MODELLO SENZA FIXED EFFECTS TEMPORALI
model_no_time_fe <- plm(total_expenditure_growth_real ~ 
                          unemp_rate + recession + old_dep + pop_density +
                          gov_debt_pct_gdp + corruption_control + gov_state_market,
                        data = final_dataset_full,
                        index = c("buyer_country"),
                        model = "within")  # Solo fixed effects paese

summary(model_no_time_fe)

# PROVA MODELLO CON GDP GROWTH MA SENZA YEAR FE
model_gdp_no_yearfe <- plm(total_expenditure_growth_real ~ 
                             gdp_growth + unemp_rate + recession + old_dep + pop_density +
                             gov_debt_pct_gdp + corruption_control + gov_state_market,
                           data = final_dataset_full,
                           index = c("buyer_country"),  # Solo country FE
                           model = "within")

cat("📊 MODELLO CON GDP GROWTH E SENZA YEAR FE:\n")
summary(model_gdp_no_yearfe)

# TABELLA RIASSUNTIVA TUTTI I MODELLI
# INSTALLA E USA MODSUMMARIES COME ALTERNATIVA
install.packages("modelsummary")

# SE NON FUNZIONA, USA QUESTO CODICE BASE:
library(broom)

# CREA TABELLA DI CONFRONTO MANUALE
comparison_table <- bind_rows(
  broom::tidy(model_full_corrected) %>% mutate(Model = "A: No GDP + Year FE"),
  broom::tidy(model_no_time_fe) %>% mutate(Model = "B: No GDP + No Year FE"),
  broom::tidy(model_gdp_no_yearfe) %>% mutate(Model = "C: With GDP + No Year FE")
) %>%
  select(Model, term, estimate, std.error, p.value) %>%
  pivot_wider(names_from = Model, values_from = c(estimate, std.error, p.value),
              names_glue = "{Model}_{.value}")

# STAMPA TABELLA RIASSUNTIVA
cat("🎯 COMPARISON OF MODEL SPECIFICATIONS\n")
cat("======================================\n\n")

# Per ogni variabile, mostra i tre coefficienti
variables <- unique(broom::tidy(model_full_corrected)$term)
variables <- variables[variables != "(Intercept)"]

for(var in variables) {
  cat(paste0("\n", var, ":\n"))
  
  coef_a <- comparison_table %>% filter(term == var) %>% pull("A: No GDP + Year FE_estimate")
  coef_b <- comparison_table %>% filter(term == var) %>% pull("B: No GDP + No Year FE_estimate") 
  coef_c <- comparison_table %>% filter(term == var) %>% pull("C: With GDP + No Year FE_estimate")
  
  cat(sprintf("  A (No GDP + Year FE):    %7.3f\n", coef_a))
  cat(sprintf("  B (No GDP + No Year FE): %7.3f\n", coef_b))
  cat(sprintf("  C (With GDP + No Year FE):%7.3f\n", coef_c))
}

# STATISTICHE DI BONTÀ DEL MODELLO
cat("\n\n📊 MODEL FIT STATISTICS:\n")
cat("========================\n")

fit_stats <- tibble(
  Model = c("A: No GDP + Year FE", "B: No GDP + No Year FE", "C: With GDP + No Year FE"),
  Observations = c(model_full_corrected$n, model_no_time_fe$n, model_gdp_no_yearfe$n),
  R_Squared = c(summary(model_full_corrected)$r.squared[1],
                summary(model_no_time_fe)$r.squared[1],
                summary(model_gdp_no_yearfe)$r.squared[1]),
  Adj_R_Squared = c(summary(model_full_corrected)$r.squared[2],
                    summary(model_no_time_fe)$r.squared[2], 
                    summary(model_gdp_no_yearfe)$r.squared[2])
)

print(fit_stats)