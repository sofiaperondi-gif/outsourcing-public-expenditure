

setwd("~/Desktop/Tesina IV /Data analysis /Downloaded data")

install.packages(c("Rcpp", "cli"))
install.packages("stringmagic")
library(tidyverse)
install.packages("dreamerr")
install.packages("fixest")   # reinstallarlo subito dopo, per sicurezza

# === STEP 1: Caricamento & media per Paese (cross-section) ===
df <- read_csv("FINAL_ANALYSIS_DATASET_2006_2021.csv") %>%
  filter(tender_year >= 2011) %>%
  rename(country = buyer_country, year = tender_year)

# Una riga per Paese-anno
df_clean <- df %>%
  group_by(country, year) %>%
  summarise(across(c(total_expenditure_growth_real, unemp_rate, old_dep,
                     pop_density, gov_debt_pct_gdp, corruption_control,
                     gov_state_market, gdp_growth_real),
                   ~ na.omit(.x)[1]),
            .groups = "drop")

# Media per Paese 2011–2021
df_cross_section <- df_clean %>%
  group_by(country) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# === STEP 2: Outlier detection (IQR method) per colonna ===
detect_outliers_iqr <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  as.integer(x < lower | x > upper)
}

# Applica outlier detection a ogni variabile
outlier_matrix <- df_cross_section %>%
  mutate(across(-country, detect_outliers_iqr)) %>%
  column_to_rownames("country")

# Tabella finale: Paesi x Variabili = 0/1 (outlier)
print(outlier_matrix)

# (Opzionale) Conta quanti outlier ha ciascun Paese
outlier_summary <- outlier_matrix %>%
  rowSums() %>%
  sort(decreasing = TRUE)

print(outlier_summary)

# (Opzionale) Esporta
write_csv(as.data.frame(outlier_matrix), "OUTLIER_CROSS_SECTION.csv")

library(tidyverse)

# === INPUT: dati e matrice degli outlier ===
# df_cross_section → media per Paese dal 2011 al 2021 (già preparato)
# outlier_matrix → tabella 0/1 (Paesi x Variabili)

# Funzione per analizzare outlier in una singola variabile
analizza_outlier_variabile <- function(df_data, var_name, paesi_outlier) {
  media <- mean(df_data[[var_name]], na.rm = TRUE)
  mediana <- median(df_data[[var_name]], na.rm = TRUE)
  
  df_data %>%
    arrange(desc(.data[[var_name]])) %>%
    mutate(ranking = row_number()) %>%
    filter(country %in% paesi_outlier) %>%
    transmute(
      country,
      valore = .data[[var_name]],
      media_EU = round(media, 2),
      mediana_EU = round(mediana, 2),
      scostamento_media = round(valore - media, 2),
      scostamento_mediana = round(valore - mediana, 2),
      ranking
    )
}

# === CICLO AUTOMATICO SU TUTTE LE VARIABILI ===
# Ottiene lista variabili (esclude "year" se presente)
variabili <- setdiff(colnames(outlier_matrix), "year")

# Itera su ciascuna variabile
risultati_outlier <- list()

for (var in variabili) {
  paesi_out <- outlier_matrix %>%
    rownames_to_column("country") %>%
    filter(.data[[var]] == 1) %>%
    pull(country)
  
  if (length(paesi_out) > 0) {
    tabella <- analizza_outlier_variabile(df_cross_section, var, paesi_out)
    risultati_outlier[[var]] <- tabella
  }
}

# Ora puoi stampare ogni tabella con:
risultati_outlier[["pop_density"]]
risultati_outlier[["unemp_rate"]]
risultati_outlier[["total_expenditure_growth_real"]]
risultati_outlier[["old_dep"]]
risultati_outlier[["gov_debt_pct_gdp"]]
risultati_outlier[["gdp_growth_real"]]

library(tidyverse)

# === Variabile da analizzare ===
variabile_da_analizzare <- "gdp_growth_real"  # cambia qui

# === Prepara dataset filtrato ===
df_var <- df %>%
  select(country, year, all_of(variabile_da_analizzare)) %>%
  drop_na() %>%
  distinct()  # <-- molto importante: evita duplicati

# === Calcola z-score dinamico anno per anno ===
z_score_annuale <- df_var %>%
  group_by(year) %>%
  mutate(
    media_anno = mean(.data[[variabile_da_analizzare]], na.rm = TRUE),
    sd_anno = sd(.data[[variabile_da_analizzare]], na.rm = TRUE),
    z_score = (.data[[variabile_da_analizzare]] - media_anno) / sd_anno
  ) %>%
  ungroup()

# === Identifica outlier per soglia z-score ===
outlier_temporali <- z_score_annuale %>%
  filter(abs(z_score) > 2) %>%
  mutate(
    tipo_outlier = case_when(
      z_score > 0 ~ "valore superiore alla media EU",
      z_score < 0 ~ "valore inferiore alla media EU"
    )
  ) %>%
  arrange(desc(abs(z_score))) %>%
  distinct(country, year, .keep_all = TRUE)  # <-- evita ripetizioni

# === Visualizza solo le colonne chiave in modo pulito ===
risultati_outlier <- outlier_temporali %>%
  select(
    country, year,
    valore = all_of(variabile_da_analizzare),
    media_anno, z_score, tipo_outlier
  )

# === Stampa leggibile e controllata ===
cat("\n==========================\n")
cat("📌 OUTLIER TEMPORALI:", variabile_da_analizzare, "\n")
cat("==========================\n")
print(risultati_outlier, n = 30)  # stampa max 30 righe

library(tidyverse)

# === Scegli la variabile da analizzare ===
variabile_da_analizzare <- "gdp_growth_real"  # cambia qui

# === Prepara dataset ===
df_ts <- df %>%
  select(country, year, all_of(variabile_da_analizzare)) %>%
  drop_na() %>%
  distinct()

# === Calcola media e sd per ciascun Paese (storico) ===
media_std_country <- df_ts %>%
  group_by(country) %>%
  summarise(
    media_storica = mean(.data[[variabile_da_analizzare]], na.rm = TRUE),
    sd_storica = sd(.data[[variabile_da_analizzare]], na.rm = TRUE),
    .groups = "drop"
  )

# === Unisci e calcola z-score rispetto al proprio trend ===
z_score_own <- df_ts %>%
  left_join(media_std_country, by = "country") %>%
  mutate(
    z_score_own = (.data[[variabile_da_analizzare]] - media_storica) / sd_storica
  )

# === Identifica outlier rispetto al proprio trend (|z| > 2) ===
outlier_interni <- z_score_own %>%
  filter(abs(z_score_own) > 2) %>%
  mutate(
    tipo_outlier = case_when(
      z_score_own > 0 ~ "valore superiore al proprio trend",
      z_score_own < 0 ~ "valore inferiore al proprio trend"
    )
  ) %>%
  arrange(desc(abs(z_score_own))) %>%
  select(
    country, year,
    valore = all_of(variabile_da_analizzare),
    media_storica, z_score_own, tipo_outlier
  )

# === Output leggibile ===
cat("\n====================================================\n")
cat("📌 OUTLIER TEMPORALI (rispetto al trend individuale):", variabile_da_analizzare, "\n")
cat("====================================================\n")
print(outlier_interni, n = 30)





