

# Set your working directory - CHANGE THIS PATH to where your CSV files are
setwd("/Users/sofiaperondi/Desktop/Tesina IV /Data selection and summary analysis /Downloaded data")  # Change this to your actual folder

# Clear memory and restart
rm(list = ls())
gc()

# Load packages
library(dplyr)
library(tidyr)

# Read the files
gov_main <- read.csv("gov_10a_main.csv", colClasses = "character")
cofog <- read.csv("gov_10a_exp.csv", colClasses = "character")

# EU countries list
eu_countries <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", 
                  "FR", "GR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", 
                  "NL", "PL", "PT", "RO", "SE", "SI", "SK")

# Filter main government data - USING ONLY VARIABLES THAT EXIST
gov_clean <- gov_main %>%
  mutate(
    TIME_PERIOD = as.numeric(TIME_PERIOD),
    OBS_VALUE = as.numeric(OBS_VALUE)
  ) %>%
  filter(
    na_item %in% c("P3", "TE", "B1G", "P2", "P5", "D1PAY"), 
    sector == "S13",
    geo %in% eu_countries,
    TIME_PERIOD >= 2006 & TIME_PERIOD <= 2021
  ) %>%
  distinct(geo, TIME_PERIOD, na_item, .keep_all = TRUE) %>%
  select(geo, TIME_PERIOD, na_item, OBS_VALUE) %>%
  pivot_wider(
    names_from = na_item, 
    values_from = OBS_VALUE
  ) %>%
  rename(
    final_consumption = P3,
    total_expenditure = TE,
    gdp = B1G,
    intermediate_consumption = P2,
    gross_fixed_capital = P5,
    time = TIME_PERIOD,
    compensation_employees = D1PAY
  )

# Filter COFOG data
cofog_clean <- cofog %>%
  mutate(
    TIME_PERIOD = as.numeric(TIME_PERIOD),
    OBS_VALUE = as.numeric(OBS_VALUE)
  ) %>%
  filter(
    cofog99 %in% c("GF01", "GF03", "GF04", "GF07", "GF09", "GF10"),
    sector == "S13",
    geo %in% eu_countries, 
    TIME_PERIOD >= 2006 & TIME_PERIOD <= 2021
  ) %>%
  distinct(geo, TIME_PERIOD, cofog99, .keep_all = TRUE) %>%
  select(geo, TIME_PERIOD, cofog99, OBS_VALUE) %>%
  pivot_wider(
    names_from = cofog99, 
    values_from = OBS_VALUE
  ) %>%
  rename(
    public_services = GF01,
    public_order = GF03,
    economic_affairs = GF04,
    health = GF07,
    education = GF09,
    social_protection = GF10,
    time = TIME_PERIOD
  )

# Merge both datasets
final_data <- gov_clean %>%
  left_join(cofog_clean, by = c("geo", "time"))

# Add country names
final_data <- final_data %>%
  mutate(country_name = case_when(
    geo == "AT" ~ "Austria", geo == "BE" ~ "Belgium", geo == "BG" ~ "Bulgaria",
    geo == "CY" ~ "Cyprus", geo == "CZ" ~ "Czechia", geo == "DE" ~ "Germany",
    geo == "DK" ~ "Denmark", geo == "EE" ~ "Estonia", geo == "ES" ~ "Spain",
    geo == "FI" ~ "Finland", geo == "FR" ~ "France", geo == "GR" ~ "Greece",
    geo == "HR" ~ "Croatia", geo == "HU" ~ "Hungary", geo == "IE" ~ "Ireland",
    geo == "IT" ~ "Italy", geo == "LT" ~ "Lithuania", geo == "LU" ~ "Luxembourg",
    geo == "LV" ~ "Latvia", geo == "MT" ~ "Malta", geo == "NL" ~ "Netherlands",
    geo == "PL" ~ "Poland", geo == "PT" ~ "Portugal", geo == "RO" ~ "Romania",
    geo == "SE" ~ "Sweden", geo == "SI" ~ "Slovenia", geo == "SK" ~ "Slovakia"
  ))

# Save the final data
write.csv(final_data, "eurostat_clean_data.csv", row.names = FALSE)

cat("Done! Final dataset has", nrow(final_data), "rows\n")
print(head(final_data))

dim(final_data)
table(final_data$geo)
colSums(is.na(final_data))
unique(gov_main$na_item)

library(dplyr)
library(tidyr)

# Upload cleaned dataset
country_data <- read.csv("eurostat_clean_data.csv")

# Download regional data
regional_gdp <- read.csv("nama_10r_2gdp.csv") 

# Explore the structure of regional data
cat("Struttura dati regionali:\n")
print(head(regional_gdp))
cat("Nomi colonne regionali:", names(regional_gdp), "\n")
cat("Anni disponibili:", unique(regional_gdp$TIME_PERIOD), "\n")

# Control geographical codes
cat("Esempio codici NUTS2:", head(unique(regional_gdp$geo)), "\n")

# Cleand data at regional level (NUTS 2)
regional_gdp_clean <- regional_gdp %>%
  filter(
    nchar(geo) == 4,  # Solo codici NUTS2 (4 caratteri: ES61, ITC4, etc.)
    substr(geo, 1, 2) %in% c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", "FR", 
                             "HR", "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT", 
                             "PL", "PT", "RO", "SI", "SK", "FI", "SE")
  ) %>%
  mutate(
    time = as.numeric(TIME_PERIOD),
    regional_gdp = as.numeric(OBS_VALUE),
    country_code = substr(geo, 1, 2)  # Estrae il codice paese per unire dopo
  ) %>%
  select(geo, country_code, time, regional_gdp)

# Verify
cat("Dati regionali NUTS2 puliti:\n")
print(head(regional_gdp_clean))
cat("Numero di regioni NUTS2:", n_distinct(regional_gdp_clean$geo), "\n")
cat("Osservazioni totali:", nrow(regional_gdp_clean), "\n")

# Merge dataset keeping regional level 
final_dataset <- country_data %>%
  inner_join(regional_gdp_clean, by = c("geo" = "country_code", "time")) %>%
  rename(
    country_code = geo,          # Codice paese
    nuts2_region = geo.y,        # Codice regione NUTS2
    gdp_national = gdp,          # GDP nazionale (dal dataset country)
    gdp_regional = regional_gdp  # GDP regionale (nuova variabile)
  )

# Verify union
cat("=== DATASET FINALE MULTI-LIVELLO ===\n")
cat("Osservazioni totali:", nrow(final_dataset), "\n")
cat("Numero di paesi:", n_distinct(final_dataset$country_code), "\n")
cat("Numero di regioni NUTS2:", n_distinct(final_dataset$nuts2_region), "\n")
cat("Anni coperti:", toString(unique(final_dataset$time)), "\n")
cat("Struttura finale:\n")
print(head(final_dataset))

# Save dataset 
write.csv(final_dataset, "thesis_multilevel_dataset.csv", row.names = FALSE)

cat("✅ Dataset multi-livello salvato con successo!\n")
cat("📊 Struttura finale:", nrow(final_dataset), "osservazioni regione-anno\n")
cat("🇪🇺 Paesi:", n_distinct(final_dataset$country_code), "\n")
cat("🗺️ Regioni NUTS2:", n_distinct(final_dataset$nuts2_region), "\n")


