############################################################
# 02_eurostat_public_expenditure_real_FIXED.R
# CORRECTED VERSION - Fixes unit consistency and deflation
############################################################

# --- 0. Setup ---------------------------------------------------------------
rm(list = ls())
gc()

library(dplyr)
library(tidyr)
library(data.table)

# --- 1. Paths ---------------------------------------------------------------
setwd("/Users/sofiaperondi/Desktop/Tesina IV /Data selection and summary analysis /Downloaded data")

gov_main <- fread("gov_10a_main.csv")
cofog    <- fread("gov_10a_exp.csv")
gdp_data <- fread("nama_10_gdp__custom_18275172_linear_2_0.csv")

# --- 2. Define EU countries -------------------------------------------------
eu_countries <- c("AT","BE","BG","CY","CZ","DE","DK","EE","ES","FI",
                  "FR","GR","EL","HR","HU","IE","IT","LT","LU","LV",
                  "MT","NL","PL","PT","RO","SE","SI","SK")

# --- 3. Clean gov_main - Use EUROS not national currency ---
gov_clean <- gov_main %>%
  mutate(
    TIME_PERIOD = as.numeric(TIME_PERIOD),
    OBS_VALUE   = as.numeric(OBS_VALUE)
  ) %>%
  filter(
    unit == "MIO_EUR",                   # CHANGED: Millions of EUROS, not national currency
    sector == "S13",                     # general government
    na_item %in% c("P2","P3","P5","TE","B1G","D1PAY"), 
    geo %in% eu_countries,
    TIME_PERIOD >= 2006 & TIME_PERIOD <= 2021
  ) %>%
  distinct(geo, TIME_PERIOD, na_item, .keep_all = TRUE) %>%
  select(geo, TIME_PERIOD, na_item, OBS_VALUE) %>%
  pivot_wider(
    names_from  = na_item, 
    values_from = OBS_VALUE
  ) %>%
  rename(
    intermediate_consumption = P2,
    final_consumption        = P3,
    gross_fixed_capital      = P5,
    total_expenditure        = TE,
    gdp_nominal              = B1G,
    compensation_employees   = D1PAY,
    year = TIME_PERIOD
  )

# --- 4. Clean COFOG - Use EUROS ---
cofog_clean <- cofog %>%
  mutate(
    TIME_PERIOD = as.numeric(TIME_PERIOD),
    OBS_VALUE   = as.numeric(OBS_VALUE)
  ) %>%
  filter(
    unit == "MIO_EUR",                   # CHANGED: Millions of EUROS
    sector == "S13",
    cofog99 %in% c("GF01","GF03","GF04","GF07","GF09","GF10"), 
    geo %in% eu_countries,
    TIME_PERIOD >= 2006 & TIME_PERIOD <= 2021
  ) %>%
  group_by(geo, TIME_PERIOD, cofog99) %>%
  summarise(OBS_VALUE = mean(OBS_VALUE, na.rm=TRUE), .groups="drop") %>%
  pivot_wider(
    names_from  = cofog99, 
    values_from = OBS_VALUE
  ) %>%
  rename(
    public_services   = GF01,
    public_order      = GF03,
    economic_affairs  = GF04,
    health            = GF07,
    education         = GF09,
    social_protection = GF10,
    year = TIME_PERIOD
  )

# --- 5. Get GDP Deflator PROPERLY ---
# We need both nominal and real GDP in the same currency (Euros)

# 5.1 Nominal GDP (in current Euros)
gdp_nominal_euro <- gov_main %>%
  filter(na_item == "B1G", sector == "S13", unit == "MIO_EUR",
         TIME_PERIOD >= 2006 & TIME_PERIOD <= 2021,
         geo %in% eu_countries) %>%
  transmute(geo, year = as.numeric(TIME_PERIOD),
            nominal_gdp = as.numeric(OBS_VALUE))

# 5.2 Real GDP (in constant 2010 Euros)
gdp_real_euro <- gdp_data %>%
  filter(na_item == "B1G", unit == "CLV10_MEUR",  # Constant 2010 Euros
         TIME_PERIOD >= 2006 & TIME_PERIOD <= 2021,
         geo %in% eu_countries) %>%
  transmute(geo, year = as.numeric(TIME_PERIOD),
            real_gdp = as.numeric(OBS_VALUE))

# 5.3 Compute GDP deflator (2010 = 100)
gdp_deflator <- gdp_nominal_euro %>%
  left_join(gdp_real_euro, by = c("geo","year")) %>%
  mutate(gdp_deflator = (nominal_gdp / real_gdp) * 100)  # 2010 base year

# --- 6. Merge and Deflate Expenditure Data ---
final_data <- gov_clean %>%
  left_join(cofog_clean, by = c("geo","year")) %>%
  left_join(gdp_deflator %>% select(geo, year, gdp_deflator, real_gdp), by = c("geo","year")) %>%  # FIXED: Include real_gdp
  mutate(
    # Deflate to 2010 prices using GDP deflator
    total_expenditure_real        = total_expenditure / (gdp_deflator/100),
    intermediate_consumption_real = intermediate_consumption / (gdp_deflator/100),
    compensation_employees_real   = compensation_employees / (gdp_deflator/100),
    gross_fixed_capital_real      = gross_fixed_capital / (gdp_deflator/100),
    gdp_real                      = real_gdp  # Now this works!
  )

# Quick check
cat("Variables in gdp_deflator:", names(gdp_deflator), "\n")

# --- 7. Compute Growth Rates PROPERLY ---
final_data_with_growth <- final_data %>%
  arrange(geo, year) %>%
  group_by(geo) %>%
  mutate(
    total_expenditure_growth_real = 100 * (total_expenditure_real / lag(total_expenditure_real) - 1),
    intermediate_consumption_growth_real = 100 * (intermediate_consumption_real / lag(intermediate_consumption_real) - 1),
    compensation_employees_growth_real = 100 * (compensation_employees_real / lag(compensation_employees_real) - 1),
    gross_fixed_capital_growth_real = 100 * (gross_fixed_capital_real / lag(gross_fixed_capital_real) - 1),
    gdp_growth_real = 100 * (gdp_real / lag(gdp_real) - 1)
  ) %>%
  ungroup()

# --- 8. Save Clean Data ---
fwrite(final_data_with_growth, "eurostat_data_real_with_growth_CORRECTED.csv")

# --- 9. Data Quality Check ---
cat("=== DATA QUALITY CHECK ===\n")
cat("Observations:", nrow(final_data_with_growth), "\n")
cat("Countries:", length(unique(final_data_with_growth$geo)), "\n")
cat("Years:", min(final_data_with_growth$year), "-", max(final_data_with_growth$year), "\n")

# Check Germany 2021 values
de_2021 <- final_data_with_growth %>% filter(geo == "DE", year == 2021)
cat("\nGermany 2021 (CORRECTED):\n")
cat("Nominal GDP:", round(de_2021$gdp_nominal), "million EUR\n")
cat("Real GDP:", round(de_2021$gdp_real), "million EUR (2010 prices)\n")
cat("Real Expenditure:", round(de_2021$total_expenditure_real), "million EUR (2010 prices)\n")
cat("Expenditure/GDP ratio:", round(de_2021$total_expenditure_real / de_2021$gdp_real * 100, 1), "%\n")

# Check growth rates
growth_check <- final_data_with_growth %>%
  summarise(
    expend_growth_min = min(total_expenditure_growth_real, na.rm = TRUE),
    expend_growth_max = max(total_expenditure_growth_real, na.rm = TRUE),
    gdp_growth_min = min(gdp_growth_real, na.rm = TRUE),
    gdp_growth_max = max(gdp_growth_real, na.rm = TRUE)
  )
cat("\nGrowth rate ranges (should be reasonable):\n")
print(growth_check)

# Quick verification
data_check <- fread("eurostat_data_real_with_growth_CORRECTED.csv")
cat("Germany 2021 check:\n")
de <- data_check %>% filter(geo == "DE", year == 2021)
cat("Real GDP:", de$gdp_real, "million EUR\n")
cat("Real Expenditure:", de$total_expenditure_real, "million EUR\n")
cat("Ratio:", round(de$total_expenditure_real / de$gdp_real * 100, 1), "%\n")

cat("\n✅ CORRECTED dataset saved: eurostat_data_real_with_growth_CORRECTED.csv\n")

############################################################
# SOURCE DATA INVESTIGATION
############################################################

# Check what's in the original gov_10a_main.csv
gov_main_check <- fread("gov_10a_main.csv")

# Look at Germany 2021 expenditure components
de_expenditure_components <- gov_main_check %>%
  filter(geo == "DE", TIME_PERIOD == 2021, unit == "MIO_EUR", sector == "S13") %>%
  filter(na_item %in% c("TE", "P2", "P3", "P5", "D1PAY", "B1G")) %>%
  select(na_item, OBS_VALUE) %>%
  arrange(desc(OBS_VALUE))

cat("Germany 2021 - Expenditure Components (million EUR):\n")
print(de_expenditure_components)

# Check if there are multiple definitions of "total expenditure"
te_definitions <- gov_main_check %>%
  filter(na_item == "TE", unit == "MIO_EUR") %>%
  distinct(sector, .keep_all = TRUE)

cat("\nDifferent sectors with TE (total expenditure):\n")
print(te_definitions %>% select(sector, na_item, unit))

