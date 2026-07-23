############################################################
# 02_eurostat_public_expenditure_real.R
# EU-27: Cleaning, merging Eurostat gov_10a_main + gov_10a_exp
# Add GDP deflator to compute real expenditure & growth
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

# --- 1. Clean gov_main (economic type expenditures) ---
gov_clean <- gov_main %>%
  mutate(
    TIME_PERIOD = as.numeric(TIME_PERIOD),
    OBS_VALUE   = as.numeric(OBS_VALUE)
  ) %>%
  filter(
    unit == "MIO_NAC",                   # millions of national currency, current prices
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

# --- 2. Clean COFOG (policy areas) ---
cofog_clean <- cofog %>%
  mutate(
    TIME_PERIOD = as.numeric(TIME_PERIOD),
    OBS_VALUE   = as.numeric(OBS_VALUE)
  ) %>%
  filter(
    unit == "MIO_NAC",
    sector == "S13",
    cofog99 %in% c("GF01","GF03","GF04","GF07","GF09","GF10"), 
    geo %in% eu_countries,
    TIME_PERIOD >= 2006 & TIME_PERIOD <= 2021
  ) %>%
  group_by(geo, TIME_PERIOD, cofog99) %>%   # remove duplicates by averaging
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

# --- 3. Load GDP deflator (Eurostat nama_10_gdp) ---

gdp_deflator_raw <- fread("nama_10_gdp__custom_18275172_linear_2_0.csv")
names(gdp_deflator_raw)
head(gdp_deflator_raw)

# 1. Nominal GDP (gov_10a_main, unit = MIO_NAC, sector = S13)
gdp_nominal <- gov_main %>%
  filter(na_item == "B1G", sector == "S13",
         TIME_PERIOD >= 2006 & TIME_PERIOD <= 2021,
         geo %in% eu_countries) %>%
  transmute(geo, year = as.numeric(TIME_PERIOD),
            nominal_gdp = as.numeric(OBS_VALUE))

# 2. Real GDP (nama_10_gdp file, unit = CLV10_MEUR)
gdp_real <- gdp_deflator_raw %>%
  filter(na_item == "B1G", unit == "CLV10_MEUR",
         TIME_PERIOD >= 2006 & TIME_PERIOD <= 2021,
         geo %in% eu_countries) %>%
  transmute(geo, year = as.numeric(TIME_PERIOD),
            real_gdp = as.numeric(OBS_VALUE))

# 3. Merge + compute GDP deflator
gdp_both <- gdp_nominal %>%
  left_join(gdp_real, by = c("geo","year")) %>%
  mutate(gdp_deflator = (nominal_gdp / real_gdp) * 100)

# --- 5. Growth rates (YoY %) ---
final_data <- gov_clean %>%
  left_join(cofog_clean, by = c("geo","year")) %>%
  left_join(gdp_both %>% select(geo, year, gdp_deflator), by = c("geo","year")) %>%
  mutate(
    total_expenditure_real        = total_expenditure / (gdp_deflator/100),
    intermediate_consumption_real = intermediate_consumption / (gdp_deflator/100),
    compensation_employees_real   = compensation_employees / (gdp_deflator/100),
    gross_fixed_capital_real      = gross_fixed_capital / (gdp_deflator/100),
    gdp_real                      = gdp_nominal / (gdp_deflator/100)   # fix here
  )

# --- 6. Save ---
fwrite(final_data, "eurostat_expenditure_clean.csv")

cat("✅ Final Eurostat dataset saved with deflated series and growth rates\n")

# === Compute real growth rates (year-on-year) ===
data <- data %>%
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

# Save again after adding growth rates
fwrite(data, "eurostat_data_real_with_growth.csv")

cat("✅ Added real growth rates (year-on-year) for expenditure and components.\n")
cat("New variables:\n",
    " - total_expenditure_growth_real\n",
    " - intermediate_consumption_growth_real\n",
    " - compensation_employees_growth_real\n",
    " - gross_fixed_capital_growth_real\n",
    " - gdp_growth_real\n")

