setwd("/Users/sofiaperondi/Desktop/Tesina IV /Data selection and summary analysis /Downloaded data")

install.packages(c("tidyverse", "janitor", "countrycode", "naniar"))
install.packages("RSQLite")
install.packages("DBI")
library(tidyverse)
library(janitor)
library(countrycode)
library(naniar)
library(dplyr)

years <- 2006:2021
eu27 <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU",
          "IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")

#Control: gdp growth to account for recession
gdp_raw <- read_csv("eurostat_gdp_clv10.csv") %>% 
  clean_names()
names(gdp_raw)

gdp_clean <- gdp_raw %>%
  rename(
    year = time_period,
    value = obs_value
  ) %>%
  select(geo, year, na_item, unit, value)

unique(gdp_clean$na_item)
unique(gdp_clean$unit)

gdp <- gdp_clean %>%
  filter(
    na_item == "B1GQ",            # GDP at market prices
    unit == "CLV10_MEUR",         # <-- correct unit in your file
    geo %in% eu27,
    year %in% years
  ) %>%
  select(geo, year, gdp_vol = value) %>%
  arrange(geo, year) %>%
  group_by(geo) %>%
  mutate(
    gdp_growth = 100 * (gdp_vol / lag(gdp_vol) - 1),
    recession = ifelse(gdp_growth < 0, 1, 0)
  ) %>%
  ungroup()

summary(gdp$gdp_growth)
table(gdp$recession, useNA = "ifany")

# save cleaned version
write_csv(gdp, "gdp_growth_recession_clean.csv")

# Control: unemployment rate 
unemp_raw <- read_csv("eurostat_unemployment.csv") %>% 
  clean_names()

glimpse(unemp_raw)
names(unemp_raw)

years <- 2006:2021
eu27 <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU",
          "IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")

# Clean and filter
unemp <- unemp_raw %>%
  rename(
    year = time_period,
    unemp_rate = obs_value
  ) %>%
  select(geo, age, unit, sex_2, year, unemp_rate) %>%
  filter(
    age == "Y15-74",           # age group
    unit == "PC_ACT",          # % of active population
    sex_2 == "Total",          # both sexes combined
    geo %in% eu27,
    year %in% years
  ) %>%
  arrange(geo, year)

# Check results
summary(unemp$unemp_rate)
miss_var_summary(unemp)

write_csv(unemp, "unemployment_clean.csv")

# Control variable: government debt (% of GDP)
debt_raw <- read_csv("eurostat_gov_debt.csv") %>%
  clean_names()

glimpse(debt_raw)
names(debt_raw)

years <- 2006:2021
eu27 <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU",
          "IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")

debt <- debt_raw %>%
  rename(
    year = time_period,
    value = obs_value
  ) %>%
  select(geo, na_item, sector, unit, year, value) %>%
  filter(
    na_item == "GD",       # Government consolidated gross debt
    sector == "S13",       # General government
    unit == "PC_GDP",      # % of GDP
    geo %in% eu27,
    year %in% years
  ) %>%
  transmute(geo, year, gov_debt_pct_gdp = value) %>%
  arrange(geo, year)

summary(debt$gov_debt_pct_gdp)
miss_var_summary(debt)

write_csv(debt, "gov_debt_clean.csv")

# Control: old dependency ratio

old_raw <- read_csv("wdi_old_dependency.csv", skip = 4) %>%
  clean_names()

glimpse(old_raw)
names(old_raw)[1:10]   # peek at the first few column names

years <- 2006:2021
eu27 <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU",
          "IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")

old <- old_raw %>%
  # Keep only the columns we need
  select(country_name, country_code, indicator_code, starts_with("x")) %>%
  # Convert wide (x2006, x2007, ...) to long
  pivot_longer(
    cols = starts_with("x"),
    names_to = "year",
    values_to = "old_dep"
  ) %>%
  mutate(
    # Remove the "x" prefix and convert to numeric
    year = as.numeric(str_remove(year, "^x")),
    # Map ISO3 → Eurostat country code (e.g., GR → EL)
    geo = countrycode(country_code, "iso3c", "eurostat")
  ) %>%
  filter(
    indicator_code == "SP.POP.DPND.OL",
    geo %in% eu27,
    year %in% years
  ) %>%
  select(geo, year, old_dep) %>%
  arrange(geo, year)

unique(old$geo)
length(unique(old$geo))

summary(old$old_dep)
miss_var_summary(old)

write_csv(old, "old_dependency_clean.csv")

# Control: Population density 
pop_raw <- read_csv("wdi_population_density.csv", skip = 4) %>%
  clean_names()

glimpse(pop_raw)
names(pop_raw)[1:10]

years <- 2006:2021
eu27 <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU",
          "IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")

popd <- pop_raw %>%
  select(country_name, country_code, indicator_code, starts_with("x")) %>%
  pivot_longer(
    cols = starts_with("x"),
    names_to = "year",
    values_to = "pop_density"
  ) %>%
  mutate(
    year = as.numeric(str_remove(year, "^x")),
    geo = countrycode(country_code, "iso3c", "eurostat")
  ) %>%
  filter(
    indicator_code == "EN.POP.DNST",  # population density indicator
    geo %in% eu27,
    year %in% years
  ) %>%
  select(geo, year, pop_density) %>%
  arrange(geo, year)

summary(popd$pop_density)
miss_var_summary(popd)

write_csv(popd, "population_density_clean.csv")

# Control: continuous variable for political ideology (state_market)

setwd("/Users/sofiaperondi/Desktop/Tesina IV /Data selection and summary analysis /Downloaded data")

library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)
library(countrycode)

library(readxl)
library(janitor)

setwd("/Users/sofiaperondi/Desktop/Tesina IV /Data selection and summary analysis /Downloaded data")

manifesto_raw <- read_excel("MPDataset_MP2025a.xlsx") %>%
  clean_names()

# check basic structure
glimpse(manifesto_raw)

# see how many variables we have
ncol(manifesto_raw)

# look at the first few column names
names(manifesto_raw)[1:20]

manifesto_ideo <- manifesto_raw %>%
  mutate(
    year = floor(date / 100),  # convert YYYYMM to YYYY
    seat_share = absseat / totseats
  ) %>%
  select(countryname, year, partyname, absseat, seat_share, rile, markeco, planeco) %>%
  filter(!is.na(rile)) %>%
  clean_names()

summary(manifesto_ideo$rile)
length(unique(manifesto_ideo$countryname))
unique(manifesto_ideo$countryname)

manifesto_ideo_sm <- manifesto_ideo %>%
  mutate(state_market = markeco - planeco) %>%
  group_by(countryname, year) %>%
  summarise(
    gov_state_market = weighted.mean(state_market, w = seat_share, na.rm = TRUE),
    n_parties = n()
  ) %>%

  summary(manifesto_ideo_sm$gov_state_market)
length(unique(manifesto_ideo_sm$countryname))
head(manifesto_ideo_sm)

mutate(geo = countrycode(countryname, "country.name", "eurostat"))

library(countrycode)

manifesto_ideo_sm <- manifesto_ideo_sm %>%
  mutate(
    geo = countrycode(countryname, "country.name", "eurostat")
  )
table(is.na(manifesto_ideo_sm$geo))

manifesto_ideo_sm_clean <- manifesto_ideo_sm %>%
  filter(!is.na(geo))

manifesto_ideo_sm_clean <- manifesto_ideo_sm %>%
  filter(
    !is.na(geo), 
    !is.na(gov_state_market),
    year >= 2006,
    year <= 2021
  )

# Observations and coverage
nrow(manifesto_ideo_sm_clean)
length(unique(manifesto_ideo_sm_clean$geo))

# Summary stats
summary(manifesto_ideo_sm_clean$gov_state_market)

# Years per country
manifesto_ideo_sm_clean %>%
  group_by(geo) %>%
  summarise(
    n_years = n(),
    first_year = min(year),
    last_year = max(year),
    mean_state_market = mean(gov_state_market, na.rm = TRUE)
  ) %>%
  arrange(desc(n_years)) %>%
  print(n = 20)

library(ggplot2)

ggplot(manifesto_ideo_sm_clean, aes(x = gov_state_market)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    title = "Distribution of Government State–Market Orientation (2006–2021)",
    x = "Pro-market vs State orientation (higher = pro-market)",
    y = "Count"
  )

example_countries <- c("DE", "FR", "IT", "ES", "SE")

manifesto_ideo_sm_clean %>%
  filter(geo %in% example_countries) %>%
  ggplot(aes(x = year, y = gov_state_market, color = geo)) +
  geom_line(size = 1) +
  labs(
    title = "Government Pro-Market Orientation (2006–2021)",
    x = "Year",
    y = "Pro-market orientation"
  ) +
  theme_minimal()

saveRDS(manifesto_ideo_sm_clean,
        file = "manifesto_political_control_sm_2006_2021.rds")

write.csv(manifesto_ideo_sm_clean,
          "manifesto_political_control_sm_2006_2021.csv",
          row.names = FALSE)

eu27_codes <- c(
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR",
  "DE", "EL", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL",
  "PL", "PT", "RO", "SK", "SI", "ES", "SE"
)

manifesto_ideo_eu27 <- manifesto_ideo_sm_clean %>%
  filter(geo %in% eu27_codes)


length(unique(manifesto_ideo_eu27$geo))   # should be 27 (or close)
nrow(manifesto_ideo_eu27)                 # total country-years
summary(manifesto_ideo_eu27$gov_state_market)

setdiff(eu27_codes, unique(manifesto_ideo_eu27$geo))

manifesto_ideo_eu27 <- manifesto_ideo_sm_clean %>%
  filter(geo %in% eu27_codes)

length(unique(manifesto_ideo_eu27$geo))
setdiff(eu27_codes, unique(manifesto_ideo_eu27$geo))

saveRDS(manifesto_ideo_eu27,
        "manifesto_political_control_state_market_EU27_2006_2021.rds")

write.csv(manifesto_ideo_eu27,
          "manifesto_political_control_state_market_EU27_2006_2021.csv",
          row.names = FALSE)

# Last control: corruption index (World Bank corruption perception) - 2006-2021 timeframe
install.packages("WDI")
library(WDI)
library(dplyr)
library(ggplot2)

# --- 1. Eurostat country codes --------------------------------------------
eu27 <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU",
          "IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")

# --- 2. Download corruption data from World Bank --------------------------
# Control of Corruption indicator from Worldwide Governance Indicators
# Indicator code: "CC.EST" - Estimate of Control of Corruption
# Scale: -2.5 (weak) to +2.5 (strong control of corruption)

cat("📥 Downloading corruption data from World Bank...\n")

corruption_data <- WDI(
  country = "all", 
  indicator = "CC.EST", 
  start = 2006, 
  end = 2021,
  extra = TRUE  # Get additional country info
)

# --- 3. Explore the downloaded data --------------------------------------
cat("📊 Data structure:\n")
print(dim(corruption_data))
cat("\nColumn names:\n")
print(names(corruption_data))
cat("\nFirst few rows:\n")
print(head(corruption_data))

# --- 4. Filter for EU countries and clean --------------------------------
cat("\n🔧 Cleaning data for EU-27 countries...\n")

eu_corruption <- corruption_data %>%
  filter(iso2c %in% eu27) %>%
  select(
    geo = iso2c,
    Country = country,
    Year = year,
    Corruption_Control = CC.EST,
    Region = region,
    Income_Level = income
  ) %>%
  mutate(
    # Convert to similar scale as CPI (0-100) for comparison
    # Original: -2.5 to +2.5 -> Convert to 0-100
    CPI_Score_Converted = ((Corruption_Control + 2.5) / 5) * 100,
    # Keep original for reference
    Corruption_Control_Original = Corruption_Control
  ) %>%
  filter(!is.na(Corruption_Control)) %>%
  arrange(geo, Year)

# --- 5. Check data completeness ------------------------------------------
cat("\n📈 Data completeness check:\n")
completeness <- eu_corruption %>%
  group_by(geo) %>%
  summarise(
    Years = n(),
    First_Year = min(Year),
    Last_Year = max(Year),
    Avg_Corruption_Control = round(mean(Corruption_Control, na.rm = TRUE), 3)
  ) %>%
  arrange(desc(Avg_Corruption_Control))

print(completeness)

# Check which EU countries are missing
missing_eu <- setdiff(eu27, unique(eu_corruption$geo))
if(length(missing_eu) > 0) {
  cat("\n⚠️ Missing EU countries:", paste(missing_eu, collapse = ", "), "\n")
} else {
  cat("\n✅ All 27 EU countries found!\n")
}

# Updated mapping for World Bank country codes
eu_corruption <- corruption_data %>%
  select(
    geo = iso2c,
    Country = country,
    Year = year,
    Corruption_Control = CC.EST
  ) %>%
  mutate(
    # Map World Bank codes to Eurostat codes
    geo = case_when(
      geo == "GR" ~ "EL",  # Greece: GR → EL
      geo == "UK" ~ "GB",  # UK if needed
      TRUE ~ geo
    ),
    CPI_Score_Converted = ((Corruption_Control + 2.5) / 5) * 100,
    Corruption_Control_Original = Corruption_Control
  ) %>%
  filter(geo %in% eu27) %>%  # Now filter after mapping
  filter(!is.na(Corruption_Control)) %>%
  arrange(geo, Year)

# --- 6. Save the dataset -------------------------------------------------
write.csv(eu_corruption, "corruption_control_eu27_2006_2021.csv", row.names = FALSE)
cat("💾 Saved: corruption_control_eu27_2006_2021.csv\n")

# --- 7. Summary statistics -----------------------------------------------
cat("\n📊 Summary statistics (2006-2021):\n")
cat("Time period:", min(eu_corruption$Year), "-", max(eu_corruption$Year), "\n")
cat("Total observations:", nrow(eu_corruption), "\n")
cat("Countries:", length(unique(eu_corruption$geo)), "/ 27\n")

yearly_summary <- eu_corruption %>%
  group_by(Year) %>%
  summarise(
    Avg_Control = round(mean(Corruption_Control, na.rm = TRUE), 3),
    Countries = n_distinct(geo)
  )

cat("\nYearly averages:\n")
print(yearly_summary)

# --- 8. Visualizations ---------------------------------------------------
# 8.1 Time trend
p1 <- eu_corruption %>%
  group_by(Year) %>%
  summarise(Avg_Control = mean(Corruption_Control, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = Avg_Control)) +
  geom_line(color = "#2E86AB", size = 1.5) +
  geom_point(color = "#2E86AB", size = 2) +
  labs(
    title = "Control of Corruption - EU-27 Average (2006-2021)",
    subtitle = "World Bank Worldwide Governance Indicators",
    x = "Year",
    y = "Control of Corruption Score\n(-2.5 = weak, +2.5 = strong)"
  ) +
  theme_minimal()

print(p1)
ggsave("corruption_control_trend.png", width = 10, height = 6, dpi = 300)

# 8.2 Heatmap by country
p2 <- ggplot(eu_corruption, aes(x = Year, y = reorder(geo, Corruption_Control), fill = Corruption_Control)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Control of\nCorruption") +
  scale_x_continuous(breaks = seq(2006, 2021, by = 2)) +
  labs(
    title = "Control of Corruption - EU Countries (2006-2021)",
    x = "Year",
    y = "Country Code"
  ) +
  theme_minimal()

print(p2)
ggsave("corruption_control_heatmap.png", width = 12, height = 8, dpi = 300)

# 8.3 Latest year comparison (2021)
latest_data <- eu_corruption %>%
  filter(Year == 2021) %>%
  arrange(desc(Corruption_Control))

p3 <- ggplot(latest_data, aes(x = reorder(geo, Corruption_Control), y = Corruption_Control)) +
  geom_col(aes(fill = Corruption_Control)) +
  scale_fill_viridis_c(option = "plasma", name = "Control Score") +
  coord_flip() +
  labs(
    title = "Control of Corruption - EU Countries (2021)",
    subtitle = "Higher values = better control of corruption",
    x = "Country Code",
    y = "Control of Corruption Score"
  ) +
  theme_minimal()

print(p3)
ggsave("corruption_control_2021.png", width = 10, height = 8, dpi = 300)

cat("📊 Visualizations saved!\n")

# --- 9. Show final dataset -----------------------------------------------
cat("\n🔍 Sample of final dataset:\n")
print(head(eu_corruption, 10))

cat("\n✅ World Bank corruption data download complete!\n")
cat("📁 Files created:\n")
cat("   - corruption_control_eu27_2006_2021.csv\n")
cat("   - corruption_control_trend.png\n")
cat("   - corruption_control_heatmap.png\n")
cat("   - corruption_control_2021.png\n")



