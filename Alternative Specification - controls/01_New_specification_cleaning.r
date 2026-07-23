library(data.table)
library(dplyr)

setwd("~/Desktop/Tesina IV /Data analysis /Downloaded data")

te <- fread("gov_10a_main_TE_MIOEUR.csv")
int <- fread("gov_10a_main_D41PAY_MIOEUR.csv")
gdp <- fread("nama_10_gdp__custom_18275172_linear_2_0.csv") # o il tuo file già salvato

# === Clean and merge ===
clean_fun <- function(df, code) {
  df %>%
    filter(sector == "S13", unit == "MIO_EUR", na_item == code) %>%
    transmute(geo, year = as.numeric(TIME_PERIOD), value = as.numeric(OBS_VALUE))
}

te_clean  <- clean_fun(te, "TE")
int_clean <- clean_fun(int, "D41PAY")

# === GDP nominal & real ===
gdp_nom <- gdp %>% filter(na_item == "B1G", unit == "MIO_EUR") %>%
  transmute(geo, year = as.numeric(TIME_PERIOD), gdp_nominal = as.numeric(OBS_VALUE))
gdp_real <- gdp %>% filter(na_item == "B1G", unit == "CLV10_MEUR") %>%
  transmute(geo, year = as.numeric(TIME_PERIOD), gdp_real = as.numeric(OBS_VALUE))

gdp_def <- gdp_nom %>%
  left_join(gdp_real, by = c("geo", "year")) %>%
  mutate(gdp_deflator = (gdp_nominal / gdp_real) * 100)

# === Merge all and deflate ===
merged <- te_clean %>%
  left_join(int_clean, by = c("geo", "year"), suffix = c("_te", "_int")) %>%
  left_join(gdp_def, by = c("geo", "year")) %>%
  mutate(
    te_real = value_te / (gdp_deflator / 100),
    int_real = value_int / (gdp_deflator / 100),
    primary_real = te_real - int_real
  ) %>%
  group_by(geo) %>%
  arrange(year) %>%
  mutate(
    te_growth_real = 100 * (te_real / lag(te_real) - 1),
    primary_growth_real = 100 * (primary_real / lag(primary_real) - 1)
  ) %>%
  ungroup()

fwrite(merged, "eurostat_primary_reconstructed.csv")
cat("✅ New base with TE, D41PAY, GDP deflator and primary expenditure saved.\n")

