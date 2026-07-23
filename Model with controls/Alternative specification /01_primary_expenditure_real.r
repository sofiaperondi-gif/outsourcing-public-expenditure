

setwd("~/Desktop/Tesina IV /Data analysis /Downloaded data")
# --- 0️⃣ Setup ---------------------------------------------------------------
rm(list = ls())
gc()

library(dplyr)
library(data.table)

eu_countries <- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI",
                  "FR","HR","HU","IE","IT","LT","LU","LV","MT","NL","PL",
                  "PT","RO","SE","SI","SK")

# Filtra solo livelli nazionali
te_clean2 <- te_raw %>%
  mutate(year = as.numeric(TIME_PERIOD),
         value_te = as.numeric(OBS_VALUE)) %>%
  filter(
    unit == "MIO_EUR",
    sector == "S13",
    geo %in% eu_countries,
    year >= 2006 & year <= 2021
  ) %>%
  distinct(geo, year, .keep_all = TRUE) %>%
  select(geo, year, value_te)

int_clean2 <- fread("gov_10a_main_D41PAY.csv") %>%
  mutate(year = as.numeric(TIME_PERIOD),
         value_int = as.numeric(OBS_VALUE)) %>%
  filter(
    unit == "MIO_EUR",
    sector == "S13",
    geo %in% eu_countries,
    year >= 2006 & year <= 2021
  ) %>%
  distinct(geo, year, .keep_all = TRUE) %>%
  select(geo, year, value_int)

merged <- te_clean2 %>%
  left_join(int_clean2, by = c("geo", "year")) %>%
  left_join(gdp_deflator, by = c("geo", "year")) %>%
  mutate(
    te_real = value_te / (gdp_deflator / 100),
    int_real = value_int / (gdp_deflator / 100),
    primary_real = te_real - int_real
  ) %>%
  arrange(geo, year) %>%
  group_by(geo) %>%
  mutate(
    te_growth_real = 100 * (te_real / lag(te_real) - 1),
    primary_growth_real = 100 * (primary_real / lag(primary_real) - 1)
  ) %>%
  ungroup()

cat("✅ Rows:", nrow(merged), "(expected 27×16=432)\n")

library(dplyr)

merged %>%
  distinct(geo) %>%
  arrange(geo) %>%
  head(40)

group_by(geo) %>%
  mutate(
    te_growth_real = 100 * (te_real / lag(te_real) - 1),
    primary_growth_real = 100 * (primary_real / lag(primary_real) - 1)
  ) %>%
  ungroup()

# --- Controlli rapidi ---
cat("✅ Countries:", length(unique(merged$geo)), "\n")
cat("✅ Years:", min(merged$year), "-", max(merged$year), "\n")
cat("✅ Rows:", nrow(merged), "(expected 27×16=432)\n\n")

summary(merged$primary_growth_real)

merged %>%
  filter(geo == "IT", year %in% 2020:2021) %>%
  select(geo, year, te_real, int_real, primary_real, primary_growth_real)

gdp_deflator %>%
  count(geo, year, name = "n_rows") %>%
  arrange(desc(n_rows)) %>%
  head(10)

gdp_deflator %>%
  filter(geo == "IT", year == 2020)

gdp_deflator_clean <- gdp_deflator %>%
  group_by(geo, year) %>%
  summarise(gdp_deflator = mean(gdp_deflator, na.rm = TRUE), .groups = "drop")

# poi rifai il merge
merged <- te_clean2 %>%
  left_join(int_clean2, by = c("geo", "year")) %>%
  left_join(gdp_deflator_clean, by = c("geo", "year")) %>%
  mutate(
    te_real = value_te / (gdp_deflator / 100),
    int_real = value_int / (gdp_deflator / 100),
    primary_real = te_real - int_real
  ) %>%
  arrange(geo, year) %>%
  group_by(geo) %>%
  mutate(
    te_growth_real = 100 * (te_real / lag(te_real) - 1),
    primary_growth_real = 100 * (primary_real / lag(primary_real) - 1)
  ) %>%
  ungroup()

# --- 1️⃣ Deflattore pulito e coerente ---
gdp_deflator_clean <- gdp_deflator %>%
  # Escludi valori chiaramente fuori scala (>200 o <50)
  filter(gdp_deflator > 50 & gdp_deflator < 200) %>%
  group_by(geo, year) %>%
  summarise(
    gdp_deflator = mean(gdp_deflator, na.rm = TRUE),
    .groups = "drop"
  )

cat("✅ Deflator cleaned:", nrow(gdp_deflator_clean), "rows (should be 27×16=432)\n")

# --- 2️⃣ Ricrea dataset finale coerente ---
merged <- te_clean2 %>%
  left_join(int_clean2, by = c("geo", "year")) %>%
  left_join(gdp_deflator_clean, by = c("geo", "year")) %>%
  mutate(
    te_real = value_te / (gdp_deflator / 100),
    int_real = value_int / (gdp_deflator / 100),
    primary_real = te_real - int_real
  ) %>%
  arrange(geo, year) %>%
  group_by(geo) %>%
  mutate(
    te_growth_real = 100 * (te_real / lag(te_real) - 1),
    primary_growth_real = 100 * (primary_real / lag(primary_real) - 1)
  ) %>%
  ungroup()

# --- 3️⃣ Controlli rapidi ---
cat("✅ Countries:", length(unique(merged$geo)), "\n")
cat("✅ Years:", min(merged$year), "-", max(merged$year), "\n")
cat("✅ Rows:", nrow(merged), "(expected 27×16=432)\n\n")
summary(merged$primary_growth_real)

# --- 🔍 DATA CHECK COMPLETO PRIMA DEL SALVATAGGIO ---

# 1️⃣ Controllo generale sulle variabili principali
summary(merged$te_real)
summary(merged$int_real)
summary(merged$primary_real)
summary(merged$primary_growth_real)

# 2️⃣ Controllo logico: primary expenditure < total expenditure
check_primary <- merged %>%
  summarise(
    n_violation = sum(primary_real > te_real, na.rm = TRUE),
    share_viol = mean(primary_real > te_real, na.rm = TRUE)
  )
cat("⚙️  Controllo primary < total expenditure: ", check_primary$n_violation, 
    " violazioni (", round(100 * check_primary$share_viol, 2), "%)\n")

# 3️⃣ Confronto visivo per alcuni Paesi
merged %>%
  filter(geo %in% c("IT", "DE", "FR", "EL") & year %in% 2019:2021) %>%
  select(geo, year, te_real, int_real, primary_real, primary_growth_real) %>%
  arrange(geo, year) %>%
  print(n = Inf)

# 4️⃣ Range dei tassi di crescita plausibili
growth_check <- merged %>%
  summarise(
    min_growth = min(primary_growth_real, na.rm = TRUE),
    q1_growth = quantile(primary_growth_real, 0.25, na.rm = TRUE),
    median_growth = median(primary_growth_real, na.rm = TRUE),
    mean_growth = mean(primary_growth_real, na.rm = TRUE),
    q3_growth = quantile(primary_growth_real, 0.75, na.rm = TRUE),
    max_growth = max(primary_growth_real, na.rm = TRUE)
  )
cat("\n📊 Range tassi di crescita (primary):\n")
print(growth_check)

# 5️⃣ Controllo su un singolo Paese
cat("\n🇮🇹 Italia 2020–2021:\n")
merged %>%
  filter(geo == "IT", year %in% 2020:2021) %>%
  select(geo, year, te_real, int_real, primary_real, primary_growth_real) %>%
  print()

# 6️⃣ Controllo sul deflatore
cat("\n🧮 Deflator range (plausibile ~90–120):\n")
summary(merged$gdp_deflator)

fwrite(merged, "eurostat_primary_expenditure_real.csv")
cat("✅ File salvato: eurostat_primary_expenditure_real.csv\n")
