############################################################
# DEBUG SCRIPT - Check data issues
############################################################

# --- Setup ---
options(scipen = 999)
library(data.table)
library(dplyr)

# --- Load the data ---
setwd("/Users/sofiaperondi/Desktop/Tesina IV /Data selection and summary analysis /Downloaded data")
data <- fread("eurostat_data_real_with_growth.csv")

# --- Data Diagnostics ---
cat("=== DATA DIAGNOSTICS ===\n")

# Check the actual values in the data
cat("\n1. VALUE RANGES (Original Data):\n")
cat("Total expenditure:", min(data$total_expenditure_real, na.rm = TRUE), "to", 
    max(data$total_expenditure_real, na.rm = TRUE), "\n")
cat("GDP:", min(data$gdp_real, na.rm = TRUE), "to", 
    max(data$gdp_real, na.rm = TRUE), "\n")

# Check a few specific countries for reality check
cat("\n2. SAMPLE COUNTRY VALUES (2021):\n")
sample_2021 <- data %>% 
  filter(year == 2021) %>%
  select(geo, total_expenditure_real, gdp_real) %>%
  head(5)
print(sample_2021)

# Check if values look reasonable
# Germany's GDP should be around 3-4 trillion EUR → 3,000,000 - 4,000,000 million EUR
# If your numbers are in thousands instead of millions, we'll see 3,000,000,000 - 4,000,000,000

cat("\n3. CHECK FOR DEFLATION ISSUES:\n")
# Let's see if the deflation was applied correctly
# Compare nominal and real values if you have them
if("total_expenditure_nominal" %in% names(data)) {
  cat("Nominal vs Real comparison available\n")
  comparison <- data %>% 
    filter(year == 2021) %>%
    select(geo, total_expenditure_nominal, total_expenditure_real) %>%
    head(3)
  print(comparison)
} else {
  cat("Only real values available in dataset\n")
}

# Check growth rates - they should be reasonable percentages
cat("\n4. GROWTH RATE CHECK:\n")
cat("Expenditure growth range:", min(data$total_expenditure_growth_real, na.rm = TRUE), "to",
    max(data$total_expenditure_growth_real, na.rm = TRUE), "\n")
cat("GDP growth range:", min(data$gdp_growth_real, na.rm = TRUE), "to",
    max(data$gdp_growth_real, na.rm = TRUE), "\n")

# Check if growth rates look like percentages or decimals
growth_sample <- data %>% 
  select(geo, year, total_expenditure_growth_real, gdp_growth_real) %>%
  filter(!is.na(total_expenditure_growth_real)) %>%
  head(5)
print(growth_sample)

cat("\n5. CORRELATION DIAGNOSTIC:\n")
# Check what we're actually correlating
corr_check <- data %>%
  filter(is.finite(total_expenditure_growth_real),
         is.finite(gdp_growth_real))

cat("Observations for correlation:", nrow(corr_check), "\n")
cat("Unique expenditure growth values:", length(unique(corr_check$total_expenditure_growth_real)), "\n")
cat("Unique GDP growth values:", length(unique(corr_check$gdp_growth_real)), "\n")

# If correlation is exactly 1, check if variables are identical
if(nrow(corr_check) > 0) {
  identical_check <- all.equal(corr_check$total_expenditure_growth_real, 
                               corr_check$gdp_growth_real)
  cat("Are growth rates identical?", identical_check, "\n")
}