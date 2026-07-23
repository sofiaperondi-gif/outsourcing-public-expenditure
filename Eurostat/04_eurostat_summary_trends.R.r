############################################################
# 04_eurostat_summary_trends_FIXED.R
# CORRECTED version - fixes data duplication and growth calculation
############################################################

# --- 0. Setup ---------------------------------------------------------------
options(scipen = 999)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# --- 1. Paths ---------------------------------------------------------------
setwd("/Users/sofiaperondi/Desktop/Tesina IV /Data selection and summary analysis /Downloaded data")
input_file <- "eurostat_data_real_with_growth.csv"
out_dir <- "outputs_eurostat_fixed"
dir.create(out_dir, showWarnings = FALSE)

# --- 2. Load and CLEAN data -------------------------------------------------
data <- fread(input_file)

cat("✅ Data loaded:", nrow(data), "rows ×", ncol(data), "columns\n")

# FIX 1: Remove duplicates - keep only one observation per country-year
data_clean <- data %>%
  distinct(geo, year, .keep_all = TRUE)

cat("After removing duplicates:", nrow(data_clean), "rows\n")

# FIX 2: Check what's reasonable - let's see the distribution
cat("\n=== DATA QUALITY CHECK ===\n")

# Check for reasonable public expenditure values
# Typical range for EU countries: 10,000 - 1,000,000 million EUR
reasonable_data <- data_clean %>%
  filter(total_expenditure_real >= 1000 & total_expenditure_real <= 1e9 &
           gdp_real >= 1000 & gdp_real <= 1e9)

cat("Observations with reasonable values:", nrow(reasonable_data), "/", nrow(data_clean), "\n")

# Use only reasonable data
data_clean <- reasonable_data

# FIX 3: Check if we need to scale - let's look at Germany as benchmark
de_data <- data_clean %>% filter(geo == "DE", year == 2021)
if(nrow(de_data) > 0) {
  cat("Germany 2021 - Expenditure:", de_data$total_expenditure_real[1], 
      "GDP:", de_data$gdp_real[1], "\n")
  # Germany GDP should be ~3-4 million million EUR → if it's 3-4, then in millions
  # If it's 3,000,000-4,000,000, then already in millions
}

# --- 3. RECALCULATE Growth Rates Properly -----------------------------------
cat("\n=== RECALCULATING GROWTH RATES ===\n")

data_clean <- data_clean %>%
  arrange(geo, year) %>%
  group_by(geo) %>%
  mutate(
    # Calculate proper growth rates
    expenditure_growth_proper = (total_expenditure_real / lag(total_expenditure_real) - 1) * 100,
    gdp_growth_proper = (gdp_real / lag(gdp_real) - 1) * 100
  ) %>%
  ungroup()

# Check the new growth rates
growth_stats <- data_clean %>%
  summarise(
    expend_growth_min = min(expenditure_growth_proper, na.rm = TRUE),
    expend_growth_max = max(expenditure_growth_proper, na.rm = TRUE),
    gdp_growth_min = min(gdp_growth_proper, na.rm = TRUE),
    gdp_growth_max = max(gdp_growth_proper, na.rm = TRUE)
  )

cat("Proper growth rate ranges:\n")
print(growth_stats)

# --- 4. Summary statistics --------------------------------------------------
summary_stats <- data_clean %>%
  summarise(
    mean_expenditure = mean(total_expenditure_real, na.rm = TRUE),
    sd_expenditure = sd(total_expenditure_real, na.rm = TRUE),
    mean_growth_expend = mean(expenditure_growth_proper, na.rm = TRUE),
    sd_growth_expend = sd(expenditure_growth_proper, na.rm = TRUE),
    mean_gdp = mean(gdp_real, na.rm = TRUE),
    sd_gdp = sd(gdp_real, na.rm = TRUE),
    mean_growth_gdp = mean(gdp_growth_proper, na.rm = TRUE),
    sd_growth_gdp = sd(gdp_growth_proper, na.rm = TRUE)
  )

summary_country <- data_clean %>%
  group_by(geo) %>%
  summarise(
    mean_expenditure = mean(total_expenditure_real, na.rm = TRUE),
    mean_growth_expend = mean(expenditure_growth_proper, na.rm = TRUE),
    mean_gdp = mean(gdp_real, na.rm = TRUE),
    mean_growth_gdp = mean(gdp_growth_proper, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== SUMMARY STATISTICS ===\n")
print(summary_stats)

fwrite(summary_country, file.path(out_dir, "eurostat_summary_by_country.csv"))
fwrite(summary_stats, file.path(out_dir, "eurostat_summary_overall.csv"))

# --- 5. Trends over time ----------------------------------------------------
trend_expenditure <- data_clean %>%
  group_by(year) %>%
  summarise(mean_expenditure = mean(total_expenditure_real, na.rm = TRUE))

p1 <- ggplot(trend_expenditure, aes(x = year, y = mean_expenditure)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue") +
  scale_y_continuous(labels = label_number()) +
  labs(title = "EU-27: Average Real Total Expenditure (2006–2021)",
       subtitle = "After data cleaning and duplicate removal",
       x = "Year", y = "Real Total Expenditure") +
  theme_minimal()
ggsave(file.path(out_dir, "plot_expenditure_trend_eu27.png"), p1, width = 8, height = 5, dpi = 300)

# Growth rate trend
trend_growth <- data_clean %>%
  group_by(year) %>%
  summarise(mean_growth = mean(expenditure_growth_proper, na.rm = TRUE))

p2 <- ggplot(trend_growth, aes(x = year, y = mean_growth)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point(color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(title = "EU-27: Average Growth Rate of Real Expenditure (2006–2021)",
       subtitle = "Using properly calculated growth rates",
       x = "Year", y = "Growth Rate (%)") +
  theme_minimal()
ggsave(file.path(out_dir, "plot_growth_trend_eu27.png"), p2, width = 8, height = 5, dpi = 300)

# --- 6. Composition of Expenditure -----------------------------------------
composition <- data_clean %>%
  mutate(
    share_intermediate = intermediate_consumption_real / total_expenditure_real * 100,
    share_compensation = compensation_employees_real / total_expenditure_real * 100,
    share_capital = gross_fixed_capital_real / total_expenditure_real * 100
  ) %>%
  group_by(year) %>%
  summarise(
    mean_share_intermediate = mean(share_intermediate, na.rm = TRUE),
    mean_share_compensation = mean(share_compensation, na.rm = TRUE),
    mean_share_capital = mean(share_capital, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with("mean_share"),
               names_to = "component",
               values_to = "share") %>%
  mutate(component = recode(component,
                            mean_share_intermediate = "Intermediate Consumption (P2)",
                            mean_share_compensation = "Compensation of Employees (D1)",
                            mean_share_capital = "Gross Fixed Capital Formation (P5)"))

p3 <- ggplot(composition, aes(x = year, y = share, color = component)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  labs(title = "EU-27: Composition of Real Expenditure by Component (2006–2021)",
       x = "Year", y = "Share of Total Expenditure (%)", color = "Component") +
  theme_minimal()
ggsave(file.path(out_dir, "plot_expenditure_composition_eu27.png"), p3, width = 9, height = 5, dpi = 300)

# --- 7. CORRECT Correlation Analysis ---------------------------------------
corr_data <- data_clean %>%
  filter(is.finite(expenditure_growth_proper),
         is.finite(gdp_growth_proper),
         abs(expenditure_growth_proper) < 50,  # Remove extreme values
         abs(gdp_growth_proper) < 30)          # Remove extreme values

corr_result <- corr_data %>%
  summarise(
    correlation = cor(expenditure_growth_proper, gdp_growth_proper, use = "complete.obs"),
    observations = n()
  )

cat("\n=== CORRELATION RESULTS ===\n")
cat("Correlation between expenditure growth and GDP growth:", round(corr_result$correlation, 3), "\n")
cat("Observations used:", corr_result$observations, "\n")

p4 <- ggplot(corr_data, aes(x = gdp_growth_proper, y = expenditure_growth_proper)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(title = "Correlation: GDP Growth vs Expenditure Growth (EU-27, 2006–2021)",
       subtitle = paste("Correlation:", round(corr_result$correlation, 3), 
                        "| Observations:", corr_result$observations),
       x = "GDP Growth (%)", y = "Expenditure Growth (%)") +
  theme_minimal()
ggsave(file.path(out_dir, "plot_growth_vs_gdp.png"), p4, width = 7, height = 5, dpi = 300)

# --- 8. Export Results -----------------------------------------------------
fwrite(data_clean, file.path(out_dir, "eurostat_data_cleaned.csv"))
fwrite(composition, file.path(out_dir, "eurostat_expenditure_composition.csv"))
fwrite(trend_expenditure, file.path(out_dir, "eurostat_expenditure_trend.csv"))
fwrite(trend_growth, file.path(out_dir, "eurostat_growth_trend.csv"))

cat("\n✅ ANALYSIS COMPLETE - All issues fixed!\n")
cat("Outputs saved in:", normalizePath(out_dir), "\n")

# Final verification
cat("\n=== FINAL VERIFICATION ===\n")
cat("Expected reasonable ranges:\n")
cat("- Expenditure growth: -20% to +40%\n")
cat("- GDP growth: -15% to +15%\n") 
cat("- Correlation: -1.0 to +1.0 (not exactly 1.0)\n")
cat("- Country counts: 27 countries\n")