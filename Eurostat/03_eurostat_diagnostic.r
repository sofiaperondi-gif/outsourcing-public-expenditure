############################################################
# 01_eurostat_diagnostics.R
# Diagnostic and coverage analysis of Eurostat fiscal data
# (Country–Year, 2006–2021)
############################################################

# --- 0. Setup ---------------------------------------------------------------
rm(list = ls()); gc()
options(scipen = 999)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# --- 1. Paths ---------------------------------------------------------------
setwd("/Users/sofiaperondi/Desktop/Tesina IV /Data selection and summary analysis /Downloaded data")
input_file <- "eurostat_data_real_with_growth.csv"
out_dir <- "outputs_eurostat"
dir.create(out_dir, showWarnings = FALSE)

# --- 2. Load data -----------------------------------------------------------
data <- fread(input_file)
cat("✅ Data loaded:", nrow(data), "rows ×", ncol(data), "columns\n")
cat("Years:", min(data$year), "–", max(data$year), "\n")
cat("Countries:", length(unique(data$geo)), "\n")

# --- 3. Variable overview ---------------------------------------------------
summary(select(
  data,
  total_expenditure_real,
  intermediate_consumption_real,
  compensation_employees_real,
  gross_fixed_capital_real,
  gdp_real
))

# --- 4. Missingness diagnostics --------------------------------------------
missingness <- sapply(data, function(x) mean(is.na(x)) * 100)
missingness_df <- data.frame(
  variable = names(missingness),
  missing_pct = round(missingness, 2)
) %>% arrange(desc(missing_pct))

fwrite(missingness_df, file.path(out_dir, "missingness_summary.csv"))
cat("\nTop 10 variables by missingness:\n")
print(head(missingness_df, 10))

# Plot missingness
p_missing <- ggplot(missingness_df, aes(x = reorder(variable, -missing_pct), y = missing_pct)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Missing Values by Variable", x = "Variable", y = "Missing share (%)") +
  theme_minimal()
ggsave(file.path(out_dir, "plot_missingness_variables.png"), p_missing, width = 8, height = 6, dpi = 300)
print(p_missing)

# --- 5. Coverage by country-year -------------------------------------------
coverage <- data %>%
  group_by(geo, year) %>%
  summarise(obs = n(), .groups = "drop") %>%
  complete(geo, year = 2006:2021, fill = list(obs = 0)) %>%
  mutate(has_data = ifelse(obs > 0, 1, 0))

coverage_summary <- coverage %>%
  group_by(geo) %>%
  summarise(
    first_year = min(year[has_data == 1], na.rm = TRUE),
    last_year = max(year[has_data == 1], na.rm = TRUE),
    total_years = sum(has_data),
    .groups = "drop"
  )

fwrite(coverage_summary, file.path(out_dir, "coverage_summary.csv"))

# Heatmap
p_coverage <- ggplot(coverage, aes(x = year, y = geo, fill = factor(has_data))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "lightgrey", "1" = "steelblue"),
                    labels = c("No data", "Available"), name = "Data") +
  labs(title = "Data Coverage by Country and Year (2006–2021)",
       x = "Year", y = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(out_dir, "plot_coverage_heatmap.png"), p_coverage, width = 9, height = 6, dpi = 300)
print(p_coverage)

# --- 6. Real vs Nominal Consistency Check -----------------------------------
if ("total_expenditure" %in% names(data)) {
  consistency_check <- data %>%
    mutate(ratio_real_nominal = total_expenditure_real / total_expenditure * 100) %>%
    summarise(
      mean_ratio = mean(ratio_real_nominal, na.rm = TRUE),
      min_ratio = min(ratio_real_nominal, na.rm = TRUE),
      max_ratio = max(ratio_real_nominal, na.rm = TRUE)
    )
  print(consistency_check)
  fwrite(consistency_check, file.path(out_dir, "real_vs_nominal_consistency.csv"))
}

# --- 7. Save processed dataset ---------------------------------------------
save(data, file = file.path(out_dir, "eurostat_diagnostic_env.RData"))
cat("\n✅ Diagnostics completed. Results saved in:", normalizePath(out_dir), "\n")