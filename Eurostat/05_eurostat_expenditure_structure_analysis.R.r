############################################################
# 04_eurostat_expenditure_structure_analysis_IMPROVED.R
# Purpose: Comprehensive analysis of expenditure structure and substitution dynamics
# IMPROVED VERSION - Includes all plots and proper data cleaning
############################################################

# --- 0. Setup ---------------------------------------------------------------
library(data.table)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

setwd("/Users/sofiaperondi/Desktop/Tesina IV /Data selection and summary analysis /Downloaded data")
out_dir <- "outputs_eurostat_comprehensive"
dir.create(out_dir, showWarnings = FALSE)

# --- 1. Load and CLEAN data -------------------------------------------------
data <- fread("eurostat_data_real_with_growth.csv")

cat("✅ Data loaded:", nrow(data), "rows ×", ncol(data), "columns\n")

# Apply comprehensive data cleaning
data_clean <- data %>%
  distinct(geo, year, .keep_all = TRUE) %>%
  filter(total_expenditure_real >= 1000 & total_expenditure_real <= 1e9 &
           gdp_real >= 1000 & gdp_real <= 1e9)

cat("After cleaning:", nrow(data_clean), "rows (27 countries × 16 years)\n")
cat("Duplicates removed:", nrow(data) - nrow(data_clean), "\n")

# --- 2. Compute PROPER Growth Rates and Metrics -----------------------------
data_clean <- data_clean %>%
  arrange(geo, year) %>%
  group_by(geo) %>%
  mutate(
    # Growth rates for economic components
    p2_growth_real = 100 * (intermediate_consumption_real / lag(intermediate_consumption_real) - 1),
    d1_growth_real = 100 * (compensation_employees_real / lag(compensation_employees_real) - 1),
    p5_growth_real = 100 * (gross_fixed_capital_real / lag(gross_fixed_capital_real) - 1),
    total_growth_real = 100 * (total_expenditure_real / lag(total_expenditure_real) - 1),
    
    # Substitution metrics
    p2_d1_ratio = intermediate_consumption_real / compensation_employees_real,
    p2_share = intermediate_consumption_real / total_expenditure_real * 100,
    d1_share = compensation_employees_real / total_expenditure_real * 100,
    p5_share = gross_fixed_capital_real / total_expenditure_real * 100,
    
    # COFOG shares
    public_services_share = public_services / total_expenditure_real * 100,
    public_order_share = public_order / total_expenditure_real * 100,
    economic_affairs_share = economic_affairs / total_expenditure_real * 100,
    health_share = health / total_expenditure_real * 100,
    education_share = education / total_expenditure_real * 100,
    social_protection_share = social_protection / total_expenditure_real * 100
  ) %>%
  ungroup()

# --- 3. Summary Statistics --------------------------------------------------
cat("\n=== SUMMARY STATISTICS ===\n")

# Growth rate summary
growth_summary <- data_clean %>%
  summarise(
    avg_p2_growth = mean(p2_growth_real, na.rm = TRUE),
    avg_d1_growth = mean(d1_growth_real, na.rm = TRUE),
    avg_p5_growth = mean(p5_growth_real, na.rm = TRUE),
    sd_p2_growth = sd(p2_growth_real, na.rm = TRUE),
    sd_d1_growth = sd(d1_growth_real, na.rm = TRUE),
    sd_p5_growth = sd(p5_growth_real, na.rm = TRUE)
  )

cat("Average annual growth rates (EU-27, 2006–2021):\n")
cat("P2 (Intermediate Consumption):", round(growth_summary$avg_p2_growth, 2), 
    "% (SD:", round(growth_summary$sd_p2_growth, 2), ")\n")
cat("D1 (Compensation):", round(growth_summary$avg_d1_growth, 2), 
    "% (SD:", round(growth_summary$sd_d1_growth, 2), ")\n")
cat("P5 (Capital Formation):", round(growth_summary$avg_p5_growth, 2), 
    "% (SD:", round(growth_summary$sd_p5_growth, 2), ")\n")

# Share summary
share_summary <- data_clean %>%
  summarise(
    avg_p2_share = mean(p2_share, na.rm = TRUE),
    avg_d1_share = mean(d1_share, na.rm = TRUE),
    avg_p5_share = mean(p5_share, na.rm = TRUE),
    avg_p2_d1_ratio = mean(p2_d1_ratio, na.rm = TRUE)
  )

cat("\nAverage expenditure shares (EU-27, 2006–2021):\n")
cat("P2 Share:", round(share_summary$avg_p2_share, 1), "%\n")
cat("D1 Share:", round(share_summary$avg_d1_share, 1), "%\n")
cat("P5 Share:", round(share_summary$avg_p5_share, 1), "%\n")
cat("P2/D1 Ratio:", round(share_summary$avg_p2_d1_ratio, 2), "\n")

# --- 4. PLOT 1: Growth Rate Trends ------------------------------------------
growth_year <- data_clean %>%
  group_by(year) %>%
  summarise(
    mean_p2 = mean(p2_growth_real, na.rm = TRUE),
    mean_d1 = mean(d1_growth_real, na.rm = TRUE),
    mean_p5 = mean(p5_growth_real, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with("mean_"),
               names_to = "variable", values_to = "growth") %>%
  mutate(variable = recode(variable,
                           mean_p2 = "Intermediate Consumption (P2)",
                           mean_d1 = "Compensation of Employees (D1)",
                           mean_p5 = "Gross Fixed Capital Formation (P5)"))

# Remove NA values for clean plotting
growth_year_clean <- growth_year %>% filter(!is.na(growth))

p_growth <- ggplot(growth_year_clean, aes(x = year, y = growth, color = variable)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  labs(
    title = "Average Annual Growth Rate of Expenditure Components (EU-27, 2006–2021)",
    subtitle = "Year-over-year percentage change in real terms",
    x = "Year", y = "Growth Rate (%)", 
    color = "Component"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "plot_growth_rates_p2_d1_p5.png"),
       p_growth, width = 10, height = 6, dpi = 300)
cat("✅ Saved: plot_growth_rates_p2_d1_p5.png\n")
print(p_growth)

# --- 5. PLOT 2: Expenditure Composition (Area Chart) -----------------------
composition_year <- data_clean %>%
  group_by(year) %>%
  summarise(
    total = sum(total_expenditure_real, na.rm = TRUE),
    p2 = sum(intermediate_consumption_real, na.rm = TRUE),
    d1 = sum(compensation_employees_real, na.rm = TRUE),
    p5 = sum(gross_fixed_capital_real, na.rm = TRUE)
  ) %>%
  mutate(
    share_p2 = p2 / total,
    share_d1 = d1 / total,
    share_p5 = p5 / total
  ) %>%
  select(year, share_p2, share_d1, share_p5) %>%
  pivot_longer(cols = starts_with("share_"),
               names_to = "component", values_to = "share") %>%
  mutate(component = recode(component,
                            share_p2 = "Intermediate Consumption (P2)",
                            share_d1 = "Compensation of Employees (D1)",
                            share_p5 = "Gross Fixed Capital Formation (P5)"))

p_composition_area <- ggplot(composition_year, aes(x = year, y = share, fill = component)) +
  geom_area(position = "fill", alpha = 0.8) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  labs(
    title = "Composition of Public Expenditure by Economic Type (EU-27, 2006–2021)",
    subtitle = "Area chart showing relative shares over time",
    x = "Year", y = "Share of Total Expenditure", 
    fill = "Component"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "plot_expenditure_composition_area.png"),
       p_composition_area, width = 10, height = 6, dpi = 300)
cat("✅ Saved: plot_expenditure_composition_area.png\n")
print(p_composition_area)

# --- 6. PLOT 3: Expenditure Composition (Line Chart) -----------------------
p_composition_line <- ggplot(composition_year, aes(x = year, y = share, color = component)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  labs(
    title = "Expenditure Shares by Economic Type - Line Chart View",
    subtitle = "EU-27 Average (2006-2021)",
    x = "Year", y = "Share of Total Expenditure", 
    color = "Component"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "plot_expenditure_composition_line.png"),
       p_composition_line, width = 10, height = 6, dpi = 300)
cat("✅ Saved: plot_expenditure_composition_line.png\n")
print(p_composition_line)

# --- 7. PLOT 4: COFOG Composition ------------------------------------------
cofog_long <- data_clean %>%
  select(year, public_services, public_order, economic_affairs,
         health, education, social_protection, total_expenditure_real) %>%
  group_by(year) %>%
  summarise(
    across(
      c(public_services, public_order, economic_affairs,
        health, education, social_protection),
      \(x) sum(x, na.rm = TRUE)
    ),
    total = sum(total_expenditure_real, na.rm = TRUE)
  ) %>%
  mutate(across(-c(year, total), ~ .x / total, .names = "share_{col}")) %>%
  select(year, starts_with("share_")) %>%
  pivot_longer(cols = -year, names_to = "policy_function", values_to = "share") %>%
  mutate(policy_function = recode(policy_function,
                                  share_public_services = "General Public Services",
                                  share_public_order = "Public Order and Safety",
                                  share_economic_affairs = "Economic Affairs",
                                  share_health = "Health",
                                  share_education = "Education",
                                  share_social_protection = "Social Protection"))

p_cofog <- ggplot(cofog_long, aes(x = year, y = share, fill = policy_function)) +
  geom_area(alpha = 0.9) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Composition of Public Expenditure by Policy Function (COFOG, EU-27, 2006–2021)",
    x = "Year", y = "Share of Total Expenditure", 
    fill = "Function"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "plot_expenditure_composition_COFOG.png"),
       p_cofog, width = 10, height = 6, dpi = 300)
cat("✅ Saved: plot_expenditure_composition_COFOG.png\n")
print(p_cofog)

# --- 8. PLOT 5: P2/D1 Ratio Trend ------------------------------------------
substitution_trends <- data_clean %>%
  group_by(year) %>%
  summarise(
    mean_p2_share = mean(p2_share, na.rm = TRUE),
    mean_d1_share = mean(d1_share, na.rm = TRUE),
    mean_p5_share = mean(p5_share, na.rm = TRUE),
    mean_p2_d1_ratio = mean(p2_d1_ratio, na.rm = TRUE)
  )

p_p2_d1_ratio <- ggplot(substitution_trends, aes(x = year, y = mean_p2_d1_ratio)) +
  geom_line(color = "#e7298a", linewidth = 1.2) +
  geom_point(color = "#e7298a", size = 2.5) +
  labs(
    title = "P2/D1 Ratio - Procurement vs Public Employment Intensity",
    subtitle = "Higher values indicate greater reliance on external procurement\nEU-27 Average (2006-2021)",
    x = "Year", y = "P2/D1 Ratio"
  ) +
  theme_minimal()

ggsave(file.path(out_dir, "plot_p2_d1_ratio_trend.png"),
       p_p2_d1_ratio, width = 10, height = 6, dpi = 300)
cat("✅ Saved: plot_p2_d1_ratio_trend.png\n")
print(p_p2_d1_ratio)

# --- 9. PLOT 6: Component Growth Comparison --------------------------------
growth_comparison <- data_clean %>%
  group_by(year) %>%
  summarise(
    mean_p2_growth = mean(p2_growth_real, na.rm = TRUE),
    mean_d1_growth = mean(d1_growth_real, na.rm = TRUE),
    mean_p5_growth = mean(p5_growth_real, na.rm = TRUE)
  ) %>%
  filter(!is.na(mean_p2_growth))

growth_comparison_long <- growth_comparison %>%
  pivot_longer(cols = -year, names_to = "component", values_to = "growth") %>%
  mutate(component = recode(component,
                            mean_p2_growth = "Intermediate Consumption (P2)",
                            mean_d1_growth = "Compensation of Employees (D1)",
                            mean_p5_growth = "Gross Fixed Capital Formation (P5)"))

p_growth_comparison <- ggplot(growth_comparison_long, aes(x = year, y = growth, color = component)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  labs(
    title = "Growth Rate Comparison of Expenditure Components",
    subtitle = "EU-27 Average Annual Growth (2007-2021)\nDiverging trends may indicate substitution effects",
    x = "Year", y = "Growth Rate (%)", 
    color = "Component"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "plot_growth_comparison.png"),
       p_growth_comparison, width = 10, height = 6, dpi = 300)
cat("✅ Saved: plot_growth_comparison.png\n")
print(p_growth_comparison)

# --- 10. Correlation Analysis ----------------------------------------------
corr_data <- data_clean %>%
  filter(is.finite(p2_growth_real), is.finite(d1_growth_real),
         abs(p2_growth_real) < 50, abs(d1_growth_real) < 50)

correlation_p2_d1 <- cor(corr_data$p2_growth_real, corr_data$d1_growth_real, 
                         use = "complete.obs")

cat("\n=== CORRELATION ANALYSIS ===\n")
cat("Correlation between P2 growth and D1 growth:", round(correlation_p2_d1, 3), "\n")

# Interpretation
if(correlation_p2_d1 < -0.2) {
  cat("Interpretation: Strong negative correlation suggests SUBSTITUTION effect\n")
} else if(correlation_p2_d1 > 0.2) {
  cat("Interpretation: Strong positive correlation suggests COMPLEMENTARY relationship\n") 
} else if(correlation_p2_d1 < -0.1) {
  cat("Interpretation: Moderate negative correlation suggests weak substitution effect\n")
} else if(correlation_p2_d1 > 0.1) {
  cat("Interpretation: Moderate positive correlation suggests weak complementary relationship\n")
} else {
  cat("Interpretation: Very weak correlation suggests independent dynamics\n")
}

# --- 11. Country Variation Analysis ----------------------------------------
country_variation_2021 <- data_clean %>%
  filter(year == 2021) %>%
  arrange(desc(p2_d1_ratio)) %>%
  select(geo, p2_share, d1_share, p2_d1_ratio)

cat("\n=== COUNTRY VARIATION (2021) ===\n")
cat("Top 5 countries by P2/D1 ratio (highest outsourcing intensity):\n")
print(head(country_variation_2021, 5))
cat("\nBottom 5 countries by P2/D1 ratio (lowest outsourcing intensity):\n")
print(tail(country_variation_2021, 5))

# --- 12. Save All Results --------------------------------------------------
fwrite(growth_year, file.path(out_dir, "growth_rates_p2_d1_p5.csv"))
fwrite(composition_year, file.path(out_dir, "composition_economic_type.csv"))
fwrite(cofog_long, file.path(out_dir, "composition_cofog.csv"))
fwrite(substitution_trends, file.path(out_dir, "substitution_trends.csv"))
fwrite(country_variation_2021, file.path(out_dir, "country_variation_2021.csv"))

# Save summary statistics
summary_stats <- data.frame(
  Metric = c("P2_Avg_Growth", "D1_Avg_Growth", "P5_Avg_Growth", 
             "P2_Avg_Share", "D1_Avg_Share", "P5_Avg_Share",
             "P2_D1_Correlation"),
  Value = c(growth_summary$avg_p2_growth, growth_summary$avg_d1_growth, growth_summary$avg_p5_growth,
            share_summary$avg_p2_share, share_summary$avg_d1_share, share_summary$avg_p5_share,
            correlation_p2_d1)
)
fwrite(summary_stats, file.path(out_dir, "summary_statistics.csv"))

cat("\n🎉 COMPREHENSIVE ANALYSIS COMPLETE!\n")
cat("✅ Generated 6 detailed plots\n")
cat("✅ Saved multiple datasets for further analysis\n")
cat("✅ All outputs saved in:", normalizePath(out_dir), "\n")
cat("📊 Key insights ready for R Markdown reporting\n")
