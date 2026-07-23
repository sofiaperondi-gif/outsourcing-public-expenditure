############################################################
# COUNTRY-LEVEL SUBSTITUTION ANALYSIS
# Analyze if substitution prevails in high-outsourcing countries
############################################################

# --- 0. Setup ---------------------------------------------------------------
library(data.table)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

setwd("/Users/sofiaperondi/Desktop/Tesina IV /Data selection and summary analysis /Downloaded data")
out_dir <- "outputs_country_substitution"
dir.create(out_dir, showWarnings = FALSE)

# --- 1. Load and prepare data -----------------------------------------------
data <- fread("eurostat_data_real_with_growth.csv")

# Apply cleaning
data_clean <- data %>%
  distinct(geo, year, .keep_all = TRUE) %>%
  filter(total_expenditure_real >= 1000 & total_expenditure_real <= 1e9 &
           gdp_real >= 1000 & gdp_real <= 1e9)

# Calculate metrics
data_clean <- data_clean %>%
  arrange(geo, year) %>%
  group_by(geo) %>%
  mutate(
    p2_growth_real = 100 * (intermediate_consumption_real / lag(intermediate_consumption_real) - 1),
    d1_growth_real = 100 * (compensation_employees_real / lag(compensation_employees_real) - 1),
    p2_d1_ratio = intermediate_consumption_real / compensation_employees_real,
    p2_share = intermediate_consumption_real / total_expenditure_real * 100,
    d1_share = compensation_employees_real / total_expenditure_real * 100
  ) %>%
  ungroup()

# --- 2. Identify high and low outsourcing intensity countries ---------------
country_outsourcing_2021 <- data_clean %>%
  filter(year == 2021) %>%
  arrange(desc(p2_d1_ratio)) %>%
  select(geo, p2_d1_ratio, p2_share, d1_share)

# Define thresholds for high/low outsourcing
high_outsourcing <- head(country_outsourcing_2021, 5)$geo  # Top 5
low_outsourcing <- tail(country_outsourcing_2021, 5)$geo   # Bottom 5

cat("=== COUNTRY CLASSIFICATION BY OUTSOURCING INTENSITY ===\n")
cat("High outsourcing countries (P2/D1 ratio > 0.7):", paste(high_outsourcing, collapse = ", "), "\n")
cat("Low outsourcing countries (P2/D1 ratio < 0.4):", paste(low_outsourcing, collapse = ", "), "\n")

# --- 3. Calculate country-level correlations --------------------------------
country_correlations <- data_clean %>%
  filter(!is.na(p2_growth_real), !is.na(d1_growth_real),
         abs(p2_growth_real) < 50, abs(d1_growth_real) < 50) %>%
  group_by(geo) %>%
  summarise(
    correlation = cor(p2_growth_real, d1_growth_real, use = "complete.obs"),
    observations = n(),
    avg_p2_d1_ratio = mean(p2_d1_ratio, na.rm = TRUE),
    avg_p2_share = mean(p2_share, na.rm = TRUE),
    avg_d1_share = mean(d1_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    correlation_strength = case_when(
      correlation < -0.3 ~ "Strong Substitution",
      correlation < -0.1 ~ "Weak Substitution",
      correlation > 0.3 ~ "Strong Complementary",
      correlation > 0.1 ~ "Weak Complementary",
      TRUE ~ "No Relationship"
    ),
    outsourcing_intensity = case_when(
      avg_p2_d1_ratio > 0.7 ~ "High",
      avg_p2_d1_ratio < 0.4 ~ "Low", 
      TRUE ~ "Medium"
    )
  )

# --- 4. Analyze substitution patterns by outsourcing intensity -------------
substitution_by_intensity <- country_correlations %>%
  group_by(outsourcing_intensity) %>%
  summarise(
    count = n(),
    mean_correlation = mean(correlation, na.rm = TRUE),
    substitution_count = sum(correlation < -0.1),
    complementary_count = sum(correlation > 0.1),
    no_relationship_count = sum(correlation >= -0.1 & correlation <= 0.1)
  )

cat("\n=== SUBSTITUTION PATTERNS BY OUTSOURCING INTENSITY ===\n")
print(substitution_by_intensity)

# --- 5. Detailed analysis of high-outsourcing countries --------------------
high_outsourcing_analysis <- country_correlations %>%
  filter(geo %in% high_outsourcing) %>%
  arrange(desc(correlation))

cat("\n=== DETAILED ANALYSIS: HIGH OUTSOURCING COUNTRIES ===\n")
print(high_outsourcing_analysis)

# --- 6. PLOT 1: Correlation vs Outsourcing Intensity -----------------------
p1 <- ggplot(country_correlations, aes(x = avg_p2_d1_ratio, y = correlation, 
                                       color = outsourcing_intensity, label = geo)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray50") +
  geom_text(check_overlap = TRUE, size = 3, nudge_y = 0.03) +
  scale_color_manual(values = c("High" = "#d95f02", "Medium" = "#7570b3", "Low" = "#1b9e77")) +
  labs(
    title = "Country-Level Correlation vs Outsourcing Intensity",
    subtitle = "Each point represents one EU country (2006-2021)\nNegative correlation indicates substitution effect",
    x = "Average P2/D1 Ratio (Outsourcing Intensity)",
    y = "Correlation between P2 and D1 Growth",
    color = "Outsourcing Intensity"
  ) +
  theme_minimal()

ggsave(file.path(out_dir, "plot_correlation_vs_intensity.png"), p1, width = 12, height = 8, dpi = 300)
cat("✅ Saved: plot_correlation_vs_intensity.png\n")
print(p1)

# --- 7. PLOT 2: Time trends for high-outsourcing countries ----------------
high_outsourcing_trends <- data_clean %>%
  filter(geo %in% high_outsourcing) %>%
  group_by(geo, year) %>%
  summarise(
    p2_growth = mean(p2_growth_real, na.rm = TRUE),
    d1_growth = mean(d1_growth_real, na.rm = TRUE),
    p2_d1_ratio = mean(p2_d1_ratio, na.rm = TRUE),
    .groups = "drop"
  )

p2 <- ggplot(high_outsourcing_trends, aes(x = year)) +
  geom_line(aes(y = p2_growth, color = "P2 Growth"), linewidth = 1) +
  geom_line(aes(y = d1_growth, color = "D1 Growth"), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~geo, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("P2 Growth" = "#d95f02", "D1 Growth" = "#1b9e77")) +
  labs(
    title = "Growth Trends in High-Outsourcing Countries",
    subtitle = "P2 (procurement) vs D1 (public employment) growth rates\nParallel lines = complementary, Opposite movements = substitution",
    x = "Year", y = "Growth Rate (%)",
    color = "Component"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "plot_high_outsourcing_trends.png"), p2, width = 12, height = 10, dpi = 300)
cat("✅ Saved: plot_high_outsourcing_trends.png\n")
print(p2)

# --- 8. PLOT 3: Substitution patterns across all countries ----------------
p3 <- ggplot(country_correlations, aes(x = reorder(geo, correlation), y = correlation, 
                                       fill = correlation_strength)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c(
    "Strong Substitution" = "#d73027",
    "Weak Substitution" = "#fc8d59", 
    "No Relationship" = "#ffffbf",
    "Weak Complementary" = "#91bfdb",
    "Strong Complementary" = "#4575b4"
  )) +
  labs(
    title = "P2-D1 Growth Correlation by Country",
    subtitle = "Negative values indicate substitution, positive values indicate complementary relationship",
    x = "Country", y = "Correlation Coefficient",
    fill = "Relationship Type"
  ) +
  theme_minimal()

ggsave(file.path(out_dir, "plot_country_correlations.png"), p3, width = 12, height = 10, dpi = 300)
cat("✅ Saved: plot_country_correlations.png\n")
print(p3)

# --- 9. Case study: Countries showing substitution ------------------------
substitution_countries <- country_correlations %>%
  filter(correlation < -0.1) %>%
  arrange(correlation)

if(nrow(substitution_countries) > 0) {
  cat("\n=== COUNTRIES SHOWING SUBSTITUTION EFFECTS ===\n")
  print(substitution_countries)
  
  # Plot substitution countries
  substitution_trends <- data_clean %>%
    filter(geo %in% substitution_countries$geo) %>%
    group_by(geo, year) %>%
    summarise(
      p2_growth = mean(p2_growth_real, na.rm = TRUE),
      d1_growth = mean(d1_growth_real, na.rm = TRUE),
      .groups = "drop"
    )
  
  p4 <- ggplot(substitution_trends, aes(x = year)) +
    geom_line(aes(y = p2_growth, color = "P2 Growth"), linewidth = 1) +
    geom_line(aes(y = d1_growth, color = "D1 Growth"), linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    facet_wrap(~geo, scales = "free_y", ncol = 2) +
    scale_color_manual(values = c("P2 Growth" = "#d95f02", "D1 Growth" = "#1b9e77")) +
    labs(
      title = "Substitution Patterns: Countries with Negative P2-D1 Correlation",
      subtitle = "When P2 grows, D1 tends to decrease (and vice versa)",
      x = "Year", y = "Growth Rate (%)",
      color = "Component"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(file.path(out_dir, "plot_substitution_countries.png"), p4, width = 12, height = 8, dpi = 300)
  cat("✅ Saved: plot_substitution_countries.png\n")
  print(p4)
} else {
  cat("\n❌ No countries show clear substitution effects (correlation < -0.1)\n")
}

# --- 10. Statistical test: Difference in correlations ----------------------
high_intensity_cor <- country_correlations %>% 
  filter(outsourcing_intensity == "High") %>% 
  pull(correlation)

low_intensity_cor <- country_correlations %>% 
  filter(outsourcing_intensity == "Low") %>% 
  pull(correlation)

if(length(high_intensity_cor) > 1 & length(low_intensity_cor) > 1) {
  t_test_result <- t.test(high_intensity_cor, low_intensity_cor)
  
  cat("\n=== STATISTICAL TEST: HIGH vs LOW OUTSOURCING COUNTRIES ===\n")
  cat("Mean correlation - High outsourcing countries:", round(mean(high_intensity_cor), 3), "\n")
  cat("Mean correlation - Low outsourcing countries:", round(mean(low_intensity_cor), 3), "\n")
  cat("T-test p-value:", round(t_test_result$p.value, 4), "\n")
  cat("Interpretation:", ifelse(t_test_result$p.value < 0.05, 
                                "Significant difference in correlation patterns",
                                "No significant difference in correlation patterns"), "\n")
}

# --- 11. Save results ------------------------------------------------------
fwrite(country_correlations, file.path(out_dir, "country_correlations.csv"))
fwrite(substitution_by_intensity, file.path(out_dir, "substitution_by_intensity.csv"))

if(nrow(substitution_countries) > 0) {
  fwrite(substitution_countries, file.path(out_dir, "substitution_countries.csv"))
}

cat("\n🎯 COUNTRY-LEVEL SUBSTITUTION ANALYSIS COMPLETE!\n")
cat("✅ Analyzed P2-D1 relationships for all 27 EU countries\n")
cat("✅ Compared high vs low outsourcing intensity countries\n")
cat("✅ Generated detailed visualizations\n")
cat("✅ All outputs saved in:", normalizePath(out_dir), "\n")