# === 0. Libraries ===
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

# === 1. Set working directory ===
setwd("/Users/sofiaperondi/Desktop/Tesina IV /Data selection and summary analysis /Downloaded data")

#OPEN GPPD


# === 8. Diagnostics ===
missingness <- sapply(gppd_complete, function(x) mean(is.na(x)) * 100)
missingness <- sort(missingness, decreasing = TRUE)
cat("Top missingness variables:\n")
print(head(missingness, 10))

# Check distribution of CPV / supplytype
print(head(gppd_complete$tender_supplytype, 10))
print(table(gppd_complete$tender_supplytype, useNA = "ifany"))


# === 9. Summary tables ===
# Contracts by supply type
summary_supplytype <- gppd_complete %>%
  group_by(tender_supplytype) %>%
  summarise(
    n_contracts = n(),
    total_value_billion = sum(tender_digiwhist_price, na.rm = TRUE) / 1e9,
    avg_value_million = mean(tender_digiwhist_price, na.rm = TRUE) / 1e6,
    .groups = "drop"
  )

# Contracts by country-year
summary_country_year <- gppd_complete %>%
  group_by(buyer_country, tender_year) %>%
  summarise(
    n_contracts = n(),
    total_value_billion = sum(tender_digiwhist_price, na.rm = TRUE) / 1e9,
    .groups = "drop"
  )

# Save summaries
fwrite(summary_supplytype, "gppd_summary_supplytype.csv")
fwrite(summary_country_year, "gppd_summary_country_year.csv")

cat("\n✅ Summaries saved: gppd_summary_supplytype.csv & gppd_summary_country_year.csv\n")

# Load the summaries
supply_summary <- fread("gppd_summary_supplytype.csv")
country_year_summary <- fread("gppd_summary_country_year.csv")

# Print to console
print(supply_summary)
print(head(country_year_summary, 20))  # show first 20 rows

coverage <- gppd_complete %>%
  group_by(buyer_country, tender_year) %>%
  summarise(n_contracts = n(), .groups = "drop") %>%
  tidyr::complete(buyer_country, tender_year = 2006:2021, fill = list(n_contracts = 0))

# Years per country with data
coverage_summary <- coverage %>%
  group_by(buyer_country) %>%
  summarise(
    first_year = min(tender_year[n_contracts > 0], na.rm = TRUE),
    last_year  = max(tender_year[n_contracts > 0], na.rm = TRUE),
    total_years = sum(n_contracts > 0),
    .groups = "drop"
  )

print(coverage_summary, n = 27)

# === 1. Unbalanced panel (2006–2021, fill missing with zeros) ===
panel_unbalanced <- gppd_complete %>%
  group_by(buyer_country, tender_year) %>%
  summarise(
    n_contracts = n(),
    total_value_billion = sum(tender_digiwhist_price, na.rm = TRUE) / 1e9,
    .groups = "drop"
  ) %>%
  complete(
    buyer_country,
    tender_year = 2006:2021,
    fill = list(n_contracts = 0, total_value_billion = 0)
  )

# === 2. Balanced trimmed panel (2011–2021) ===
panel_balanced <- panel_unbalanced %>%
  filter(tender_year >= 2011 & tender_year <= 2021)

# === 3. Compare coverage ===
compare_loss <- data.frame(
  Dataset = c("Unbalanced 2006–2021", "Balanced 2011–2021"),
  Years = c(length(unique(panel_unbalanced$tender_year)),
            length(unique(panel_balanced$tender_year))),
  Total_contracts = c(sum(panel_unbalanced$n_contracts),
                      sum(panel_balanced$n_contracts)),
  Total_value_B = c(sum(panel_unbalanced$total_value_billion),
                    sum(panel_balanced$total_value_billion))
)

# Calculate loss in absolute terms and percentage
loss_contracts <- compare_loss$Total_contracts[1] - compare_loss$Total_contracts[2]
loss_contracts_pct <- 100 * loss_contracts / compare_loss$Total_contracts[1]

loss_value <- compare_loss$Total_value_B[1] - compare_loss$Total_value_B[2]
loss_value_pct <- 100 * loss_value / compare_loss$Total_value_B[1]

cat("\n=== COVERAGE COMPARISON ===\n")
print(compare_loss)
cat("\nContracts lost when trimming:", loss_contracts, 
    "(", round(loss_contracts_pct,2), "% )\n")
cat("Value lost when trimming:", round(loss_value,2), "B EUR", 
    "(", round(loss_value_pct,2), "% )\n")

# Build coverage matrix
coverage_matrix <- gppd_complete %>%
  group_by(buyer_country, tender_year) %>%
  summarise(n_contracts = n(), .groups = "drop") %>%
  complete(buyer_country, tender_year = 2006:2021, fill = list(n_contracts = 0)) %>%
  mutate(has_data = ifelse(n_contracts > 0, 1, 0))

# Heatmap
ggplot(coverage_matrix, aes(x = tender_year, y = buyer_country, fill = factor(has_data))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "lightgrey", "1" = "steelblue"),
                    name = "Data available",
                    labels = c("No", "Yes")) +
  labs(title = "Coverage of GPPD Contracts by Country and Year",
       x = "Year", y = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# === FULL PANEL: 2006–2021 ===
gppd_full <- gppd_complete %>%
  filter(tender_year >= 2006, tender_year <= 2021)

write.csv(gppd_full, "gppd_panel_2006_2021.csv", row.names = FALSE)
save(gppd_full, file = "gppd_panel_2006_2021.RData")

cat("✅ Saved full panel (2006–2021):", nrow(gppd_full), "contracts\n")

# === COMPARABLE PANEL: 2011–2021 ===
gppd_2011 <- gppd_complete %>%
  filter(tender_year >= 2011, tender_year <= 2021)

write.csv(gppd_2011, "gppd_panel_2011_2021.csv", row.names = FALSE)
save(gppd_2011, file = "gppd_panel_2011_2021.RData")

cat("✅ Saved comparable panel (2011–2021):", nrow(gppd_2011), "contracts\n")