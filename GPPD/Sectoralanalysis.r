# Set your working directory 
setwd("/Users/sofiaperondi/Desktop/Tesina IV /Data selection and summary analysis /Downloaded data")  # Change this to your actual folder

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Clear memory and restart
rm(list = ls())
gc()

# Load packages
install.packages("R.utils") 
install.packages("stringi")
install.packages("Matrix", type = "binary")
install.packages("here")  # only once

library(dplyr)
library(tidyr)
library(data.table)
library(lme4)       # Mixed models for variance components
library(performance)# ICC calculation
library(ggplot2)    # Visualization
library(stringr)
library(scales)

#open GPPD
gppd_complete <- read.csv("gppd_outsourcing_data_FINAL.csv")

# Use tender_supplytype for sector classification instead of tender_maincpv
gppd_analysis <- gppd_complete %>%
  filter(!is.na(tender_digiwhist_price) & tender_digiwhist_price > 0) %>%
  mutate(
    tender_supplytype = toupper(trimws(tender_supplytype)),
    sector = case_when(
      tender_supplytype == "SERVICES" ~ "Services",
      tender_supplytype == "SUPPLIES" ~ "Supplies", 
      tender_supplytype == "WORKS" ~ "Works",
      TRUE ~ "Other"
    ),
    # Log transform contract value for normality
    log_value = log(tender_digiwhist_price + 1),
    # Convert to factors for mixed models
    sector = as.factor(sector),
    buyer_country = as.factor(buyer_country),
    tender_year = as.factor(tender_year)
    )

# Check the sector distribution
cat("=== SECTOR DISTRIBUTION ===\n")
sector_dist <- gppd_analysis %>%
  group_by(sector) %>%
  summarise(
    n_contracts = n(),
    total_value = sum(tender_digiwhist_price, na.rm = TRUE) / 1e9,  # Billions
    avg_value = mean(tender_digiwhist_price, na.rm = TRUE) / 1e6,   # Millions
    .groups = 'drop'
  ) %>%
  mutate(share = n_contracts / sum(n_contracts) * 100)

print(sector_dist)

# Model 1: Basic variance decomposition
cat("=== VARIANCE COMPONENTS ANALYSIS ===\n")

model1 <- lmer(log_value ~ 1 + (1|sector) + (1|buyer_country) + (1|tender_year), 
               data = gppd_analysis)

# Check model convergence and summary
cat("Model convergence:", ifelse(isSingular(model1), "Issues", "OK"), "\n")
summary(model1)

# Extract variance components
vc <- as.data.frame(VarCorr(model1))
cat("\nVariance Components:\n")
print(vc)

# Calculate percentage of variance explained by each component
total_variance <- sum(vc$vcov)
vc$variance_pct <- vc$vcov / total_variance * 100
cat("\nVariance Explained (%):\n")
print(vc[, c("grp", "var1", "vcov", "variance_pct")])

# Additional sector dispersion measures
cat("=== SECTORAL DISPERSION MEASURES ===\n")

# 1. Sector concentration by country
sector_concentration <- gppd_analysis %>%
  group_by(buyer_country, sector) %>%
  summarise(sector_value = sum(tender_digiwhist_price, na.rm = TRUE), .groups = 'drop') %>%
  group_by(buyer_country) %>%
  mutate(
    total_country = sum(sector_value),
    sector_share = sector_value / total_country
  ) %>%
  summarise(
    hhi = sum(sector_share^2) * 10000,  # Herfindahl-Hirschman Index
    n_sectors = n_distinct(sector),
    dominant_sector = sector[which.max(sector_share)],
    dominant_share = max(sector_share),
    .groups = 'drop'
  )

cat("Sector Concentration by Country:\n")
print(sector_concentration)

# 2. Sector trends over time
sector_trends <- gppd_analysis %>%
  group_by(tender_year, sector) %>%
  summarise(
    total_value = sum(tender_digiwhist_price, na.rm = TRUE),
    n_contracts = n(),
    .groups = 'drop'
  ) %>%
  group_by(tender_year) %>%
  mutate(
    annual_total = sum(total_value),
    sector_share = total_value / annual_total
  )

cat("\nSector Trends Over Time (Sample):\n")
print(head(sector_trends))

# Visualize variance components
library(ggplot2)

# Plot sector shares over time
ggplot(sector_trends, aes(x = as.numeric(as.character(tender_year)), y = sector_share)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(linewidth = 2) +
  facet_wrap(~sector) +
  labs(title = "Sectoral Share of Public Procurement Over Time",
       x = "Year", y = "Share of Total Contract Value") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)

# Plot variance components
vc_plot <- vc[vc$grp != "Residual", ]
vc_plot$component <- vc_plot$grp

ggplot(vc_plot, aes(x = reorder(component, variance_pct), y = variance_pct)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Variance Components in Contract Values",
       x = "Component", y = "Percentage of Total Variance Explained") +
  theme_minimal()

cat("=== SECTORAL DISPERSION ANALYSIS REPORT ===\n\n")

cat("1. DATA SUMMARY:\n")
cat("   Total contracts analyzed:", nrow(gppd_analysis), "\n")
cat("   Countries:", length(unique(gppd_analysis$buyer_country)), "\n")
cat("   Years:", paste(range(as.numeric(as.character(gppd_analysis$tender_year))), collapse = "-"), "\n")
cat("   Sectors: Services, Supplies, Works\n\n")

cat("2. VARIANCE DECOMPOSITION:\n")
cat("   Total variance in log(contract values):", round(total_variance, 3), "\n")
for (i in 1:nrow(vc)) {
  if (vc$grp[i] != "Residual") {
    cat("   -", vc$grp[i], ":", round(vc$variance_pct[i], 1), "%\n")
  }
}
cat("   - Residual (within-group) variance:", 
    round(vc$variance_pct[vc$grp == "Residual"], 1), "%\n\n")

cat("3. SECTORAL CONCENTRATION:\n")
cat("   Average HHI across countries:", round(mean(sector_concentration$hhi), 0), "\n")
cat("   Average number of sectors per country:", round(mean(sector_concentration$n_sectors), 1), "\n")

#Export data
write.csv(sector_dist, "sector_distribution.csv", row.names = FALSE)
write.csv(vc, "variance_components.csv", row.names = FALSE)
write.csv(sector_concentration, "sector_concentration.csv", row.names = FALSE)
write.csv(sector_trends, "sector_trends.csv", row.names = FALSE)

# sectoral shares
sector_plot <- sector_dist %>%
  mutate(
    total_value = total_value * 1e9,  # back to USD scale
    avg_value = avg_value * 1e6
  )

# Contracts share
p1 <- ggplot(sector_plot, aes(x = reorder(sector, -share), y = n_contracts)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::comma(n_contracts)), vjust = -0.3, size = 3) +
  labs(title = "Number of Contracts by Sector (EU-27)",
       x = "Sector", y = "Contracts") +
  theme_minimal()

print(p1)  # Number of contracts by sector
print(p2)  # Total contract value by sector

# Value share
p2 <- ggplot(sector_plot, aes(x = reorder(sector, -total_value), y = total_value/1e12)) +
  geom_col(fill = "darkorange") +
  geom_text(aes(label = paste0("$", round(total_value/1e12, 1), "T")),
            vjust = -0.3, size = 3) +
  labs(title = "Total Value of Contracts by Sector (EU-27)",
       x = "Sector", y = "Contract Value (USD Trillions)") +
  theme_minimal()

# contracts per year by sector
contracts_trends <- gppd_analysis %>%
  group_by(tender_year, sector) %>%
  summarise(n_contracts = n(), .groups = 'drop')

p3 <- ggplot(contracts_trends, aes(x = as.numeric(as.character(tender_year)), 
                                   y = n_contracts, color = sector)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(title = "Annual Distribution of Contracts by Sector (EU-27)",
       x = "Year", y = "Number of Contracts") +
  theme_minimal()

print(p3)

# log distribution by top-10 countries
top10_countries <- gppd_analysis %>%
  group_by(buyer_country) %>%
  summarise(total_value = sum(tender_digiwhist_price, na.rm = TRUE)) %>%
  top_n(10, total_value) %>%
  pull(buyer_country)

p4 <- gppd_analysis %>%
  filter(buyer_country %in% top10_countries) %>%
  ggplot(aes(x = log_value, fill = buyer_country)) +
  geom_density(alpha = 0.4) +
  labs(title = "Distribution of Contract Values (Top-10 EU Countries)",
       x = "Log Contract Value", y = "Density") +
  theme_minimal()

print(p4)

file.exists("gppd_outsourcing_data_FINAL.csv")

colnames(vc)

