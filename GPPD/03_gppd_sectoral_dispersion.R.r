############################################################
# 04_gppd_sectoral_dispersion.R
# EU-27 GPPD: sector distribution, variance components,
# concentration, annual trends, top-country distributions,
# and average prices by country × sector.
############################################################

# --- 0. Setup ---------------------------------------------------------------
options(scipen = 999)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(lme4)

# --- 1. Paths ---------------------------------------------------------------
setwd("/Users/sofiaperondi/Desktop/Tesina IV /Data selection and summary analysis /Downloaded data")
input_main <- "gppd_panel_2011_2021.csv"   # use the 2011–2021 “comparable” panel
if (!file.exists(input_main)) input_main <- "gppd_clean_selected.csv"  # fallback
out_dir <- "outputs"
dir.create(out_dir, showWarnings = FALSE)

# --- 2. Load + prepare analysis dataset ------------------------------------
eu_countries <- c("AT","BE","BG","CY","CZ","DE","DK","EE","ES","FI",
                  "FR","EL","HR","HU","IE","IT","LT","LU","LV",
                  "MT","NL","PL","PT","RO","SE","SI","SK")

gppd <- fread(input_main)

# Keep EU, years 2011–2021 if present, valid sector & positive prices
gppd <- gppd %>%
  mutate(
    buyer_country = ifelse(buyer_country == "GR", "EL", buyer_country),
    tender_year   = as.integer(tender_year),
    tender_supplytype = toupper(trimws(tender_supplytype))
  ) %>%
  filter(
    buyer_country %in% eu_countries,
    tender_year >= 2011, tender_year <= 2021,
    tender_digiwhist_price > 0,
    tender_supplytype %in% c("SERVICES","SUPPLIES","WORKS")
  ) %>%
  mutate(
    sector = factor(ifelse(tender_supplytype == "SERVICES","Services",
                           ifelse(tender_supplytype == "SUPPLIES","Supplies","Works")),
                    levels = c("Services","Supplies","Works")),
    buyer_country = factor(buyer_country, levels = sort(unique(buyer_country))),
    tender_year   = factor(tender_year),
    log_value     = log1p(tender_digiwhist_price) # log(1+x) for stability
  )

cat("Rows in analysis sample:", nrow(gppd), "\n")

# --- 3. Sector Distribution (EU-27) -----------------------------------------
sector_dist <- gppd %>%
  group_by(sector) %>%
  summarise(
    n_contracts        = n(),
    total_value_billion= sum(tender_digiwhist_price, na.rm = TRUE)/1e9,
    avg_value_million  = mean(tender_digiwhist_price, na.rm = TRUE)/1e6,
    .groups = "drop"
  ) %>%
  mutate(
    share_contracts = n_contracts/sum(n_contracts),
    share_value     = total_value_billion/sum(total_value_billion)
  )

sector_dist_clean <- sector_dist %>%
  rename(
    Sector = sector,
    `Number of Contracts` = n_contracts,
    `Total Value (Billion EUR)` = total_value_billion,
    `Average Value (Million EUR)` = avg_value_million,
    `Share of Contracts (%)` = share_contracts,
    `Share of Value (%)` = share_value
  ) %>%
  mutate(
    `Share of Contracts (%)` = round(`Share of Contracts (%)` * 100, 1),
    `Share of Value (%)` = round(`Share of Value (%)` * 100, 1)
  )

fwrite(sector_dist_clean, file.path(out_dir, "sector_distribution_eu27_clean.csv"))
print(sector_dist_clean)

# --- 4. Variance Components in Log Contract Values --------------------------
model1 <- lmer(log_value ~ 1 + (1|sector) + (1|buyer_country) + (1|tender_year), data = gppd)
vc <- as.data.frame(VarCorr(model1))
total_var <- sum(vc$vcov)
vc$variance_pct <- 100 * vc$vcov / total_var

vc_clean <- vc %>%
  rename(
    Component = grp,
    Variance = vcov,
    `Variance Explained (%)` = variance_pct
  ) %>%
  mutate(`Variance Explained (%)` = round(`Variance Explained (%)`, 2))

fwrite(vc_clean, file.path(out_dir, "variance_components_logvalue.csv"))

cat("\nVariance components (% of total):\n")
print(vc[,c("grp","var1","vcov","variance_pct")])
cat("Convergence singular?", isSingular(model1), "\n")

# --- 5. Sectoral Concentration by Country (HHI on value shares) -------------
sector_conc <- gppd %>%
  group_by(buyer_country, sector) %>%
  summarise(value = sum(tender_digiwhist_price, na.rm = TRUE), .groups = "drop_last") %>%
  mutate(total = sum(value), share = ifelse(total>0, value/total, 0)) %>%
  summarise(
    hhi = sum(share^2) * 10000, # HHI 0–10,000
    n_sectors = n_distinct(sector[share>0]),
    dominant_sector = sector[which.max(share)],
    dominant_share  = max(share),
    .groups = "drop"
  )

sector_conc_clean <- sector_conc %>%
  rename(
    Country = buyer_country,
    `Herfindahl-Hirschman Index (0–10,000)` = hhi,
    `Number of Active Sectors` = n_sectors,
    `Dominant Sector` = dominant_sector,
    `Share of Dominant Sector (%)` = dominant_share
  ) %>%
  mutate(`Share of Dominant Sector (%)` = round(`Share of Dominant Sector (%)`*100, 2))

fwrite(sector_conc_clean, file.path(out_dir, "sector_concentration_hhi_by_country.csv"))
print(head(sector_conc, 10))

# --- 6. Number of Contracts by Sector (EU-27) -------------------------------
contracts_by_sector <- gppd %>%
  count(sector, name = "n_contracts") %>%
  mutate(share = n_contracts/sum(n_contracts))

contracts_by_sector_clean <- contracts_by_sector %>%
  rename(
    Sector = sector,
    `Number of Contracts` = n_contracts,
    `Share of Contracts (%)` = share
  ) %>%
  mutate(`Share of Contracts (%)` = round(`Share of Contracts (%)`*100, 2))

fwrite(contracts_by_sector_clean, file.path(out_dir, "contracts_by_sector_eu27.csv"))

# --- 7. Total Value of Contracts by Sector (EU-27) --------------------------
value_by_sector <- gppd %>%
  group_by(sector) %>%
  summarise(total_value_billion = sum(tender_digiwhist_price)/1e9, .groups = "drop") %>%
  mutate(share = total_value_billion/sum(total_value_billion))

value_by_sector_clean <- value_by_sector %>%
  rename(
    Sector = sector,
    `Total Value (Billion EUR)` = total_value_billion,
    `Share of Value (%)` = share
  ) %>%
  mutate(`Share of Value (%)` = round(`Share of Value (%)`*100, 2))

fwrite(value_by_sector_clean, file.path(out_dir, "value_by_sector_eu27.csv"))

# --- 8. Annual Distribution of Contracts by Sector (EU-27) ------------------
annual_sector <- gppd %>%
  group_by(tender_year, sector) %>%
  summarise(n_contracts = n(), .groups = "drop") %>%
  group_by(tender_year) %>%
  mutate(share = n_contracts/sum(n_contracts)) %>%
  ungroup()
annual_sector_clean <- annual_sector %>%
  rename(
    Year = tender_year,
    Sector = sector,
    `Number of Contracts` = n_contracts,
    `Share of Contracts (%)` = share
  ) %>%
  mutate(`Share of Contracts (%)` = round(`Share of Contracts (%)`*100, 2))

fwrite(annual_sector_clean, file.path(out_dir, "annual_contracts_by_sector_eu27.csv"))

# Plot: sector shares over time
p_sector_shares <- ggplot(annual_sector,
                          aes(x = as.numeric(as.character(tender_year)),
                              y = share, color = sector)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks = 2011:2021) +
  labs(title = "EU-27: Annual Share of Contracts by Sector",
       x = "Year", y = "Share of contracts") +
  theme_minimal()
ggsave(file.path(out_dir, "plot_annual_sector_shares.png"),
       p_sector_shares, width = 9, height = 5, dpi = 300)

# --- 9. Distribution of Contract Values (Top-10 EU Countries) ----------------
top10_countries <- gppd %>%
  group_by(buyer_country) %>%
  summarise(total_value = sum(tender_digiwhist_price), .groups = "drop") %>%
  arrange(desc(total_value)) %>%
  slice(1:10) %>%
  pull(buyer_country) %>% as.character()

gppd_top10 <- gppd %>% filter(buyer_country %in% top10_countries)

# Boxplots on log scale (safer than hist by country)
p_top10_box <- ggplot(gppd_top10,
                      aes(x = buyer_country, y = tender_digiwhist_price)) +
  geom_boxplot(outlier.alpha = 0.2) +
  scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_si(""), accuracy = 0.1)) +
  labs(title = "Distribution of Contract Values (Top-10 EU Countries by Total Value)",
       x = "Country", y = "Contract value (log scale)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(out_dir, "plot_distribution_values_top10_box.png"),
       p_top10_box, width = 10, height = 6, dpi = 300)

# --- 10. Average Price by Country × Sector ----------------------------------
avg_price_cs <- gppd %>%
  group_by(buyer_country, sector) %>%
  summarise(
    n_contracts = n(),
    mean_value  = mean(tender_digiwhist_price, na.rm = TRUE),
    median_value= median(tender_digiwhist_price, na.rm = TRUE),
    total_value = sum(tender_digiwhist_price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mean_value_million   = mean_value/1e6,
    median_value_million = median_value/1e6,
    total_value_billion  = total_value/1e9
  )

avg_price_cs_clean <- avg_price_cs %>%
  rename(
    Country = buyer_country,
    Sector = sector,
    `Number of Contracts` = n_contracts,
    `Mean Contract Value (EUR)` = mean_value,
    `Median Contract Value (EUR)` = median_value,
    `Total Value (EUR)` = total_value,
    `Mean Contract Value (Million EUR)` = mean_value_million,
    `Median Contract Value (Million EUR)` = median_value_million,
    `Total Value (Billion EUR)` = total_value_billion
  ) %>%
  mutate(
    `Mean Contract Value (Million EUR)` = round(`Mean Contract Value (Million EUR)`, 2),
    `Median Contract Value (Million EUR)` = round(`Median Contract Value (Million EUR)`, 2),
    `Total Value (Billion EUR)` = round(`Total Value (Billion EUR)`, 2)
  )

fwrite(avg_price_cs_clean, file.path(out_dir, "avg_price_by_country_sector.csv"))

# Heatmap of mean contract value (optional visual)
p_heat_mean <- avg_price_cs %>%
  mutate(buyer_country = as.character(buyer_country)) %>%
  ggplot(aes(x = sector, y = buyer_country, fill = mean_value_million)) +
  geom_tile(color = "white") +
  scale_fill_continuous(labels = scales::label_number(accuracy = 0.1, suffix = "M")) +
  labs(title = "Mean Contract Value by Country × Sector",
       x = "Sector", y = "Country", fill = "Mean value") +
  theme_minimal()
ggsave(file.path(out_dir, "plot_heatmap_mean_value_country_sector.png"),
       p_heat_mean, width = 8, height = 9, dpi = 300)

# === SAVE CLEAN VERSIONS ==============================================
fwrite(sector_dist_clean, file.path(out_dir, "sector_distribution_clean.csv"))
fwrite(vc_clean, file.path(out_dir, "variance_components_clean.csv"))
fwrite(sector_conc_clean, file.path(out_dir, "sector_concentration_clean.csv"))
fwrite(contracts_by_sector_clean, file.path(out_dir, "contracts_by_sector_clean.csv"))
fwrite(value_by_sector_clean, file.path(out_dir, "value_by_sector_clean.csv"))
fwrite(annual_sector_clean, file.path(out_dir, "annual_contracts_by_sector_clean.csv"))
fwrite(avg_price_cs_clean, file.path(out_dir, "avg_price_by_country_sector_clean.csv"))

cat("\n✅ Clean tables saved with descriptive column names in:", normalizePath(out_dir), "\n")

cat("\n✅ Sectoral dispersion analysis complete.\n",
    "Tables and plots saved in: ", normalizePath(out_dir), "\n", sep = "")

out_dir <- "outputs"
dir.create(out_dir, showWarnings = FALSE)

infile <- if (file.exists("gppd_panel_2011_2021.csv")) "gppd_panel_2011_2021.csv" else "gppd_clean_selected.csv"
g <- fread(infile)

g <- g %>%
  filter(!is.na(tender_digiwhist_price) & tender_digiwhist_price > 0) %>%
  mutate(
    log_value = log(tender_digiwhist_price + 1),
    sector = factor(toupper(trimws(tender_supplytype))),
    buyer_country = factor(buyer_country),
    tender_year = factor(tender_year)
  )

# --- 2) Models --------------------------------------------------------------
# M0: intercept only (for reference on residual variance)
M0 <- lmer(log_value ~ 1 + (1|tender_year), data=g, REML=TRUE)

# M1: + random intercept by sector
M1 <- lmer(log_value ~ 1 + (1|sector) + (1|tender_year), data=g, REML=TRUE)

# M2: + random intercept by country
M2 <- lmer(log_value ~ 1 + (1|buyer_country) + (1|tender_year), data=g, REML=TRUE)

# M3 (baseline you used): both sector & country
M3 <- lmer(log_value ~ 1 + (1|sector) + (1|buyer_country) + (1|tender_year), data=g, REML=TRUE)

# Extract variance components as a tidy helper
extract_vc <- function(fit, tag){
  vc <- as.data.frame(VarCorr(fit))
  total <- sum(vc$vcov)
  vc$variance_pct <- 100 * vc$vcov / total
  vc$model <- tag
  vc[, c("model","grp","var1","vcov","variance_pct")]
}

vc_list <- list(
  extract_vc(M0,"M0_year"),
  extract_vc(M1,"M1_sector+year"),
  extract_vc(M2,"M2_country+year"),
  extract_vc(M3,"M3_sector+country+year")
)
vc_all <- do.call(rbind, vc_list)
fwrite(vc_all, file.path(out_dir,"variance_components_models.csv"))

cat("\n--- Variance Components (% of total) ---\n")
print(vc_all %>% select(model, grp, variance_pct) %>% arrange(model, desc(variance_pct)))

# --- 3) Likelihood-ratio tests (nested comparisons) -------------------------
# Use ML for fair LR comparisons
M1_ml <- update(M1, REML=FALSE)
M2_ml <- update(M2, REML=FALSE)
M3_ml <- update(M3, REML=FALSE)
M0_ml <- update(M0, REML=FALSE)

# Sector adds info vs year-only?
lrt_sector <- anova(M0_ml, M1_ml)
# Country adds info vs year-only?
lrt_country <- anova(M0_ml, M2_ml)
# Both adds info vs either alone?
lrt_both_from_sector <- anova(M1_ml, M3_ml)
lrt_both_from_country<- anova(M2_ml, M3_ml)

# Save LRT tables
capture.output(lrt_sector, file = file.path(out_dir,"lrt_M0_vs_M1.txt"))
capture.output(lrt_country, file = file.path(out_dir,"lrt_M0_vs_M2.txt"))
capture.output(lrt_both_from_sector, file = file.path(out_dir,"lrt_M1_vs_M3.txt"))
capture.output(lrt_both_from_country, file = file.path(out_dir,"lrt_M2_vs_M3.txt"))

# --- 4) Build a compact summary table --------------------------------------
summ <- vc_all %>%
  mutate(component = ifelse(is.na(var1), grp, paste0(grp,":",var1))) %>%
  select(model, component, vcov, variance_pct) %>%
  tidyr::pivot_wider(names_from = component, values_from = c(vcov, variance_pct), values_fill = 0)

fwrite(summ, file.path(out_dir,"variance_summary_wide.csv"))

# --- 5) Plot: variance share in M3 ------------------------------------------
vc_M3 <- as.data.frame(VarCorr(M3))
tot_M3 <- sum(vc_M3$vcov)
vc_M3$variance_pct <- 100 * vc_M3$vcov / tot_M3
vc_M3$component <- vc_M3$grp

p_vshare <- ggplot(vc_M3 %>% filter(grp!="Residual"),
                   aes(x=reorder(component, variance_pct), y=variance_pct)) +
  geom_col(fill="steelblue") +
  coord_flip() +
  labs(title="Variance Shares in log(contract value): M3",
       x="Component", y="Share of total variance (%)") +
  theme_minimal()

ggsave(file.path(out_dir,"plot_variance_share_M3.png"), p_vshare, width=7, height=4.5, dpi=300)

# --- 6) Average price by Country × Sector (already computed earlier) --------
avg_cs <- g %>%
  group_by(buyer_country, sector) %>%
  summarise(
    n_contracts = n(),
    mean_value  = mean(tender_digiwhist_price, na.rm=TRUE),
    median_value= median(tender_digiwhist_price, na.rm=TRUE),
    total_value = sum(tender_digiwhist_price, na.rm=TRUE),
    .groups="drop"
  ) %>%
  mutate(mean_M = mean_value/1e6, median_M = median_value/1e6, total_B = total_value/1e9)

fwrite(avg_cs, file.path(out_dir,"avg_price_country_sector.csv"))

# Heatmap with updated scales (no label_number_si)
p_heat <- ggplot(avg_cs, aes(x=sector, y=buyer_country, fill=mean_M)) +
  geom_tile(color="white") +
  scale_fill_continuous(labels = scales::label_number(accuracy=0.1, suffix=" M")) +
  labs(title="Mean Contract Value by Country × Sector (Million EUR)",
       x="Sector", y="Country", fill="Mean value") +
  theme_minimal()

ggsave(file.path(out_dir,"plot_heat_mean_country_sector.png"), p_heat, width=8, height=9, dpi=300)

cat("\n✅ Variance-between-groups analysis complete.\nOutputs in:", normalizePath(out_dir), "\n")

# Number of contracts by sector
p_contracts_sector <- ggplot(sector_dist, aes(x = sector, y = n_contracts, fill = sector)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Number of Contracts by Sector (EU-27, 2011–2021)",
       x = "Sector", y = "Number of Contracts") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(file.path(out_dir, "plot_contracts_by_sector.png"),
       p_contracts_sector, width = 7, height = 5, dpi = 300)

# Total value of contracts by sector
p_value_sector <- ggplot(sector_dist, aes(x = sector, y = total_value_billion, fill = sector)) +
  geom_col() +
  labs(title = "Total Value of Contracts by Sector (EU-27, 2011–2021)",
       x = "Sector", y = "Total Value (Billion EUR)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(file.path(out_dir, "plot_value_by_sector.png"),
       p_value_sector, width = 7, height = 5, dpi = 300)

# Density distribution of contract values for top 10 countries
p_density_top10 <- ggplot(gppd_top10, aes(x = log1p(tender_digiwhist_price), color = buyer_country)) +
  geom_density(linewidth= 1) +
  labs(title = "Distribution of Contract Values (Top-10 EU Countries by Total Value)",
       x = "Log Contract Value (EUR)", y = "Density") +
  theme_minimal()

ggsave(file.path(out_dir, "plot_density_contract_values_top10.png"),
       p_density_top10, width = 10, height = 6, dpi = 300)

# --- Total Value of Contracts by Country-Year (in Billion USD PPP) ---
total_cy <- gppd %>%
  group_by(buyer_country, tender_year) %>%
  summarise(
    total_value_billion = sum(tender_digiwhist_price, na.rm = TRUE) / 1e9,
    n_contracts = n(),
    median_value_million = median(tender_digiwhist_price, na.rm = TRUE) / 1e6,
    .groups = "drop"
  )

# Save table
fwrite(total_cy, file.path(out_dir, "total_contract_value_by_country_year.csv"))

# --- Plot: Total contract value over time for top 10 countries ---
top10_total <- total_cy %>%
  group_by(buyer_country) %>%
  summarise(total_B = sum(total_value_billion)) %>%
  arrange(desc(total_B)) %>%
  slice(1:10) %>%
  pull(buyer_country)

p_total_cy <- total_cy %>%
  filter(buyer_country %in% top10_total) %>%
  ggplot(aes(x = as.numeric(as.character(tender_year)), 
             y = total_value_billion, color = buyer_country)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Total Contract Value by Country (Top-10 EU, 2011–2021)",
       x = "Year", y = "Total Value (Billion USD, PPP-adjusted)", color = "Country") +
  theme_minimal()

ggsave(file.path(out_dir, "plot_total_contract_value_country_year.png"),
       p_total_cy, width = 10, height = 6, dpi = 300)
print(p_total_cy)

# --- Identify Italy 2016 contracts ---
italy_2016 <- gppd %>%
  filter(buyer_country == "IT", tender_year == 2016) %>%
  summarise(
    n_contracts = n(),
    total_value_billion = sum(tender_digiwhist_price, na.rm = TRUE)/1e9,
    mean_value_million = mean(tender_digiwhist_price, na.rm = TRUE)/1e6,
    max_value_billion = max(tender_digiwhist_price, na.rm = TRUE)/1e9
  )

print(italy_2016)  # See the size of the outlier(s)

# --- Option A: Exclude Italy 2016 completely ---
gppd_no_it2016 <- gppd %>%
  filter(!(buyer_country == "IT" & tender_year == 2016))

# Recompute total values by country-year
total_cy_noit2016 <- gppd_no_it2016 %>%
  group_by(buyer_country, tender_year) %>%
  summarise(total_value_billion = sum(tender_digiwhist_price, na.rm = TRUE)/1e9,
            .groups = "drop")

# Plot with and without Italy 2016
p_total_with <- ggplot(total_cy %>% filter(buyer_country %in% top10_total),
                       aes(x = as.numeric(as.character(tender_year)),
                           y = total_value_billion, color = buyer_country)) +
  geom_line(size=1) + geom_point() +
  labs(title = "Total Contract Value by Country (with Italy 2016 outlier)",
       x = "Year", y = "Total Value (Billion USD, PPP-adjusted)") +
  theme_minimal()

p_total_without <- ggplot(total_cy_noit2016 %>% filter(buyer_country %in% top10_total),
                          aes(x = as.numeric(as.character(tender_year)),
                              y = total_value_billion, color = buyer_country)) +
  geom_line(size=1) + geom_point() +
  labs(title = "Total Contract Value by Country (excluding Italy 2016)",
       x = "Year", y = "Total Value (Billion USD, PPP-adjusted)") +
  theme_minimal()

ggsave(file.path(out_dir, "plot_total_contract_value_with_italy2016.png"),
       p_total_with, width = 10, height = 6, dpi = 300)
ggsave(file.path(out_dir, "plot_total_contract_value_without_italy2016.png"),
       p_total_without, width = 10, height = 6, dpi = 300)

print(p_total_without)
print(p_total_with)

