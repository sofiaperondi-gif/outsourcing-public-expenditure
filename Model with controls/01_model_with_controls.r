# ANALISI DEFINITIVA - CONTROLLI CONCORDATI
library(tidyverse)
library(plm)
library(broom)

setwd("/Users/sofiaperondi/Desktop/Tesina IV /Data selection and summary analysis /Downloaded data")

# Carica dataset
final_dataset <- read_csv("FINAL_ANALYSIS_DATASET_2006_2021.csv")

# Pulizia finale
final_dataset_clean <- final_dataset %>%
  select(-lot_updateddurationdays, -bid_subcontractedproportion) %>%
  filter(tender_year >= 2006 & tender_year <= 2021)

#STEP 1: scegliere qual'è il modello di stima più adatto

