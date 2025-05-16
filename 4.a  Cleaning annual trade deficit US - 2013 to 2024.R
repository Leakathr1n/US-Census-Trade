########################################################################
# Title: 4.a  Cleaning annual trade deficit US - 2013 to 2024
# Author: Lea Roeller
# Date: 03/02/2025
# Last date modified: 13/05/2025
#######################################################################

#########################################################
# 0. Installing and Loading the necessary packages 
########################################################
library(dplyr)
library(tidyr)
library(glue)
library(writexl)

#Clean everthing
rm(list = ls())

#Define file path for output
setwd("")

#########################################################
# 1. Cleaning and Creating Country Groups
########################################################

load("Clean data/US Census Deficit.RData")

#Clean so that we only keep the country names
trade_data <- trade_data%>%
  filter(!(grepl("^00\\d{2}$", CTY_CODE) | grepl("^[1-7]\\XXX", CTY_CODE)))


#Importing the ISO 2 codes
url <- "https://www.census.gov/foreign-trade/schedules/c/country.txt"
US_codes <- read.delim(url, header = FALSE, sep = "|", strip.white = TRUE)
colnames(US_codes) <- as.character(unlist(US_codes[4, ]))
US_codes<- US_codes[-c(1:5), ]

US_codes <- US_codes %>%
  rename(CTY_CODE = Code) %>%  # Rename 'Code' to 'CTY_CODE'
  select(CTY_CODE, 'ISO Code')  # Keep other relevant columns

# Merge with final_data on CTY_CODE
trade_data <- trade_data %>%
  left_join(US_codes, by = "CTY_CODE")

#Getting in the country groups

# EU27 countries
# China
# Canada
# Mexico
# Viet Nam
# ASEAN excl Viet Nam
# Other


EU_countries <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", 
                  "DE", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", 
                  "PL", "PT", "RO", "SK", "SI", "ES", "SE")

#excluding Viet Nam
ASEAN_countries <- c("BN", "KH", "ID", "LA", "MY", "MM", "PH", "SG", "TH")

trade_data <- trade_data %>%
  mutate(Region = case_when(
    `ISO Code` %in% EU_countries ~ "EU27",
    `ISO Code` == "CN" ~ "China",
    `ISO Code` == "CA" ~ "Canada",
    `ISO Code` == "MX" ~ "Mexico",
    `ISO Code` == "VN" ~ "Viet Nam",
    `ISO Code` %in% ASEAN_countries ~ "ASEAN excl Viet Nam",
    `CTY_CODE` == "-" ~ "Total",  # Exclude "-"
    TRUE ~ "Other"  # Everything else is Other
  ))

#########################################################
# 4. Collapsing per country group
########################################################

trade_data_graphic <- trade_data %>%
  filter(deficit < 0) %>%  # Keep only rows where deficit is negative
  group_by(Region, YEAR) %>%
  summarise(
    imports = sum(imports, na.rm = TRUE),
    exports = sum(exports, na.rm = TRUE),
    deficit = sum(deficit, na.rm = TRUE),
    .groups = "drop"  # Prevents unnecessary grouping
  )

trade_data_graphic <- trade_data_graphic %>%
  select(Region, YEAR, deficit) %>%  # Keep only relevant columns
  pivot_wider(names_from = YEAR, values_from = deficit)  # Pivot to wide format

#########################################################
# 5. Exporting to monthly briefing
########################################################

# Create a list of dataframes to write to Excel
data_list <- list("full data" = trade_data, "graphic data" = trade_data_graphic)

# Save to Excel
write_xlsx(data_list, "Output/4.a USCensus_Trade_deficit.xlsx")

