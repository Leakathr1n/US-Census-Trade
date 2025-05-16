####################################################################################
# Title: 3. Top Importers for US Largest Trade Deficits by Product Categories (HS2)
# Author: Lea Roeller
# Date: 07/02/2025
# Last date modified: 13/05/2025
###################################################################################

# We want to look at the categories where the US has the largest trade deficit
# Based on data from ITC trade map, we filter for those HS codes
# We put countries into WESP regional grouping and export this grouping


#########################################################
# 0. Installing and Loading the necessary packages 
########################################################
library(dplyr)
library(readxl)
library(writexl)

#Clean everthing
rm(list = ls())


#Set your working directory 
setwd()


###############################################################################################
# 1. Importing the data. Make sure to run 1. Getting started with US Census Trade data before
###############################################################################################

load("Clean data/US Census Monthly.RData")

#start by dropping country_codes that we do not need anymore: 
# CTY_CODE: - = TOTAL FOR ALL COUNTRIES
# CTY_CODE: 00XX = represents country groupings, e.g. NATO, APEC, EU etc.
# CTY_CODE: 1XXX - 7XXX = represents the continents

final_data <- final_data %>%
  filter(!(grepl("^00\\d{2}$", CTY_CODE) | grepl("^[1-7]\\XXX", CTY_CODE)))

#############################################################
# 2. Only keep HS that we are interested in
#############################################################

#Based on a brief analysis using ITC trade map, we found out that those were the goods where the US has the largest trade deficit
#For more informatino: https://www.trademap.org/Index.aspx?AspxAutoDetectCookieSupport=1

HS <- c("84", "85", "87", "30", "94", "61", "95", "62", "73", "64")

final_data <- final_data %>% 
  filter(I_COMMODITY %in% HS)

#############################################################
# 4. Creating yearly data
#############################################################

final_data <- final_data %>%
  mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO))

final_data  <- final_data %>%
  select(-c(MONTH, COMM_LVL)) %>%  # Remove the MONTH column
  group_by(YEAR, I_COMMODITY, I_COMMODITY_LDESC, I_COMMODITY_SDESC, CTY_CODE, CTY_NAME) %>%
  summarize(across(everything(), sum, na.rm = TRUE), .groups = "drop")  # Summarize and drop grouping after

#############################################################
# 5. Adding WESP category & development status
#############################################################

#text-file with 2 iso codes: https://www.census.gov/foreign-trade/schedules/c/country.txt

url <- "https://www.census.gov/foreign-trade/schedules/c/country.txt"
US_codes <- read.delim(url, header = FALSE, sep = "|", strip.white = TRUE)
colnames(US_codes) <- as.character(unlist(US_codes[4, ]))
US_codes<- US_codes[-c(1:5), ]

US_codes <- US_codes %>%
  rename(CTY_CODE = Code) %>%  # Rename 'Code' to 'CTY_CODE'
  select(CTY_CODE, 'ISO Code')  # Keep other relevant columns

# Merge with final_data on CTY_CODE
final_data <- final_data %>%
  left_join(US_codes, by = "CTY_CODE")

#file with 2 to 3 iso-codes: UNSD — Methodology
# Can be downloaded here: https://unstats.un.org/unsd/methodology/m49/overview/
ISO <- read_excel("Additional material/UNSD — Methodology.xlsx", sheet = "Sheet1")
ISO <- ISO %>% select(`ISO-alpha2 Code`, `ISO-alpha3 Code`)
final_data <- final_data %>%
  left_join(ISO, by = c("ISO Code" = "ISO-alpha2 Code"))

rm(ISO)
rm(US_codes)

# This file shows the development status of the countries. Internal and can not be publicly downloaded  
DVL_status <- read_excel("", sheet = "Sheet1")
DVL_status <- DVL_status %>%
  select (ISO, `Region (WESP Annex)`, `Region/ subregion (WESP Annex)`, Development_status)

final_data <- final_data %>%
  right_join(DVL_status, by = c("ISO-alpha3 Code" = "ISO"))

#############################################################
# 6. Filtering per WESP category 
#############################################################

# EU27: Region/ subregion (WESP Annex) = Europe: European Union
# South Asia: Region/ subregion (WESP Annex) = Asia: South Asia
# East Asia: Region/ subregion (WESP Annex) = Asia: East Asia
# Western Asia: Region/ subregion (WESP Annex) = Asia: Western Asia
# LAC: Region (WESP Annex) = Latin America and the Caribbean
# Africa: Region (WESP Annex) = Africa 

# China: ISO-alpha3 Code = CHN
# Mexico & Canada: ISO-alpha3 Code = MEX or CAN

# Others: fill in those with NA in Region column

final_data <- final_data %>%
  mutate(Region = case_when(
    `Region/ subregion (WESP Annex)` == "Europe: European Union" ~ "EU27",
    `Region/ subregion (WESP Annex)` == "Asia: South Asia" ~ "South Asia",
    `Region/ subregion (WESP Annex)` == "Asia: East Asia" ~ "East Asia",
    `Region/ subregion (WESP Annex)` == "Asia: Western Asia" ~ "Western Asia",
    `Region (WESP Annex)` == "Latin America and the Caribbean" ~ "LAC (excl Mexico)",
    `Region (WESP Annex)` == "Africa" ~ "Africa",
    TRUE ~ "Others"  # Assign NA for everything else
  ))

final_data <- final_data %>%
  mutate(Region = case_when(
    `ISO-alpha3 Code` == "CHN" ~ "China",
    `ISO-alpha3 Code` %in% c("MEX", "CAN") ~ "Mexico & Canada",
    TRUE ~ Region  # Keep the original value for everything else
  ))

#collapse per region & only 2024
final_data_graphic <- final_data %>%
  filter(YEAR == 2024) %>%  # Filter for year 2024
  group_by(Region, YEAR, I_COMMODITY, I_COMMODITY_LDESC, I_COMMODITY_SDESC) %>%
  summarise(
    imports = sum(GEN_VAL_MO, na.rm = TRUE),  # Example for summing the trade deficit
    .groups = "drop"  # Prevents unnecessary grouping
  )

#############################################################
# 7. Exporting
#############################################################

# Create a list of dataframes to write to Excel
data_list <- list("full data" = final_data, "graphic data" = final_data_graphic)

# Save to Excel
write_xlsx(data_list, "Output/3. USCensus_Imports_HS.xlsx")

