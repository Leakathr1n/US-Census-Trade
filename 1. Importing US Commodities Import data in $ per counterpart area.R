########################################################################
# Title: 1. Importing US Commdities Import data in $ per counterpart area
# Author: Lea Roeller
# Date: 03/02/2025
# Last date modified: 07/05/2025
#######################################################################

# In this file, we construct the URL to download US Trade Census data via an API


###############################################################
# 0. Loading the necessary packages & defining necessary paths
###############################################################
library(jsonlite)
library(glue)
library(writexl)
library(dplyr)
library(readxl)

# For graphics later, we want to disable scientific notation
options(scipen = 999)

#Clean everthing
rm(list = ls())

#Define file path for output
setwd()

#Create the folders that we need
if (!dir.exists("Clean data")) dir.create("Clean data")
if (!dir.exists("Output")) dir.create("Output")

#API key is only needed when sending more than 500 calls within 24 hours
api_key <- ""

#########################################################
# 1. Importing the data
########################################################

# More information on how to construct the URL / API can be found in the US Census guide linked the line below
# US Census trade manual: https://www.census.gov/foreign-trade/reference/guides/Guide_to_International_Trade_Datasets.pdf

#if we want to gather more years, we need to split them up, otherwise, R crashes!
month <- sprintf("%02d", 1:12)
year <- as.character(2023:2024)
final_data <- list()


for (y in year) {
  for (m in month) {
    # Print the processing message
    print(paste("Processing", y, "-", m))
    Sys.sleep(2) # Making sure that we do not send too many requests in too short time
    # Construct the URL 
    # We are currently downloading US Commodities imports in $ with all available countries / regions per month using HS2 codes
    url <- glue("https://api.census.gov/data/timeseries/intltrade/imports/hs?get=I_COMMODITY,I_COMMODITY_LDESC,I_COMMODITY_SDESC,CTY_CODE,CTY_NAME,GEN_VAL_MO&YEAR={y}&MONTH={m}&COMM_LVL=HS2")
    
    # Data comes in JSON format
    json_data <- fromJSON(url)
    
    # Convert to a dataframe
    df <- as.data.frame(json_data)
    
    # Set the first row as column names
    colnames(df) <- df[1, ]
    
    # Remove the first row from data
    df <- df[-1, ]
    
    final_data[[length(final_data) + 1]] <- df
    
  }
}
final_data <-do.call(rbind,final_data)


save(final_data, file = "Clean data/US Census Monthly.RData")

rm(df)
rm(json_data)
rm(api_key)
rm(final_data)

