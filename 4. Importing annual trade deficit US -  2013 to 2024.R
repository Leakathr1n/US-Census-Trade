########################################################################
# Title: 4. Importing annual trade deficit US - 2013 to 2024
# Author: Lea Roeller
# Date: 03/02/2025
#######################################################################

#########################################################
# 0. Installing and Loading the necessary packages 
########################################################
library(jsonlite)    
library(glue)        
library(dplyr)       

#Clean everthing
rm(list = ls())

#Define file path for output
setwd()


#########################################################
# 1. Importing the data
########################################################

#importing the import data
month <- sprintf("%02d", 1:12)
year <- as.character(2013:2024)
final_data <- list()


for (y in year) {
  for (m in month) {
    # Print the processing message
    print(paste("Processing", y, "-", m))
    Sys.sleep(2) # Making sure that we do not send too many requests
    # Construct the URL 
    url <- glue("https://api.census.gov/data/timeseries/intltrade/imports/hs?get=CTY_CODE,CTY_NAME,GEN_VAL_MO&YEAR={y}&MONTH={m}")
    
    # Read in JSON
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

imports <- final_data
rm(df)
rm(json_data)
rm(final_data)


# Let's now import the export data
month <- sprintf("%02d", 1:12)
year <- as.character(2013:2024)
final_data <- list()


#importing the exports
final_data <- list()


for (y in year) {
  for (m in month) {
    # Print the processing message
    print(paste("Processing", y, "-", m))
    Sys.sleep(2) # Making sure that we do not send too many requests
    # Construct the URL 
    url <- glue("https://api.census.gov/data/timeseries/intltrade/exports/hs?get=CTY_CODE,CTY_NAME,ALL_VAL_MO&YEAR={y}&MONTH={m}")
    
    # Read in JSON
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

exports <- final_data
rm(df)
rm(json_data)
rm(final_data)

#########################################################
# 2. Merging the data
########################################################

# Rename columns before merging
imports <- imports %>%
  rename(imports = GEN_VAL_MO)%>%
  mutate(imports = as.numeric(imports))

exports <- exports %>%
  rename(exports = ALL_VAL_MO) %>%
  mutate(exports = as.numeric(exports))


#perform a join on imports and CTY_CODE, CTY_NAME, YEAR, MONTH
#check other columns are called imports and exports
trade_data <- merge(imports, exports, by = c("CTY_CODE", "CTY_NAME", "YEAR", "MONTH"), all = TRUE)

#turn into yearly data
trade_data <- trade_data %>%
  group_by(CTY_CODE, CTY_NAME, YEAR) %>%
  summarise(
    imports = sum(imports, na.rm = TRUE),
    exports = sum(exports, na.rm = TRUE)
  ) %>%
  ungroup()

# Calculate the trade deficit
trade_data$deficit <- trade_data$exports - trade_data$imports

file_name_rdata <- glue("Clean data/US Census Deficit.RData")
save(trade_data, file = file_name_rdata)
rm(imports)
rm(exports)

