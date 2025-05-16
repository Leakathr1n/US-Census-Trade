########################################################################
# Title: 2. Quarterly Ranking of US Imports by Partner and Commodity (HS2)
# Author: Lea Roeller
# Date: 03/02/2025
# Last date modified: 07/05/2025
#######################################################################

# In this file, we work with monthly trade data
# 1. Clean the data for countries as trade partners only
# 2. We create quarterly data
# 3. We are looking for the top 5 commodities according to HS2 codes, where Canada and Mexico are the strongest importers according to USD value
# 4. We collect those 5 commodities and other strong (5) importing countries. The rest will be summarised as ROW
# 5. We calculate shares to show absolute and relative "import dependence"
# 6. We display data in stacked bar charts


###############################################################
# 0. Loading the necessary packages & defining necessary paths
###############################################################
library(dplyr)
library(writexl)
library(openxlsx)
library(ggplot2)

rm(list = ls())

#Set your working directory 
setwd()

#########################################################
# 1. Cleaning the coutnries and regions
########################################################
load("Clean data/US Census Monthly.RData")


#start by dropping country_codes that we do not need anymore: 
# CTY_CODE: - = TOTAL FOR ALL COUNTRIES
# CTY_CODE: 00XX = represents country groupings, e.g. NATO, APEC, EU etc.
# CTY_CODE: 1XXX - 7XXX = represents the continents

final_data <- final_data %>%
  filter(!(grepl("^00\\d{2}$", CTY_CODE) | grepl("^[1-7]\\XXX", CTY_CODE)))

#############################################################
# 2. Creating quarterly data
#############################################################

# Let's now create quarterly data
get_quarter <- function(month) {
  case_when(
    month %in% c("01", "02", "03") ~ "Q1",
    month %in% c("04", "05", "06") ~ "Q2",
    month %in% c("07", "08", "09") ~ "Q3",
    month %in% c("10", "11", "12") ~ "Q4",
    TRUE ~ NA_character_
  )
}

final_data <- final_data %>%
  mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO))

final_data <- final_data %>%
  mutate(QUARTER = get_quarter(MONTH)) %>%  # Create QUARTER column
  group_by(YEAR, QUARTER, I_COMMODITY, I_COMMODITY_LDESC, CTY_CODE, CTY_NAME) %>%
  summarise(TOTAL_GEN_VAL_MO = sum(GEN_VAL_MO, na.rm = TRUE), .groups = "drop")

#############################################################
# 3. Creating Top 5 HS codes for selected coutnries
#############################################################

#More countries can simply be added to this list
countries <- c("CANADA", "MEXICO")

for (c in countries) {

#let's create the top 5 categories for Canada
top5 <- final_data %>%
  filter(CTY_NAME == "CANADA") %>%
  group_by(YEAR, I_COMMODITY) %>%
  summarise(TOTAL_GEN_VAL_MO = sum(TOTAL_GEN_VAL_MO, na.rm = TRUE), .groups = "drop") %>%
  group_by(YEAR) %>%
  filter(YEAR == "2024") %>%
  slice_max(order_by = TOTAL_GEN_VAL_MO, n = 5, with_ties = FALSE) %>%
  ungroup()

country <- final_data %>%
  semi_join(top5, by = "I_COMMODITY")

#let's create the top 5
country <- country %>%
  group_by(YEAR, QUARTER, I_COMMODITY) %>%
  # Select top 6 countries
  slice_max(order_by = TOTAL_GEN_VAL_MO, n = 6, with_ties = FALSE) %>%
  ungroup()

# Compute ROW values for Canada
data_with_ROW <- country %>%
  group_by(YEAR, QUARTER, I_COMMODITY,I_COMMODITY_LDESC,) %>%
  summarise(
    TOTAL_ALL_COUNTRIES = sum(TOTAL_GEN_VAL_MO[CTY_CODE == "-"], na.rm = TRUE),  # Get total
    TOTAL_OTHERS = sum(TOTAL_GEN_VAL_MO[CTY_CODE != "-"], na.rm = TRUE),  # Sum all except "-"
    .groups = "drop"
  ) %>%
  mutate(
    CTY_CODE = "ROW",
    CTY_NAME = "ROW",
    TOTAL_GEN_VAL_MO = TOTAL_ALL_COUNTRIES - TOTAL_OTHERS  # ROW = Total - Others
  ) %>%
  select(YEAR, QUARTER, I_COMMODITY, I_COMMODITY_LDESC, CTY_CODE, CTY_NAME, TOTAL_GEN_VAL_MO)

# Append the ROW data to the original summaries
country <- bind_rows(country, data_with_ROW)

# Save the data set as Excel
write_xlsx(country, paste0("Output/2. Top_", c,".xlsx"))

#construct data set name & store data there
assign(paste0("Top_", c), country)

rm(top5, data_with_ROW, country)



}

rm(final_data)


#############################################################
# 4. Computing shares
#############################################################

for (c in countries) {
  
  df_name <- paste0("Top_", c)
  Top_c <- get(df_name)  # Retrieve the actual data frame

  # Compute SHARE 
  Top_c <- Top_c %>%
    group_by(YEAR, QUARTER, I_COMMODITY) %>%
    mutate(
      TOTAL_FOR_COMMODITY = sum(TOTAL_GEN_VAL_MO[CTY_CODE == "-"], na.rm = TRUE),  # Get total "-"
      SHARE = ifelse(TOTAL_FOR_COMMODITY > 0, TOTAL_GEN_VAL_MO / TOTAL_FOR_COMMODITY *100, NA)
    ) %>%
    ungroup()
  
  Top_c <- Top_c %>%
    filter(CTY_CODE != "-") %>%  # Remove total rows
    select(-TOTAL_FOR_COMMODITY)  # Drop the total column
  
  assign(df_name, Top_c)
  
  # Now, let's add this back to the Excel that we saved earlier
  
  # Define file path & load workbook
  file_path <- paste0("Output/2. Top_", c, ".xlsx")
  wb <- loadWorkbook(file_path)
  
  # Remove "Shares" sheet if it already exists to update with new data
  if ("Shares" %in% names(wb)) {
    removeWorksheet(wb, "Shares")
  }
  addWorksheet(wb, "Shares")
  writeData(wb, "Shares", Top_c)
  
  # Save the updated workbook
  saveWorkbook(wb, file_path, overwrite = TRUE)

}

#############################################################
# 5. Creating stacked bar charts
#############################################################

# This loads the colours / design you want to have 
# Replace with own template if you have one
source()  

for (c in countries) {
  
  df_name <- paste0("Top_", c)
  Top_c <- get(df_name)  # Retrieve the actual data frame
  
  Top_c  <- Top_c  %>%
  mutate(YEAR_QUARTER = paste0(YEAR, "-", QUARTER))

  # Loop over unique HS codes
  for (HS_code in unique(Top_c $I_COMMODITY)) {
    
    # Filter data for the current HS code
    data_subset <- Top_c  %>% filter(I_COMMODITY == HS_code)
    
    # Sort countries by total share per quarter (ensuring largest at bottom)
    sorted_countries <- data_subset %>%
      group_by(CTY_NAME) %>%
      summarise(TOTAL_SHARE = sum(SHARE, na.rm = TRUE)) %>%
      arrange(TOTAL_SHARE) %>%
      pull(CTY_NAME)
    
    # Assign colors dynamically from wesp_colors, ensuring "ROW" is grey
    color_mapping <- setNames(wesp_colors[1:length(sorted_countries)], sorted_countries)
    color_mapping["ROW"] <- "grey"  # Set "ROW" to grey
    
    # Create the stacked bar chart
    p <- ggplot(data_subset, aes(x = YEAR_QUARTER, y = SHARE, 
                                 fill = factor(CTY_NAME, levels = sorted_countries))) +
      geom_bar(stat = "identity", position = "stack") +  # Stacked bars
      facet_wrap(~ I_COMMODITY_LDESC, scales = "free_y") +  # One plot per commodity description
      labs(
        title = "Main Trade Partners of the US",
        x = "",
        y = "Share of Total Trade",
        fill = ""
      ) +
      scale_fill_manual(values = color_mapping) +  # Apply pastel colors
      my_theme +  # Apply your custom theme
      theme(
        legend.position = "bottom",         # Move legend below
        legend.justification = "center",    # Center the legend
        legend.direction = "horizontal",    # Arrange legend items in a row
        legend.box = "horizontal"           # Ensures legend stays horizontal
      )
    
    # Save the plot
    ggsave(paste0("Output/2.", c, "_Top5_HS", HS_code, ".png"), plot = p, width = 8, height = 5)
  }

}