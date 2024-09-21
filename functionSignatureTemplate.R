# Author: Katy Kearns & Chris Hill
# Date: Wednesday, October 2, 2024
# Purpose: Our goal is to write functions that will manipulate and process data 
#          sets that come from the Public Use Microdata Sample (PUMS) Census 
#          API. We’ll create generic functions to automatically summarize and 
#          plot certain returned data. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load required libraries 
library(tidyverse)
library(jsonlite)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Take user input to build a valid url
build_query_url <- function(year = 2022, 
                            numeric_vars = c("AGEP", "PWGTP"), 
                            categorical_vars = c("SEX"), 
                            geography = "All", 
                            subset = NULL) {
  
  # Check that a valid value was given (number between 2010 and 2022)
  
  # Verify numeric variables 
  
  
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Census API Query Function
query_census_with_url(url) {
  
  
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper Function to Process and Clean Data 
process_census_data <- function(raw_data, 
                                numeric_vars, 
                                categorical_vars) {
  # Parse JSON data
  
  # turn variables into numeric values or time values (use the middle of the 
  # time period) where appropriate.
  
  # Assign class for custom methods
  # class(your_tibble) <- c("census", class(your_tibble)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Summary Function for Census Class
summary.census_data <- function(data, 
                                numeric_vars = NULL, 
                                categorical_vars = NULL) {
  
  
  
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plotting Function for Census Class 
plot.census_data <- function(data, 
                             cat_var, 
                             num_var) {
  
  
  
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function for Querying Multiple Years
query_multiple_years <- function(years, 
                                 numeric_vars = c("AGEP", "PWGTP"), 
                                 categorical_vars = c("SEX"), 
                                 geography = "All", 
                                 subset = NULL) {
  
  
  
  
}

# USAGE CASE

num_vars <- c()
cat_vars <- c()


