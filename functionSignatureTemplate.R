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

# User interface to take inputs and return fully processed data tibble
# CHRIS
get_data_tibble_from_api <- function(year = 2022, 
                                     numeric_vars = c("AGEP", "PWGTP"), 
                                     categorical_vars = c("SEX"), 
                                     geography = "All", 
                                     subset = NULL) {

  # validate the user inputs
  validate_year(year)
  validate_numeric_vars(numeric_vars)
  validate_categorical_vars(categorical_vars)
  validate_geography_level(geography)
  
  # Send inputs to retrieve data
  # build_query_url |> query_census_with_url |> process_census_data
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

validate_year <- function(year){
  
  if (!(year %in% 2010:2022)) stop("Year must be between 2010 and 2022.")
}

validate_numeric_vars <- function(numeric_vars) {
  
  valid_numeric_vars <- get_valid_numeric_variables()
  numeric_vars <- intersect(numeric_vars, valid_numeric_vars)
  
  if (length(numeric_vars) < 2 || !"PWGTP" %in% numeric_vars) {
    stop("PWGTP and at least one other numeric variable must be selected.")
  }
}

validate_categorical_vars <- function(categorical_vars) {
  
}

validate_geography_level <- function(geography) {
  
}

validate_subset <- function(subset) {
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

get_valid_numeric_variables <- function() {
  c("AGEP", "PWGTP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP")
}

get_valid_categorical_variables <- function() {
  c("SEX", "FER", "HHL", "HISPEED", "JWAP", "JWDP", "JWTRNS", "SCH", "SCHL")
}

get_valid_geographical_levels <- function() {
  c("All", "Region", "Division", "State")
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build a valid url
# CHRIS
build_query_url <- function(year = 2022, 
                            numeric_vars = c("AGEP", "PWGTP"), 
                            categorical_vars = c("SEX"), 
                            geography = "All", 
                            subset = NULL) {
  

  
  
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Census API Query Function
# KATY - take url all the way to tibble 
# (nice and neat and columns in correct format etc)
query_census_with_url <- function(url) {
  
  # retrieve data in list form from API
  census_raw <- httr::GET(url)
  
  # call helper function to turn API raw data into a raw tibble
  census_raw_tbl <- query_helper(census_raw)

  # a bunch of other stuff to clean tibble
  
  # return final clean tibble
  
}

# helper function for query_census_with_url: put json stuff into raw tibble (all char)
query_helper <- function(census_raw) {
  
  # convert JSON string raw data to data frame (first row are column names)
  parsed_census <- as.data.frame(fromJSON(rawToChar(census_raw$content)))
  
  # convert to a tibble, use 1st row from raw df as column names
  census_tbl <- as_tibble(parsed_census[-1,])
  colnames(census_tbl) <- parsed_census[1,]
  
  # return final tibble
  return(census_tbl)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper Function to Process and Clean Data 
# KATY
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
# CHRIS
summary.census_data <- function(data, 
                                numeric_vars = NULL, 
                                categorical_vars = NULL) {
  
  
  
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plotting Function for Census Class 
plot.census_data <- function(data_as_tibble, 
                             cat_var, 
                             num_var) {
  
  ggplot(data_as_tibble,
         aes(x = get(cat_var), 
             y = get(num_var), 
             weight = PWGTP)) +
    geom_boxplot()
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function for Querying Multiple Years
# KATY
query_multiple_years <- function(years, 
                                 numeric_vars = c("AGEP", "PWGTP"), 
                                 categorical_vars = c("SEX"), 
                                 geography = "All", 
                                 subset = NULL) {
  
  # call the user interface for each year
  

  
}

# USAGE CASE

num_vars <- c()
cat_vars <- c()


