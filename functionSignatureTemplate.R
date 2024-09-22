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
library(httr) 

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
  build_query_url(year,
                  numeric_vars,
                  categorical_vars,
                  geography,
                  subset) #|> query_census_with_url |> process_census_data
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Year must be between 2010 and 2022.
validate_year <- function(year){
  
  if (!(year %in% 2010:2022)) 
    stop("Year must be between 2010 and 2022.")
}

# PWGTP and at least one other valid numeric variable must be selected
validate_numeric_vars <- function(numeric_vars) {
  
  valid_numeric_vars <- get_valid_numeric_vars()
  
  numeric_vars <- intersect(numeric_vars, valid_numeric_vars)
  
  if (length(numeric_vars) < 2 || !"PWGTP" %in% numeric_vars) {
    stop("PWGTP and at least one other numeric variable must be selected.")
  }
}

# At least one valid categorical variable must be selected
validate_categorical_vars <- function(categorical_vars) {
  
  valid_categorical_vars <- get_valid_categorical_vars()
  
  categorical_vars <- intersect(categorical_vars, valid_categorical_vars)
  
  if (length(categorical_vars) < 1) 
    stop("At least one categorical variable must be selected.")
}

# There are set geographical regions
validate_geography_level <- function(geography) {
  
  valid_geography_levels <- get_valid_geographical_levels()

  if (!(geography %in% valid_geography_levels)) {
    stop("Invalid geography level. Must be one of: ", 
         paste(valid_geography_levels, collapse = ", "))
  }
}

# Subset must be in line with provided geography
validate_subset <- function(geography, subset) {
  
  if (geography == "All") 
    stop("Subsetting is not allowed when geography is 'All'.")
  
  valid_options <- list(
    Region = c("Northeast", "Midwest", "South", "West"),
    Division = c("New England", "Middle Atlantic", "East North Central", 
                 "West North Central", "South Atlantic", "East South Central", 
                 "West South Central", "Mountain", "Pacific"),
    State = sprintf("%02d", 1:56)
  )
  
  if (!(subset %in% valid_options[[geography]]))
    stop(paste("Invalid subset for ", geography, ". Must be one of:", 
               paste(valid_options[[geography]], collapse = ", ")))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

get_valid_numeric_vars <- function() {
  c("AGEP", "PWGTP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP")
}

get_valid_categorical_vars <- function() {
  c("SEX", "FER", "HHL", "HISPEED", "JWAP", "JWDP", "JWTRNS", "SCH", "SCHL")
}

get_valid_geographical_levels <- function() {
  c("All", "Region", "Division", "State")
}

# Function to get the appropriate subset code for Regions or Divisions
get_subset_code <- function(geography, subset) {
  
  # Mappings for regions and divisions
  region_codes <- list(
    "Northeast" = "1", 
    "Midwest" = "2", 
    "South" = "3", 
    "West" = "4"
  )
  
  division_codes <- list(
    "New England" = "1", 
    "Middle Atlantic" = "2", 
    "East North Central" = "3", 
    "West North Central" = "4", 
    "South Atlantic" = "5", 
    "East South Central" = "6", 
    "West South Central" = "7", 
    "Mountain" = "8", 
    "Pacific" = "9"
  )
  
  if (is.null(subset)) {
    return("*")
    
  } else if (geography == "Region") {
    return(region_codes[[subset]])  
    
  } else if (geography == "Division") {
    return(division_codes[[subset]])  
    
  } else if (geography == "State") {
    # currently taking state code from user
    return(subset)
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CHRIS
# Build a valid URL for the Census API (assuming inputs are validated)
build_query_url <- function(year = 2022, 
                            numeric_vars = c("AGEP", "PWGTP"), 
                            categorical_vars = c("SEX"), 
                            geography = "All", 
                            subset = NULL) {

  dataset_type <- ifelse(year == 2021 || year == 2022, "acs1", "acs5")
  
  base_url <- paste0("https://api.census.gov/data/", 
                     year, "/acs/", dataset_type, "/pums?")
  
  
  # Handle numeric and categorical inputs
  query_vars <- c(numeric_vars, categorical_vars)
  query_string <- paste0("get=", paste(query_vars, collapse = ","))
  
  # Handle geography levels ("All" will require no 'for' clause)
  geography_query <- ""
  
  if (geography != "All") {

    # Subsets need to be numeric codes. If null will return *
    subset <- get_subset_code(geography, subset)
    
    geography_query <- paste0("for=", gsub(" ", "%20", geography), ":", subset)
  }
  
  # Concatenate base_url, query_string, and geography_query
  final_url <- paste0(base_url, query_string)
  
  if (geography_query != "") {
    final_url <- paste0(final_url, "&", geography_query)
  }
  
  return(final_url)
}


# TESTING URL
is_url_valid <- function(url) {

  response <- GET(url)

  is_success <- http_status(response)$category == "Success"

  response_content <- content(response, as = "text")
  has_content <- !is.null(response_content) && nchar(response_content) > 0
  
  return(is_success && has_content)
}

urlTest <- build_query_url(year = 2021, 
                        numeric_vars = c("AGEP", "PWGTP", "JWAP"), 
                        categorical_vars = c("SEX", "SCHL"), 
                        geography = "All", 
                        subset = NULL)

cat("Testing:\n", urlTest, "\n")
result <- is_url_valid(urlTest)
cat("URL is valid: ", result, "\n")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Census API Query Function
# KATY - take url all the way to tibble 
# (nice and neat and columns in correct format etc)
query_census_with_url <- function(url) {
  
  # retrieve data in list form from API
  census_raw <- httr::GET(url)
  
  # call helper function to turn API raw data into a raw tibble
  census_raw_tbl <- json_to_raw_tbl_helper(census_raw)

  # a bunch of other stuff to clean tibble
  
  # return final clean tibble
  
}

# helper function for query_census_with_url: put json stuff into raw tibble (all char)
json_to_raw_tbl_helper <- function(census_raw) {
  
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
  
  # turn vars into numeric values or time values (use the middle of the 
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
# NOTE: I have not tested this, it may not work.
query_multiple_years <- function(years, 
                                 numeric_vars = c("AGEP", "PWGTP"), 
                                 categorical_vars = c("SEX"), 
                                 geography = "All", 
                                 subset = NULL) {
  
  # create empty list to store data frames
  multi_year_list <- list()
  
  # call the user interface for each year
  for (yr in years) {
    
    # retrieve single year data tibble
    census_single_yr <- get_data_tibble_from_api(yr,
                                                 numeric_vars,
                                                 categorical_vars,
                                                 geography,
                                                 subset)
    
    # append year to the tibble
    census_single_yr_tbl <- tibble(Year = yr, census_single_yr)
    
    # check how many elements are currently in list
    elements <- length(multi_year_list)
    
    # insert the tbl into the list as the last element
    multi_year_list[[elements + 1]] <- census_single_yr_tbl
  }
  
  # union of all year-specific results
  census_multi_year_tbl <- bind_rows(multi_year_list)
  
  # return the final tibble
  return(census_multi_year_tbl)
  
}

# USAGE CASE

num_vars <- c()
cat_vars <- c()


