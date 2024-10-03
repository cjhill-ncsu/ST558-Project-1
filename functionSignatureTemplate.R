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
library(hms)

# User interface to take inputs and return fully processed data tibble
get_data_tibble_from_census_api <- function(year = 2022, 
                                     numeric_vars = c("AGEP", "PWGTP"), 
                                     categorical_vars = c("SEX"), 
                                     geography = "State", 
                                     subset = "CO") {
  
  # validate the user inputs
  validate_year(year)
  validate_numeric_vars(numeric_vars)
  validate_categorical_vars(categorical_vars)
  validate_geography_and_subset(geography, subset)
  
  # Send inputs to retrieve data
  build_query_url(year,
                  numeric_vars,
                  categorical_vars,
                  geography,
                  subset) |> 
    query_census_with_url()
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# VALIDATION FUNCTIONS
# Year must be between 2010 and 2022.
validate_year <- function(year){
  
  if (!(year %in% 2010:2022)) 
    stop("Year must be between 2010 and 2022.")
}

# PWGTP and at least one other valid numeric variable must be selected
validate_numeric_vars <- function(numeric_vars) {
  
  # Not worried about case
  valid_numeric_vars <- toupper(get_valid_numeric_vars())
  numeric_vars <- toupper(numeric_vars)

  numeric_vars <- intersect(numeric_vars, valid_numeric_vars)
  
  if (length(numeric_vars) < 2 || !"PWGTP" %in% numeric_vars) {
    stop("PWGTP and at least one other numeric variable must be selected.")
  }
}

# At least one valid categorical variable must be selected
validate_categorical_vars <- function(categorical_vars) {
  
  # Not worried about case
  valid_categorical_vars <- toupper(get_valid_categorical_vars())
  categorical_vars <- toupper(categorical_vars)
  
  categorical_vars <- intersect(categorical_vars, valid_categorical_vars)
  
  if (length(categorical_vars) < 1) {
    stop("At least one valid categorical variable must be selected from: ", 
         paste(valid_categorical_vars, collapse = ", "))
  }
}

# Geography & Subset Together
validate_geography_and_subset <- function(geography, subset) {
  
  # Convert to uppercase for case-insensitive comparison
  valid_geography_levels <- tolower(get_valid_geography_levels())
  geography <- tolower(geography)
  
  # Validate the geography
  if (!(geography %in% valid_geography_levels)) {
    stop("Invalid geography level. Must be one of: ", 
         paste(valid_geography_levels, collapse = ", "))
  }
  
  # If geography is "all", subsetting is not allowed
  if (geography == "all" && !is.null(subset)) {
    stop("Subsetting is not allowed when geography is 'All'.")
  }
  
  # Valid options for regions and divisions
  valid_region_division_options <- list(
    region = tolower(c("Northeast", "Midwest", "South", "West")),
    division = tolower(c("New England", "Middle Atlantic", 
                         "East North Central", "West North Central", 
                         "South Atlantic", "East South Central", 
                         "West South Central", "Mountain", "Pacific"))
  )
  
  # Consolidated check for region and division
  if (geography %in% c("region", "division")) {
    if (!(tolower(subset) %in% valid_region_division_options[[geography]])) {
      stop("Invalid ", geography, ". Must be one of: ", 
           paste(valid_region_division_options[[geography]], collapse = ", "))
    }
  }
  
  print(geography)
  print(subset)
  
  # Handle State geography
  if (geography == "state") {
    print(subset)
    state_code <-  get_state_code(subset)
  }
}

# Check we got something from the API using GET(URL)
validate_url_response <- function(response) {
  
  is_success <- http_status(response)$category == "Success"
  
  response_content <- content(response, as = "text")
  has_content <- !is.null(response_content) && nchar(response_content) > 0
  
  if (!is_success || !has_content) {
    stop("API request failed: ", 
         if (!is_success) 
           http_status(census_raw)$message 
         else 
           "Empty response from API."
         )
  }
  
  print("API request successful")
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GETTERS

get_valid_numeric_vars <- function() {
  c("AGEP", "PWGTP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP")
}

get_valid_categorical_vars <- function() {
  c("SEX", "FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL")
}

get_valid_geography_levels <- function() {
  c("All", "Region", "Division", "State")
}

# Function to get state code from state name or abbreviation
get_state_code <- function(state_input) {

  state_input <- tolower(state_input)
  
  # Provided state codes
  state_codes <- get_cat_refs("ST")
  
  state_codes_tibble <- state_codes |> 
    separate_wider_delim(description, delim = "/", 
                         names = c("state", "abbreviation")) |>
    mutate(state = tolower(state), abbreviation = tolower(abbreviation))
  
  print(state_codes_tibble)
  
  # Filter down to match input
  result <- state_codes_tibble |>
    filter(state == state_input | abbreviation == state_input) |>
    pull(ST)
  
  # Return the state code or stop if not found
  if (length(result) == 0) {
    stop("Invalid state name or abbreviation")
  }
  
  return(result)
}

get_subset_code <- function(geography, subset) {
  
  if (is.null(subset)) {
    return("*")
  }
  
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
  
  geography <- tolower(geography)
  
  # Switch based on geography type
  switch(geography,
         "region" = region_codes[[subset]],
         "division" = division_codes[[subset]],
         "state" = get_state_code(subset),  
         stop("Invalid geography type"))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build a valid URL for the Census API
build_query_url <- function(year = 2022, 
                            numeric_vars = c("AGEP", "PWGTP"), 
                            categorical_vars = c("SEX"), 
                            geography = "State", 
                            subset = "CO") {

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
  
  cat("URL: ", final_url)
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

year <- 2015
num_vars <- c("AGEP", "PWGTP", "GRPIP", "JWAP") 
cat_vars <- c("SEX", "HHL")
geo <- "State"
subset <- "CO"

urlTest <- build_query_url(year, 
                        num_vars, 
                        cat_vars, 
                        geo, 
                        subset)

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
  
  # check that data was returned
  validate_url_response(census_raw)
  
  # call helper function to turn API raw data into a raw tibble
  census_raw_tbl <- json_to_raw_tbl_helper(census_raw)

  # call helper function to clean tibble
  census_clean_tbl <- process_census_data(census_raw_tbl)
  
  # return final clean tibble
  return(census_clean_tbl)
  
}

# helper function for query_census_with_url: put json stuff into raw tibble (all char)
json_to_raw_tbl_helper <- function(census_raw) {
  
  # convert JSON string raw data to data frame (first row are column names)
  parsed_census <- as.data.frame(fromJSON(rawToChar(census_raw$content)))
  
  # convert to a tibble, use 1st row from raw df as column names
  census_tbl <- as_tibble(parsed_census[-1,])
  colnames(census_tbl) <- toupper(parsed_census[1,])
  
  # return final tibble
  return(census_tbl)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper Function to Process and Clean Data 
# KATY
process_census_data <- function(census_data_tbl) {

  # if state is a column, rename as ST
  if ("STATE" %in% names(census_data_tbl)) {
    names(census_data_tbl)[names(census_data_tbl) == "STATE"] <- "ST"
  }
  
  # retrieve valid categorical variables
  cat_vars <- 
    get_valid_categorical_vars() |>      # get all valid categorical variables
    c("DIVISION", "REGION", "ST") |>     # append regional categories
    intersect(names(census_data_tbl))    # keep only values in the data set
  
  # convert categorical variables to actual descriptive values, and as factors
  for (var in cat_vars){
    census_data_tbl[var] <- convert_cat_code_to_description(census_data_tbl[var])
  } 
  
  # convert categorical variables to factors ##DELETE AFTER ABOVE HELPER WORKING
  # for (var in cat_vars){
  #   census_data_tbl[[var]] <- as.factor(census_data_tbl[[var]])
  # } 
    
  ## TESTING ONLY--copy the JWAP/JWDP columns
  #census_data_tbl["JWAP_char"] <- census_data_tbl["JWAP"]
  #census_data_tbl["JWDP_char"] <- census_data_tbl["JWDP"]
  
  # retrieve valid numeric vars, keeping only the ones that exist in 
  # the input raw data (note JWAP and JWDP will still need to be changed to times)
  num_vars <- 
    get_valid_numeric_vars() |>
    intersect(names(census_data_tbl))
  
  # turn vars into numeric values in the tibble 
  for (var in num_vars){
    census_data_tbl[[var]] <- as.numeric(census_data_tbl[[var]])
  } 
  
  # collect the time variables to convert
  time_vars <- 
    c("JWAP", "JWDP") |>
    intersect(names(census_data_tbl))
  
  # call helper function to convert time codes to numeric time (won't run if 
  # time_vars is empty)
  for (time_code in time_vars) {
    census_data_tbl <- convert_num_code_to_time(census_data_tbl, time_code)
  }
  
  # Assign class for custom methods
  class(census_data_tbl) <- c("census", class(census_data_tbl))
  
  # return clean tibble
  return(census_data_tbl)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# helper function to convert JWAP/JWDP code columns to numeric time
convert_num_code_to_time <- function(census_data_tbl, time_code) {
  
  # get time references from API`
  times_reference <- get_time_refs(time_code)
  
  # join new JWAP/JWDP to table with proper times
  census_data_tbl <- 
    census_data_tbl |>
    left_join(times_reference) # natural join on time code
    
  # assign the cleaned time values to the JWAP/JWDP column
  census_data_tbl[time_code] <- census_data_tbl[paste0(time_code, "_clean")]
  
  # Drop the extra column from the time reference table
  census_data_tbl <- census_data_tbl |>
    select(-one_of(paste0(time_code, "_clean")))
  
  return(census_data_tbl)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# helper function to get clean reference tibble for converting JWDP/JWAP to time
#   possible url's:
#     https://api.census.gov/data/2022/acs/acs1/pums/variables/JWDP.json
#     https://api.census.gov/data/2022/acs/acs1/pums/variables/JWAP.json

get_time_refs <- function(time_code) {
  
  # construct url from the time_code (JWDP or JWAP)
  time_url <- paste0("https://api.census.gov/data/2022/acs/acs1/pums/variables/",
                    time_code, ".json")
  
  # retrieve data in list form from API, then bind rows to put in 1 by x tibble,
  # then transpose the data to get key-value pair in columns
  times_ref <- 
    fromJSON(time_url)$values |>
    bind_rows() |>
    pivot_longer(cols = everything(), 
                 names_to = time_code, 
                 values_to = "time_range")
  
  # convert 1st column (JWAP/JWDP) to numeric 
  times_ref[[time_code]] <- as.numeric(times_ref[[time_code]])
  
  # filter on the row(s) where JWAP/JWDP == 0, change the value for time_range
  # to missing (it is a string that can't be converted to time, starts with "N/A")
  times_ref$time_range[times_ref[time_code] == 0] <- NA
  
  # parse the time_range string to find the start and stop times
  times_ref <-
    times_ref |>
    separate_wider_delim(time_range,
                         delim = " to ",
                         names = c("start_time", "end_time"),
                         cols_remove = FALSE) 
  
  # convert new start/end columns to time
  for (col in c("start_time", "end_time")) {
    times_ref[[col]] <-
      times_ref[[col]] |>
      toupper() |>                              # change to upper case
      str_replace_all("[.]", "") |>             # remove periods
      parse_date_time('%I:%M %p', tz = "EST")   # convert to date-time
  }

  # calculate time to the midpoint between the start and end times
  times_ref <-
    times_ref |>
    mutate(midpoint = difftime(end_time, start_time) / 2)
  
  # assign new clean time code variable as correct time
  times_ref[paste0(time_code, "_clean")] <-
    times_ref$start_time + times_ref$midpoint

  # convert format from date-time to time (reference by [[]] not [])
  times_ref[paste0(time_code, "_clean")] <-
    hms::as_hms(times_ref[[paste0(time_code, "_clean")]])
  
  # drop extra columns, keeping only 2 (time code, time code clean)
  times_ref <-
    times_ref |>
    select(-time_range, -start_time, -end_time, -midpoint)

  # return final clean ref table
  return(times_ref)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# take categorical raw value and convert to descriptive value, and make a factor
convert_cat_code_to_description <- function(data_column) {

  # get the variable name that has to be looked up
  var <- colnames(data_column)
    
  # get time references from API`
  cat_reference <- get_cat_refs(var)
  
  # remove leading zeroes (census raw data sometimes have them, sometimes don't)
  cat_reference[[var]] <- as.character(as.numeric(cat_reference[[var]]))
  
  # remove leading zeroes from the data column too
  data_column[[var]] <- as.character(as.numeric(data_column[[var]]))
  
  # join new lookup table to the data column with proper values
  data_column <- 
    data_column |>
    left_join(cat_reference) # natural join on coded value
  
  # return new data column with descriptive values, as factor
  return(as.factor(data_column[[2]]))
  
  # # assign the descriptive values to the original column, as factor
  # data_column[[1]] <- as.factor(data_column[[2]])
  # 
  # # # Drop the extra column from the time reference table
  # # census_data_tbl <- census_data_tbl |>
  # #   select(-one_of(paste0(time_code, "_clean")))
  # 
  # # return just the original column, which now has descriptive values 
  # return(data_column[[1]])
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_cat_refs <- function(cat_code) {
  
  # construct url from the time_code (JWDP or JWAP)
  cat_url <- paste0("https://api.census.gov/data/2022/acs/acs1/pums/variables/",
                     cat_code, ".json")
  
  # retrieve data in list form from API, then bind rows to put in 1 by x tibble,
  # then transpose the data to get key-value pair in columns
  cat_ref <- 
    fromJSON(cat_url)$values |>
    bind_rows() |>
    pivot_longer(cols = everything(), 
                 names_to = cat_code, 
                 values_to = "description")

  return(cat_ref)
  
  }

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Summary Function for Census Class
summary.census <- function(data, 
                           numeric_vars = NULL, 
                           categorical_vars = NULL) {
  
  # Determine the variables that are actually in the dataset
  valid_numeric_vars <- get_valid_numeric_vars()
  valid_categorical_vars <- get_valid_categorical_vars()
  
  data_names <- toupper(names(data))
  
  numeric_vars_in_data <- intersect(data_names, valid_numeric_vars)
  categorical_vars_in_data <- intersect(data_names, valid_categorical_vars)
  
  # Default: Summarize all numeric variables except PWGTP in dataset
  if (is.null(numeric_vars)) {
    numeric_vars <- numeric_vars_in_data[numeric_vars_in_data != "PWGTP"]
  } else {
    # otherwise filter only for those provided
    numeric_vars <- intersect(toupper(numeric_vars), numeric_vars_in_data)
  }
  
  # Default: Summarize all categorical variables in dataset
  if (is.null(categorical_vars)) {
    categorical_vars <- categorical_vars_in_data
  } else {
    # otherwise filter only for those provided
    categorical_vars <- intersect(toupper(categorical_vars),
                                  categorical_vars_in_data)
  }
  
  weight <- data$PWGTP
  summary_list <- list()

  # Summarize numeric variables
  for (var in numeric_vars) {
    
    # Check if the variable is a time variable
    is_time_var <- var %in% c("JWAP", "JWDP")
    
    if (is_time_var) {
      # Convert time to seconds
      numeric_vector <- as.numeric(data[[var]])
    } else if (is.numeric(data[[var]])) {
      numeric_vector <- data[[var]]
    } else {
      stop("Unexpected non-numeric variable found for variable: ", var)
    }
    
    # Calculate weighted mean and standard deviation
    weighted_sample_mean <- sum(numeric_vector * weight, na.rm = TRUE) / 
                              sum(weight, na.rm = TRUE)
    sample_sd <- sqrt(sum((numeric_vector^2) * weight, na.rm = TRUE) / 
                        sum(weight, na.rm = TRUE) - weighted_sample_mean^2)
    
    if (is_time_var) {
      # Convert the results back to hms
      weighted_sample_mean <- as_hms(weighted_sample_mean) 
      sample_sd <- as_hms(sample_sd)
    }
    
    # Store the results
    summary_list[[var]] <- list(
      mean = weighted_sample_mean,
      sd = sample_sd
    )
  }
  
  
  # Summarize categorical variables
  for (var in categorical_vars) {
    
    counts <-  data |> count(.data[[var]])
    
    # Store the results
    summary_list[[var]] <- list(
      counts = counts
    )
  }

  return(summary_list)
}


# TESTING SUMMARY FUNCTION
test_tibble <- tibble(
  AGEP = as.numeric(c(25, 30, 45, 22, 28, 35)),    
  SEX = as.factor(c("Male", "Female", "Female", "Male", "Male", "Female")),  
  PWGTP = as.numeric(c(1.5, 2.0, 1.0, 0.5, 2.0, 1.5))
)
class(test_tibble) <- c("census", class(test_tibble))

str(test_tibble)

summary.census(test_tibble)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plotting Function for Census Class 
plot.census <- function(data, 
                        numeric_var, 
                        categorical_var,
                        sample_size = 100000) {
  
  # Check User inputs
  for (var in c(numeric_var, categorical_var, "PWGTP")) {
    if (!var %in% names(data)) {
      stop(paste("The variable", var, "is not present in the dataset. ",
                 "Select from: ", paste(names(data), collapse = ", ")))
    }
  }
  
  # Remove NA records
  data <- data |>
    filter(!is.na(.data[[numeric_var]]) & 
             !is.na(.data[[categorical_var]]))
  
  # If the dataset is large, take a random sample
  if (nrow(data) > sample_size) {
    message(nrow(data), " found in dataset. Sampled ", 
            sample_size, " rows for plotting.")
    set.seed(123)  # For reproducibility
    data <- data |> sample_n(sample_size)
  }
  
  # Plot with ggplot2
  ggplot(data, 
         aes(x = get(categorical_var), 
             y = get(numeric_var), 
             weight = PWGTP)) +
    geom_boxplot() +
    labs(
      title = paste("Boxplot of", numeric_var, "by", categorical_var),
      x = categorical_var, 
      y = numeric_var 
    ) +
    theme_minimal()
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function for Querying Multiple Years
# KATY
# NOTE: I have not tested this, it may not work.
query_multiple_years <- function(years, 
                                 numeric_vars = c("AGEP", "PWGTP"), 
                                 categorical_vars = c("SEX"), 
                                 geography = "State", 
                                 subset = 8) {
  
  # create empty list to store data frames
  multi_year_list <- list()
  
  # call the user interface for each year
  for (yr in years) {
    
    # retrieve single year data tibble
    census_single_yr <- get_data_tibble_from_census_api(yr,
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TEST SINGLE YEAR

# First, the defaults
defaults <- get_data_tibble_from_census_api()
defaults
summary.census(defaults)
plot.census(defaults, "AGEP", "SEX")

# Set variables
year <- 2015
num_vars <- c("AGEP", "PWGTP", "JWAP") 
cat_vars <- c("SEX", "HHL")
geo <- "State"
subset <- 37

test_vars <- get_data_tibble_from_census_api(year,
                                             num_vars,
                                             cat_vars,
                                             geo,
                                             subset)
test_vars
test_vars |> summary.census()
test_vars |> plot.census(numeric_var = "JWAP",
                         categorical_var = "SEX")


# TEST MULTI YEAR
years <- c(2010:2015)

query_multiple_years(years)


# test conversion of variables
distinct_converted_values <- function(data) {
  
  distinct_values <- list()
  data_columns <- names(data)
  i <- 1
  
  for (c in data_columns) {
    distinct_values[i] <- data |> distinct(data[c])
    names(distinct_values)[i] <- c
    i <- i + 1
  }
  
  return(distinct_values)
  
}