library(googledrive) # allows accessing Google Sheets by name
library(googlesheets4) # allows reading and writing Google Sheets
library(dplyr)
library(tibble)
library(tidyverse)
library(snakecase)

# TODO: Should this erase previously processed data by default? -- use a boolean param?
# TODO: Look into R package for error handling!
# TODO: Build row in a different way to allow for parallelizing operations? 
#(Contingent on identifying information like station name and deployment date not changing too soon...?)

# CONSTANTS -----------------------------------------------------------------
#station_folder_path <- "Y:/coastal_monitoring_program/data_branches/water_quality/station_folders"
station_folder_path <- "C:/Users/Rachel Woodside/OneDrive - Perennia/projects/cmp/deployment_change_code/fake_station_folders"
#string_tracking_sheet <- drive_get("STRING TRACKING")
string_tracking_sheet <- drive_get("TestSheetForCodeModification")
change_tracking_sheet <- drive_get("Deployment Change Tracking")

# MODIFY HERE!! -------------------------------------------------------------

apply_deployment_change(
  station_name = "Birchy Head",
  depl_date = "2018-02-20", 
  field_to_change = "station name", 
  old_value = "Birchy Head",
  new_value = "New Name", 
  rationale = "Conflict with NSDFA station name",
  note = "this is a test station name change")

apply_deployment_change(
  station_name = "Birchy Head",
  depl_date = "2018-02-20", 
  field_to_change = "deployment date", 
  old_value = "2018-02-20",
  new_value = "2018-04-10", 
  rationale = "Reflect patterns in data",
  note = "this is a test deployment date change"
)

create_new_station(
  station_name = "New Test Station",
  waterbody = "Test Waterbody",
  county = "Test County",
  latitude = -45.28888,
  longitude = -61.6475,
  note = "this is a test new station"
)

# TOP LEVEL FUNCTIONS --------------------------------------------------------
apply_deployment_change <- function(station_name, depl_date, field_to_change, old_value, new_value, rationale, note="") {
  # start building tracking sheet row
  tracking_sheet_row <- c(station_name, depl_date, field_to_change, old_value, new_value, rationale, today_as_yyyy_mm_dd_string())
  # TODO: Add checks for old value to match new value in the case of station name and deployment date? Also that new value is not the same as old value? maybe also check for format?
  if (field_to_change == "station name") {
    completion_record <- apply_station_name_change(station_name, depl_date, new_value)
  } else if (field_to_change == "deployment date") {
    completion_record <- apply_deployment_date_change(station_name, depl_date, new_value)
  } else if (field_to_change == "retrieval date") {
    completion_record <- apply_retrieval_date_change()
  } else if (field_to_change == "waterbody") {
    completion_record <- apply_waterbody_change()
  } else {
    stop("Please enter a valid deployment feature to change. 
         Options are: 'station name', 'deployment date', 'retrieval date', 'waterbody'")
  }
  todo_items <- c("TODO", "TODO", "TODO", "TODO", "TODO", "TODO")
  tracking_sheet_row <- c(tracking_sheet_row, completion_record, todo_items, note)
  tracking_sheet_row_df <- as.data.frame(t(tracking_sheet_row), stringsAsFactors=FALSE)
  sheet_append(change_tracking_sheet, tracking_sheet_row_df, sheet = "Water Quality")
}

create_new_station <- function(station_name, waterbody, county, latitude, longitude, note) {
  # Create station folder
  # Update Area Info
  new_area_info_row <- c(station_name, waterbody, county, latitude, longitude, note)
  new_area_info_row_df <- as.data.frame(t(new_area_info_row), stringsAsFactors=FALSE)
  sheet_append(string_tracking_sheet, new_area_info_row_df, sheet = "Area Info")
}

# MID LEVEL FUNCTIONS - BY TYPE OF CHANGE -----------------------------------

apply_station_name_change <- function(station_name, depl_date, new_value) {
  # TODO: Declare status variables for each change in one place?
  # Update String Tracking Sheet
  # Write old station name into old station name column, appending if there is already a value there
  is_old_name_archived <- update_string_tracking_column(string_tracking_sheet, station_name, depl_date, "old station name", station_name, append=TRUE)
  # Replace old station name in station column
  is_old_name_replaced <- update_string_tracking_column(string_tracking_sheet, station_name, depl_date, "station", new_value)
  completion_record <- c(is_old_name_archived && is_old_name_replaced)
  # TODO: Update Station Folder
  is_station_folder_updated <- FALSE
  completion_record <- c(completion_record, is_station_folder_updated)
  # TODO: Update Deployment Folder
  is_depl_folder_updated <- FALSE
  completion_record <- c(completion_record, is_depl_folder_updated)
  # TODO: Update README (at station folder level)
  is_readme_updated <- FALSE
  completion_record <- c(completion_record, is_readme_updated)
  # TODO: Update Config Table
  config_table_is_updated <- FALSE
  completion_record <- c(completion_record, is_config_table_updated)
  # TODO: Update Log Content
  is_log_content_updated <- FALSE
  # TODO: Update Log Name
  is_log_name_updated <- FALSE
  completion_record <- c(completion_record, is_log_content_updated && is_log_name_updated)
  return(completion_record)
}

apply_deployment_date_change <- function(station_name, depl_date, new_value) {
  completion_record <- c()
  # TODO: Update String Tracking
  completion_record <- c(completion_record, FALSE)
  # Update Station Folder N/A
  completion_record <- c(completion_record, "N/A")
  # TODO: Update README (at deployment folder level)
  completion_record <- c(completion_record, FALSE)
  # TODO: Update Deployment Folder
  completion_record <- c(completion_record, FALSE)
  # TODO: Update Config Table
  completion_record <- c(completion_record, FALSE)
  # TODO: Update Log Content
  # TODO: Update Log Name
  completion_record <- c(completion_record, FALSE)
  return(completion_record)
}

apply_retrieval_date_change <- function() {
  # Update String Tracking N/A
  # Update Station Folder N/A
  # TODO: Update README (at deployment folder level)
  # Update Deployment Folder N/A
  # Update Config Table N/A
  # TODO: Update Log Content
  return(completion_record)
}

apply_waterbody_change <- function() {
  # TODO: Update String Tracking
  # Update Station Folder N/A
  # TODO: Update README (at station folder level)
  # Update Deployment Folder N/A
  # TODO: Update Config Table
  # TODO: Update Log Content
  return(completion_record)
}

# FUNCTIONS FOR EACH POTENTIAL CHANGE TO BE MADE ----------------------------

update_string_tracking_column <- function(ss, station_name, depl_date, column_name, new_data, append=FALSE) {
  # Read in Tracking tab of sheet
  sheet_data <- read_sheet(ss, sheet="Tracking")
  # Identify appropriate cell and write new data to it
  cell_id <- find_string_tracking_cell(sheet_data, station_name, depl_date, column_name)
  write_google_sheet_cell(ss, "Tracking", new_data, cell_id, append)
  return(TRUE)
}

update_area_info_column <- function(ss, station_name, column_name, new_data, append=FALSE) {
  # Read in Area Info tab of sheet
  sheet_data <- read_sheet(ss, sheet="Area Info")
  # Identify appropriate cell and write new data to it
  cell_id <- find_area_info_cell(sheet_data, station_name, column_name)
  write_google_sheet_cell(ss, "Area Info", new_data, cell_id, append)
  return(TRUE)
}


update_deployment_folder <- function() {
  # needs: location to look for folder
}

update_config_table <- function() {
  
}

write_readme_file <- function(file_path, content) {
  full_file_name <- paste0(file_path, "/README.txt")
  dated_content <- paste(today_as_yyyy_mm_dd_string(), "update:", content)
  file_list <- list.files(file_path, pattern="README.txt", ignore.case=TRUE)
  if (length(file_list) > 1) {
    print("Multiple README files found. Please manually condense the files into one.")
    # return FALSE for a failure
    return(FALSE)
  } else {
    if (length(file_list == 1)) {
      print("README file found")
      # If there readme already exists, add newline character for readability
      cat('\n\n', file=full_file_name, append=TRUE)
    } else {
      print("README file not found. README file will be created.")
    }
    # Append to existing README if it exists or create and write if it does not
    # Note: this is not case-sensitive
    # TODO: Potentially get this to write the date of the change as well?
    cat(dated_content, file=full_file_name, append=TRUE)
    # return TRUE for a success
    return(TRUE)
  }
}

update_log <- function() {
  # generate new log name
  # modify appropriate log contents
  
}

update_station_folder <- function() {
  # create new station folder if necessary
  # move everything!
}

# HELPER FUNCTIONS ----------------------------------------------------------

# FILE AND DIRECTORY HELPERS

get_file_path_to_station_folder <- function(station_folder_path, station_name) {
  return(file.path(station_folder_path, to_snake_case(station_name)))
}

get_file_path_to_depl_folder <- function(station_folder_path, station_name, depl_date) {
  snake_case_station_name <- to_snake_case(station_name)
  depl_folder <- paste(snake_case_station_name, depl_date, sep="_")
  return(file.path(station_folder_path, snake_case_station_name, depl_folder))
}

rename_file <- function(old_name, new_name) {
  # should be able to rename a file or folder
}

today_as_yyyy_mm_dd_string <- function() {
  return(format(today(tzone="GMT"), format="%Y-%m-%d"))
}


# TODO: edit an excel sheet?

# GOOGLESHEET HELPERS
# NOTE: Sorting does not seem to be an issue as long as it remains the same
# between reading in the sheet and writing to the sheet
# Filtering seems to be completely ignored because the row numbers are maintained
# at the Google sheet level and the googlesheets4 package reads in all rows regardless

# Map columns of a data frame to letters to match spreadsheet cell identification
map_col_names_to_letters <- function(sheet_data) {
  # TODO: Could extend to work beyond A-Z?
  # If not, then error if number of columns is greater than 26
  num_cols <- ncol(sheet_data) # how many columns the data has
  col_name_to_letter_map <- c(LETTERS[1:num_cols]) # which letters we need
  col_names <- colnames(sheet_data)
  names(col_name_to_letter_map) <- col_names
  return(col_name_to_letter_map)
}

find_string_tracking_cell <- function(sheet_data, station_name, depl_date, column_name) {
  # Convert date string to POSIXct? 
  # TODO: Remove and ensure that depl_date is provided as posixct in the first place?
  depl_date <- as.POSIXct(paste(depl_date, "0:00:00"), tz="GMT")
  
  # Identify row to edit based on station name and deployment date
  # Note: increment by one to account for the fact that there is a row for
  # column names in the Google sheet but not in the tibble
  row <- sheet_data %>% with(which(station==station_name & deployment==depl_date)) + 1
  
  # Map the names of the columns to letters as in the Google sheet
  col_name_to_letter_map <- map_col_names_to_letters(sheet_data)
  
  # Get cell identifier based on row number and column name to letter map
  cell_id <- paste(col_name_to_letter_map[column_name], row, sep="")
  return(cell_id)
}

find_area_info_cell <- function(sheet_data, station_name, column_name) {
  # Identify row to edit based on station name
  # Note: increment by one to account for the fact that there is a row for
  # column names in the Google sheet but not in the tibble
  row <- sheet_data %>% with(which(station==station_name)) + 1
  
  # Map the names of columns to letters as in the Google sheet
  col_name_to_letter_map <- map_col_names_to_letters(sheet_data)
  
  # Get cell identifier based on row number and column name to letter map
  cell_id <- paste(col_name_to_letter_map[column_name], row, sep="")
}

write_google_sheet_cell <- function(ss, sheet_tab, new_data, cell_id, append) {
  # TODO: Check that inputted data is the correct format? or maybe just a string?
  data_to_write <- new_data
  if (append) {
    # Read in the existing data and append the new data to it
    existing_data <- range_read(
      ss,
      sheet = sheet_tab,
      range = cell_id,
      col_names = FALSE
    )
    # Returned tibble should have no more than 1 row and 1 column (a single cell)
    if (nrow(existing_data) > 1 || ncol(existing_data) > 1) {
      print("Could not write to Google Sheet. More than one cell was supplied to write_google_sheet_cell().")
      # Return with a flag for failure
      return(FALSE)
    # Confirming the tibble is not empty)
    } else if (nrow(existing_data) + ncol(existing_data) != 0) {
      data_to_write <- paste0(existing_data, ", ", new_data)
    }
    # If the tibble we don't need to append to any existing data
  }
  data_to_write <- as.data.frame(data_to_write, stringsAsFactors=FALSE)
  # Write to the specified cell
  range_write(
    ss, 
    data_to_write, 
    sheet=sheet_tab, 
    range=cell_id, 
    col_names=FALSE, 
    reformat=FALSE
  )
}