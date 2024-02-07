library(googledrive) # allows accessing Google Sheets by name
library(googlesheets4) # allows reading and writing Google Sheets
library(dplyr)
library(tibble)
library(tidyverse)

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
  # Default value for cell_id is FALSE
  cell_id <- FALSE
  
  # Convert date string to POSIXct? 
  # TODO: Remove and ensure that depl_date is provided as posixct in the first place?
  depl_date <- as.POSIXct(paste(depl_date, "0:00:00"), tz="GMT")
  
  # Identify row to edit based on station name and deployment date
  # Note: increment by one to account for the fact that there is a row for
  # column names in the Google sheet but not in the tibble
  row <- sheet_data %>% with(which(station==station_name & deployment==depl_date)) + 1
  
  if (!is_empty(row)) {
    # Map the names of the columns to letters as in the Google sheet
    col_name_to_letter_map <- map_col_names_to_letters(sheet_data)
    
    # Get cell identifier based on row number and column name to letter map
    cell_id <- paste(col_name_to_letter_map[column_name], row, sep="")
  }
  
  return(cell_id)
}

find_area_info_cell <- function(sheet_data, station_name, column_name) {
  # Default value for cell_id is FALSE
  cell_id <- FALSE
  
  # Identify row to edit based on station name
  # Note: increment by one to account for the fact that there is a row for
  # column names in the Google sheet but not in the tibble
  row <- sheet_data %>% with(which(station==station_name)) + 1
  
  if (!is_empty(row)) {
    # Map the names of the columns to letters as in the Google sheet
    col_name_to_letter_map <- map_col_names_to_letters(sheet_data)
    
    # Get cell identifier based on row number and column name to letter map
    cell_id <- paste(col_name_to_letter_map[column_name], row, sep="")
  }
  
  return(cell_id)
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