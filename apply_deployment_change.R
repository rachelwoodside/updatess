source("googlesheet_helpers.R")
source("file_system_helpers.R")
source("excel_sheet_helpers.R")

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
  
  # TODO: Update Log
  # TODO: Update Log Content
  is_log_content_updated <- FALSE
  # TODO: Update Log Name
  is_log_name_updated <- FALSE
  completion_record <- c(completion_record, is_log_content_updated && is_log_name_updated)
  
  # TODO: Update Config Table
  is_config_table_updated <- FALSE
  completion_record <- c(completion_record, is_config_table_updated)
  
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
  # TODO: Update Log Content
  # Update Config Table N/A
  return(completion_record)
}

apply_waterbody_change <- function() {
  # TODO: Update String Tracking
  # Update Station Folder N/A
  # TODO: Update README (at station folder level)
  # Update Deployment Folder N/A
  # TODO: Update Log Content
  # TODO: Update Config Table
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
