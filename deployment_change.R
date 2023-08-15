source("googlesheet_helpers.R")
source("file_system_helpers.R")
source("excel_sheet_helpers.R")

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
