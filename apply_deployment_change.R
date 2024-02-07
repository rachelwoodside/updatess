source("googlesheet_helpers.R")
source("file_system_helpers.R")
source("excel_sheet_helpers.R")
library(fs)

apply_deployment_change <- function(stations_folder_path, station_name, depl_date, field_to_change, old_value, new_value, rationale, note="") {
  # start building tracking sheet row
  tracking_sheet_row <- c(station_name, depl_date, field_to_change, old_value, new_value, rationale, today_as_yyyy_mm_dd_string())
  # TODO: Add checks for old value to match new value in the case of station name and deployment date? Also that new value is not the same as old value? maybe also check for format?
  # TODO: Add error handler to manage errors and ensure partially completed operations are recorded as done in completion record
  if (field_to_change == "station name") {
    completion_record <- apply_station_name_change(stations_folder_path, station_name, depl_date, new_value, rationale, note)
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

apply_station_name_change <- function(stations_folder_path, old_station_name, depl_date, new_station_name, rationale, note) {
  completion_record <- c()
  
  # Get relevant directory structure from old values
  message("Resolving current directory structure...")
  rel_stations_folder_path <- get_relative_path_from_wd_to_stations_folder(stations_folder_path)
  old_station_folder_path <- get_relative_path_to_station_folder(rel_stations_folder_path, old_station_name)
  new_station_folder_path <- get_relative_path_to_station_folder(rel_stations_folder_path, new_station_name)
  old_deployment_folder_path <- get_relative_path_to_depl_folder(old_station_folder_path, old_station_name, depl_date)
  
  #Check if the station exists, both in the folder structure and Area Info
  if (!check_station_folder_exists(new_station_folder_path)) {
    stop("Station folder structure does not exist. Please run create_new_station() to create it.")
  }
  if (!check_station_area_info_exists(ss, new_station_name)) {
    stop("Station area info does not exist. Please run create_new_station() to create it.")
  }
  
  message("UPDATING FOLDER STRUCTURE")
  # Create new deployment folder - station folder should either already exist or be created by 
  new_deployment_folder_path <- create_deployment_folder(rel_stations_folder_path, new_station_name, depl_date)
  
  # Copy content to new folders and delete old folders and contents
  are_deployment_files_copied <- copy_deployment_files(old_deployment_folder_path, new_deployment_folder_path)
  are_station_files_copied <- copy_station_files(old_station_folder_path, new_station_folders_path)
  are_deployment_files_deleted <- safe_delete_old_folder(old_deployment_folder_path, new_deployment_folder_path)
  is_depl_folder_updated <- (are_deployment_files_copied && are_station_files_copied && are_deployment_files_deleted)
  completion_record <- c(completion_record, is_depl_folder_updated)
  
  message("UPDATING STRING TRACKING SHEET")
  # Write old station name into old station name column, appending if there is already a value there
  is_old_name_archived <- update_string_tracking_column(string_tracking_sheet, old_station_name, depl_date, "old station name", old_station_name, append=TRUE)
  # Replace old station name in station column
  is_old_name_replaced <- update_string_tracking_column(string_tracking_sheet, old_station_name, depl_date, "station", new_station_name)
  completion_record <- c(completion_record, is_old_name_archived && is_old_name_replaced)
  
  message("UPDATING LOG CONTENTS AND NAME")
  # Update Log
  is_log_content_updated <- update_log_data(new_deployment_folder_path, "Location_Description", old_station_name, new_station_name)
  # Update Log Name
  is_log_name_updated <- update_log_name(new_deployment_folder_path, new_station_name, depl_date)
  completion_record <- c(completion_record, is_log_content_updated && is_log_name_updated)
  
  message("UPDATING README IN THE STATION FOLDER")
  # Update README (at station folder level)
  readme_content <- build_readme_content("station name", old_station_name, new_station_name, rationale, note)
  is_readme_updated <- write_readme_file(new_station_folder_path, readme_content)
  completion_record <- c(completion_record, is_readme_updated)
  
  message("UPDATING CONFIGURATION TABLE")
  # Update Config Table
  is_config_table_updated <- update_config_table_entry(old_station_name, depl_date, new_station_name)
  completion_record <- c(completion_record, is_config_table_updated)
  return(completion_record)
}



apply_deployment_date_change <- function(station_name, depl_date, new_value) {
  completion_record <- c()
  
  # TODO: Update String Tracking
  completion_record <- c(completion_record, FALSE)
  
  # TODO: Update Log Content
  # TODO: Update Log Name
  completion_record <- c(completion_record, FALSE)
  
  # TODO: Update README (at deployment folder level)
  completion_record <- c(completion_record, FALSE)
  
  # TODO: Update Deployment Folder
  completion_record <- c(completion_record, FALSE)
  
  # Update Station Folder N/A
  completion_record <- c(completion_record, "N/A")

  # TODO: Update Config Table
  completion_record <- c(completion_record, FALSE)

  return(completion_record)
}

apply_retrieval_date_change <- function() {
  # Update String Tracking N/A
  # TODO: Update Log Content
  # TODO: Update README (at deployment folder level)
  # Update Deployment Folder N/A
  # Update Station Folder N/A
  # Update Config Table N/A
  return(completion_record)
}

apply_waterbody_change <- function() {
  # TODO: Update String Tracking
  # TODO: Update Log Content
  # TODO: Update README (at station folder level)
  # Update Deployment Folder N/A
  # Update Station Folder N/A
  # TODO: Update Config Table
  return(completion_record)
}

# FUNCTIONS TO MANAGE STATIONS ---------------------------------------------------

check_station_folder_exists <- function(station_folder_path) {
  return(dir_exists(station_folder_path))
}

check_station_area_info_exists <- function(ss, station_name) {
  sheet_data <- read_sheet(ss, sheet="Area Info")
  area_info_cell_found <- find_area_info_cell(sheet_data, station_name, "station")
  if (is.character(area_info_cell_found)) {
    return(TRUE)
  }
  return(FALSE)
}

create_new_station <- function(station_folder_path, ss, station_name, waterbody, county, latitude, longitude, note) {
  # Create station folder if it does not exist
  if (!check_station_folder_exists(station_folder_path)) {
    # Create station folder
    create_station_folder(station_folders_path, station_name)
    message("Station folder created")
  } 
  # Append row in area info if it does not exist
  sheet_data <- read_sheet(ss, sheet="Area Info")
  if (!find_area_info_cell(station_name, "station")) {
    new_area_info_row <- c(station_name, waterbody, county, latitude, longitude, note)
    new_area_info_row_df <- as.data.frame(t(new_area_info_row), stringsAsFactors=FALSE)
    sheet_append(string_tracking_sheet, new_area_info_row_df, sheet = "Area Info")
    message("Station row in Area Info created")
  }
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

# TODO: Newer deployments will not be listed in the configuration table
# Consider how to manage this (simply ignore and allow user to check manually?)
update_config_table_entry <- function(station_name, deployment_date, new_value) {
  config_table_file_path <- file.path("R:/program_documents/cmp_hiring/intern/2023_rachel/projects/cmp/deployment_change_tracking/deployment_change_code/fake_config_tables")
  config_table_file <- glue("{config_table_file_path}/water_quality_configuration_table.xlsx")
  cb_config_table_file <- glue("{config_table_file_path}/water_quality_cape_breton_configuration.xlsx")
  config_data <- read_spreadsheet_data(config_table_file, "xlsx")
  cb_config_data <- read_spreadsheet_data(cb_config_table_file, "xlsx")
  # Check for entry in cape breton configuration table
  cb_config_entry <- nrow(cb_config_data %>% filter(Station_Name == station_name & Depl_Date == ymd(deployment_date))) == 1
  # Check for entry in configuration table
  config_table_entry <- nrow(config_data %>% filter(Station_Name == station_name & Depl_Date == ymd(deployment_date))) == 1
  if (cb_config_entry) {
    date_formatted_cb_config_data <- cb_config_data %>% 
      mutate(Depl_Date = ymd(Depl_Date))
    updated_cb_config_data <- date_formatted_cb_config_data %>%
      mutate(Station_Name = case_when((Station_Name == station_name & Depl_Date == ymd(deployment_date)) ~ new_value,
                                      .default = Station_Name))
    write_spreadsheet_data(updated_cb_config_data, cb_config_table_file, "xslx")
  } else if (config_table_entry) {
    date_formatted_config_data <- config_data %>% 
      mutate(Depl_Date = ymd(Depl_Date))
    updated_config_data <- date_formatted_config_data %>%
      mutate(Station_Name = case_when((Station_Name == station_name & Depl_Date == ymd(deployment_date)) ~ new_value,
                                      .default = Station_Name))
    write_spreadsheet_data(updated_config_data, config_table_file, "xlsx")
  } else {
    # TODO: Add a check to find similar entries in case of typo?
    message("No entry found in configuration table. Skipping configuration table data update. If deployment is from 2023 or earlier, please double-check to confirm it does not have a configuration table entry.")
    return(FALSE)
  }
  return(TRUE)
}

write_readme_file <- function(file_path, content) {
  full_file_name <- paste0(file_path, "/README.txt")
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
    cat(content, file=full_file_name, append=TRUE)
    # return TRUE for a success
    return(TRUE)
  }
}

build_readme_content <- function(field_to_change, old_value, new_value, rationale, note) {
  dated_content <- glue("{today_as_yyyy_mm_dd_string()} update: changed {field_to_change} from {old_value} to {new_value}.\n 
                        Rationale: {rationale} \n
                        Additional Note: {note}")
  return(dated_content)
}

update_log_name <- function(deployment_folder_path, updated_station_name, updated_depl_date) {
  # access log from file path
  log_file_path <- extract_log_folder_path(deployment_folder_path)
  log_file_name <- extract_log_file_name(deployment_folder_path)
  # TODO: consider/test how copying will work if file already exists with same name?
  return(rename_log(log_file_path, log_file_name, updated_station_name, updated_depl_date))
}

update_log_data <- function(deployment_folder_path, column_to_update, old_value, new_value) {
  # retrieve log file path and name
  log_file_path <- extract_log_folder_path(deployment_folder_path)
  log_file_name <- extract_log_file_name(deployment_folder_path)
  message(glue("Log folder path: {log_file_path}"))
  message(glue("Log file path: {log_file_name}"))
  # archive old log
  archive_log(log_file_path, log_file_name)
  # get file type
  log_file_extension <- path_ext(log_file_name)
  # read in log data
  log_data <- read_spreadsheet_data(log_file_name, log_file_extension)
  date_formatted_log_data <- log_data %>% 
    mutate(Deployment = ymd(Deployment)) %>%
    mutate(Retrieval = ymd(Retrieval))
  # TODO: Check that the log value is in fact the provided old value to make sure we are looking at the right column?
  # TODO: Type checking (during testing I replaced a string with a date and had to switch it back manually through the log)
  updated_log_data <- date_formatted_log_data %>% 
    mutate_at(vars(column_to_update), ~ replace(., TRUE, new_value))
  # write new data to log file 
  write_spreadsheet_data(updated_log_data, log_file_name, log_file_extension)
  return(TRUE)
}
