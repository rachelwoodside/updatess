library(dplyr)
library(tidyverse)
library(readxl)
library(data.table)
library(glue)
library(writexl)
library(lubridate)
source("file_system_helpers.R")

read_spreadsheet_data <- function(file_path, file_type) {
  if (file_type == "xls" | file_type == "xlsx") {
    file <- read_excel(file_path, na = c("", "n/a", "N/A"))
  }
  else if (file_type == "csv") {
    file <- fread(
      file_path,
      data.table = FALSE,
      na.strings = c("", "n/a", "N/A")
    )
  }
  else
  {
    stop("No .xls, .xlsx, or .csv files found in the specified folder")
  }
  return(file)
}

write_spreadsheet_data <- function(updated_data, file_path, file_type) {
  #writexl currently only supports writing the whole file
  if (file_type == "xls" | file_type == "xlsx") {
    file <- write_xlsx(updated_data, file_path)
  }
  #fwrite is also currently easiest to use by overwriting the whole file
  else if (file_type == "csv") {
    file <- fwrite(
      updated_data,
      file_path
    )
  }
  return(file)
}

update_log_data <- function(deployment_folder_path, column_to_update, old_value, new_value) {
  # retrieve log file name
  log_file_name <- extract_log_file_name(deployment_folder_path)
  # get file type
  log_file_extension <- extract_file_extension(log_file_name)
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
  return(log_data)
}

# testing update_log_data
updated_log_data <- update_log_data("R:/program_documents/cmp_hiring/intern/2023_rachel/projects/cmp/deployment_change_tracking/deployment_change_code/updatess/fake_station_folders/birchy_head/birchy_head_2018-02-20", 
                "Location_Description",
                "Birchy Head",
                "Birchy Head 1")

update_config_table_entry <- function(station_name, deployment_date, old_value, new_value) {
  config_table_file_path <- file.path("R:/program_documents/cmp_hiring/intern/2023_rachel/projects/cmp/deployment_change_tracking/deployment_change_code/updatess/fake_config_tables")
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
  }
}

updated_config_data <- update_config_table_entry("Birchy Head", "2018-02-21", "old", "new")
