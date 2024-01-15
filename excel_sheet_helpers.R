library(dplyr)
library(tidyverse)
library(readxl)
library(data.table)
library(glue)
library(writexl)
library(lubridate)

# File extension extraction function pulled from helpers-misc.R in sensorstrings package on 2024-01-15
extract_file_extension <- function(file_name) {
  extension <- file_name %>%
    data.frame() %>%
    separate(col = 1, into = c(NA, "EXT"), sep = "\\.")
  
  extension$EXT
}

# TODO: Expand this to work with the config table as well?
extract_log_file_name <- function(deployment_folder_path) {
  # Read in log as in ss_read_log.R from sensorstrings package on 2024-01-15
  # extract the name of the log folder (e.g. Log, log, LOG)
  log_folder <- list.files(deployment_folder_path) %>%
    str_extract(regex("log", ignore_case = TRUE)) %>%
    na.omit()
  
  log_path <- glue("{deployment_folder_path}/{log_folder}")
  
  dat_files <- list.files(log_path, all.files = FALSE, pattern = "*xlsx|*xls|*csv")
  
  # remove files that start with "~"
  if (any(substring(dat_files, 1, 1) == "~")) {
    dat_files <- dat_files[-which(substring(dat_files, 1, 1) == "~")]
  }
  
  if (length(dat_files) > 1) {
    stop("More than one file found in the Log folder")
  }
  else if (length(dat_files) == 0) {
    stop("No files found in the Log folder")
  }
  return(glue("{log_path}/{dat_files}"))
}

read_log_data <- function(log_path, file_type) {
  if (file_type == "xls" | file_type == "xlsx") {
    log <- read_excel(log_path, na = c("", "n/a", "N/A"))
  }
  else if (file_type == "csv") {
    log <- fread(
      log_path,
      data.table = FALSE,
      na.strings = c("", "n/a", "N/A")
    )
  }
  else
  {
    stop("No .xls, .xlsx, or .csv files found in the Log folder")
  }
  return(log)
}

write_log_data <- function(updated_log_data, log_path, file_type) {
  #writexl currently only supports writing the whole file
  if (file_type == "xls" | file_type == "xlsx") {
    log <- write_xlsx(updated_log_data, log_path)
  }
  #fwrite is also currently easiest to use by overwriting the whole file
  else if (file_type == "csv") {
    log <- fwrite(
      updated_log_data,
      log_path
    )
  }
  else
  {
    stop("No .xls, .xlsx, or .csv files found in the Log folder")
  }
  return(log)
  
}

update_log_data <- function(deployment_folder_path, column_to_update, old_value, new_value) {
  # retrieve log file name
  log_file_name <- extract_log_file_name(deployment_folder_path)
  # get file type
  log_file_extension <- extract_file_extension(log_file_name)
  # read in log data
  log_data <- read_log_data(log_file_name, log_file_extension)
  date_formatted_log_data <- log_data %>% 
                              mutate(Deployment = ymd(Deployment)) %>%
                              mutate(Retrieval = ymd(Retrieval))
  # TODO: Check that the log value is in fact the provided old value to make sure we are looking at the right column?
  updated_log_data <- date_formatted_log_data %>% 
                      mutate_at(vars(column_to_update), ~ replace(., TRUE, new_value))
  # write new data to log file 
  write_log_data(updated_log_data, log_file_name, log_file_extension)
  return(log_data)
}

# Error occurring with mutate when working with .xlsx log file and trying to write dates (Deployment column)
# Can write changes to text successfully, but overwrites dates with weird format
deployment_folder_path <- "R:/program_documents/cmp_hiring/intern/2023_rachel/projects/cmp/deployment_change_tracking/deployment_change_code/updatess/fake_station_folders/birchy_head/birchy_head_2022-05-12"
log_file_name <- extract_log_file_name(deployment_folder_path)
log_file_extension <- extract_file_extension(log_file_name)
log_data <- read_log_data(log_file_name, log_file_extension)
date_formatted_log_data <- log_data %>% 
                            mutate(Deployment = ymd(Deployment)) %>%
                            mutate(Retrieval = ymd(Retrieval))
updated_log_data <- date_formatted_log_data %>%
                    mutate_at(vars("Deployment"), ~ replace(., TRUE, "2022-05-12"))
write_log_data(updated_log_data, log_file_name, log_file_extension)

# testing update_log_data
updated_log_data <- update_log_data("R:/program_documents/cmp_hiring/intern/2023_rachel/projects/cmp/deployment_change_tracking/deployment_change_code/updatess/fake_station_folders/birchy_head/birchy_head_2022-05-12", 
                "Location_Description",
                "Birchy Head",
                "Birchy Head 1")
