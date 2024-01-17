library(dplyr)
library(tibble)
library(tidyverse)
library(snakecase)
# Package that could be useful if base R is insufficient: 
#https://www.datanovia.com/en/blog/how-to-easily-manipulate-files-and-directories-in-r/

get_file_path_to_station_folder <- function(station_folder_path, station_name) {
  return(file.path(station_folder_path, to_snake_case(station_name)))
}

get_file_path_to_depl_folder <- function(station_folder_path, station_name, depl_date) {
  snake_case_station_name <- to_snake_case(station_name)
  depl_folder <- paste(snake_case_station_name, depl_date, sep="_")
  return(file.path(station_folder_path, snake_case_station_name, depl_folder))
}

# File extension extraction function pulled from helpers-misc.R in sensorstrings package on 2024-01-15
extract_file_extension <- function(file_name) {
  extension <- file_name %>%
    data.frame() %>%
    separate(col = 1, into = c(NA, "EXT"), sep = "\\.")
  
  extension$EXT
}

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

rename_file <- function(old_name, new_name) {
  # should be able to rename a file or folder
}

today_as_yyyy_mm_dd_string <- function() {
  return(format(today(tzone="GMT"), format="%Y-%m-%d"))
}

construct_new_log_name <- function(station_name, deployment_date) {
  
}

create_archive_folder <- function() {
  # TODO: Check if archive folder exists, create if not
}

move_file_to_archive_folder <- function() {
  
}

# file.rename docs https://r-lang.com/how-to-rename-a-file-in-r/

# required for station name or deployment date changes
rename_log <- function(log_file_name, new_file_name) {
  if (file.exists(old_file)) {
    file.rename(old_file, new_file)
  } else {
    cat("The file does not exist")
  }
}

# required for station name changes
rename_station_folder <- function() {
  
}

# required for station name or deployment date changes
rename_deployment_folder <- function() {
  
}

create_station_folder <- function() {
  
}