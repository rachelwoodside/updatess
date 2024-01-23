library(dplyr)
library(tibble)
library(tidyverse)
library(snakecase)
library(glue)
# Package that could be useful if base R is insufficient: 
#https://www.datanovia.com/en/blog/how-to-easily-manipulate-files-and-directories-in-r/

get_absolute_file_path_to_station_folder <- function(station_folders_path, station_name) {
  return(file.path(station_folders_path, to_snake_case(station_name)))
}

get_relative_file_path_to_station_folder <- function(station_name) {
  return(file.path(to_snake_case(station_name)))
}

get_absolute_path_to_depl_folder <- function(station_folder_path, station_name, depl_date) {
  snake_case_station_name <- to_snake_case(station_name)
  depl_folder <- paste(snake_case_station_name, depl_date, sep="_")
  return(file.path(station_folder_path, snake_case_station_name, depl_folder))
}

get_relative_path_to_depl_folder <- function(station_name, depl_date) {
  snake_case_station_name <- to_snake_case(station_name)
  depl_folder <- glue("{snake_case_station_name}_{depl_date}")
  return(file.path(snake_case_station_name, depl_folder))
}

# File extension extraction function pulled from helpers-misc.R in sensorstrings package on 2024-01-15
extract_file_extension <- function(file_name) {
  extension <- file_name %>%
    data.frame() %>%
    separate(col = 1, into = c(NA, "EXT"), sep = "\\.")
  
  extension$EXT
}

extract_log_folder_name <- function(deployment_folder_path) {
  # Read in log as in ss_read_log.R from sensorstrings package on 2024-01-15
  # extract the name of the log folder (e.g. Log, log, LOG)
  log_folder <- list.files(deployment_folder_path) %>%
                str_extract(regex("log", ignore_case = TRUE)) %>%
                na.omit()
  return(log_folder)
}

extract_log_folder_path <- function(deployment_folder_path) {
  log_folder <- extract_log_folder_name(deployment_folder_path)
  log_path <- glue("{deployment_folder_path}/{log_folder}")
  return(log_path)
}

extract_log_file_name <- function(deployment_folder_path) {
  
  log_path <- extract_log_folder_path(deployment_folder_path)
  
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

rename_log <- function(old_log_file_path, old_log_file_name, updated_station_name, updated_depl_date) {
  updated_station_name <- to_snake_case(updated_station_name)
  file_ext <- extract_file_extension(old_log_file_name)
  new_log_name <- glue("{old_log_file_path}/{updated_station_name}_{updated_depl_date}_log.{file_ext}")
  file.rename(old_log_file_name, new_log_name)
  return(TRUE)
}

#new_log_name <- rename_log("R:/program_documents/cmp_hiring/intern/2023_rachel/projects/cmp/deployment_change_tracking/deployment_change_code/updatess/fake_station_folders/birchy_head/birchy_head_2018-02-20/Log",
#           "R:/program_documents/cmp_hiring/intern/2023_rachel/projects/cmp/deployment_change_tracking/deployment_change_code/updatess/fake_station_folders/birchy_head/birchy_head_2018-02-20/Log/Birchy Head 2018-02-20 Log.csv",
#           "Birchy Head 1",
#           "2018-02-20")

today_as_yyyy_mm_dd_string <- function() {
  return(format(today(tzone="GMT"), format="%Y-%m-%d"))
}

archive_log <- function(log_file_path, log_file_name) {
  # Create archive folder if it does not exist
  archive_path <- glue("{log_file_path}/archive")
  message(glue("Creating archive folder in {archive_path}"))
  dir.create(archive_path)
  # Copy log to archive folder
  archive_log_file_name_local <- str_extract(log_file_name, regex("log\\/([a-z0-9_\\- ]+\\.csv$|[a-z0-9_\\- ]+\\.xlsx$|[a-z0-9_\\- ]+\\.xls$)", ignore_case = TRUE))
  archive_log_file_name <- glue("{archive_path}/{archive_log_file_name_local}")
  message(glue("Copying {archive_log_file_name_local} from {log_file_path} to {archive_path}"))
  file.copy(log_file_name, archive_path, copy.date=TRUE)
}

#archive_log("R:/program_documents/cmp_hiring/intern/2023_rachel/projects/cmp/deployment_change_tracking/deployment_change_code/updatess/fake_station_folders/birchy_head/birchy_head_2018-02-20/Log",
#            "R:/program_documents/cmp_hiring/intern/2023_rachel/projects/cmp/deployment_change_tracking/deployment_change_code/updatess/fake_station_folders/birchy_head/birchy_head_2018-02-20/Log/Birchy Head 2018-02-20 Log.csv")

# required for station name or deployment date changes
create_deployment_folder <- function(station_folders_path, station_name, deployment_date) {
  # Make sure the station folder has already been created so the path to the station folder should exist
  new_station_folder_path <- create_station_folder(station_folders_path, station_name)
  snake_case_station_name <- to_snake_case(station_name)
  new_deployment_folder_path <- glue("{new_station_folder_path}/{snake_case_station_name}_{deployment_date}")
  # TODO: Reformulate this with trycatch? Could lead to confusing errors if path passed to function is not valid?
  # if condition to make sure we don't get a warning for trying to create it when it exists
  if (!dir.exists(new_deployment_folder_path)) {
    dir.create(new_deployment_folder_path)
    message(glue("Deployment folder created: '{new_deployment_folder_path}'\n"))
  } else {
    message(glue("Deployment folder already exists. Continuing execution.\n"))
  }
  return(new_deployment_folder_path)
}

# TODO: Check that snake case conversion works even if snake case is already provided
# for use with create_deployment_folder() function
create_station_folder <- function(station_folders_path, station_name) {
  snake_case_station_name <- to_snake_case(station_name)
  new_station_folder_path <- glue("{station_folders_path}/{snake_case_station_name}")
  # TODO: Reformulate this with trycatch? Could lead to confusing errors if path passed to function is not valid?
  # if condition to make sure we don't get a warning for trying to create it when it exists
  if (!dir.exists(new_station_folder_path)) {
    dir.create(new_station_folder_path)
    message(glue("Station folder created: '{new_station_folder_path}'\n"))
  } else {
    message(glue("Station folder already exists. Continuing execution.\n"))
  }
  return(new_station_folder_path)
}

#create_station_folder("R:/program_documents/cmp_hiring/intern/2023_rachel/projects/cmp/deployment_change_tracking/deployment_change_code/updatess/fake_station_folders",
                      #"A New Station")

#create_deployment_folder("R:/program_documents/cmp_hiring/intern/2023_rachel/projects/cmp/deployment_change_tracking/deployment_change_code/updatess/fake_station_folders",
                      #"A New Station",
                      #"2018-02-20")