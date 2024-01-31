library(dplyr)
library(tibble)
library(tidyverse)
library(snakecase)
library(glue)
library(fs)

# GENERIC HELPERS --------------------------------------------------------------

today_as_yyyy_mm_dd_string <- function() {
  return(format(today(tzone="GMT"), format="%Y-%m-%d"))
}


# TODO: Replace this with file_ext from the fs package?
# File extension extraction function pulled from helpers-misc.R in sensorstrings package on 2024-01-15
extract_file_extension <- function(file_name) {
  extension <- file_name %>%
    data.frame() %>%
    separate(col = 1, into = c(NA, "EXT"), sep = "\\.")
  
  extension$EXT
}

# PATH FINDING HELPERS ---------------------------------------------------------

# TODO: Add check to see if directory exists?
get_relative_path_to_station_folder <- function(rel_stations_folder_path, station_name) {
  snake_case_station_name <- to_snake_case(station_name)
  station_folder_path <- path_norm(glue("{rel_stations_folder_path}/{snake_case_station_name}"))
  return(station_folder_path)
}

# TODO: Add check to see if directory exists?
get_relative_path_to_depl_folder <- function(rel_station_folder_path, station_name, depl_date) {
  snake_case_station_name <- to_snake_case(station_name)
  depl_folder_path <- path_norm(glue("{rel_station_folder_path}/{snake_case_station_name}_{depl_date}"))
  return(depl_folder_path)
}

# TODO: Add check for max path error with error message to the effect of:
# Template must be closer to the directory containing all the station folders due to max path length limitations
# TODO: Add check to confirm this path exists - this should also confirm that it is correctly calculated
# relative to the current working directory
get_relative_path_from_wd_to_stations_folder <- function(stations_folder_path) {
  cwdir <- getwd()
  relative_path <- path_rel(stations_folder_path, start=cwdir)
  return(relative_path)
}

strip_path_start <- function(full_path, path_start) {
  path_start_regex <- regex(paste0(path_start, "\\/([a-z0-9\\/_\\-]+)"), ignore_case=TRUE)
  match <- str_extract(full_path, pattern=path_start_regex, group=1)
  return(match)
}

# COPYING AND DELETING OPERATIONS ----------------------------------------------

copy_deployment_files <- function(old_deployment_folder_path, new_deployment_folder_path) {
  dirs_to_copy <- dir_ls(old_deployment_folder_path, type="directory")
  message(glue("Copying folders from {old_deployment_folder_path}..."))
  for (dir in dirs_to_copy) {
    message(glue("Copying {dir}"))
    dir_copy(dir, new_deployment_folder_path)
  }
  files_to_copy <- dir_ls(old_deployment_folder_path, type="file")
  message(glue("Copying files from {old_deployment_folder_path}..."))
  for (file in files_to_copy) {
    message(glue("Copying {file}"))
    file_copy(file, new_deployment_folder_path)
  }
  return(TRUE)
}

#copy_deployment_files("../fake_station_folders/birchy_head/birchy_head_2018-02-20",
#                     "../fake_station_folders/birchy_head_1/birchy_head_1_2018-02-20")

copy_station_files <- function(old_station_folder_path, new_station_folder_path) {
  files_to_copy <- dir_ls(old_station_folder_path, type="file")
  message(glue("Copying files from {old_station_folder_path}"))
  for (file in files_to_copy) {
    message(glue("Copying {file}"))
    file_copy(file, new_station_folder_path)
  }
  return(TRUE)
}

safe_delete_old_deployment_folder <- function(old_deployment_folder_path, new_deployment_folder_path) {
  # Check that directories that exist in the old deployment folder also exist in the new deployment folder
  message(glue("Checking that all directories that exist in {old_deployment_folder_path} also exist in {new_deployment_folder_path}..."))
  # Note: recurse option set to true to check nested files
  old_dirs <- dir_ls(old_deployment_folder_path, type="directory", recurse=TRUE)
  old_dirs <- map_chr(old_dirs, strip_path_start, old_deployment_folder_path)
  new_dirs <- dir_ls(new_deployment_folder_path, type="directory", recurse=TRUE)
  dir_diff <- setdiff(old_dirs, new_dirs)
  message(old_dirs)
}

#safe_delete_old_deployment_folder("../fake_station_folders/birchy_head/birchy_head_2018-02-20",
                                  #"../fake_station_folders/birchy_head_1/birchy_head_1_2018-02-20")

# LOG OPERATIONS ---------------------------------------------------------------

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
  file_ext <- path_ext(old_log_file_name)
  new_log_name <- glue("{old_log_file_path}/{updated_station_name}_{updated_depl_date}_log.{file_ext}")
  file.rename(old_log_file_name, new_log_name)
  return(TRUE)
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

# CREATING NEW FOLDERS ---------------------------------------------------------

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