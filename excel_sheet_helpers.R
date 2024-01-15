library(dplyr)
library(tidyverse)
library(readxl)
library(data.table)
library(glue)
library(sensorstrings)

# File extension extraction function pulled from helpers-misc.R in sensorstrings package on 2024-01-15
extract_file_extension <- function(file_name) {
  extension <- file_name %>%
    data.frame() %>%
    separate(col = 1, into = c(NA, "EXT"), sep = "\\.")
  
  extension$EXT
}

read_log_data <- function(deployment_folder_path) {
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
  
  # file extension
  file_type <- extract_file_extension(dat_files)
  
  if (file_type == "xls" | file_type == "xlsx") {
    log <- read_excel(paste(log_path, dat_files, sep = "/"), na = c("", "n/a", "N/A"))
  }
  else if (file_type == "csv") {
    log <- fread(
      paste(log_path, dat_files, sep = "/"),
      data.table = FALSE,
      na.strings = c("", "n/a", "N/A")
      )
  }
  else
  {
    stop("No .xls or .xlsx files found in the Log folder")
  }
}

# testing read_log_data
read_log_data("R:/program_documents/cmp_hiring/intern/2023_rachel/projects/cmp/deployment_change_tracking/deployment_change_code/updatess/fake_station_folders/birchy_head/birchy_head_2018-02-20")
