library(dplyr)
library(tibble)
library(tidyverse)
library(snakecase)

get_file_path_to_station_folder <- function(station_folder_path, station_name) {
  return(file.path(station_folder_path, to_snake_case(station_name)))
}

get_file_path_to_depl_folder <- function(station_folder_path, station_name, depl_date) {
  snake_case_station_name <- to_snake_case(station_name)
  depl_folder <- paste(snake_case_station_name, depl_date, sep="_")
  return(file.path(station_folder_path, snake_case_station_name, depl_folder))
}

rename_file <- function(old_name, new_name) {
  # should be able to rename a file or folder
}

today_as_yyyy_mm_dd_string <- function() {
  return(format(today(tzone="GMT"), format="%Y-%m-%d"))
}