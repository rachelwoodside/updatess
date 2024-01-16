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

rename_file <- function(old_name, new_name) {
  # should be able to rename a file or folder
}

today_as_yyyy_mm_dd_string <- function() {
  return(format(today(tzone="GMT"), format="%Y-%m-%d"))
}

create_archive_folder <- function() {
  # TODO: Check if archive folder exists, create if not
}

move_file_to_archive_folder <- function() {
  
}

# file.rename docs https://r-lang.com/how-to-rename-a-file-in-r/

# required for station name or deployment date changes
rename_log <- function() {
  
}

# required for station name changes
rename_station_folder <- function() {
  
}

# required for station name or deployment date changes
rename_deployment_folder <- function() {
  
}

create_station_folder <- function() {
  
}