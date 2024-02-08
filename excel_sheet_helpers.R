library(dplyr)
library(tidyverse)
library(readxl)
library(data.table)
library(glue)
library(writexl)
library(lubridate)
source("file_system_helpers.R")

read_spreadsheet_data <- function(file_path, file_type) {
  message(glue("Reading spreadsheet data from: {file_path}"))
  if (file_type == "xls" | file_type == "xlsx") {
    file <- read_excel(file_path, na = c("", "n/a", "N/A", "NA"))
  }
  else if (file_type == "csv") {
    file <- fread(
      file_path,
      data.table = FALSE,
      na.strings = c("", "n/a", "N/A", "NA")
    )
  }
  else
  {
    stop("No .xls, .xlsx, or .csv files found in the specified folder")
  }
  return(file)
}

write_spreadsheet_data <- function(updated_data, file_path, file_type) {
  message(glue("Writing new data to log file: {file_path}"))
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
