source("googlesheet_helpers.R")
source("file_system_helpers.R")
source("excel_sheet_helpers.R")

create_new_station <- function(station_name, waterbody, county, latitude, longitude, note) {
  # TODO: Create station folder
  # Update Area Info
  new_area_info_row <- c(station_name, waterbody, county, latitude, longitude, note)
  new_area_info_row_df <- as.data.frame(t(new_area_info_row), stringsAsFactors=FALSE)
  sheet_append(string_tracking_sheet, new_area_info_row_df, sheet = "Area Info")
}