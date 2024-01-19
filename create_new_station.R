source("googlesheet_helpers.R")
source("file_system_helpers.R")
source("excel_sheet_helpers.R")

create_new_station <- function(station_name, waterbody, county, latitude, longitude, note) {
  # Create station folder
  create_station_folder(station_folders_path, station_name)
  # Update Area Info
  new_area_info_row <- c(station_name, waterbody, county, latitude, longitude, note)
  new_area_info_row_df <- as.data.frame(t(new_area_info_row), stringsAsFactors=FALSE)
  sheet_append(string_tracking_sheet, new_area_info_row_df, sheet = "Area Info")
}

create_new_station(
  station_name = "New Test Station",
  waterbody = "Test Waterbody",
  county = "Test County",
  latitude = -45.28888,
  longitude = -61.6475,
  note = "this is a test new station"
)
