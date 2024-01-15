source("create_new_station.R")
source("apply_deployment_change.R")

# TODO: Should this erase previously processed data by default? -- use a boolean param?
# TODO: Look into R package for error handling! Use warnings instead of errors too
# TODO: Is try-catch an option to manage continuation of execution if one change encounters an error and fails (e.g. catch block returns FALSE?)
# TODO: Build row in a different way to allow for parallelizing operations? 
#(Contingent on identifying information like station name and deployment date not changing too soon...?)

# CONSTANTS -----------------------------------------------------------------
#station_folder_path <- "Y:/coastal_monitoring_program/data_branches/water_quality/station_folders"
station_folder_path <- "R:/program_documents/cmp_hiring/intern/2023_rachel/projects/cmp/deployment_change_tracking/deployment_change_code/updatess/fake_station_folders"
#string_tracking_sheet <- drive_get("STRING TRACKING")
string_tracking_sheet <- drive_get("TestSheetForCodeModification")
change_tracking_sheet <- drive_get("Deployment Change Tracking")
config_table_folder_path <- "R:/tracking_sheets"

# MODIFY HERE!! -------------------------------------------------------------

apply_deployment_change(
  station_name = "Birchy Head",
  depl_date = "2018-02-20", 
  field_to_change = "station name", 
  old_value = "Birchy Head",
  new_value = "New Name", 
  rationale = "Conflict with NSDFA station name",
  note = "this is a test station name change")

apply_deployment_change(
  station_name = "Birchy Head",
  depl_date = "2018-02-20", 
  field_to_change = "deployment date", 
  old_value = "2018-02-20",
  new_value = "2018-04-10", 
  rationale = "Reflect patterns in data",
  note = "this is a test deployment date change"
)

create_new_station(
  station_name = "New Test Station",
  waterbody = "Test Waterbody",
  county = "Test County",
  latitude = -45.28888,
  longitude = -61.6475,
  note = "this is a test new station"
)

