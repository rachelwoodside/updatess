source("apply_deployment_change.R")

# TODO: Should this erase previously processed data by default? -- use a boolean param?
# TODO: Look into R package for error handling! Use warnings instead of errors too
# TODO: Is try-catch an option to manage continuation of execution if one change encounters an error and fails (e.g. catch block returns FALSE?)
# TODO: Build row in a different way to allow for parallelizing operations? 
#(Contingent on identifying information like station name and deployment date not changing too soon...?)
#(Also there are enough dependencies, particularly on directory structure that this is unlikely to be worthwhile)

# SET UP DIRECTORIES AS GLOBAL VARIABLES ------------------------------------
# Currently assumes opening as an R project, but should the directory be set just in case?
# Adding the commented code would break things if the template is in a different location from the R project
# (But then it would be broken anyways - it still wouldn't source the helper function files properly)
#template_home_path <- dirname(rstudioapi::getSourceEditorContext()$path)
#setwd(template_home_path)
#station_folders_path <- "Y:/coastal_monitoring_program/data_branches/water_quality/station_folders"
stations_folder_path <- "R:/program_documents/cmp_hiring/intern/2023_rachel/projects/cmp/deployment_change_tracking/deployment_change_code/fake_station_folders"
#string_tracking_sheet <- drive_get("STRING TRACKING")
string_tracking_sheet <- drive_get("TestSheetForCodeModification")
change_tracking_sheet <- drive_get("Deployment Change Tracking")
#config_table_folder_path <- "R:/tracking_sheets"
config_table_folder_path <- "R:/program_documents/cmp_hiring/intern/2023_rachel/projects/cmp/deployment_change_tracking/deployment_change_code/fake_config_tables"

# MODIFY HERE!! -------------------------------------------------------------

apply_deployment_change(
  stations_folder_path = stations_folder_path,
  station_name = "Birchy Head",
  depl_date = "2018-02-20", 
  field_to_change = "station name", 
  old_value = "Birchy Head",
  new_value = "Birchy Head 1", 
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


