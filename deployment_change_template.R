source("apply_deployment_change.R")

# TODO: Should this erase previously processed data by default?
# could maybe use a boolean param?
# TODO: Look into R package for error handling! 
#Use warnings instead of errors too
# TODO: Is try-catch an option to manage continuation of execution 
#if one change encounters an error and fails (e.g. catch block returns FALSE?)
# TODO: Build row in a different way to allow for parallelizing operations? 
#(Contingent on identifying information like station name and deployment date 
#not changing too soon...?)
#(Also there are enough dependencies, particularly on directory structure 
#that this is unlikely to be worthwhile)

# SET UP DIRECTORIES AS GLOBAL VARIABLES ------------------------------------
# GLOBAL VARIABLES FOR TESTING
stations_folder_path <- "R:/program_documents/cmp_hiring/intern/2023_rachel/projects/cmp/deployment_change_tracking/deployment_change_code/fake_station_folders"
config_table_folder_path <- "R:/program_documents/cmp_hiring/intern/2023_rachel/projects/cmp/deployment_change_tracking/deployment_change_code/fake_config_tables"
string_tracking_sheet <- drive_get("TestSheetForCodeModification")
change_tracking_sheet <- drive_get("Deployment Change Tracking")

# GLOBAL VARIABLES FOR IMPLEMENTING CHANGES
#station_folders_path <- "Y:/coastal_monitoring_program/data_branches/water_quality/station_folders"
#config_table_folder_path <- "R:/tracking_sheets"
#string_tracking_sheet <- drive_get("STRING TRACKING")
#change_tracking_sheet <- drive_get("Deployment Change Tracking")

# MODIFY HERE!! -------------------------------------------------------------

apply_deployment_change(stations_folder_path = stations_folder_path,
                        string_tracking_sheet = string_tracking_sheet,
                        station_name = "Birchy Head",
                        depl_date = "2018-02-20", 
                        field_to_change = "station name", 
                        old_value = "Birchy Head",
                        new_value = "Birchy Head 1", 
                        rationale = "Conflict with NSDFA station name",
                        note = "this is a test station name change")

apply_deployment_change(stations_folder_path = stations_folder_path,
                        string_tracking_sheet = string_tracking_sheet,
                        station_name = "Birchy Head",
                        depl_date = "2018-02-20", 
                        field_to_change = "deployment date", 
                        old_value = "2018-02-20",
                        new_value = "2018-04-10", 
                        rationale = "Reflect patterns in data",
                        note = "this is a test deployment date change")

create_new_station(stations_folder_path = stations_folder_path,
                   ss = string_tracking_sheet,
                   station_name = "Birchy Head 1",
                   waterbody = "St. Margarets Bay",
                   county = "Lunenburg",
                   latitude = 44.570026,
                   longitude = -64.03449956,
                   note = "")


