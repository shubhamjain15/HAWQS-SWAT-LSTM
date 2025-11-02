setwd("E:/Projects_FY24-25/HAWQS_LSTM/R_Projects/HAWQS_Daily_Calibration/")
source("load_libraries.R")
source("create_observed_rch_file.R")
source("create_SUFI2_bat_files.R")
source("run_swatcup.R")
source("copy_required_files.R")
source("get_rch_ids.R")
source("create_swatcup_folders.R")
source("get_HAWQS_area.R")
source("write_cio.R")
#A. Set up file locations#######################################################
main_directory <- "E:/Projects_FY24-25/HAWQS_LSTM/HAWQS_Projects/CAMELS_HAWQS_PRISM"  #Where all your inputs/outputs are saved
hawqs_projects_directory <- paste0(main_directory,"/HAWQS_CAMELS_531") #Where zipped HAWQS files exist
zipped_hawqs_projects <- list.files(hawqs_projects_directory,full.names = TRUE)
usgs_huc_ids <- read_csv(paste0(main_directory,"/gages_huc14_hawqs.txt"))%>%
                  rename(usgs_id = "STAID", huc_id = "Ending_Subbasin")   #identify what is what
inputs_folder_swatcup <- "E:/Projects_FY24-25/HAWQS_LSTM/R_Projects/Calibrated_HAWQS_to_LSTM_Inputs/Inputs" 
#B. Create required folders if not already existing#############################
ifelse(!dir.exists(file.path(paste0(main_directory,"/temp"))), dir.create(file.path(paste0(main_directory,"/temp"))), FALSE) #temp folder
ifelse(!dir.exists(file.path(paste0(main_directory,"/swatcup_calibration_files"))), dir.create(file.path(paste0(main_directory,"/swatcup_calibration_files"))), FALSE) #main output folder
ifelse(!dir.exists(file.path(paste0(main_directory,"/swatcup_calibration_files/SUFI2_OUT_10years"))), dir.create(file.path(paste0(main_directory,"/swatcup_calibration_files/SUFI2_OUT_10years"))), FALSE) #static files folder

#D. Calibration#################################################################
completed <- as.numeric(list.files("E:/Projects_FY24-25/HAWQS_LSTM/HAWQS_Projects/CAMELS_HAWQS_PRISM/swatcup_calibration_files/SUFI2_OUT_10years"))
usgs_huc_ids <- usgs_huc_ids%>%filter(!as.numeric(usgs_id)%in%completed)
zipped_hawqs_projects <- zipped_hawqs_projects[str_extract(zipped_hawqs_projects, "\\d{14}") %in% usgs_huc_ids$huc_id]

process_data <- function(i){
  hawqs_project <- zipped_hawqs_projects[i]
  archive_extract(hawqs_project, dir = paste0(main_directory,"/temp/",file_path_sans_ext(basename(hawqs_project))), files = NULL)     
  project <- list.files(paste0(main_directory,"/temp/",file_path_sans_ext(basename(hawqs_project))),full.names = TRUE)
  
  huc_id <- str_extract(hawqs_project, "\\d{14}")  #14digit huc number extract
  usgs_id <- usgs_huc_ids$usgs_id[usgs_huc_ids$huc_id == huc_id]
  basin_id <- as.numeric(usgs_id)  #No appended zeros
  run_swatcup(project,inputs_folder_swatcup,StationID = usgs_id,huc_id = huc_id,
              start_date = "1999-01-01", end_date = "2008-12-31",
              start_date_obs = "1999-10-01", end_date_obs = "2008-09-30",
              obj_func = 5, min_value = 0.5,
              NYSKIP = 2,IPRINT = 1)  #Start date is NOT including warm up
  ifelse(!dir.exists(file.path(paste0(main_directory,"/swatcup_calibration_files/SUFI2_OUT_10years/",basin_id))), dir.create(file.path(paste0(main_directory,"/swatcup_calibration_files/SUFI2_OUT_10years/",basin_id))), FALSE) #static files folder
  file.copy(from = list.files(file.path(project,"swatcup.Sufi2.SwatCup/SUFI2.OUT/"), full.names = TRUE), 
            to = paste0(main_directory,"/swatcup_calibration_files/SUFI2_OUT_10years/",basin_id,"/"),overwrite = TRUE)
  unlink(project,recursive = TRUE)
}

safe_process_data <- safely(process_data)

plan(multisession, workers = 50)  #I tried with 90 but it crashes. Keep it low. 
options(future.globals.maxSize = 2 * 1024 * 1024^2)
output <- future_pmap(
  list(1:50),
  safe_process_data
)
