setwd("E:/Projects_FY24-25/HAWQS_LSTM/R_Projects/Calibrated_HAWQS_to_LSTM_Inputs/")
source("load_libraries.R")
source("create_observed_rch_file.R")
source("create_SUFI2_bat_files.R")
source("update_swat_files.R")
source("copy_required_files.R")
source("get_rch_ids.R")
source("create_swatcup_folders.R")
source("get_HAWQS_area.R")
source("write_cio.R")
source("run_swat.R")
source("run_hawqs_static_attributes.R")
source("run_hawqs_dynamic_attributes.R")
source("run_hawqs_to_lstm.R")
source("run_df_to_nc.R")
source("update_swat_from_SUFI_OUT.R")
#A. Set up file locations#######################################################
main_directory <- "E:/Projects_FY24-25/HAWQS_LSTM/HAWQS_Projects/CAMELS_HAWQS_PRISM"  #Where all your inputs/outputs are saved
hawqs_projects_directory <- paste0(main_directory,"/HAWQS_CAMELS_531") #Where zipped HAWQS files exist
zipped_hawqs_projects <- list.files(hawqs_projects_directory,full.names = TRUE)
usgs_huc_ids <- read_csv(paste0(main_directory,"/gages_huc14_hawqs.txt"))%>%
                  rename(usgs_id = "STAID", huc_id = "Ending_Subbasin")   #identify what is what
inputs_folder_swatcup <- "E:/Projects_FY24-25/HAWQS_LSTM/R_Projects/Calibrated_HAWQS_to_LSTM_Inputs/Inputs"
calibration_SUFI_OUT_folder <- "E:/Projects_FY24-25/HAWQS_LSTM/HAWQS_Projects/CAMELS_HAWQS_PRISM/swatcup_calibration_files/SUFI2_OUT_LSTM_R11"

#B. Create required folders if not already existing#############################
ifelse(!dir.exists(file.path(paste0(main_directory,"/temp"))), dir.create(file.path(paste0(main_directory,"/temp"))), FALSE) #temp folder
ifelse(!dir.exists(file.path(paste0(main_directory,"/calibrated_hawqs_lstm_files_R11"))), dir.create(file.path(paste0(main_directory,"/calibrated_hawqs_lstm_files_R11"))), FALSE) #main output folder
ifelse(!dir.exists(file.path(paste0(main_directory,"/calibrated_hawqs_lstm_files_R11/attributes"))), dir.create(file.path(paste0(main_directory,"/calibrated_hawqs_lstm_files_R11/attributes"))), FALSE) #static files folder
ifelse(!dir.exists(file.path(paste0(main_directory,"/calibrated_hawqs_lstm_files_R11/time_series"))), dir.create(file.path(paste0(main_directory,"/calibrated_hawqs_lstm_files_R11/time_series"))), FALSE) #dynamic files folder

#D. Calibration#################################################################
calibrated_files <- list.files(file.path(main_directory,"swatcup_calibration_files/SUFI2_OUT_LSTM_R11"))
completed_files <- list.files("E:/Projects_FY24-25/HAWQS_LSTM/HAWQS_Projects/CAMELS_HAWQS_PRISM/calibrated_hawqs_lstm_files_R11/attributes")
usgs_huc_ids <- usgs_huc_ids%>%filter(as.numeric(usgs_id) %in% as.numeric(calibrated_files))
write.table(as.numeric(usgs_huc_ids$usgs_id), paste0(main_directory,"/calibrated_hawqs_lstm_files_R11/","basin_ids.csv"), row.names = FALSE, col.names = FALSE)
usgs_huc_ids <- usgs_huc_ids%>%filter(!as.numeric(usgs_id) %in% as.numeric(file_path_sans_ext(completed_files)))

zipped_hawqs_projects <- list.files(hawqs_projects_directory,full.names = TRUE)
zipped_hawqs_projects <- zipped_hawqs_projects[str_extract(zipped_hawqs_projects,"\\d{14}") %in% usgs_huc_ids$huc_id]

process_data <- function(i){
  for(i in 1:6){
  hawqs_project <- zipped_hawqs_projects[i]
  archive_extract(hawqs_project, dir = paste0(main_directory,"/temp2/",file_path_sans_ext(basename(hawqs_project))), files = NULL)     
  project <- list.files(paste0(main_directory,"/temp2/",file_path_sans_ext(basename(hawqs_project))),full.names = TRUE)
  
  huc_id <- str_extract(hawqs_project, "\\d{14}")  #14digit huc number extract
  usgs_id <- usgs_huc_ids$usgs_id[usgs_huc_ids$huc_id == huc_id]
  basin_id <- as.numeric(usgs_id)  #No appended zeros
  update_swat_files(project,inputs_folder_swatcup,huc_id = huc_id,
              start_date = "1983-01-01", end_date = "2020-12-31", NYSKIP = 2,IPRINT = 1,  #Start date is NOT including warm up
              par_inf_file = file.path(inputs_folder_swatcup,"par_inf.txt"), 
              best_par_file = paste0(calibration_SUFI_OUT_folder,"/",basin_id,"/best_par.txt"))
  
  hawqs_to_lstm(project = file.path(project,"swatcup.Sufi2.SwatCup"), main_directory,basin_id,huc_id,start_date = "1983-01-01",end_date = "2020-12-31",obs_flow = TRUE, usgs_id)
  
  unlink(project,recursive = TRUE)
  print(i)
  }
}

safe_process_data <- safely(process_data)

plan(multisession, workers = 50)  #I tried with 90 but it crashes. Keep it low. 
options(future.globals.maxSize = 2 * 1024 * 1024^2)
output <- future_pmap(
  list(1:nrow(usgs_huc_ids)),
  safe_process_data
)
