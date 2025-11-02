create_swatcup_folders <- function(project){
  #folder names
  swatcup_main_folder <- file.path(project,"swatcup.Sufi2.SwatCup")
  swatcup_backup_folder <- file.path(project,"swatcup.Sufi2.SwatCup/Backup")
  #create calibration folder and backup folder
  dir.create(swatcup_main_folder)
  dir.create(swatcup_backup_folder)
  dir.create(file.path(swatcup_main_folder,"Echo"))
  dir.create(file.path(swatcup_main_folder,"SUFI2.IN"))
  dir.create(file.path(swatcup_main_folder,"SUFI2.OUT"))
}
