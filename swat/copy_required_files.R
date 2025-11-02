copy_required_files <- function(project,inputs_folder){
  txtinout_folder <- file.path(project,"TxtInOut")
  swatcup_main_folder <- file.path(project,"swatcup.Sufi2.SwatCup")
  swatcup_backup_folder <- file.path(project,"swatcup.Sufi2.SwatCup/Backup")
  base_folder <- file.path(inputs_folder,"Base_folder")
  
  print("Copying SWATCUP files")
  #Copy Txtinout files
  file.copy(from = file.path(txtinout_folder, list.files(txtinout_folder)), to = swatcup_main_folder,overwrite = TRUE)
  file.copy(from = file.path(txtinout_folder, list.files(txtinout_folder)), to = swatcup_backup_folder,overwrite = TRUE)
  
  #Copy base folder
  file.copy(from = file.path(base_folder,list.files(base_folder)), to = swatcup_main_folder,overwrite = FALSE)
  
  #Copy par_inf and swEdit
  file.copy(from = file.path(inputs_folder,"par_inf.txt"), to = file.path(swatcup_main_folder,"SUFI2.IN/"), overwrite = TRUE)
  file.copy(from = file.path(inputs_folder,"SUFI2_swEdit.def"), to = swatcup_main_folder, overwrite = TRUE)
}
