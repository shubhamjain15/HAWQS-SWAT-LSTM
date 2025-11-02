create_SUFI2_bat_files <- function(project,rch_id,total_rch,begin, end, time_step = 1){
  SUFI2_extract <- rbind("@echo","SUFI2_extract_rch.exe")
  write.table(SUFI2_extract, file.path(project, "swatcup.Sufi2.SwatCup/SUFI2_extract.bat"), row.names = F, col.names = F, quote = F)
  
  SUFI2_Pre <- rbind("@echo off",'Pushd "%~dp0"', 'SUFI2_LH_sample.exe', "popd")
  write.table(SUFI2_Pre, file.path(project, "swatcup.Sufi2.SwatCup/SUFI2_Pre.bat"), row.names = F, col.names = F, quote = F)
  
  SUFI2_Run <- rbind("@echo off",'Pushd "%~dp0"', "sufi2_execute.exe", "popd")
  write.table(SUFI2_Run, file.path(project, "swatcup.Sufi2.SwatCup/SUFI2_Run.bat"), row.names = F, col.names = F, quote = F)
  
  SUFI2_Post <- rbind("@echo off",'Pushd "%~dp0"', "SUFI2_goal_fn.exe", "SUFI2_new_pars.exe", "SUFI2_95ppu.exe", "SUFI2_95ppu_beh.exe", "popd")
  write.table(SUFI2_Post, file.path(project, "swatcup.Sufi2.SwatCup/SUFI2_Post.bat"), row.names = F, col.names = F, quote = F)
  
  row1 <- paste0("output.rch")
  SUFI2_extract_rch <-rbind(row1, 1, 7, "", total_rch, "", 1, rch_id, "", begin, end,"", time_step)
  write.table(SUFI2_extract_rch, file.path(project, "swatcup.Sufi2.SwatCup/SUFI2_extract_rch.def"), row.names = F, col.names = F, quote = F)
  
  var <- paste0("Flow_out_", rch_id)
  write.table(paste0(var, ".txt"), file.path(project, "swatcup.Sufi2.SwatCup/SUFI2.IN/var_file_rch.txt"), row.names = F, col.names = F, quote = F)
  write.table(paste0(var, ".txt"), file.path(project, "swatcup.Sufi2.SwatCup/SUFI2.IN/var_file_name.txt"), row.names = F, col.names = F, quote = F)
}
