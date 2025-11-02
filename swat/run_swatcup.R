run_swatcup <- function(project,inputs_folder,StationID,huc_id,start_date, end_date,start_date_obs, end_date_obs,obj_func = 5, min_value = 0.5, NYSKIP = 2,IPRINT = 1){
  #Outlet and total RCH
  rch_ids <- get_rch_ids(project,huc_id)
  total_rch <- nrow(rch_ids)
  rch_id <- rch_ids$subbasin[rch_ids$huc_id == rch_ids$huc_subbasin]
  
  NBYR <- round(as.numeric(interval(start_date, end_date), "years"),2) + NYSKIP
  IYR <- year(start_date) - NYSKIP
  write_cio(project,NBYR = NBYR, IYR = IYR, NYSKIP = NYSKIP,IPRINT = IPRINT)
  shell.exec2(file.path(project,"TxtInOut/swat_64rel.exe"))
  while(!file.exists(file.path(project,paste0("TxtInOut/output.rch")))){
    Sys.sleep(10)
  }
  
  ndays <- (as.duration(interval(start_date, end_date)) / ddays(1))+1
  while(!length(read_lines(file.path(project,"TxtInOut/output.rch"))) >= ndays*total_rch+5){
    Sys.sleep(10)
  }
  create_swatcup_folders(project)
  copy_required_files(project, inputs_folder)
  
  create_observed_rch_file(StationID,rch_id,start_date,end_date, start_date_obs, end_date_obs,project, obj_func = obj_func, min_value = min_value)
  create_SUFI2_bat_files(project,rch_id,total_rch,begin = as.numeric(year(start_date)), end = as.numeric(year(end_date)), time_step = 1)
  
  pre_loc <- file.path(project,"swatcup.Sufi2.SwatCup/SUFI2_Pre.bat")
  
  system2(pre_loc, input = "Y\n", stdout = TRUE, stderr = TRUE)
  shell.exec2(file.path(project, "swatcup.Sufi2.SwatCup/SUFI2_Run.bat"))
  
  while (!file.exists(file.path(project,paste0("swatcup.Sufi2.SwatCup/SUFI2.OUT/Flow_out_",rch_id,".txt")))) {
     Sys.sleep(10)
  }
  obs_num <- as.numeric(read_lines(file.path(project,"swatcup.Sufi2.SwatCup/SUFI2.IN/Observed_rch.txt"))[4])
  num_sim <- as.numeric(gsub("[^0-9]", "",read_lines(file.path(project,"swatcup.Sufi2.SwatCup/SUFI2.IN/par_inf.txt"))[2]))
  while(!length(read.delim(file.path(project,paste0("swatcup.Sufi2.SwatCup/SUFI2.OUT/Flow_out_",rch_id,".txt")), header = F)$V1) >= num_sim*obs_num){
    Sys.sleep(10)
  }
  
  shell.exec2(file.path(project, "swatcup.Sufi2.SwatCup/SUFI2_Post.bat"))
  while(!file.exists(file.path(project,paste0("swatcup.Sufi2.SwatCup/SUFI2.OUT/summary_stat.txt")))){
    Sys.sleep(5)
  }
  Sys.sleep(120)
}
