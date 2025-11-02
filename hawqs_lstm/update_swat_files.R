update_swat_files <- function(project,inputs_folder,par_inf_file, best_par_file,huc_id,start_date, end_date, NYSKIP = 2,IPRINT = 1){
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
  
  create_SUFI2_bat_files(project,rch_id,total_rch,begin = as.numeric(year(start_date)), end = as.numeric(year(end_date)), time_step = 1)
  
  update_swat_from_SUFI_OUT(project,par_inf_file,best_par_file,HUCID = huc_id)

}
