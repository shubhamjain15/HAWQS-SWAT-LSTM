create_observed_rch_file <- function(StationID,rch_id,start_date, end_date,project,hawqs_area, obj_func = 9, min_value = 0.5){
  flows <- readNWISdv(StationID, parameterCd = "00060", startDate = start_date, endDate = end_date)
  flows <- renameNWISColumns(flows)
  
  #DAR
  usgs_area <- readNWISsite(StationID)$drain_area_va * 2.58999
  hawqs_area <- get_hawqs_area(project)
  if(!is.na(usgs_area) & !is.na(hawqs_area)){
    DAR <- hawqs_area/usgs_area
  } else {
    DAR <- 1
  }
  
  flows$Flow <- round(flows$Flow * 0.0283 * DAR,3)
  flows <- flows%>%
              filter(Flow >= 0 & !is.na(Flow))
  flows$seq <- as.numeric(flows$Date - as.Date(start_date) + 1)
  flows$id <- paste0(year(flows$Date), "_", str_pad(yday(flows$Date),3, pad = "0")) 
  
  
  var <- paste0("Flow_out_", rch_id)
  obs_num <- nrow(flows)
  
  obs_rch1 <- cbind(flows$seq, flows$id, flows$Flow)
  obs_rch <- as.data.frame(merge(rbind(1, "", var, obs_num, ""), obs_rch1, by = 0, all=T, sort = F, row.names = NULL))
  obs_rch[is.na(obs_rch)] <- ""
  obs_rch$Row.names<- NULL
  colnames(obs_rch) <- NULL
  write.table(obs_rch, file.path(project,"swatcup.Sufi2.SwatCup/SUFI2.IN/Observed_rch.txt"), row.names = F, col.names = F, quote = F)
  
  #observed.txt
  obj_func= obj_func
  min_value<- min_value   #min value of objective function threshold for the behavioral solutions
  MNS <- 1          #if objective function is 11=MNS (modified NS),indicate the power, p.
  stat <- rbind(1,-1,-1,1,1,10)  
  stat_inf <- rbind(1, obj_func, min_value, MNS, "")
  obs_txt <- merge(rbind(stat_inf, var, stat, obs_num, ""), obs_rch1, by = 0, all=T, sort = F, row.names = NULL)
  obs_txt[is.na(obs_txt)] <- ""
  obs_txt$Row.names<- NULL
  colnames(obs_txt) <- NULL
  write.table(obs_txt, file.path(project,"swatcup.Sufi2.SwatCup/SUFI2.IN/Observed.txt"), row.names = F, col.names = F, quote = F)
}
