downloadFlow <- function(usgs_id,start_date = "1981-01-01",end_date = "2020-12-31") {
  flow <- readNWISdv(usgs_id,startDate = start_date,
                    endDate = end_date,
                    parameterCd = "00060")
  flow <- renameNWISColumns(flow)
  flow <- flow%>%select(c("Date","Flow"))%>%rename(date = "Date",Q_obs = "Flow")
  
  #Convert to mm/day
  da <- readNWISsite(usgs_id)
  da <- da$drain_area_va * 2.58999 * 10^6  #square meter
  flow$Q_obs <- flow$Q_obs * 0.028317/da   #meter/s
  flow$Q_obs <- flow$Q_obs * 1000 * 86400    #mm/day
  
  full_dates <- data.frame(date = seq.Date(as.Date(start_date), as.Date(end_date), by = "day"))
  flow <- full_dates %>% left_join(flow, by = "date")
  
  return(list(flow = flow, DRAIN_SQKM = da/10^6))
}

hawqs_to_lstm <- function(project, main_directory,basin_id,huc_id,start_date = "2006-01-01",end_date = "2020-12-31",obs_flow = TRUE, usgs_id){
  
  setwd(project)
  shell("swat_64rel.exe")
  
  #Get static chars
  static_chars <- read_hawqs_static_chars(project,huc_id)%>%
                      mutate(huc_project = basin_id)%>%  #lazy way to change huc number column to basinid
                      rename(basin_id = "huc_project")
  
  #Get dynamic chars
  dynamic_chars <- read_hawqs_dynamic_chars(huc_id,project,start_date,end_date)
  
  #Observed flow and DA
  DRAIN_SQKM <- NA
  if(obs_flow == TRUE){
    observed <- downloadFlow(usgs_id,start_date,end_date)
    flow <- observed[["flow"]]
    DRAIN_SQKM <- observed[["DRAIN_SQKM"]]
    dynamic_chars <- dynamic_chars%>%left_join(flow, by = "date")
  }
  
  #mm/day
  dynamic_chars$FLOW_OUT <- dynamic_chars$FLOW_OUTcms *10^9 * 86400 / (as.numeric(static_chars$HAWQS_area) * 10^12)
  dynamic_chars <- dynamic_chars%>%select(-"FLOW_OUTcms")
  
  #Choose DA  for static table (HAWQS or USGS)
  if(is.na(DRAIN_SQKM)){
    static_chars$Area <- static_chars$HAWQS_area
  } else{
    static_chars$Area <- DRAIN_SQKM
    static_chars <- static_chars%>%select(-"HAWQS_area")
  }
  
  #Write files
  write_csv(static_chars,file = paste0(main_directory,"/calibrated_hawqs_lstm_files_R11/attributes/",basin_id,".csv"))
  df_to_nc(dynamic_chars, basin_id, file_path = paste0(main_directory,"/calibrated_hawqs_lstm_files_R11/time_series/"))
  
}
