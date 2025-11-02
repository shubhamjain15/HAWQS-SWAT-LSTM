read_swat_output <- function(folder, output_type, output_file, starting_date, ending_date){
  
  ##Reading text files and writing column names for .rch, .sub and .hru SWAT output files
  if (output_file == "rch"){
    ##Reading in the output.rch data
    output_raw <- read_table2(paste0(folder, "/", "output.", output_file), 
                              skip = 9, 
                              col_types = cols(X3 = col_skip(), 
                                               X4 = col_character()), 
                              col_names = FALSE) %>%
      mutate(X4 = suppressWarnings(as.numeric(X4)))
    ## column names for output.rch file
    col_names <- c("FILE","RCH","MON","AREAkm2","FLOW_INcms","FLOW_OUTcms","EVAPcms","TLOSScms",
                   "SED_INtons","SED_OUTtons","SEDCONCmg_kg","ORGN_INkg","ORGN_OUTkg","ORGP_INkg","ORGP_OUTkg",
                   "NO3_INkg","NO3_OUTkg","NH4_INkg","NH4_OUTkg","NO2_INkg","NO2_OUTkg","MINP_INkg","MINP_OUTkg",
                   "CHLA_INkg","CHLA_OUTkg","CBOD_INkg","CBOD_OUTkg","DISOX_INkg","DISOX_OUTkg","SOLPST_INmg",
                   "SOLPST_OUTmg","SORPST_INmg","SORPST_OUTmg","REACTPSTmg","VOLPSTmg","SETTLPSTmg","RESUSP_PSTmg",
                   "DIFFUSEPSTmg","REACBEDPSTmg","BURYPSTmg","BED_PSTmg","BACTP_OUTct","BACTLP_OUTct","CMETAL_1kg",
                   "CMETAL_2kg","CMETAL_3kg","TOTNkg","TOTPkg","NO3_mg_l","WTMPdegc", "RCH2")
    
  } else if(output_file == "sub"){
    ##Reading in the output.sub data
    output_raw <- read_table2(paste0(folder, "/", "output.", output_file), 
                              skip = 9, 
                              col_types = cols(X3 = col_skip(), 
                                               X4 = col_character()), 
                              col_names = FALSE)%>%
      separate(3, into = c('M', 'A'), sep = "\\.") %>% ##One column have to be separated as is jointed
      mutate(A = as.numeric(paste0(".", A)),
             M = suppressWarnings(as.numeric(M)))
    ##Column names for output.sub file
    col_names <- c("BIGSUB","SUB", "MON", "AREAkm2", "PRECIPmm", "SNOMELTmm", "PETmm", "ETmm", "SWmm",
                   "PERCmm", "SURQmm", "GW_Qmm", "WYLDmm", "SYLDt/ha", "ORGNkg/ha", "ORGPkg/ha",
                   "NSURQkg/ha", "SOLPkg/ha", "SEDPkg/ha", "LATQmm", "LATNO3kg/ha", "GWNO3kg/ha",
                   "CHOLAmic/L", "CBODUmg/L", "DOXQmg/L", "TNO3kg/ha", "RCH")
    
  } else if(output_file == "hru"){
    ##Reading in the output.hru data
    output_raw <- read_table2(paste0(folder, "/", "output.", output_file), 
                              skip = 9, 
                              col_types = cols(X5 = col_skip(), 
                                               X6 = col_character()), 
                              col_names = FALSE) %>%
      separate(5, into = c('M', 'A'), sep = "\\.") %>% 
      mutate(A = as.numeric(paste0(".", A)),
             M = suppressWarnings(as.numeric(M)))
    ##Column names for output.hru file
    col_names <- c("LULC","HRU","GIS","SUB","MON","AREAkm2","SNOFALLmm","DAILYCN","TMP_MXdgC","TMP_MNdgC","SOLARMJm2")
  } else {
    stop("Output file is not supported!!! Only 'rch', 'sub', 'hru' are available!!!" )
  }
  ##Assigning column names
  colnames(output_raw) <- col_names
  
  ##Generating daily time for all modeled period
  date_daily <- data.frame(date = rep(seq(from = as.Date(starting_date), to = as.Date(ending_date), by = 1),
                                      max(unique(output_raw[, 2])))) %>%
    arrange(date) %>%
    mutate(Year = year(date),
           Month = month(date),
           Day = mday(date))
  
  ##Dealing with output of different timescale
  if (output_type == 1){ ##For daily output
    output <- bind_cols(date_daily, output_raw) ##Binding dates to the dataframe
    
  } else if (output_type == 2){ ##For yearly output
    output <- bind_cols(filter(date_daily, Month == 1 & Day == 1), filter(output_raw, MON > 1900))
    
  } else if (output_type == 0){ ##For monthly output
    output <- bind_cols(filter(date_daily, Day == 15), filter(output_raw, MON <= 12))
    
  } else {
    stop("Output type could only be a numbers 0 for monthly, 1 for daily and 2 for yearly output!!!")
  }
  
  ##Returning result table
  return(output)
}

read_hawqs_dynamic_chars <- function(huc_number,project,start_date = "1983-01-01",end_date = "2020-12-31"){
  txtinout_folder <- project
  #3b_i. HRU
  hru_file <- read_swat_output(txtinout_folder, output_type = 1, output_file = "hru", starting_date = start_date, ending_date = end_date)
  df <- hru_file %>%
    group_by(date) %>%
    summarize(SNOWFALLmm = sum(AREAkm2 *SNOFALLmm) / sum(AREAkm2),
              DAILYCN = sum(AREAkm2 *DAILYCN) / sum(AREAkm2),
              TMP_MXdgC = sum(AREAkm2 *TMP_MXdgC) / sum(AREAkm2),
              TMP_MNdgC = sum(AREAkm2 *TMP_MNdgC) / sum(AREAkm2),
              SOLAR = sum(AREAkm2 * SOLARMJm2) / sum(AREAkm2))
  
  #3b_ii. SUB
  sub_file <- read_swat_output(txtinout_folder, output_type = 1, output_file = "sub", starting_date = start_date, ending_date = end_date)
  df2 <- sub_file %>%
    group_by(date) %>%
    summarize(PRCP = sum(AREAkm2 *PRECIPmm) / sum(AREAkm2),
              SNOWMELTmm = sum(AREAkm2 *SNOMELTmm) / sum(AREAkm2),
              #PETmm = sum(AREAkm2 *PETmm) / sum(AREAkm2),
              ETmm = sum(AREAkm2 *ETmm) / sum(AREAkm2),
              SWmm = sum(AREAkm2 *SWmm) / sum(AREAkm2),
              PERCmm = sum(AREAkm2 *PERCmm) / sum(AREAkm2),
              SURQmm = sum(AREAkm2 *SURQmm) / sum(AREAkm2),
              GW_Qmm = sum(AREAkm2 *GW_Qmm) / sum(AREAkm2))
              #WYLDmm = sum(AREAkm2 *WYLDmm) / sum(AREAkm2))
  
  
  #3b_iii. Get outlet HUC reach number
  pdat_files <- files <- list.files(path = txtinout_folder, pattern = "^[0-9]+p\\.dat$", full.names = TRUE)
  for(i in pdat_files){
    first_line <- readLines(i, n = 1)
    subbasin_number <-str_extract(first_line, "Subbasin:(\\d+)")
    subbasin_number <- as.numeric(str_extract(subbasin_number, "\\d+"))
    huc <- str_extract(first_line, "\\((\\d{14})\\)")
    huc <- gsub("\\(|\\)", "", huc)
    if(huc ==  huc_number){
      outlet_rch <- subbasin_number
    }
  }
  
  #3b_iv. RCH
  rch_file <- read_swat_output(txtinout_folder, output_type = 1, output_file = "rch", starting_date = start_date, ending_date = end_date)
  rch_file <- rch_file%>%
    filter(RCH == outlet_rch)%>%
    select(c("date","FLOW_OUTcms"))
  out <- df2%>%
    left_join(df,by = "date")%>%
    left_join(rch_file,by = "date")
  return(out)
}
