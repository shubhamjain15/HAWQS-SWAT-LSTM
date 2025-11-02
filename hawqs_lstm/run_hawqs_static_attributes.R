#1. Get subbasin IDs for HUCs from p.dat files in txtinout#####################  
hawqs_get_subbasin_numbers <- function(txtinout_folder,huc_project){
  file_pattern <- "^\\d+p\\.dat$"
  file_list <- list.files(path = txtinout_folder, pattern = file_pattern, full.names = TRUE)
  out <- data.frame()
  for(i in file_list){
    df <- readLines(i)[1]
    subbasin_number <- strsplit(regmatches(df, regexpr("Subbasin:(\\d+)", df))[[1]],":")[[1]][2]
    huc_number <- gsub("[()]","",regmatches(df, regexpr("\\(\\d+\\)", df))[[1]])
    df <- data.frame(HUC_Project = rep(huc_project,length(huc_number)),HUC_Subbasin = huc_number, Subbasin = as.numeric(subbasin_number))
    out <- rbind(out,df)
    out <- out[order(out$Subbasin),]
    rownames(out) <- NULL
  }
  return(out)
}

#2. get water cycle component data from output.std##############################
hawqs_get_watercycle_comp <- function(txtinout_folder,huc_project){
  output <- data.frame(huc_project = huc_project)
  out_std <- read.delim(paste0(txtinout_folder,"/output.std" ), header =F)
  avg_ann_pos <- grep("AVE ANNUAL BASIN VALUES", out_std$V1)[1]
  ann_avg_hyd <- out_std[avg_ann_pos:(avg_ann_pos+23),]
  output$AVG_PRCP <- as.numeric(lapply(strsplit(as.character( ann_avg_hyd[2]), " "), function(x){x[!x ==""]})[[1]][3])
  output$SNOW <- as.numeric(lapply(strsplit(as.character( ann_avg_hyd[3]), " "), function(x){x[!x ==""]})[[1]][4])
  output$SNOW_MELT <- as.numeric(lapply(strsplit(as.character( ann_avg_hyd[4]), " "), function(x){x[!x ==""]})[[1]][4])
  output$SURFACE_RUNOFF <- as.numeric(lapply(strsplit(as.character( ann_avg_hyd[6]), " "), function(x){x[!x ==""]})[[1]][5])
  output$LAT_SOIL_Q <- as.numeric(lapply(strsplit(as.character( ann_avg_hyd[7]), " "), function(x){x[!x ==""]})[[1]][5])
  output$TOT_AQ_RCHRG <- as.numeric(lapply(strsplit(as.character( ann_avg_hyd[13]), " "), function(x){x[!x ==""]})[[1]][5])
  output$TOT_WATER_YIELD <- as.numeric(lapply(strsplit(as.character( ann_avg_hyd[14]), " "), function(x){x[!x ==""]})[[1]][5])
  output$AVG_ET <- as.numeric(lapply(strsplit(as.character( ann_avg_hyd[16]), " "), function(x){x[!x ==""]})[[1]][3])
  output$AVG_PET <- as.numeric(strsplit(lapply(strsplit(as.character( ann_avg_hyd[17]), " "), function(x){x[!x ==""]})[[1]][3], "MM")[[1]])
  return(output)
}

#3. get soil hydrology group data from input.std################################
hawqs_get_HYDGRP <- function(txtinout_folder,huc_project){
  output <- data.frame(huc_project = huc_project)
  in_std <- readLines(paste0(txtinout_folder, "/input.std" ))
  trgt_line_in_std_start <- grep("Hydgrp", in_std)+1
  trgt_line_in_std_end <- grep("HRU Input Summary Table 3:", in_std)-2
  
  hyd_grp_lines <- in_std[trgt_line_in_std_start:trgt_line_in_std_end]
  hyd_grps <- substr(hyd_grp_lines, 54, 54)
  soil_names <- substr(hyd_grp_lines, 35, 39)
  hru_areas <- as.numeric(substr(hyd_grp_lines, 23, 34))
  HUC12_area <- sum(hru_areas)
  
  output$hydA <- round(sum(hru_areas[hyd_grps == "A"])/sum(hru_areas), 3)
  output$hydB <- round(sum(hru_areas[hyd_grps == "B"])/sum(hru_areas), 3)
  output$hydC <- round(sum(hru_areas[hyd_grps == "C"])/sum(hru_areas), 3)
  output$hydD <- round(sum(hru_areas[hyd_grps == "D"])/sum(hru_areas), 3)
  return(output)
}

#4. To calculate channel dimensions#############################################
hawqs_get_chanel_dim <- function(txtinout_folder,huc_project){
  output <- data.frame(huc_project = huc_project)
  num_rte <- length(list.files(path = txtinout_folder, pattern = ".rte$"))
  Tot_L <- 0   ## total length of channel across watershed
  wCH_W <- 0   ## length weighted channel width
  wCH_D <- 0   ## length weighted channel depth
  wCH_S <- 0   ## length weighted channel slope
  
  for (ind_rte in 1:num_rte) {
    rte_num <- as.character( ifelse( ind_rte < 10, paste0("0000", ind_rte, "0000.rte"), 
                                     ifelse(ind_rte < 100, paste0("000", ind_rte, "0000.rte"),  
                                            paste0("00", ind_rte, "0000.rte")) ) )
    rte_file <- readLines(paste0(txtinout_folder, "/", rte_num ))
    wid_ind = as.numeric(substr(rte_file[2], 1, 7))
    dep_ind = as.numeric(substr(rte_file[3], 1, 7))
    slp_ind = as.numeric(substr(rte_file[4], 1, 7))
    len_ind = as.numeric(substr(rte_file[5], 1, 7))
    Tot_L <- Tot_L + len_ind
    wCH_W <- wCH_W + wid_ind*len_ind
    wCH_D <- wCH_D + dep_ind*len_ind
    wCH_S <- wCH_S + slp_ind*len_ind
  }
  
  output$wCH_W = wCH_W/Tot_L
  output$wCH_D = wCH_D/Tot_L
  output$wCH_S = wCH_S/Tot_L
  return(output)
}

#5. To calculate slopes#########################################################
hawqs_get_slope_CN <- function(txtinout_folder,huc_project){
  output <- data.frame(huc_project = huc_project)
  num_hru <- length(list.files(path = txtinout_folder, pattern = "\\.hru$"))-1
  
  hru_slp_sub <- c()
  wt_slp_sub <- 0
  wt_K_sub <- 0
  sub_fr <- 0
  
  sed_cat <- c()  ## erosion category  Vhigh, high, mod, low, Vlow, water
  K_all <- c()  ##Erodibility factor
  hru_fr_all <- c()  ## hru fraction of the sub-basin
  CN2_hru_sub <- 0
  
  hru_files <- list.files(path = txtinout_folder, pattern = "\\.hru$", full.names = TRUE)
  filtered_hru_files <- hru_files[!grepl("/output\\.hru$", hru_files)]
  
  for (ind_hru in filtered_hru_files) {
    hru_num <- sub(".*/(\\d+)\\.hru", "\\1", ind_hru)
    
    hru_file <- read.delim(paste0(txtinout_folder,"/", hru_num, ".hru" ), header =F)
    hru_slp <- as.numeric(unlist(strsplit(hru_file$V1[4], " "))[1])
    hru_fr <- as.numeric(unlist(strsplit(hru_file$V1[2], " "))[1])
    wt_slp_hru <- hru_fr*hru_slp
    
    sub_fr <- sub_fr + hru_fr
    wt_slp_sub = wt_slp_sub + wt_slp_hru
    
    CN2_hru <- as.numeric(substr(readLines(paste0(txtinout_folder, "/", hru_num, ".mgt" ))[11], 1, 6))
    CN2_hru_wt <- CN2_hru*hru_fr
    CN2_hru_sub <- CN2_hru_sub + CN2_hru_wt
  }
  output$CN2 <- CN2_hru_sub/sub_fr
  output$Slope <- wt_slp_sub/sub_fr
  return(output)
}

#6. Get HAWQS DA################################################################
hawqs_get_area <- function(txtinout_folder,huc_project){
  output <- data.frame(huc_project = huc_project)
  out_std <- read.delim(paste0(txtinout_folder,"/output.std" ), header =F)
  out_std <- as.character(out_std[7,])
  output$HAWQS_area <- sub(".*:(\\s*)([0-9.]+).*", "\\2", out_std)
  return(output)
}

#7. Read all static characteristics#############################################
read_hawqs_static_chars <- function(project,huc_id){
  watershed_chars <- data.frame()
  txtinout_folder <- paste0(project)
  df <- hawqs_get_area(txtinout_folder,huc_id)
  watershed_chars <- df%>%
    left_join(hawqs_get_watercycle_comp(txtinout_folder,huc_id))%>%
    left_join(hawqs_get_HYDGRP(txtinout_folder,huc_id))%>%
    left_join(hawqs_get_slope_CN(txtinout_folder,huc_id))%>%
    left_join(hawqs_get_chanel_dim(txtinout_folder,huc_id))
  watershed_chars$aridity <- watershed_chars$AVG_PET/watershed_chars$AVG_PRCP
  return(watershed_chars)
}
