update_swat_from_SUFI_OUT <- function(project,par_inf_file,best_par_file,HUCID){
  folder_path <- file.path(project,"swatcup.Sufi2.SwatCup")
  par_file = readLines(par_inf_file)
  best_par = readLines(best_par_file)
  ps_files <- list.files(folder_path,pattern = "\\d+p\\.dat", full.names = FALSE)
  
  par_EPCO_line = which(substr(par_file, 1, 11) == "v__EPCO.hru")[1]     ## [1]: in case there are multiple EPCO
  
  par_file = data.frame("V1" = par_file[par_EPCO_line:(length(par_file))])
  num_par = length(par_file$V1)
  if(num_par > 22){num_par_org = 19} else {num_par_org = num_par}
  par_TIMP_lines = which(substr(par_file$V1, 1, 11) == "v__TIMP.bsn")
  if( length(par_TIMP_lines) == 2 ){ par_file = as.data.frame(par_file$V1[-par_TIMP_lines[1]]) }
  colnames(par_file) = "V1"
  if(exists("trgt_EPCO_line")){rm("trgt_EPCO_line")}
  
  best_par_EPCO_line = which(substr(best_par, 1, 11) == "v__EPCO.hru")
  
  if(length(best_par_EPCO_line) > 1){
    for(p in 1:length(ps_files)){
      ps_file = read.delim(paste0(folder_path, "\\", ps_files[p]), header =F)[1,]
      sub_nu = gsub("[p.dat]", "", stringr::word(ps_file, 1) )  ## sub number of the HUC12 in the ps file
      
      HUCID_ps = stringr::word(ps_file, -1)
      HUCID_ps= gsub("[()]", "", HUCID_ps) ## HUC12 number in the ps file
      if(HUCID_ps == HUCID){ ps_ID = unlist(strsplit(ps_files[p], "p"))[1] }
    }
    
    for(q in best_par_EPCO_line){
      EPCO_line_ind = best_par[q]
      sub_ID = unlist(strsplit(unlist(lapply(strsplit(EPCO_line_ind, " "), function(x){x[!x ==""]}))[1], "-"))[2]
      if(is.na(as.numeric(sub_ID)) && !is.na(sub_ID) ) {
        sub_x = unlist(strsplit(unlist(lapply(strsplit(EPCO_line_ind, " "), function(x){x[!x ==""]}))[1], "-"))
        if( !is.na(as.numeric(sub_x[length(sub_x)])) ){ sub_ID = sub_x[length(sub_x)]  } else {
          sub_x = unlist(strsplit(sub_x, ","))
          sub_ID = sub_x[length(sub_x)]
        }
        
      } else if(is.na(sub_ID)){ 
        sub_ID = unlist(strsplit(unlist(lapply(strsplit(EPCO_line_ind, " "), function(x){x[!x ==""]}))[1], "________"))[2]
        if(is.na(as.numeric(sub_ID))){
          sub_ID = unlist(strsplit(sub_ID, ","))
          sub_ID = sub_ID[length(sub_ID)]
        }
      }
      
      if(sub_ID == ps_ID){ trgt_EPCO_line = q  }
    }
    
  } else {
    trgt_EPCO_line = best_par_EPCO_line
  }
  
  if(!exists("trgt_EPCO_line")){ trgt_EPCO_line =  best_par_EPCO_line[length(best_par_EPCO_line)] }
  best_par = best_par[trgt_EPCO_line:(length(best_par))]
  
  ## to check if there are two TIMP
  TIMP_lines = which(substr(best_par, 1, 11) == "v__TIMP.bsn")
  if( length(TIMP_lines) == 2 ){ best_par = best_par[-TIMP_lines[1]] }
  
  par_names <- c()
  for(i in 1:length(par_file$V1)){par_names = c(par_names, unlist(strsplit(par_file$V1[i], " "))[1])}
  
  par_names_EPCO_line = which(substr(par_names, 1, 11) == "v__EPCO.hru")
  
  if(exists("trgt_EPCO_line")){rm("trgt_EPCO_line")}
  if(length(par_names_EPCO_line) > 1){
    for(p in 1:length(ps_files)){
      ps_file = read.delim(paste0(folder_path, "\\", ps_files[p]), header =F)[1,]
      sub_nu = gsub("[p.dat]", "", stringr::word(ps_file, 1) )  ## sub number of the HUC12 in the ps file
      
      HUCID_ps = stringr::word(ps_file, -1)
      HUCID_ps= gsub("[()]", "", HUCID_ps) ## HUC12 number in the ps file
      if(HUCID_ps == HUCID){ ps_ID = unlist(strsplit(ps_files[p], "p"))[1] }
    }
    
    for(q in par_names_EPCO_line){
      EPCO_line_ind = par_names[q]
      sub_ID = unlist(strsplit(unlist(lapply(strsplit(EPCO_line_ind, " "), function(x){x[!x ==""]}))[1], "-"))[2]
      if(is.na(as.numeric(sub_ID)) && !is.na(sub_ID) ) {
        sub_x = unlist(strsplit(unlist(lapply(strsplit(EPCO_line_ind, " "), function(x){x[!x ==""]}))[1], "-"))
        if(!is.na(as.numeric(sub_x[length(sub_x)]))){ sub_ID = sub_x[length(sub_x)]  } else {
          sub_x = unlist(strsplit(sub_x, ","))
          sub_ID = sub_x[length(sub_x)]
        }
        
      } else if(is.na(sub_ID)){ 
        sub_ID = unlist(strsplit(unlist(lapply(strsplit(EPCO_line_ind, " "), function(x){x[!x ==""]}))[1], "________"))[2]
        if(is.na(as.numeric(sub_ID))){
          sub_ID = unlist(strsplit(sub_ID, ","))
          sub_ID = sub_ID[length(sub_ID)]
        }
      }
      
      if(sub_ID == ps_ID){ trgt_EPCO_line = q  }
    }
    
  } else {
    trgt_EPCO_line = par_names_EPCO_line
  }
  if(!exists("trgt_EPCO_line")){ trgt_EPCO_line =  par_names_EPCO_line[length(par_names_EPCO_line)] }
  
  par_names = par_names[trgt_EPCO_line:(length(par_names))]
  num_par = length(par_names)
  
  ValueType = data.frame()
  Parameter = data.frame()
  InputFile = data.frame()
  Value = data.frame()
  LanduseConditions= data.frame()
  main_HUC12 = data.frame()
  subs = data.frame()
  
  for(q in 1:num_par){                                                                                                                                                                    #5th-2 level start
    par = substr( best_par[q], 1, nchar(par_names[q]) )
    par = lapply(strsplit(par, " "), function(x){x[!x ==""]})[[1]][1]
    ValueType1  = substr( par, 1, 1 )
    ValueType= rbind(ValueType, ValueType1)
    Parameter2 = lapply(strsplit(strsplit(par, "__")[[1]][2], " "), function(x){x[!x ==""]})
    Parameter1 = strsplit(Parameter2[[1]][1], "\\.")
    Parameter = rbind(Parameter, Parameter1[[1]][1])
    InputFile = rbind(InputFile, Parameter1[[1]][2])
    Value = rbind( Value, substr( best_par[q], nchar(par_names[q])+1, nchar(best_par[q]) ) )
    main_HUC12= rbind(main_HUC12, HUCID)
    
    letters_only <- function(x) !grepl("[^A-Za-z]", x)
    if( letters_only(substr(par, nchar(par),nchar(par))) ){
      if(length(lapply(strsplit(par, c("__", " ")), function(x){x[!x ==""]})[[1]])==2){
        LanduseConditions= rbind(LanduseConditions, "")
        subs= rbind(subs, "")
      } else {
        LanduseConditions= rbind(LanduseConditions, lapply(strsplit(par, c("__", " ")), function(x){x[!x ==""]})[[1]][3])
        subs= rbind(subs, "")
      }
    } else {
      if(length(lapply(strsplit(par, c("__", " ")), function(x){x[!x ==""]})[[1]])==3){
        LanduseConditions= rbind(LanduseConditions, "")
        subs= rbind(subs, lapply(strsplit(par, c("__", " ")), function(x){x[!x ==""]})[[1]][3])
      } else {
        LanduseConditions= rbind(LanduseConditions, lapply(strsplit(par, c("__", " ")), function(x){x[!x ==""]})[[1]][3])
        subs= rbind(subs, lapply(strsplit(par, c("__", " ")), function(x){x[!x ==""]})[[1]][4])
      }
    }
    
  }                                                                                                                                                                                        #5th-2 level end
  print("Best par done")
  
  
  cal_bestpar = data.frame(ValueType, Parameter, LanduseConditions, InputFile, as.numeric(unlist(Value)), main_HUC12, subs)
  colnames(cal_bestpar) = c("ValueType", "Parameter", "LanduseConditions", "InputFile", "Value", "main_HUC12", "Subbasins")
  cal_bestpar_jac = cal_bestpar  #[(length(cal_bestpar$ValueType)-num_par_org+1):length(cal_bestpar$ValueType), ]
  
  ## remove multiple TIMP
  line_num = which( cal_bestpar_jac$Parameter == "TIMP" )
  if( length(line_num) > 1 ){ 
    line_num = which( cal_bestpar_jac$Parameter == "TIMP" )[1]
    cal_bestpar_jac = cal_bestpar_jac[-line_num, ]
  }
  
  model_in <-  cbind.data.frame( paste0( cal_bestpar_jac$ValueType, "__", cal_bestpar_jac$Parameter, ".", cal_bestpar_jac$InputFile ), cal_bestpar_jac$Value)
  CANMXline_num = which( cal_bestpar_jac$Parameter == "CANMX" )
  model_in[CANMXline_num, 1] = paste0( cal_bestpar_jac$ValueType[CANMXline_num], "__", cal_bestpar_jac$Parameter[CANMXline_num], ".", cal_bestpar_jac$InputFile[CANMXline_num], 
                                       "______", cal_bestpar_jac$LanduseConditions[CANMXline_num])
  model_in[CANMXline_num, 2] = cal_bestpar_jac$Value[CANMXline_num]
  
  if(!is.null(model_in)){
    write.table(model_in, file.path(folder_path, "model.in"), row.names = F, col.names = F, quote = F)
    
    shell.exec(file.path(folder_path, "Swat_Edit.exe"))
    print("Running Shell script")
    
    alphabf_d_in = model_in[substr(model_in[,1], 4, 13) == "ALPHA_BF_D", ]
    alphabf_d_in = round(as.numeric(alphabf_d_in[length(alphabf_d_in[,2]),2]), 3)
    alphabf_d_bsn = str_extract(alphabf_d_in[length(alphabf_d_in)], "[[:digit:]]+")
    
    
    bsn_ins = model_in[substr(model_in[,1], 4, 7) == "TIMP", 2]
    bsn_ins = round(as.numeric(bsn_ins), 3)
    
    alphabf_d_file = list.files(folder_path, pattern = ".gw$")
    alphabf_d_file = paste0(folder_path, "\\", alphabf_d_file[length(alphabf_d_file)])
    
    
    bsn_file <- list.files(folder_path, pattern = ".bsn$", full.names = T)
    if(length(bsn_ins) > 0){
      repeat{
        wait_period = 15
        print(paste0("Wait period swat edit: ", wait_period, " seconds"))
        Sys.sleep(wait_period)
        
        bsn_outs = round(as.numeric(unlist(strsplit(read.delim(bsn_file, header = F)$V1[8], "\\|"))[1]), 3)
        alphabf_d_out = round(as.numeric(unlist(strsplit(read.delim(alphabf_d_file[length(alphabf_d_file)], header = F)$V1[17], "\\|"))[1]), 3)
        
        if ( bsn_outs == bsn_ins && alphabf_d_out == alphabf_d_in ){break}
      }
    } else {
      repeat{
        wait_period = 10
        print(paste0("Wait period swat edit: ", wait_period, " seconds"))
        Sys.sleep(wait_period)
        alphabf_d_out = round(as.numeric(unlist(strsplit(read.delim(alphabf_d_file[length(alphabf_d_file)], header = F)$V1[17], "\\|"))[1]), 3)
        
        if (alphabf_d_out == alphabf_d_in ){break}
      }
    }
  }
}
