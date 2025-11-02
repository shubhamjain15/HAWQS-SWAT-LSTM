write_cio <- function(project, NBYR = NA_real_, IYR = NA_real_, NYSKIP = NA_real_,IPRINT = 1){
  if( file.exists(paste0(project, "/TxtInOut/1p.dat"))){
    txtinout <- paste0(project, "/TxtInOut")
    file.cio <- read.delim(paste0(txtinout, "/file.cio"), header =F )
    
    if(is.na(NBYR)){
      NBYR <- as.numeric(substr(file.cio$V1[7], 1,3))     #NBYR : Number of years simulated
    }
    if(is.na(IYR)){
      IYR <- as.numeric(substr(file.cio$V1[8], 1,4))      #IYR: Beginning year of simulation
    }
    if(is.na(NYSKIP)){
      NYSKIP <- as.numeric(substr(file.cio$V1[55], 1,2))  #NYSKIP: number of years to skip output printing/summarization") 
    }
    
    ##################################################################################################################################################################
    ###########   Editing file.cio: needed first time to generate entire output.hru
    file.cio$V1[7] <- paste0(NBYR,"                  | NBYR : Number of years simulated")
    file.cio$V1[8] <- paste0(IYR,"                | IYR : Beginning year of simulation")
    file.cio$V1[55] <- paste0(NYSKIP,"                    | NYSKIP: number of years to skip output printing/summarization")
    file.cio$V1[54] <- paste0(IPRINT, "                   | IPRINT: print code (month, day, year)")   ## to change daily
    file.cio$V1[64] <- "  2     23      25      26      28       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0"      ## HRU output variables
    file.cio$V1[66] <- "       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0"   ## writing all HRUs
    #file.cio$V1[9] <- paste0(IDAF,"                | IDAF : Beginning julian day of simulation")
    #file.cio$V1[10] <- paste0(IDAL,"                 | IDAL : Ending julian day of simulation")
    
    empty_row <- ""
    insertRow <- function(existingDF, newrow, r) {
      existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
      existingDF[r,] <- newrow
      existingDF
    }
    
    file.cio <- insertRow(file.cio, empty_row, 5)
    file.cio <- insertRow(file.cio, empty_row, 36)
    file.cio <- insertRow(file.cio, empty_row, 37)
    file.cio <- insertRow(file.cio, empty_row, 40)
    file.cio <- insertRow(file.cio, empty_row, 41)
    file.cio <- insertRow(file.cio, empty_row, 86)
    file.cio <- file.cio[!is.na(file.cio$V1),]
    write.table(file.cio, paste0(txtinout, "/file.cio"), row.names = F, col.names = F, quote = F)
  }
}

