run_SWAT <- function(project){
  if( file.exists(paste0(project, "/", "TxtInOut/1p.dat"))){
    txtinout <- paste0(project, "/TxtInOut")
    file.cio <- read.delim(paste0(txtinout, "/file.cio"), header =F )
    
    NBYR <- as.numeric(substr(file.cio$V1[7], 1,3))     #NBYR : Number of years simulated
    IYR <- as.numeric(substr(file.cio$V1[8], 1,4))      #IYR: Beginning year of simulation
    NYSKIP <- as.numeric(substr(file.cio$V1[55], 1,2))  #NYSKIP: number of years to skip output printing/summarization") 
    
    
    ##################################################################################################################################################################
    ###########   Editing file.cio: needed first time to generate entire output.hru
    
    file.cio$V1[54] <- paste0(1, "                   | IPRINT: print code (month, day, year)")   ## to change daily
    file.cio$V1[64] <- "  2     23      25      26      28       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0"      ## HRU output variables
    file.cio$V1[66] <- "       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0       0"   ## writing all HRUs
    
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
    
    write.table(file.cio, paste0(txtinout, "/file.cio"), row.names = F, col.names = F, quote = F)
    setwd(txtinout)
    shell("swat_64rel.exe")
  }
}
