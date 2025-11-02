get_rch_ids <- function(project,huc_id){
  file_pattern <- "^\\d+p\\.dat$"
  file_list <- list.files(path = file.path(project,"TxtInOut"), pattern = file_pattern, full.names = TRUE)
  out <- data.frame()
  for(i in file_list){
    df <- readLines(i)[1]
    subbasin_number <- strsplit(regmatches(df, regexpr("Subbasin:(\\d+)", df))[[1]],":")[[1]][2]
    huc_number <- gsub("[()]","",regmatches(df, regexpr("\\(\\d+\\)", df))[[1]])
    df <- data.frame(huc_id = rep(huc_id,length(subbasin_number)),huc_subbasin = huc_number, subbasin = as.numeric(subbasin_number))
    out <- rbind(out,df)
    out <- out[order(out$subbasin),]
    rownames(out) <- NULL
  }
  return(out)
}
