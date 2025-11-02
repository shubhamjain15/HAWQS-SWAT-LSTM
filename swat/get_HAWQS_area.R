get_hawqs_area <- function(project){
  out_std <- read.delim(file.path(project,"TxtInOut/output.std" ), header =F)
  out_std <- as.character(out_std[7,])
  HAWQS_area <- as.numeric(sub(".*:(\\s*)([0-9.]+).*", "\\2", out_std))
  return(HAWQS_area)
}
