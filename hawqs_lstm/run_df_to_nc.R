df_to_nc <- function(df, basin_id, file_path){
  df <- df %>%
          mutate(date = ymd(df$date))
  
  dim_time <- ncdim_def("date", "days since 1970-01-01", as.numeric(df$date - as.Date("1970-01-01")))
  vars <- names(df%>%select(-"date"))
  var_defs <- lapply(vars, function(var) {
    ncvar_def(name = var, units = "", dim = list(dim_time), missval = NA)
  })
  
  # Create NetCDF file
  nc_path <- file.path(paste0(file_path,basin_id, ".nc"))
  nc <- nc_create(nc_path, var_defs)
  
  # Write data to NetCDF file
  for (var in vars) {
    ncvar_put(nc, var, df[[var]])
  }
  nc_close(nc)
}

