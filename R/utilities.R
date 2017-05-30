#' BEMS DB Timestamp to R datetime format
#' @keywords internal
bems_db_timestamp <- function(t, format = "%Y%m%d%H%M%S"){
  as.POSIXct(t, format)
}

#' EDAS Default Model Output Files
#' @keywords internal
edas_get_model_output_file_name <- function(name, type=c(1,2,3))
{
  
}

#' EDAS Image Output File Name
#' @keywords internal
edas_get_png_output_file_name <- function(name)
{
  ifelse (is.na(name), "DEFAULT-PNG")
}
