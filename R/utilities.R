#' BEMS DB Timestamp to R datetime format
#' @keywords internal
bems_db_timestamp <- function(t){
  as.POSIXct(t, format = "%Y%m%d%H%M%S")
}
