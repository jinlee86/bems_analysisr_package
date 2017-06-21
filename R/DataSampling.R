
#' Data Sampling
#'
#' @param edasDBObj EDAS.DB.API Object
#' @param table EDAS DataBase Table Name
#' @param samplingrate a sampling ratio ( < 1 ) or number ( >= 1) 
#'
#' @return sample data from the specified table 
#'
#' @import RMySQL
#' @family EDAS DB Integration API Interface
#' @examples
#' get.table.sample(testobj, "EDAS_S_USAGE", .0002)
#' get.table.sample(testobj, "EDAS_S_USAGE", 1) 
#' @export

get.table.sample <- function(edasDBObj, table, samplingrate){
  if (!is.numeric(samplingrate) )
  {
    stop("Invalid Argument for samplingrate - Requires a number",
         call. = FALSE)
  }
  
  if (!trimws(table) %in% edas@tablelist) {
    stop( sprintf("Invalid table name - %s not found in the %s database!!", 
                  trimws(table), edas@database), 
          call. = FALSE)
  }
  
  sql_request <- ifelse(samplingrate >= 1, 
                        paste0("SELECT * FROM ", 
                               table,
                               " limit ", 
                               samplingrate, 
                               "; "),
                        paste0("SELECT * FROM ", 
                               table,
                               " where RAND() <= ", samplingrate, 
                               "; ")
  )
  
  connector <- edas.db.api.createConnection(edas)
  resultset <- edas.db.api.query(connector, sql_request)
  RMySQL::dbDisconnect(connector)
  return(list("tableInfo"=resultset, "sql"=sql_request))
}
