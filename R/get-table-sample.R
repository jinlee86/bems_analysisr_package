#' EDAS DB Integration API Interface - Data Sampling
#' DB 테이블 데이터 샘플링   
#'
#' @param edasDBObj EDAS.DB.API  
#' @param table EDAS DataBase Table Name - 테이블 명 
#' @param samplingrate a sampling ratio ( < 1 ) or number ( >= 1) - 샘플 데이터 건수 또는 비율 
#'
#' @return sample data from the specified table 
#'
#' @importFrom RMySQL dbDisconnect
#' @family EDAS
#' @examples
#'db_server <- "10.0.0.1" 
#' db_port <- 13306
#' db_username <- "test"
#' db_password <- "xxxx"
#' db_name <- "edas"
#' dbConnector <- edas.db(testuser_name, testuser_password, "edas", testpoc_server, testpoc_port)
#' get.table.sample(dbConnector, "EDAS_S_USAGE", .0002)
#' get.table.sample(dbConnector, "EDAS_S_USAGE", 1) 
#' @export

get.table.sample <- function(edasDBObj, table, samplingrate){
  if (!is.numeric(samplingrate) )
  {
    stop("Invalid Argument for samplingrate - Requires a number",
         call. = FALSE)
  }
  
  if (!trimws(table) %in% edasDBObj@tablelist) {
    stop( sprintf("Invalid table name - %s not found in the %s database!!", 
                  trimws(table), edasDBObj@database), 
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
  
  connector <- edas.db.api.createConnection(edasDBObj)
  resultset <- edas.db.api.query(connector, sql_request)
  RMySQL::dbDisconnect(connector)
  return(list("tableInfo"=resultset, "sql"=sql_request))
}
