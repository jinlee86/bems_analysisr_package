#' EDAS DB Integration API Interface - EDAS DB Table Description 
#' 테이블 스키마 정보 
#' 
#' @param connector EDAS.DB.API
#' @param table 테이블 명 
#'
#' @return 
#' tableInfo - 테이블 스키마 
#' sql - 쿼리 
#' @importFrom  RMySQL dbDisconnect
#' @family EDAS
#' @examples
#' db_server <- "10.0.0.1" 
#' db_port <- 13306
#' db_username <- "test"
#' db_password <- "xxxx"
#' db_name <- "edas"
#' dbConnector <- edas.db(testuser_name, testuser_password, "edas", testpoc_server, testpoc_port)
#' describe.table(dbConnector, "EDAS_S_USAGE")
#' @export 

describe.table <- function(edas, table){
  if (!is(edas, "EDAS.DB.API")){
    stop("Invalid Argument is provided!! - edas.db object is needed!!",
         call. = FALSE)
  }
  
  if (!trimws(table) %in% edas@tablelist) {
    stop( sprintf("Invalid table name - %s not found in the %s database!!", 
                  trimws(table), edas@database), 
          call. = FALSE)
  }
  
  connector <- edas.db.api.createConnection(edas)
  sql_request <- paste0("DESCRIBE ",
                        tablename, 
                        ";")
  resultset <- edas.db.api.query(connector, sql_request)
  RMySQL::dbDisconnect(connector)
  return(list("tableInfo"=resultset, "sql"=sql_request))
}