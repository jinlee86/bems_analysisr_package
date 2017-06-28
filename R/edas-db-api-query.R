#' EDAS DB Integration API Interface - EDAS DB Query Executor
#' EDAS 데이터베이스 쿼리 실행 API
#' 
#' @param connector EDAS.DB.API 
#' @param sql a SQL query statement - 쿼리 
#'
#' @return list - 쿼리 결과 
#'
#' @importFrom RMySQL dbSendQuery fetch dbClearResult
#' @family EDAS
#' @examples
#' db_server <- "10.0.0.1"
#' db_port <- 13306
#' db_username <- "test"
#' db_password <- "xxxx"
#' db_name <- "edas"
#' dbConnector <- edas.db(db_username, db_password, db_name, db_server, db_port)
#' edas.db.api.query(dbConnector,"SELECT * FROM Test")
#'
#' @keywords internal

edas.db.api.query <- function(connector, sql){
  rs <- RMySQL::dbSendQuery(connector, sql)
  sqlResponse <- RMySQL::fetch(rs, n=-1)
  RMySQL::dbClearResult(rs)
  return(sqlResponse)
}