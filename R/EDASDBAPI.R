#' EDAS DB Query Executor
#'
#' @param connector Database Connection
#' @param sql a SQL query statement
#'
#' @return SQL Query Response 
#'
#' @import RMySQL
#' @family EDAS DB Integration API Interface
#' @examples
#' edas.db.api.query(connector,"SELECT * FROM Test")
#' @keywords internal
#' 

edas.db.api.query <- function(connector, sql){
  rs <- RMySQL::dbSendQuery(connector, sql)
  sqlResponse <- RMySQL::fetch(rs, n=-1)
  RMySQL::dbClearResult(rs)
  return(sqlResponse)
}

#' EDAS DB Establish Connection 
#'
#' @param edasDBObj EDAS.DB.API Object
#'
#' @return Database Connection
#'
#' @import RMySQL
#' @family EDAS DB Integration API Interface
#' @examples
#' testobj <- edas.db(testuser_name, testuser_password, "edas", testpoc_server, testpoc_port)
#' edas.db.api.createConnection(testobj)
#' @keywords internal
#' 
edas.db.api.createConnection <-function(edasDBObj) {
  connector <- RMySQL::dbConnect(RMySQL::MySQL(),
                                 user=obj@username,
                                 password=obj@password,
                                 host=obj@serverIP,
                                 port=obj@serverPort,
                                 dbname=obj@database)
  return(connector)
}