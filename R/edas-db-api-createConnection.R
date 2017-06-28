#' EDAS DB Establish Connection 
#' EDAS DB 연결 
#' @param obj EDAS.DB.API
#' @return Database Connection
#'
#' @importFrom RMySQL dbConnect
#' @family EDAS DB Integration API Interface
#' @examples
#' db_server <- "10.0.0.1"
#' db_port <- 13306
#' db_username <- "test"
#' db_password <- "xxxx"
#' db_name <- "edas"
#' dbConnector <- edas.db(db_username, db_password, db_name, db_server, db_port)
#' edas.db.api.createConnection(dbConnector)
#' @keywords internal
#' 
edas.db.api.createConnection <-function(obj) {
  connector <- RMySQL::dbConnect(RMySQL::MySQL(),
                                 user=obj@username,
                                 password=obj@password,
                                 host=obj@serverIP,
                                 port=obj@serverPort,
                                 dbname=obj@database)
  return(connector)
}