#' EDAS DB Connection Maker
#'
#' @param username EDAS DataBase User Name
#' @param passwd EDAS DataBase Password
#' @param dbName EDAS DataBase Name
#' @param host DataBase Server IP Addrerss
#' @param port DataBase Server Port
#'
#' @return EDAS DB Connection, Tables and Attributes
#'
#' @import RMySQL
#' @family EDAS DB Integration API Interface
#' @examples
#' edas.db("TestUser","TestPassword","edas")
#' edas.db("TestUser","TestPassword","edas", host="10.0.0.1", port=1234)
#' @export

edas.db <- function(username, password, database, host, port){
  if (!requireNamespace("RMySQL", quietly = TRUE)) {
    stop("RMySQL package is required!!",
         call. = FALSE)
  }
  if (missing(host)){
    host <- Sys.getenv("EDAS_DB_HOST")
  }
  if (missing(port)){
    port <- Sys.getenv("EDAS_DB_PORT")
  }
  obj = new("EDAS.DB.API", 
            username=username, 
            password=password,
            database=database,
            serverIP=host,
            serverPort=port
  )
  connector <- edas.db.createConnection(obj) 
  obj@tablelist <- RMySQL::dbListTables(connector)  
  obj@tables <- vector(mode="list")
  
  for (EDAS_TABLE in obj@tablelist)
  {
    obj@tables[[EDAS_TABLE]] <- RMySQL::dbListFields(connector, EDAS_TABLE)
  }
  RMySQL::dbDisconnect(connector)
  return(obj)
}




