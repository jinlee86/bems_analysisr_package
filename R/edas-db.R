#' EDAS DB Integration API Interface - EDAS DB Connection Interface
#' EDAS DB 접속 인터페이스 
#' 
#' @param username EDAS DataBase User Name - 접속정보 계정명
#' @param passwd EDAS DataBase Password - 접속정보 계정비번  
#' @param dbName EDAS DataBase Name - 접속 DB명 
#' @param host DataBase Server IP Addrerss - EDAS DataBase 서버 정보 
#' @param port DataBase Server Port - EDAS Database 서버 포트 정보
#'
#' @return EDAS.DB.API 
#'
#' @import RMySQL
#' @family EDAS
#' @examples
#' db_server <- "10.0.0.1" 
#' db_port <- 13306
#' db_username <- "test"
#' db_password <- "xxxx"
#' db_name <- "edas"
#' dbConnector <- edas.db(db_username, db_password, db_name, db_server, db_port)
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
  connector <- edas.db.api.createConnection(obj) 
  obj@tablelist <- RMySQL::dbListTables(connector)  
  obj@tables <- vector(mode="list")
  
  for (EDAS_TABLE in obj@tablelist)
  {
    obj@tables[[EDAS_TABLE]] <- RMySQL::dbListFields(connector, EDAS_TABLE)
  }
  RMySQL::dbDisconnect(connector)
  return(obj)
}




