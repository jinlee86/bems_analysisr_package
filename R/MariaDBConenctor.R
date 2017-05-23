#' EDAS DB Initialization
#'
#' @param username EDAS DataBase User Name
#' @param passwd EDAS DataBase Password
#' @param dbName EDAS DataBase Name
#' @param server DataBase Server IP Addrerss
#' @param serverPort DataBase Server Port
#'
#' @return EDAS DB Connection, Tables and Attributes
#'
#' @import RMySQL
#' @family EDAS DB Integration API
#' @examples
#' dbInit("EDASTest","EDASPW")
#' dbInit("EDASTest","EDASPW",  dbName="edas", server="58.224.211.82", serverPort=13306)
#' @export
dbInit <- function(username, passwd,
                          dbName="edas",
                          server="58.224.211.82",
                          serverPort=13306){
  if (!requireNamespace("RMySQL", quietly = TRUE)) {
    stop("RMySQL package is required!!",
       call. = FALSE)
  }
  if (length( intersect(search(), "EDAS_DB_CONNECTION_OBJECT") ) > 0){
    detach(EDAS_DB_CONNECTION_OBJECT)
  }

  .EDAS_DB_CONNECTOR <- RMySQL::dbConnect(RMySQL::MySQL(),
                                    user=username,
                                    password=passwd,
                                    host=server,
                                    port=serverPort,
                                    dbname=dbName)

  .EDAS_DB_TABLES <- RMySQL::dbListTables(.EDAS_DB_CONNECTOR)
  table_list <- vector(mode="list")
  for (EDAS_TABLE in .EDAS_DB_TABLES)
  {
    table_list[[EDAS_TABLE]] <- dbListFields(.EDAS_DB_CONNECTOR, EDAS_TABLE)
  }
  EDAS_DB_CONNECTION_OBJECT <- list(.EDAS_DB_CONNECTOR=.EDAS_DB_CONNECTOR, tables=table_list)
  attach(EDAS_DB_CONNECTION_OBJECT, name="EDAS_DB_CONNECTION_OBJECT")
}





