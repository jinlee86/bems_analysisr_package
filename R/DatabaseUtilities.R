#' EDAS DB Connection Maker
#'
#' @param username EDAS DataBase User Name
#' @param passwd EDAS DataBase Password
#'
#' @return EDAS DB Connection, Tables and Attributes
#'
#' @import RMySQL
#' @family EDAS DB Integration API Interface
#' @examples
#' edas.db("TestUser","TestPassword","edas")
#' edas.db("TestUser","TestPassword","edas", host="10.0.0.1", port=1234)
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