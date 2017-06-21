#' EDAS DB API Class 
#' 
#' @import methods
#' @export EDAS.DB.API
#' @exportClass EDAS.DB.API
EDAS.DB.API <- setClass(Class="EDAS.DB.API",
                        slots=c(username="character",
                                password="character",
                                database="character",
                                serverIP="character",
                                serverPort="numeric",
                                tablelist="character",
                                tables="list"),
                        validity = function(object)
                        {
                          if( object@username=="" || object@password==""){
                            return("Wrong Username or Password!! - can not have empty string for username or password.")
                          }
                          return(TRUE)
                        }
)

setGeneric("connect.db", function(object){
  standardGeneric("connect.db")
})

setMethod("connect.db", signature(object="EDAS.DB.API"),
          function(object){
            
          })

setMethod("show",
          "EDAS.DB.API",
          function(object) 
          {
            cat("--Database Configuration Settings--", "\n")
            cat("User Name: ", object@username, "\n")
            cat("Datbase Server IP: ", object@serverIP, "\n")
            cat("Datbase Server Port: ", object@serverPort, "\n")
            cat("Database: ", object@database, "\n")
            if (length(object@tablelist) > 0)
            {
              cat("\n")
              cat("--Tables (",length(object@tablelist),")--", "\n")
              cat(object@tablelist, "\n")
              for (edas_db_table in object@tablelist)
              {
                cat("\n")
                cat("Table:", edas_db_table, "\n")
                cat("Attributes: ", paste(object@tables[[edas_db_table]], sep=","), "\n")
              }
            }
          }
)