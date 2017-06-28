#' EDAS Data Validation Methods 
#'
#' @param dataset a dataframe 
#' @param type a list of a validation method to apply on the dataset  
#' @param dateTimeColumn (optional) a datetime format, giving datetimeCol is not in POSIXCT type, 
#' the function first tries to convert "datetimeCol" to POSIXCT type.  
#' @param groupByColumn (optional) a group by column to create subsets which the function is applied individually.
#' @return units, stats, hist, dataset 
#' @family EDAS
#' @examples
#' validate.edas.data(dataset, "DATE")
#' validate.edas.data(dataset, "DATE", "ID")
#' validate.edas.data(dataset, "DATE", "ID", type="daytype")
#' validate.edas.data(dataset, "DATE", "ID", c("daytype","timetype"))
#' @export
validate.edas.data <- function(dataset, 
                               dateTimeColumn=NULL,
                               groupByColumn=NULL,
                               type=c("timeinterval", "daytype", "timetype"),
                               ...)
{
  
  if (is.null(dataset) || !is.data.frame(dataset))
  {
    stop("dataset must be a dataframe!!")
  }
  
  if (is.null(dateTimeColumn) || !dateTimeColumn %in% colnames(dataset) || !is(dataset[,dateTimeColumn], "POSIXct"))
  {
    stop(sprintf("Invalid datetime column is given - %s, is in POSIXct format?", 
                 dateTimeColumn),
         call. = FALSE)
  }
  
  type <- match.arg(type, several.ok = TRUE)
  
  #edas.datapreprocessing.api.validatetimeinterval  
  if ("timeinterval" %in% type)
  {
    EDAS::edas.datapreprocessing.api.validatetimeinterval(...)
  }
  
  #edas.datapreprocessing.api.daytype
  if ("daytype" %in% type)
  {
    EDAS::edas.datapreprocessing.daytype(...)
  }
  
  #edas.datapreprocessing.api.timetype
  if ("timetype" %in% type)
  {
    EDAS::edas.datapreprocessing.timetype(...)
  }
  
}
