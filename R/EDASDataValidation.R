#' EDAS Data Validation Report Class 
#' 
#' @import methods
#' @export EDAS.DATA.Validation.Report
#' @exportClass EDAS.Data.Validation.Report
EDAS.DATA.Validation.Report = setRefClass('EDAS.Data.Validation.Report',
                                          fields=list(name="character",
                                                      report="list")
                                          )


#' Data Validation Methods 
validate.EDAS.data <- function(dataset, type, 
                               dateTimeColumn="DateTime",
                               groupByColumn=NA,
                               ...)
{
  names(list(...))
}

#' Data Validation Method - Time Interval  
#'
#' @param dataset DataFrame 
#' @param dateTimeColumn DateTime Column (requires POSIXCT Type)
#' @param groupByColumn Group by Column (Optional)
#'
#' @return EDAS DB Connection, Tables and Attributes
#' @keywords  validation_methods
#' @import dplyr utilities
#' @family data validation
#' @examples
#' dbInit("EDASTest","EDASPW")
#' dbInit("EDASTest","EDASPW",  dbName="edas", server="58.224.211.82", serverPort=13306)
validation_time_interval <- function(dataset, 
                                     dateTimeColumn="DateTime",
                                     groupByColumn=NA){
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is required!!",
         call. = FALSE)
  }
  if (!dateTimeColumn %in% colnames(dataset))
  {
    stop(sprintf("datetime column: %s does not exist in the given data frame.\n", 
                 dateTimeColumn),
         call. = FALSE)
  }
  if (!is(dataset[,dateTimeColumn], "POSIXct"))
  {
    tryCatch(
      {
        sprintf("Type Error - trying to parse the column %s with the default format: yyyymmddHHMMSS...")
        dataset[,dateTimeColumn] <- BEMSRAPI::bems_db_timestamp(dataset[,dateTimeColumn])
        print("OK!")
      },
      error=function(e){
        print(e)
        stop(sprintf("Failed to parse the column (%s) into the POSIXct datetime format.\n", 
                     dateTimeColumn),
             call. = FALSE)
      }
    )
  }

  names(dataset)[names(dataset) == dateTimeColumn] <- "DateTime"
  
  # Start Time Interval Validation  
  if (!is.na(groupByColumn))
  {
    if (!groupByColumn %in% colnames(dataset))
    {
        stop(sprintf("Given groupbyColumn (%s) does not exist!!\n", 
                     groupByColumn),
           call. = FALSE)
    }
    names(dataset)[names(dataset) == groupByColumn] <- "ID"
    evaluated_dataset <- dataset %>% arrange(ID, DateTime) %>%
      group_by(ID) %>% mutate(interval = DateTime - lag(DateTime)) %>% as.data.frame()
    
    evaluated_stats <- evaluated_dataset %>% 
      group_by(ID) %>% 
      do(data.frame(as.list(summary(as.numeric(.$interval, units="auto"))), check.names = FALSE))

    names(evaluated_dataset)[names(evaluated_dataset) == "ID"] <- groupByColumn
  }
  else
  {
    evaluated_dataset <- dataset %>% arrange(DateTime) %>%
      mutate(interval = DateTime - lag(DateTime)) %>% as.data.frame()
    
    evaluated_stats <- evaluated_dataset %>% 
      do(data.frame(as.list(summary(as.numeric(.$interval, units="auto"))), check.names = FALSE))
  }
  names(evaluated_dataset)[names(evaluated_dataset) == "DateTime"] <- dateTimeColumn
  #Difftime calculation units
  interval_units <- attributes(validate_time_interval$gap)$units
  
  validation_report <- list("units"=interval_units,
                            "stats"=evaluated_stats,
                            "dataset"=evaluated_dataset) 
  return(validation_report)
}

validate_time_interval <- preprocessed_merged_df %>%
  arrange(POINT_ID, DATETIME) %>%
  group_by(POINT_ID) %>%
  mutate(gap = DATETIME - lag(DATETIME)) %>% as.data.frame()



