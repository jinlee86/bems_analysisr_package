#' EDAS Data Validation Method - Time Intervals  
#' 데이터 수집 주기 추출  
#' 데이터의 시간 컬럼(datetimeColumn) 값을 기준으로 주기를 계산  
#' 그룹 컬럼(groupByColumn) 값이 주여졌을 경우 groupByColumn 값 기준으로 
#' 데이터를 나눈후 주기를 계산  
#' 
#' @param dataset DataFrame 
#' @param dateTimeColumn DateTime Column (requires POSIXCT Type)
#' @param groupByColumn Group by Column (Optional)
#'
#' @return 
#' units - time interval unit
#' stats - descriptive statistics of the estimated time intervals of the dataset or the groups 
#' dataset - the dataset with the evaluated time intervals, which is added as "Interval" 
#' @import dplyr 
#' @family EDAS
#' @examples
#' edas.datavalidation.api.timeinterval(df,"DateTime")
#' edas.datavalidation.api.timeinterval(df,"DateTime", "ID")
#' 
#' dataset <- data.frame("DateTime"=seq(ISOdate(1910,1,1), ISOdate(1999,1,1), length.out=10),
#' "ID"=letters[seq(from = 1, to =10)])
#' edas.datavalidation.api.timeinterval(dataset, "DateTime")
#' 
#' 
#' dataset2 <- data.frame("DateTime"=seq(ISOdate(1911,1,1), ISOdate(2000,1,1), length.out=10),
#'                       "ID"=letters[seq(from = 1, to =10)])
#'                       mergedDF <- rbind(dataset, dataset2)
#'                       edas.datavalidation.api.timeinterval(mergedDF, "DateTime", "ID")
#' @keywords  internal

edas.datavalidation.api.timeinterval <- function(dataset, 
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
        dataset[,dateTimeColumn] <- EDAS::bems_db_timestamp(dataset[,dateTimeColumn])
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
  interval_units <- attributes(evaluated_dataset$interval)$units
  
  validation_report <- list("units"=interval_units,
                            "stats"=evaluated_stats,
                            "dataset"=evaluated_dataset) 
  return(validation_report)
}