#' EDAS Data Preprocess - Working/Non-Working/Special Days
#' 데이터를 주중/주말/특별한 날로 구분
#'
#' @param dataset 데이터 프레임 (data.frame) 형식의 input 데이터 셋
#' @param datetimeCol POSIXCT 타입의 날짜 시간 값을 나타내는 컬럼 명   
#' @param workingdays 주중을 정의 하는 요일들(1~7) numeric list, 기본값으로 월요일부터 금요일 (1~5)
#' @param specialdays Optional 특별한 날을 정의 하는 character list, 날짜 포맷 yyyy-mm-dd
#' @return 
#' indextable - 데이터 프레임 (type, value), 주중/주말/특별한 날로 데이터가 어떤 값으로 구분되어있는지를 보여줌  
#' workingdays - list 형식의 업무요일 값(들) 
#' nonworkingdays - list 형식의 비업무요일 값(들)
#' specialdays - list 형식의 특별한 날짜(들) 
#' eval - input 데이터 셋(데이터 프레임)에 요일(dayOfweek) 값과 주중/주말/특별한 날을(datetype) 나타내는 값을 
#' 추가한 결과  
#' @family EDAS
#' @examples
#' #generate a sample dataset
#' dataset <- data.frame("Datetime"=seq(ISOdate(1900,1,1), ISOdate(1901,1,1), length.out=20),
#' "x"=seq(1,10,length.out=20))
#' 
#' edas.datapreprocessing.daytype(dataset, "Datetime")
#' 
#' specialdays <- c("2016-01-02", "2016-01-03")
#' edas.datapreprocessing.daytype(dataset, "Datetime", specialdays = specialdays )
#' @export
edas.datapreprocessing.daytype <- function(dataset, 
                                               datetimeCol,
                                               workingdays=c(1,2,3,4,5),
                                               specialdays=NULL)
{
  dataset$dayOfweek <- format(dataset[,datetimeCol], format="%u")
  dataset$datetype <- sapply(dataset$dayOfweek, function(x) ifelse(x %in% workingdays, 1, 2))
  
  if (!is.null(specialdays) && is(specialdays, "Date")) 
    
    dataset[format(dataset[,datetimeCol], format="%Y-%m-%d") %in% as.character(specialdays),]$datetype <- 3
  
  daytypeIndexTable <- data.frame(type=c("workingday", "non-workingday", "specialday"),
                                  value=c(1,2,3))
  
  preprocess_report <- list("indextable"=daytypeIndexTable,
                            "workingdays"=workingdays,
                            "nonworkingdays"=setdiff(1:7,workingdays),
                            "specialdays"=specialdays,
                            "eval"=dataset)
  
  return(preprocess_report)
}
