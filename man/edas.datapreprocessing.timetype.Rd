% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edas-datapreprocessing-timetype.R
\name{edas.datapreprocessing.timetype}
\alias{edas.datapreprocessing.timetype}
\title{EDAS Data Preprocess - Working/Non-Working Hours
데이터를 업무 시간과 비업무 시간으로 구분}
\usage{
edas.datapreprocessing.timetype(dataset, datetimeCol, workinghours = 9:18)
}
\arguments{
\item{dataset}{데이터 프레임 (data.frame) 형식의 input 데이터 셋}

\item{datetimeCol}{POSIXCT 타입의 날짜 시간 값을 나타내는 컬럼 명}

\item{workinghours}{업무시간을 정의 하는 list (0~23), 기본값으로 오전 9시부터 오후 6시(포함)까지}
}
\value{
indextable - 데이터 프레임 (type, value), 업무시간과 비업무시간이 어떤 값으로 구분되어있는지를 나타냄 
workinghours - list 형식의 업무시간 값(들) 
nonworkinghours - list 형식의 비업무시간 값(들)
eval - input 데이터 셋(데이터 프레임)에 시간(hours) 값과 업무시간/비업무시간을(timetype) 나타내는 값을 
추가한 결과
}
\description{
EDAS Data Preprocess - Working/Non-Working Hours
데이터를 업무 시간과 비업무 시간으로 구분
}
\examples{
#generate a sample dataset
dataset <- data.frame("Datetime"=seq(ISOdate(1900,1,1), ISOdate(1901,1,1), length.out=20),
"x"=seq(1,10,length.out=20))

edas.datapreprocessing.timetype(dataset, "Datetime")
}
