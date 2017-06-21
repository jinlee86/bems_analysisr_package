#' EDAS Analysis Model Initialization Method
#'
#' @keywords  analysis
#' @import optparse
#' @family EDAS analysis model API
#' @examples
#' modelInit()
#' @export
modelInit <- function()
{
  if (!requireNamespace("optparse", quietly = TRUE)) {
    stop("optparse package is required!!",
         call. = FALSE)
  }
  option_list = list(
    make_option(c("-timestamp", "--ts"), type="character", default=NULL, 
                help="timestamp", metavar="character"),
    make_option(c("-o", "--out"), type="character", default=NULL,
                help="output directory", metavar="character"),
    make_option(c("-test", "--test"), type="character", default=TRUE, 
                help="output directory", metavar="character")
    
  ); 
  
  opt_parser = OptionParser(option_list=option_list)
  opt = parse_args(opt_parser)
  
  if (is.null(opt$ts)){
    print_help(opt_parser)
    stop("Timestamp is missing", call.=FALSE)
  } else if (is.null(opt$out)){
    print_help(opt_parser)
    stop("Output directory is not specified", call.=FALSE)
  } 
 
  BEMSRAPI::initModelOutputs()  
}

#' EDAS Analysis Model - Show Message 
#'
#' @keywords  analysis
#' @import optparse
#' @family BEMS analysis model API
#' @examples
#' initModels()
#' @export
initModelOutputs <- function(){
  if (length( intersect(search(), "EDAS_ANALYSIS_MODEL_PLOTS") ) > 0){
    detach(EDAS_ANALYSIS_MODEL_PLOTS)
  }
  if (length( intersect(search(), "EDAS_ANALYSIS_MODEL_TABLES") ) > 0){
    detach(EDAS_ANALYSIS_MODEL_TABLES)
  }
  if (length( intersect(search(), "EDAS_ANALYSIS_MODEL_MESSAGES") ) > 0){
    detach(EDAS_ANALYSIS_MODEL_MESSAGES)
  }
  EDAS_DB_CONNECTION_OBJECT <- list(.EDAS_DB_CONNECTOR=.EDAS_DB_CONNECTOR, tables=table_list)
  attach(EDAS_DB_CONNECTION_OBJECT, name="EDAS_DB_CONNECTION_OBJECT")
  
  .EDAS_ANALYSIS_PLOT_NAMES <- vector(mode="list")
  .EDAS_ANALYSIS_PLOTS <- vector(mode="list")
  .EDAS_ANALYSIS_MODEL_PLOTS <- list(Names=.EDAS_ANALYSIS_PLOT_NAMES, Plots=.EDAS_ANALYSIS_PLOTS)
  attach(.EDAS_ANALYSIS_MODEL_PLOTS, name="EDAS_ANALYSIS_MODEL_PLOTS")

  .EDAS_ANALYSIS_MESSAGES <- vector(mode="list")
  .EDAS_ANALYSIS_MODEL_MESSAGES <- list(Messages=.EDAS_ANALYSIS_MESSAGES)
  attach(.EDAS_ANALYSIS_MODEL_MESSAGES, name="EDAS_ANALYSIS_MODEL_MESSAGES")
  
  .EDAS_ANALYSIS_TABLE_NAMES <- vector(mode="list")
  .EDAS_ANALYSIS_TABLES <- vector(mode="list")
  .EDAS_ANALYSIS_MODEL_TABLES <- list(Names=.EDAS_ANALYSIS_TABLE_NAMES, Plots=.EDAS_ANALYSIS_TABLES)
  attach(.EDAS_ANALYSIS_TABLES, name="EDAS_ANALYSIS_MODEL_TABLES")
}


#' EDAS Analysis Model - Show Message 
#'
#' @keywords  analysis
#' @import optparse
#' @family EDAS analysis model API
#' @examples
#' saveMsg("Hello! World!")
#' @export
registerMessage <- function(msg)
{
  
}

#' EDAS Analysis Model - Show Plots 
#'
#' @keywords  analysis
#' @import optparse
#' @family EDAS analysis model API
#' @examples
#' modelInit()
#' @export
registerPlot <- function(plot, plotName=NA, replace=FALSE){
  
}

#' EDAS Analysis Model - Register Tables
#'
#' @keywords  analysis
#' @import optparse
#' @family EDAS analysis model API
#' @examples
#' modelInit()
#' @export
registerTable <- function(df, tableName=NA, replace=FALSE){
  
}


