#' EDAS Analysis Model API - EDAS Analysis Model Initialization Method
#'
#' @importFrom optparse make_option OptionParser
#' @family EDAS
#' @examples
#' edas.newModel()
#' @export
edas.newModel <- function()
{
  if (!requireNamespace("optparse", quietly = TRUE)) {
    stop("optparse package is required!!",
         call. = FALSE)
  }
  option_list = list(
    optparse::make_option(c("-timestamp", "--ts"), type="character", default=NULL, 
                help="timestamp", metavar="character"),
    optparse::make_option(c("-o", "--out"), type="character", default=NULL,
                help="output directory", metavar="character"),
    optparse::make_option(c("-test", "--test"), type="character", default=TRUE, 
                help="output directory", metavar="character")
    
  ); 
  
  opt_parser = optparse::OptionParser(option_list=option_list)
  opt = parse_args(opt_parser)
  
  if (is.null(opt$ts)){
    print_help(opt_parser)
    stop("Timestamp is missing", call.=FALSE)
  } else if (is.null(opt$out)){
    print_help(opt_parser)
    stop("Output directory is not specified", call.=FALSE)
  } 
}




