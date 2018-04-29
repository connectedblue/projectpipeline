#' Handle R files in a project pipeline
#'
#' This function executes R code in a source file as part of a \code{\link{projectpipeline}} 
#' configuration.
#'
#' @param file   The name of the R source to be executed
#'               
#' @param meta   Not implemented.  Included for compatability with \code{projectpipeline}
#' 
#'
#' @return No value is returned; this function is called for its side effects.
#'
#' @export
#'
#' @seealso \code{\link{run_project}, 
#'
#' @examples
#' library('projectpipeline')
#'
#' \dontrun{run_project()}
pp_handle_r <- function(file, meta=NULL) {
  
  print(file)
  source(file)
  
}


