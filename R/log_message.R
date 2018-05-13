#' Log a message in the projectpipeline log
#'
#' Allows a script in the projectpipeline to write a bespoke message into the project
#' pipeline log 
#'
#' @param log_message A character string containing the log message
#'               
#' @param module If specified, a string to identify where the message has originated
#'
#' @return No value is returned; this function is called for its side effects.
#'
#' @export
#'
#' @seealso \code{\link{project_config}}, \code{\link{zip_project}}
#'
#' @examples
#' library('projectpipeline')
#'
#' \dontrun{log_message("Application specific message", "Script 4")}
log_message <- function(log_message, module="") {
  if (exists(".current_log_file", envir=.GlobalEnv)) {
    log_message <- paste0(Sys.time(), "\t",module,"\t",  log_message)
    cat(log_message, file=get(".current_log_file", envir=.GlobalEnv), append=TRUE, sep = "\n")
  }
}

