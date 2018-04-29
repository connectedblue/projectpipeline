#' Load packages and execute files for a project.
#'
#' This function automatically loads all of the packages and executes all
#' the relevant source files used by the project from which it is called.  
#' The behaviour is be controlled the settings in the \code{\link{project_config}} 
#' configuration.
#'
#' @param config The name of the configuration file to use for this run.  The value is
#'               set to \code{<name>} for it to refer to the file located at
#'               \code{config/<name>.config}, relative to the \code{location} parameter
#'               
#' @param location The root directory of the project (ignored if \code{tarball} is specified)
#' 
#' @param tarball The location of a specific zipped version of the project, created using
#'                \code{\link{zip_project}}.  It is unzipped to a temporary location prior
#'                to running
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
#' \dontrun{run_project()}
run_project <- function(config = 'default',
                        location = getwd(),
                        tarball = NULL) {
  
  # location of config is hard-coded:  it must always be a subdirectory called
  # config from the top level location of the project.
  # Similarly the config file itself must have the suffix .config
  config_dir <- file.path(location, 'config')
  config_file <- file.path(config_dir, paste0(config, '.config'))
  
  # read the config file
  params <- .get_project_config(config_file)
  
  # load the packages needed for the project
  .load_packages(params$packages)
  
  # get the handlers for each extension type in the project
  # They should all be stored underneath the config directory
  handlers <- .get_handlers(params$handlers, config_dir)
  
  # get project files to handle in order specified
  # by the pipeline parameter in the config
  project_files <- .files_in_pipeline(params)

  # Process the project pipeline one file at a time
  for (f in project_files) {
    if (f$cacheable) {
      # to be implemented
    }
    else {
      do.call(handlers[[f$ext]], list(file=f$file_name, meta=f$meta_data))
    }
  }
  
}


# read the config file
# Input:  
#    config_file -- text string containing the location of the config file
#                   File is in a DCF format
# Output:
#    params -- a list of parameters read from the config file
.get_project_config <- function (config_file){
  
}


.dcf_to_dataframe <- function(dcf) {
  
  settings <- as.data.frame(read.dcf(dcf))
  
  
  settings <- setNames(as.list(as.character(settings)), colnames(settings))
  
  # Check each setting to see if it contains R code or a comment
  for (s in names(settings)) {
    if (grepl('^#', s)) {
      settings[[s]] <- NULL
      next
    }
    value <- settings[[s]]
    r_code <- gsub("^`(.*)`$", "\\1", value)
    if (nchar(r_code) != nchar(value)) {
      settings[[s]] <- eval(parse(text=r_code))
    }
  }
  settings
}


# executes a file defined by a dataframe called file with one row



# load packages
.load_packages <- function(packages) {
  lapply(packages, require, character.only = TRUE)
}