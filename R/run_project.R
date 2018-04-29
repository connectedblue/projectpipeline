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
  
  # read the project parameters from the appropriate config file
  params <- .get_project_config(config, location)
  
  # load the packages needed for the project
  .load_packages(params$packages)
  
  # get the handlers for each extension type in the project
  # They should all be stored underneath the config directory
  handlers <- .get_handlers(params$handlers, params$config_dir)
  
  # get project files to handle in order specified
  # by the pipeline parameter in the config
  pipeline_files <- .files_in_pipeline(params)

  # Process the project pipeline one file at a time
  for (f in pipeline_files) {
    if (f$cacheable) {
      # to be implemented
    }
    else {
      do.call(handlers[[f$ext]], list(file=f$file_name, meta=f$meta_data))
    }
  }
}

