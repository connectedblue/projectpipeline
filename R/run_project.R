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
  
  # Initialise logging if configured
  .start_logging(params)
  
  # load the packages needed for the project
  .load_packages(params$packages)
  
  # get the handlers for each extension type in the project
  # They should all be stored underneath the config directory
  handlers <- .get_handlers(params$handlers, params$config_dir)
  
  # get project files to handle in order specified
  # by the pipeline parameter in the config.
  # Only get files for which there exists a handler
  pipeline_files <- .files_in_pipeline(params, handlers)

  # Process the project pipeline one file at a time
  for (f in pipeline_files) {
    if (.meta_parameter_true(f, "cacheable")) {
      # check if we should load from source or cache
      load_from_source <- .is.cache.stale(f$input_vars, f$file_name)
      if (load_from_source) {
        do.call(handlers[[f$ext]], list(file=f$file_name, meta=f))
        .write.cache(f$output_vars, f$input_vars, f$file_name)
        .pplog_message(paste0("Processed and saved to cache: ", f$file_name))
      }
      else {
        # load from cache
        .read.cache(f$output_vars, f$file_name)
        .pplog_message(paste0("Read from cache: ", f$file_name))
      }
    }
    else {
      # load from source
      do.call(handlers[[f$ext]], list(file=f$file_name, meta=f))
      .pplog_message(paste0("Processed file: ", f$file_name))
    }
  }
}


# check if a particular parameter is TRUE or FALSE 
.meta_parameter_true <- function (params, parameter) {
  if(!.meta_parameter_exists(params, parameter)) return (FALSE)
  return (params[[parameter]]) 
}

# check if a particular parameter exists 
.meta_parameter_exists <- function (params, parameter) {
  return(!is.null(params[[parameter]]))
}