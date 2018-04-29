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
#    config -- text string of the name of the config 
#    location -- text string of project root directory
#                   
#    config file is always a specific location relative to
#    the project root
#
# Output:
#    params -- a list of parameters read from the config file
.get_project_config <- function (config, location){
  # location of config is hard-coded:  it must always be a subdirectory called
  # config from the top level location of the project.
  # Similarly the config file itself must have the suffix .config
  config_dir <- file.path(location, 'config')
  config_file <- file.path(config_dir, paste0(config, '.config'))
  config <- .read_single_record_dcf(config_file)
  
  #record the project root and config location also
  config$project_root <- location
  config$config_dir <- config_dir
  config$config_file <- config_file
  
  # if this template inherits another, get the missing values from that file
  if(!is.null(config$inherits)) {
    inherits_file <- file.path(config_dir, paste0(config$inherits, '.config'))
    inherits <- .read_single_record_dcf(inherits_file)
    for (s in names(inherits)) {
      if(is.null(config[[s]])) config[[s]] <- inherits[[s]]
    }
  }
  
  config
}

# utility function to read a single record DCF file, strip out comments, parse
# any comma seperated values into a vector and execute any R code embedded between
# two back ticks `` in the value field
.read_single_record_dcf <- function(dcf_file) {
  
  # read raw file, stripping out comment lines
  dcf <- readLines(dcf_file)
  dcf <- sub("(.*)#.*$","\\1",dcf[!grepl("^#",dcf)])
  tmp_dcf <- tempfile()
  writeLines(dcf, tmp_dcf)
  # Only interested in the first record
  settings <- as.list((read.dcf(tmp_dcf)[1,]))
  file.remove(tmp_dcf)

  # Check each setting to see if it contains a comma separated string or R code  and
  # process accordingly
  for (s in names(settings)) {
    value <- settings[[s]]
    
    r_code <- gsub("^`(.*)`$", "\\1", value)
    if (r_code != value) {
      settings[[s]] <- eval(parse(text=r_code))
      value <- settings[[s]]
    }
    
    if (grepl(",", value)) {
      value <- gsub("\\s", "", value) 
      settings[[s]] <- strsplit(value, ",")[[1]]
    }
  }
  settings
}


# executes a file defined by a dataframe called file with one row



# load packages
.load_packages <- function(packages) {
  lapply(packages, require, character.only = TRUE)
}