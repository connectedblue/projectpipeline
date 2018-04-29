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

# get the handlers for each extension type in the project
# They should all be stored underneath the config directory

# Gets all the handlers for the project, read from the individual
# files in the handlers vector.
# Input:
#     handlers -- vector of handler definition files located underneath
#     config_dir -- root place where all the handlers live   
#
# Output:
#     handlers -- a list where the name is the extension file name
#                 and the value is the string name of the handler function
.get_handlers <- function(handler_definitions, config_dir) {
  
  handlers <- list()
  for (h in handler_definitions) {
    handler_file <- file.path(config_dir, h)
    # Add new handler records that don't already exist
    new_handlers <- .read_single_record_dcf(handler_file)
    for (nh in names(new_handlers)) {
      if(is.null(handlers[[nh]])) handlers[[nh]] <- new_handlers[[nh]]
    }
  }
  handlers
}






# Gets a list of files that need to be processed in the right order
# Input:
#    params - the parameters associated with the project
#
# Output:
#   pipeline_files -- a list of files to process for the pipeline
#                     Each entry of the list is a list with
#                     the following attributes:
#                       file_name -- name of file to process
#                       ext  -- extension name of file used to determine
#                               which handler will process the file
#                       meta_data -- a list of handler specific meta data
#                                    used to handle this file.  This is taken
#                                    from a file named file_name.meta in the
#                                    same directory as file_name.  Note that .meta
#                                    is suffixed on the end of the file name including
#                                    the extension, e.g. data.csv.meta contains the
#                                    particular parameters to pass to the csv handler
#                                    for this particular data file.
#                                    NULL if no meta data present for a file
#                       cacheable  -- does this file output variables that will be
#                                     cached?  If TRUE, then additional items are included
#                                     in the list.
#                                     determined by the presence of a file file_name.cache
#                                     in the same directory as file_name

.files_in_pipeline <- function(params) {
  pipeline <- c()
  pipeline_files <- list()
  
  # get the relevant directory for each part of the pipeline
  # order is indicated by the order in the pipeline field, not
  # the order of the parts in the config file
  for (p in params$pipeline) {
    pipeline <- c(pipeline, params[[p]])
  }
  
  for (directory in pipeline){
    
    # expand pipeline definition to include recursive subdirectories, denoted
    # by a + at the end of the directory name
    recursive <- FALSE
    if(grepl("\\+$", directory)) {
      recursive <- TRUE
      directory <- substr(directory, 1, nchar(directory) -1)
    }
    if(!dir.exists(directory)) next()
    all_files <- list.files(directory, full.names = TRUE, recursive = recursive,
                            include.dirs = FALSE, no.. = TRUE)
    for (f in all_files) {
      file_data <- list()
      file_data$file_name <- f
      file_data$ext <- sub(".*\\.(.*)$", "\\1", f)
      file_data$meta_data <- NULL
      file_data$cacheable <- FALSE  
      pipeline_files[[f]] <- file_data
    }
  }
  pipeline_files
}


