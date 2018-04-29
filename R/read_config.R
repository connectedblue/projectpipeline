# Various internal functions to the package which read different aspects
# of project configuration


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


# load packages
.load_packages <- function(packages) {
  lapply(packages, require, character.only = TRUE)
}


