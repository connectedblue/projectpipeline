

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
  
  idx <- 1
  for (directory in pipeline){
    
    # expand pipeline definition to include recursive subdirectories, denoted
    # by a + at the end of the directory name
    recursive <- FALSE
    if(grepl("\\+$", directory)) {
      recursive <- TRUE
      directory <- substr(directory, 1, nchar(directory) -1)
    }
    
    directory <- file.path(params$project_root,directory)
    if(!dir.exists(directory)) next()
    all_files <- list.files(directory, full.names = TRUE, recursive = recursive,
                            include.dirs = FALSE, no.. = TRUE)
    for (f in all_files) {
      file_data <- list()
      file_data$file_name <- f
      file_data$ext <- sub(".*\\.(.*)$", "\\1", f)
      file_data$meta_data <- NULL
      file_data$cacheable <- FALSE  
      pipeline_files[[idx]] <- file_data
      idx <- idx + 1
    }
  }
  pipeline_files
}


