# Functions to manage the cache

'   Functions to be implemented here .....
if (f$cacheable) {
  # check if we should load from source or cache
  load_from_source <- .is.cache.stale(f$input_vars, f$file_name)
  if (load_from_source) {
    do.call(handlers[[f$ext]], list(file=f$file_name, meta=f$meta_data))
    .write.cache(f$output_vars, f$input_vars, f$file_name)
  }
  else {
    # load from cache
    .read.cache(f$output_vars, f$file_name)
  }
  
}  
'


# Deterime whether a pipeline file can be cached or not
# if a filename.cache exists in the same directory, then it is cacheable
# 
# Input:
#    file_data -- a partially complete list that describes the data associated 
#                 with a pipeline file
#
# Output:
#    file_data -- an updated file_data list with the cache information
.add_cacheable_data <- function(file_data) {
  cacheable_info <- paste0(file_data$file_name, '.cache')
  file_data$cacheable <- FALSE
  if (file.exists(cacheable_info)) {
    cacheable_info <- .read_single_record_dcf(cacheable_info)
    for(i in cacheable_info) {
      file_data[[i]] <- cacheable_info[[i]]
    }
  }
  file_data
}