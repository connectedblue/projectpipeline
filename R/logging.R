# Logging utilities

.start_logging <- function(params) {
  if (.meta_parameter_true(params, "logging")) {
    if(!.meta_parameter_exists(params, "log_file"))
      stop("Logging requested but no log_file specified")
    log_dir <- dirname(params$log_file)
    log_file <- basename(params$log_file)
    if(.meta_parameter_true(params, "daily_log"))
      log_file <- paste0(Sys.Date(), "-", log_file)
    # The place to write log messages is recorded in the Global Environment
    # as a hidden variable
    assign(".current_log_file", file.path(log_dir, log_file), envir=.GlobalEnv)
  }
}


# log wrapper for project pipeline
.pplog_message <- function(log_message) {
  log_message(log_message, "projectpipeline")
}
