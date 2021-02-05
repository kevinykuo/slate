#' Etch
#'
#' @param slate Slate object.
#' @param message Message to log.
#' @param app_id Application ID.
#' @param level Logging level.
#' @export
etch <- function(slate, message, app_id = NULL, level = NULL) {
  client <- slate$client
  logging_level <- level %||% slate$config$default_level
  app_id <- app_id %||% slate$config$app_id
  client$command(c(
    "XADD", app_id, "*",
    "level", logging_level,
    "message", message
  ))
  invisible(slate)
}
