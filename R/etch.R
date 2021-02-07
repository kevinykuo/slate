#' Etch
#'
#' @param slate Slate object.
#' @param message Message to log.
#' @param ... Named fields and values.
#' @param app_id Application ID.
#' @param level Logging level.
#' @export
etch <- function(slate, message, ..., level = NULL, app_id = NULL) {
  client <- slate$client
  logging_level <- level %||% slate$config$default_level
  app_id <- app_id %||% slate$config$app_id

  l <- list(...)

  if (!is.null(l)) {
    if (is.null(names(l)) || any(nchar(names(l)) == 0)) {
      stop("All fields must be named.", call. = FALSE)
    }

    l <- c(rbind(names(l), l)) %>%
      unlist(recursive = FALSE)
  }

  client$command(c(
    "XADD", app_id, "*",
    "level", logging_level,
    "message", message,
    l
  ))
  invisible(slate)
}
