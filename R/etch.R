#' Etch
#'
#' @param slate Slate object.
#' @param message Message to log.
#' @param ... Named fields and values.
#' @param app_id Application ID.
#' @param level Logging level.
#' @export
etch <- function(slate, message, ..., level = NULL, app_id = NULL) {

  rc <- slate$rc
  logging_level <- level %||% slate$default_level
  app_id <- app_id %||% slate$default_app_id
  key <- app_id_to_key(app_id)

  # message <- glue::glue(message)

  l <- list(...)

  if (length(l)) {
    if (is.null(names(l)) || any(nchar(names(l)) == 0)) {
      stop("All fields must be named.", call. = FALSE)
    }

    l <- c(rbind(names(l), l)) %>%
      unlist(recursive = FALSE)
  }

  rc$command(c(
    "XADD", key, "*",
    "level", logging_level,
    "message", message,
    l
  ))
  invisible(slate)
}

#' @export
etch_debug <- function(slate, message, ..., app_id = NULL) {
  message <- glue::glue(message, .envir = parent.frame(1))
  etch(slate, message, ..., level = "DEBUG", app_id = app_id)
}

#' @export 
etch_info <- function(slate, message, ..., app_id = NULL) {
  message <- glue::glue(message, .envir = parent.frame(1))
  etch(slate, message, ..., level = "INFO", app_id = app_id)
}

#' @export 
etch_warn <- function(slate, message, ..., app_id = NULL) {
  message <- glue::glue(message, .envir = parent.frame(1))
  etch(slate, message, ..., level = "WARN", app_id = app_id)
}

#' @export 
etch_error <- function(slate, message, ..., app_id = NULL) {
  message <- glue::glue(message, .envir = parent.frame(1))
  etch(slate, message, ..., level = "ERROR", app_id = app_id)
}