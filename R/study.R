#' Study
#'
#' @param slate Slate object.
#' @param start Start time.
#' @param end End time.
#' @param level Logging level (not implemented).
#' @export
study <- function(slate, start = NULL, end = NULL, level = NULL) {
  client <- slate$client
  app_id <- slate$config$app_id
  start <- start %||% "-"
  end <- end %||% "+"
  client$command(c("XRANGE", app_id, start, end)) %>%
    sapply(function(x) {
      milliseconds_since_epoch <- substr(x[[1]], 1, 13) %>%
        as.numeric()
      timestamp <- format(
        as.POSIXct(milliseconds_since_epoch / 1000, origin = "1970-01-01"),
        "%H:%M:%OS3"
      )
      logging_level <- x[[2]][[2]]
      msg <- x[[2]][[4]]

      glue::glue("{timestamp} [{logging_level}] {app_id}: {msg}")
    }) %>%
    cat(sep = "\n")
}
