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
  df <- client$command(c("XRANGE", app_id, start, end)) %>%
    process_stream_output(app_id = app_id)
  df
  # %>%
  #   sapply(function(x) {
  #     milliseconds_since_epoch <- substr(x[[1]], 1, 13) %>%
  #       as.numeric()
  #     timestamp <- format(
  #       as.POSIXct(milliseconds_since_epoch / 1000, origin = "1970-01-01"),
  #       "%H:%M:%OS3"
  #     )
  #     logging_level <- x[[2]][[2]]
  #     msg <- x[[2]][[4]]
  #
  #     glue::glue("{timestamp} [{logging_level}] {app_id}: {msg}")
  #   }) %>%
  #   cat(sep = "\n")
}

#' Stare
#' @export
stare <- function(slate, app_ids = NULL, id = NULL) {
  client <- slate$client
  app_ids <- app_ids %||% slate$config$app_id
  id <- id %||% "$"

  interrupted <- FALSE
  res <- NULL

  while (!interrupted && is.null(res)) {
    res <- tryCatch(
      client$command(c("XREAD", "BLOCK", 1000, "STREAMS", app_ids, id)),
      interrupt = function(x) {
        interrupted <<- TRUE
        NULL
      }
    )
  }


  if (is.null(res)) return(NULL) else {
    res %>%
      lapply(function(x) process_stream_output(x[[2]], app_id = x[[1]]))
  }


}

process_stream_output <- function(x, app_id) {
  x %>%
    lapply(function(x) {
      id <- x[[1]]
      milliseconds_since_epoch <- substr(x[[1]], 1, 13) %>%
        as.numeric()
      timestamp <- as.POSIXct(milliseconds_since_epoch / 1000, origin = "1970-01-01")
      # timestamp <- format(
      #   as.POSIXct(milliseconds_since_epoch / 1000, origin = "1970-01-01"),
      #   "%H:%M:%OS3"
      # )
      logging_level <- x[[2]][[2]]
      msg <- x[[2]][[4]]

      list(id = id, timestamp = timestamp, level = logging_level, app_id = app_id, message = msg)
    }) %>%
    (function(x) do.call(Map, c(f = c, x))) %>%
    as.data.frame()
}

# slate_tile <- function() {
#
# }
#
# new_slate_tile <- function() {
#   structure(
#     x,
#     ...,
#     class = c(class, "slate_tile")
#   )
# }
