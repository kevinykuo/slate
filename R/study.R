#' Study
#'
#' @param slate Slate object.
#' @param start Start time.
#' @param end End time.
#' @export
study <- function(slate, start = NULL, end = NULL) {
  client <- slate$client
  app_id <- slate$config$app_id
  start <- start %||% "-"
  end <- end %||% "+"
  df <- client$command(c("XRANGE", app_id, start, end)) %>%
    process_stream_output(app_id = app_id)
  df %>%
    new_slate_tile()
}

#' Stare
#'
#' @param slate A slate object.
#' @param app_ids Vector of app IDs to watch.
#' @param id Starting ID.
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

#' Ticker
#'
#' @param slate A slate object.
#' @param app_ids Vector of app IDs to watch.
#'
#' @export
ticker <- function(slate, app_ids = NULL) {
  client <- slate$client
  app_ids <- app_ids %||% slate$config$app_id

  while (TRUE) {
    stare(slate = slate, app_ids = app_ids) %>%
      lapply(print)
  }

  invisible(NULL)
}

process_stream_output <- function(x, app_id) {
  x %>%
    lapply(function(x) {
      id <- x[[1]]
      milliseconds_since_epoch <- substr(x[[1]], 1, 13) %>%
        as.numeric()
      timestamp <- as.POSIXct(milliseconds_since_epoch / 1000, origin = "1970-01-01")

      logging_level <- x[[2]][[2]]
      msg <- x[[2]][[4]]

      list(id = id, timestamp = timestamp, level = logging_level, app_id = app_id, message = msg)
    }) %>%
    (function(x) do.call(Map, c(f = c, x))) %>%
    as.data.frame() %>%
    new_slate_tile()
}

new_slate_tile <- function(x, ...) {
  structure(
    x,
    ...,
    class = c("slate_tile", class(x))
  )
}

#' @export
print.slate_tile <- function(x, ...) {
  x$timestamp <- crayon::silver(format(x$timestamp, "%H:%M:%OS3"))

  apply(x, 1, function(l) {
    glue::glue(
      '{l[["timestamp"]]}',
      '{colorize_level(l[["level"]])}',
      '{l[["message"]]}',
      '{crayon::silver("app_id=")}{l[["app_id"]]}',
      .sep = " "
    )
  }) %>%
    cat("\n")
}

colorize_level <- function(lvl) {
  switch(lvl,
         INFO = crayon::green("INFO"),
         WARN = crayon::yellow("WARN"),
         ERROR = crayon::red("ERROR"))
}
