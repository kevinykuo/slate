#' Study
#'
#' @param slate Slate object.
#' @param app_id App ID.
#' @param start Start time.
#' @param end End time.
#' @export
study <- function(slate, app_id = NULL, start = NULL, end = NULL) {
  client <- slate$client
  app_id <- app_id %||% slate$app_id
  key <- app_id_to_key(app_id)
  start <- start %||% "-"
  end <- end %||% "+"
  df <- client$command(c("XRANGE", key, start, end)) %>%
    process_stream_output(app_id = app_id)
  df %>%
    new_slate_tile()
}

#' Stare
#'
#' @param slate A slate object.
#' @param app_id Vector of app IDs to watch.
#' @param id Starting ID.
#' @export
stare <- function(slate, app_id = NULL, last_id = NULL) {
  client <- slate$client
  app_id <- app_id %||% slate$default_app_id
  last_id <- last_id %||% rep("$", length(app_id))

  keys <- app_id_to_key(app_id)

  if (!identical(length(keys), length(last_id))) {
    stop("`app_id` and `last_id` must have the same length.", call. = FALSE)
  }

  interrupted <- FALSE
  res <- NULL

  while (!interrupted && is.null(res)) {
    res <- tryCatch(
      client$command(c("XREAD", "BLOCK", 100, "STREAMS", keys, last_id)),
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
ticker <- function(slate, app_id = NULL) {
  client <- slate$client
  app_id <- app_id %||% slate$default_app_id

  while (TRUE) {
    stare(slate = slate, app_id = app_id) %>%
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

      names_idx <- seq(1, length(x[[2]]) - 1, by = 2)
      l <- setNames(x[[2]][-names_idx], x[[2]][names_idx])
      # logging_level <- x[[2]][[2]]
      # msg <- x[[2]][[4]]

      c(list(id = id, timestamp = timestamp, app_id = app_id), l)
        # list(id = id, timestamp = timestamp, level = logging_level, app_id = app_id, message = msg)
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
  if (!nrow(x)) {
    return(invisible(NULL))
  }

  x$timestamp <- crayon::silver(format(x$timestamp, "%H:%M:%OS3"))

  apply(x, 1, function(l) {
    required_output_header <- glue::glue(
      '{l[["timestamp"]]}',
      '{colorize_level(l[["level"]])}',
      '{l[["message"]]}',
      .sep = " "
    )

    required_output_footer <- glue::glue(
     '{crayon::silver("app_id=")}{l[["app_id"]]}' 
    )

    l <- l[setdiff(names(l), c("id", "timestamp", "level", "message", "app_id"))]

    optional_output <- if (length(l)) {
      paste0(crayon::silver(paste0(names(l), "=")), l, collapse = " ")
    } else {
      NULL
    }

    c(required_output_header, optional_output, required_output_footer)

  }) %>%
    cat("\n")
}

colorize_level <- function(lvl) {
  switch(lvl,
         INFO = crayon::green("INFO"),
         WARN = crayon::yellow("WARN"),
         ERROR = crayon::red("ERROR"))
}
