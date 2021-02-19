#' Peruse
#'
#' @param slate Slate object.
#' @param app_id App ID.
#' @param start Start time.
#' @param end End time.
#' @export
peruse <- function(slate, app_id = NULL, start = NULL, end = NULL) {
  rc <- slate$rc
  app_id <- app_id %||% slate$default_app_id
  key <- app_id_to_key(app_id)
  start <- start %||% "-"
  end <- end %||% "+"
  df <- rc$command(c("XRANGE", key, start, end)) %>%
    process_stream_output(app_id = app_id)
  df %>%
    new_slate_tile()
}

#' Stare
#'
#' @param slate A slate object.
#' @param app_id Vector of app IDs to watch.
#' @param last_id Starting ID.
#' @export
stare <- function(slate, app_id = NULL, last_id = NULL) {
  rc <- slate$rc
  app_id <- app_id %||% get_slate_keys(slate)
  last_id <- last_id %||% rep("$", length(app_id))

  keys <- app_id_to_key(app_id)

  if (!identical(length(keys), length(last_id))) {
    stop("`app_id` and `last_id` must have the same length.", call. = FALSE)
  }

  interrupted <- FALSE
  res <- NULL

  while (!interrupted && is.null(res)) {
    res <- tryCatch(
      rc$command(c("XREAD", "BLOCK", 100, "STREAMS", keys, last_id)),
      interrupt = function(x) {
        interrupted <<- TRUE
        NULL
      }
    )
  }

  if (is.null(res)) return(invisible(NULL)) else {
    res %>%
      lapply(function(x) process_stream_output(x[[2]], app_id = key_to_app_id(x[[1]])))
  }
}

#' Gaze 
#'
#' @param slate A slate object.
#' @param app_id Vector of app IDs to watch.
#'
#' @export
gaze <- function(slate, app_id = NULL) {
  app_id <- app_id %||% get_slate_keys(slate)
  # keys <- app_id_to_key(app_id)
  ids <- rep("$", length(app_id))

  interrupted <- FALSE

  app_id_entry_ids <- rlang::env()
  rlang::env_bind(app_id_entry_ids, !!!rlang::set_names(ids, app_id))

  while (!interrupted) {
    tryCatch({
      l <- app_id_entry_ids %>% as.list()

      tiles <- stare(
        slate = slate,
        app_id = names(l),
        last_id = as.character(l)
      )

      lapply(tiles, function(x) {
        app_id_entry_ids[[attr(x, "app_id")]] <- attr(x, "last_id")
        print(x)
      })
    },
      interrupt = function(x) {
        interrupted <<- TRUE
        rlang::interrupt()
        NULL
      }
    )
  }

  invisible(NULL)
}

process_stream_output <- function(x, app_id) {
  if (!length(x)) {
    return(invisible(NULL))
  }

  x %>%
    lapply(function(x) {
      id <- x[[1]]
      milliseconds_since_epoch <- substr(x[[1]], 1, 13) %>%
        as.numeric()
      timestamp <- as.POSIXct(milliseconds_since_epoch / 1000, origin = "1970-01-01")

      names_idx <- seq(1, length(x[[2]]) - 1, by = 2)
      l <- setNames(x[[2]][-names_idx], x[[2]][names_idx])

      c(list(id = id, timestamp = timestamp, app_id = app_id), l)
    }) %>%
    (function(x) {
      df <- do.call(Map, c(f = c, x)) %>%
        as.data.frame()

      last_id <- x[[length(x)]][["id"]]

      new_slate_tile(df, app_id = app_id, last_id = last_id)
    })
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

  x$timestamp <- crayon::silver(format(x$timestamp, "%Y-%m-%d %H:%M:%OS3"))

  apply(x, 1, function(l) {
    required_output_header <- glue::glue(
      '{l[["timestamp"]]}',
      '{colorize_level(l[["level"]])}',
      '[{l[["app_id"]]}]',
      '{l[["message"]]}',
      .sep = " "
    )

    l <- l[setdiff(names(l), c("id", "timestamp", "level", "message", "app_id"))]

    optional_output <- if (length(l)) {
      paste0(crayon::silver(paste0(names(l), "=")), l, collapse = " ")
    } else {
      ""
    }

    glue::glue(required_output_header, optional_output, .sep = " ")

  }) %>%
    cat(sep = "\n")
}

colorize_level <- function(lvl) {
  switch(lvl,
         INFO = crayon::green("INFO"),
         WARN = crayon::yellow("WARN"),
         ERROR = crayon::red("ERROR"))
}
