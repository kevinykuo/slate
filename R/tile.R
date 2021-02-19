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
  rc$command(c("XRANGE", key, start, end)) %>%
    process_stream_output(app_id = app_id)
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

id_to_timestamp <- function(x) {
  milliseconds_since_epoch <- as.numeric(substr(x, 1, 13))
  as.POSIXct(milliseconds_since_epoch / 1000, origin = "1970-01-01")
}

process_stream_output <- function(x, app_id) {
  if (!length(x)) {
    return(invisible(NULL))
  }

  dt <- x %>%
    lapply(function(x) x[[2]]) %>%
    lapply(function(x) {
      len <- length(x)
      nms_ind <- seq(1, len - 1, by = 2)
      setNames(x[-nms_ind], x[nms_ind]) %>%
        data.table::as.data.table()
    }) %>%
    data.table::rbindlist(fill = TRUE)

  data.table::set(dt, i = NULL, j = "id", value = sapply(x, function(x) x[[1]]))
  data.table::set(dt, i = NULL, j = "timestamp", value = id_to_timestamp(dt$id))
  data.table::set(dt, i = NULL, j = "app_id", value = app_id)
  new_slate_tile(dt, app_id = app_id, last_id = tail(dt$id, 1))
}

new_slate_tile <- function(x, ...) {
  structure(
    x,
    ...,
    class = c("slate_tile", class(x))
  )
}

#' @export
print.slate_tile <- function(x, n = 100, ...) {

  nrows <- nrow(x)

  if (!nrows) {
    return(invisible(NULL))
  }

  x <- head(x, n)

  # browser()
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
    l <- l[!is.na(l)]

    optional_output <- if (length(l)) {
      paste0(crayon::silver(paste0(names(l), "=")), l, collapse = " ")
    } else {
      ""
    }

    glue::glue(required_output_header, optional_output, .sep = " ")

  }) %>%
    cat(sep = "\n")

  if (nrows > n) {
    cat("  ... ", nrows - n, " records omitted")
  }
}

colorize_level <- function(lvl) {
  switch(lvl,
         INFO = crayon::green("INFO"),
         WARN = crayon::yellow("WARN"),
         ERROR = crayon::red("ERROR"))
}
