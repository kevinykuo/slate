`%||%` <- function (x, y) {
  if (is.null(x))
    y
  else x
}

app_id_to_key <- function(app_id) {
  paste0("SLATE_", app_id)
}

ensure_severity <- function(lvl) {
  if (!lvl %in% c("DEBUG", "INFO", "WARN", "ERROR")) {
    stop("Log severity level must be one of DEBUG, INFO, WARN, or ERROR.", call. = FALSE)
  }

  lvl
}