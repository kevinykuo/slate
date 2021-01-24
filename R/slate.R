#' Create a Slate
#' @param path Unix domain socket.
#' @param config List of config options.
#' @export
slate <- function(path, config = slate_config()) {
  new_slate(
    list(
      client = redux::hiredis(path = path),
      config = config
    )
  )
}

slate_config <- function() {
  list(
    app_id = paste0("R_", Sys.getpid()),
    default_level = "INFO"
  )
}

new_slate <- function(x, ..., class = character()) {
  structure(
    x,
    ...,
    class = c(class, "slate")
  )
}
