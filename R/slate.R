#' Create a Slate
#' @param path Unix domain socket.
#' @param config List of config options.
#' @export
slate <- function(path, config = slate_config()) {
  client <- redux::hiredis(path = path)
  client_id <- client$command(c("CLIENT", "ID"))

  new_slate(
    list(
      client = redux::hiredis(path = path),
      client_id = client_id,
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

#' @export
print.slate <- function(x, ...) {
  cat(glue::glue("Slate [Redis Client ID: {x$client_id}, Path: {x$client$config()$path}]"))
}
