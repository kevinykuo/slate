#' Create a Redis Slate
#' @param path Unix domain socket.
#' @param config List of config options.
#' @export
redis_slate <- function(path, config = redis_slate_config()) {
  new_slate(
    list(
      client = redux::hiredis(path = path),
      config = config
    ),
    class = "slate_redis"
  )
}

redis_slate_config <- function() {
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
