#' Create a Slate
#' @param path Unix domain socket.
#' @param app_id App ID.
#' @export
slate <- function(path, default_app_id = NULL, default_level = NULL) {
  default_app_id <- default_app_id %||% paste0("R_", Sys.getpid())
  default_level <- default_level %||% "INFO"
  rc <- redux::hiredis(path = path)
  rc_id <- rc$command(c("CLIENT", "ID"))

  new_slate(
    list(
      rc = redux::hiredis(path = path),
      rc_id = rc_id,
      default_app_id = default_app_id,
      default_level = default_level
    )
  )
}

#' Modify Default Logging Settings
#' 
#' @param slate A slate object.
#' @param app_id App ID.
#' @param level Default level.
#' 
#' @export
chisel <- function(slate, default_app_id = NULL, default_level = NULL) {
  if (!is.null(default_app_id)) {
    slate$default_app_id <- default_app_id
  }

  if (!is.null(default_level)) {
    slate$default_level <- default_level
  }

  slate
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
  cat(glue::glue("Slate {crayon::silver('redis_client_id=')}{x$rc_id}",
                 "{crayon::silver('path=')}{x$rc$config()$path}",
                 "{crayon::silver('default_level=')}{x$default_level}",
                 "{crayon::silver('default_app_id=')}{x$default_app_id}",
                 .sep = " "))
}
