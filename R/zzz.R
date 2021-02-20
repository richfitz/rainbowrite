##' @importFrom R6 R6Class
##' @importFrom crayon make_style
pkg <- new.env(parent = emptyenv())


default_reset <- function() {
  pkg$default <- lol$new()
  invisible(pkg$default)
}
