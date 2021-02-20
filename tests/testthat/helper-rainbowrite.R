r6_private <- function(x) {
  environment(x$initialize)$private
}


ignore_output <- function(code) {
  capture.output(code)
  invisible()
}


with_colour <- function(code) {
  with_options(
    list(crayon.enabled = TRUE),
    force(code))
}


surround <- function(x) {
  border <- strrep("*", nchar(x) + 8)
  message(paste(c(border, sprintf("*** %s ***", x), border), collapse = "\n"))
}
