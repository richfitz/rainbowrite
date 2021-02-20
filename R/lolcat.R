##' Replacement for cat, message, and print.
##'
##' @title Taste The Rainbow
##'
##' @param ... Arguments as for [cat], including
##'   `sep`, etc, arguments passed to [message] or extra arguments
##'   to [print]
##'
##' @param file Used to check that we don't put silly output into
##'   actual files.  Base [cat] will be used when `file` is
##'   given
##'
##' @param fill Passed to [cat]
##'
##' @param lol Optional [rainbowrite::lol] engine to use. If omitted
##'   we use the default one registered when the package is loaded
##'
##' @export
##' @examples
##' for (i in 1:20) {
##'   lolcat("hello world\n")
##' }
lolcat <- function(..., file = "", fill = FALSE, lol = NULL) {
  if (file != "") {
    cat(..., file = file)
  } else {
    msg <- utils::capture.output(cat(..., fill = fill))
    reset <- fill || isTRUE(grepl("\n", switch(...length(), ...)))
    cat(render(msg, lol, reset))
  }
}


##' @export
##'
##' @rdname lolcat
##'
##' @param domain used by translations (not yet supported)
##'
##' @param appendLF logical: should messages given as a character
##' string have a newline appended?  (see [message])
lolmessage <- function(..., domain = NULL, appendLF = TRUE, # nolint
                       lol = NULL) {
  msg <- .makeMessage(..., domain = domain, appendLF = appendLF)
  message(render(msg, lol, appendLF), appendLF = FALSE)
}


##' @export
##'
##' @rdname lolcat
##' @param x Object to print
lolprint <- function(x, ..., lol = NULL) {
  msg <- utils::capture.output(print(x, ...))
  cat(render(msg, lol, TRUE))
}


render <- function(x, lol, reset) {
  if (is.null(lol)) {
    lol <- pkg$default
    if (is.null(lol)) {
      lol <- default_reset()
    }
  }
  text <- lol$render(x, reset)
  if (reset) {
    paste0(text, "\n", collapse = "")
  } else {
    paste0(text, collapse = "\n")
  }
}
