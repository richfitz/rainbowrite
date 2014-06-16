## TODO: Much of this might be simplified if we used stringr: it has
## the nice property that str_split and str_join re transitive:
##   str_join(str_split("hello\n", "\n")[[1]], collapse="\n")
##      --> "hello\n"
opts <- new.env(parent=emptyenv())
opts$os <- 1
opts$freq <- 0.1
opts$spread <- 3.0
reset <- function() {
  opts$os <- 1
  opts$freq <- 0.1
  opts$spread <- 3.0
}

## Should be very easy to use other palettes here.
rainbow <- function(freq, i) {
  red   <- sin(freq*i + 0) * 127 + 128
  green <- sin(freq*i + 2 * pi/3) * 127 + 128
  blue  <- sin(freq*i + 4 * pi/3) * 127 + 128
  rbind(red, green, blue)
}

## This is vectorised for a single string, so that's nice.  Won't be
## as slow as it seems like it could be.
rainbow_colour_line <- function(string) {
  if (nchar(string) == 0) {
    string
  } else {
    str <- strsplit(string, NULL)[[1]]
    col <- rainbow(opts$freq, opts$os + seq_along(str) / opts$spread)
    paste(paint(str, col), collapse="")
  }
}

rainbow_colour <- function(string) {
  if (length(string) != 1) {
    stop("Need a single string for now")
  }
  trailing_newline <- last_char(string) == "\n"
  string <- strsplit(string, "\n", fixed=TRUE)[[1]]
  if (trailing_newline) {
    string[[length(string)]] <- paste0(string[[length(string)]], "\n")
  }
  n <- length(string)
  for (i in seq_len(n)) {
    string[[i]] <- rainbow_colour_line(string[[i]])
    if (i < n || trailing_newline) {
      opts$os <- opts$os + 1
    }
  }
  paste(string, collapse="\n")
}

##' Replacement for cat, message, warning and stop.
##'
##' Note that the warning and stop verions don't work very well at the
##' moment, and message does not yet bold the output.
##'
##' @title Taste The Rainbow
##' @param ... Arguments as for \code{\link{cat}}, including
##' \code{sep}, etc.
##' @param file Used to check that we don't put silly output into
##' actual files.  Base \code{cat} will be used when \code{file} is
##' given.
##' @author Rich FitzJohn
##' @export
##' @examples
##' for (i in 1:20) {
##'   lolcat("hello world\n")
##' }
lolcat <- function(..., file="") {
  if (file != "") {
    base::cat(..., file=file)
  } else {
    base::cat(rainbow_colour(cat_prepare(...)))
  }
}

## NOTE: We should avoid doing anything with domain not null?
##' @export
##' @rdname lolcat
##' @param domain used by translations (not yet supported)
##' @param appendLF logical: should messages given as a character
##' string have a newline appended?  (see \code{\link{message}}).
lolmessage <- function(..., domain=NULL, appendLF=TRUE) {
  base::message(rainbow_colour(...), domain=domain, appendLF=appendLF)
}

## NOTE: Not sure how noBreaks. should be treated.
## TODO: The domain bit needs dealing with *before* passing through
## to stop/warning because we won't be able to translate once the
## string has been garbled.
##
## TODO: No highlighting on the stop -- it's possible though that we
## can leak the ascii code here first, though it won't have variation
## in colour horizontally (yup, that will work).
##
## TODO: The call information for warning and stop is wrong because
## we're off by one call.
##' @export
##' @rdname lolcat
##' @param call.,immediate.,noBreaks. Arguments passed through to
##' \code{\link{warning}} and \code{\link{stop}} - these are passed
##' directly through to the underlying function.
lolwarning <- function(..., call.=TRUE, immediate.=FALSE,
                       noBreaks.=FALSE, domain=NULL) {
  base::warning(rainbow_colour(...),
                call.=call., immediate.=immediate.,
                noBreaks.=FALSE, domain=domain)
}

## TODO: The correct way to do this is via tryCatch I think...
##' @export
##' @rdname lolcat
lolstop <- function(..., call.=TRUE, domain=NULL) {
  base::stop(rainbow_colour(...), call.=call., domain=domain)
}

last_char <- function(x) {
  n <- nchar(x)
  substr(x, n, n)
}

## Getting this working in the general case is not easy because of the
## requirement to nail the length requirement.
capture <- function(expr, n=NULL) {
  file <- tempfile()
  sink(file)
  on.exit(sink(NULL))
  eval.parent(substitute(expr))
  sink(NULL)
  on.exit()
  if (is.null(n)) {
    n <- file.info(file)$size
  }
  readChar(file, n)
}

cat_prepare <- function(..., sep=" ", fill=FALSE, labels=NULL) {
  capture(base::cat(..., sep=sep, fill=fill, labels=labels))
}
