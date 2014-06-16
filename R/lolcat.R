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
  for (i in seq_along(string)) {
    string[[i]] <- rainbow_colour_line(string[[i]])
    opts$os <- opts$os + 1
  }
  paste(string, collapse="\n")
}

##' Replacement for cat
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
lolmessage <- function(..., domain=NULL, appendLF=TRUE) {
  base::message(rainbow_colour(...), domain=domain, appendLF=appendLF)
}

## NOTE: Not sure how noBreaks. should be treated.
lolwarning <- function(..., call.=TRUE, immediate.=FALSE,
                       noBreaks.=FALSE, domain=NULL) {
  base::message(rainbow_colour(...),
                call.=call., immediate=immediate.,
                noBreaks.=FALSE, domain=domain)
}

lolstop <- function(..., call.=TRUE, domain=NULL) {
  base::message(rainbow_colour(...), call.=call., domain=domain)
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
