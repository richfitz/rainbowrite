## Should be very easy to use other palettes here.
##
## We reach period at freq * i = 2*pi -- the most important thing is
## that the palette is periodic.
rainbow <- function(freq, i) {
  red   <- sin(freq*i + 0) * 127 + 128
  green <- sin(freq*i + 2 * pi/3) * 127 + 128
  blue  <- sin(freq*i + 4 * pi/3) * 127 + 128
  rbind(red, green, blue)
}

## This is vectorised for a single string, so that's nice.  Won't be
## as slow as it seems like it could be.
rainbow_colour_line <- function(string, bold) {
  if (nchar(string) == 0) {
    string
  } else {
    str <- strsplit(string, NULL)[[1]]
    col <- rainbow(opts$freq, opts$os +
                   (opts$at + seq_along(str)) / opts$spread)
    if (bold) {
      paste(paint(str, col, "bold"), collapse="")
    } else {
      paste(paint(str, col), collapse="")
    }
  }
}

rainbow_colour <- function(string, bold=FALSE) {
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
    len <- nchar(string[[i]]) # only actually needed when i == n
    string[[i]] <- rainbow_colour_line(string[[i]], bold)
    if (i < n) {
      reset_at()
      increment_offset()
    } else if (trailing_newline) {
      reset_at()
      increment_offset()
    } else {
      increase_at(len)
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
##' @param bold Should the text be in bold?
##' @author Rich FitzJohn
##' @export
##' @examples
##' for (i in 1:20) {
##'   lolcat("hello world\n")
##' }
lolcat <- function(..., file="", bold=FALSE) {
  if (file != "") {
    base::cat(..., file=file)
  } else {
    base::cat(rainbow_colour(cat_prepare(...), bold))
  }
}

## NOTE: We should avoid doing anything with domain not null?
##' @export
##' @rdname lolcat
##' @param domain used by translations (not yet supported)
##' @param appendLF logical: should messages given as a character
##' string have a newline appended?  (see \code{\link{message}}).
lolmessage <- function(..., domain=NULL, appendLF=TRUE) {
  base::message(rainbow_colour(paste(..., collapse=""), bold=TRUE),
                domain=domain, appendLF=appendLF)
  if (appendLF) {
    increment_offset()
  }
}

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
