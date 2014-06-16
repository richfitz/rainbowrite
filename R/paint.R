## Functions to paint up output.  This is based on code from both
## xtermStyle and in the ruby gem "paint".  Eventually more features
## will be supported, but for now it's tailored to what I need for
## rainbow colouration.
##
## A different set of functions will be needed for other destinations;
## for HTML output in particular.

## This is going to be something detectable on startup.
## TODO: Determine what the current target is - see showConnections()
## and try to determine if we should actually colour anything.
mode <- 256

## Reset terminal
NOTHING <- "\033[0m"

##' "Paint" a string by wrapping it in ANSI colour code information.
##'
##' @title Paint String
##' @param string vector of strings to be coloured
##' @param ... Options.  Currently valid options are either a single
##' three-element vector or a three row matrix, both interpreted as
##' red/green/blue.  R's standard recycling rules are used.
##' @author Rich FitzJohn
##' @export
paint <- function(string, ...) {
  options <- list(...)
  if (mode == 0 || length(options) == 0 ||
      length(string) == 0 || nchar(string) == 0) {
    string
  } else {
    paste0(paint_colour(options), string, NOTHING)
  }
}

paint_colour <- function(options) {
  if (length(options) != 1) {
    ## Mostly because recycling is hard.
    stop("Don't allow stacking options yet")
  }

  mix <- NULL

  ## Interesting options:
  ##   Keys to the tables of ASCII things (Ruby does with symbols)
  ##   Keys to colours (numeric scalar, vector)
  ##   RGB as a set of 3 numbers (numeric vector, matrix)
  ##   RGB has hex code (string scalar, vector)
  ## But I think that the different RGB ones, plus some effects, are
  ## all we need.

  ## Dealing with recycling requires sorting out how to combine
  ## different length outputs nicely.  One way forward on that would
  ## be to indicate how many strings we are expecting?
  option <- options[[1]]

  if (is.matrix(option)) {
    if (is.numeric(option) && nrow(option) == 3) {
      mix <- paint_rgb(option[1,], option[2,], option[3,])
      paste0("\033[", mix, "m")
    } else {
      stop("If given as a matrix, must be numeric 3 row")
    }
  } else if (is.numeric(option)) {
    if (length(option) == 3) {
      mix <- paint_rgb(option[[1]], option[[2]], option[[3]])
      paste0("\033[", paste(mix, collapse=";"), "m")
    }
  }
}

paint_rgb <- function(red, green, blue, background=FALSE) {
  if (mode == 8 || mode == 16) {
    bg <- if (background) 4 else 3
    fg <- rgb_like_value(red, green, blue, mode == 16)
  } else {
    bg <- if (background) 48 else 38
    fg <- rgb_value(red, green, blue)
  }
  paste0(bg, fg)
}

## Returns nearest supported 256-color an rgb value, without
## fore-/background information Inspired by the rainbow gem
##
## NOTE: This behaves badly when zero-length input is given, which can
## happen if used programatically.  Detect and avoid by checing if
## string is zero-length?
rgb_value <- function(red, green, blue) {
  gray_possible <- TRUE
  sep <- 42.5
  while (gray_possible) {
    if (red < sep || green < sep || blue < sep) {
      gray <- red < sep && green < sep && blue < sep
      gray_possible <- FALSE
    }
    sep <- sep + 42.5
  }

  if (gray) {
    paste0(";5;", 232 + round((red + green + blue)/33))
  } else {
    ## Nasty bit of workaround to stay vectorised:
    ret <- colSums(floor(6 * rbind(red, green, blue, deparse.level=0) / 256) *
                   c(36, 6, 1))
    paste0(";5;", 16 + ret)
  }
}

rgb_like_value <- function(red, green, blue, background) {
  .NotYetImplemented()
}
