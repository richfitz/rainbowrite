## Functions to paint up output.  This is based on code from both
## xtermStyle and in the ruby gem "paint".  Eventually more features
## will be supported, but for now it's tailored to what I need for
## rainbow colouration.

## Reset terminal
NOTHING <- "\033[0m"

##' "Paint" a string by wrapping it in ANSI colour code information.
##'
##' @title Paint String
##' @param string vector of strings to be coloured
##' @param ... Options.  Currently valid options are either a single
##' three-element vector or a three row matrix, both interpreted as
##' red/green/blue, or a string with an ANSI effect code ('bright' or
##' 'bold' being the most useful) or a string with a colour name/hex
##' code parseable by R's \code{\link{col2rgb}}.
##' @param background Logical vector indicating if the options apply
##' to the background or the foreground.  If scalar, all options apply
##' to either foreground or background.  Otherwise the i'th element of
##' \code{background} applies to the ith option.  Currently background
##' does not deal well with trailing newlines, with the background
##' colour leaking over to the next line.  Pressing "backspace" seems
##' to reset it though.
##' @param reset Reset the colours when
##' @param normal_is_bright Is the "normal" state bright?  This should
##' be TRUE when using \code{message} in interactive mode.
##' @author Rich FitzJohn
##' @export
paint <- function(string, ..., background=FALSE, reset=TRUE,
                  normal_is_bright=FALSE) {
  options <- list(...)
  if (opts$mode == 0
      || length(options) == 0
      || identical(options, list(NULL))
      || length(string) == 0
      || nchar(string) == 0) {
    string
  } else {
    paste0(paint_colour(options, background), string,
           if (reset) NOTHING,
           if (normal_is_bright) paint_colour(list("bright")))
  }
}

paint_colour <- function(options, background=FALSE) {
  if (!is.list(options)) {
    stop("Options must be a list")
  }

  mix <- vector("list", length(options))
  background <- rep(background, length.out=length(options))

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
  for (i in seq_along(options)) {
    option <- options[[i]]
    if (is.matrix(option)) {
      if (is.numeric(option) && nrow(option) == 3) {
        mix[[i]] <- paint_rgb(option[1,], option[2,], option[3,],
                              background[[i]])
      } else {
        stop("If given as a matrix, must be numeric 3 row")
      }
    } else if (is.numeric(option)) {
      if (length(option) == 3) {
        mix[[i]] <- paint_rgb(option[[1]], option[[2]], option[[3]],
                              background[[i]])
      }
    } else if (is.character(option)) {
      mix[[i]] <- paint_string(option, background[[i]])
    }
  }

  ## I *think* this is doing the right thing...
  codes <- do.call(paste, c(mix, list(sep=";")))
  paste0("\033[", codes, "m")
}

paint_rgb <- function(red, green, blue, background=FALSE) {
  if (opts$mode == 8 || opts$mode == 16) {
    bg <- if (background) 4 else 3
    fg <- rgb_like_value(red, green, blue, opts$mode == 16)
  } else {
    bg <- if (background) 48 else 38
    fg <- rgb_value(red, green, blue)
  }
  paste0(bg, fg)
}

## Returns nearest supported 256-color an rgb value, without
## fore-/background information Inspired by the rainbow gem
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

paint_string <- function(string, background=FALSE) {
  ret <- rep_len(NA_character_, length(string))
  ## Here is the order:
  ##   ANSI_EFFECTS
  ##   X11 colours and hex codes via R's colors() / col2rgb() functions.
  i <- match(string, names(ANSI_EFFECTS))
  j <- !is.na(i)
  if (length(j) > 0) {
    ret[j] <- as.character(unname(ANSI_EFFECTS[i[j]]))
  }

  ## col2rgb will throw an error if a colour name is not valid.
  live <- is.na(ret)
  if (any(live)) {
    col <- col2rgb(string[live])
    ret[live] <- paint_rgb(col[1,], col[2,], col[3,], background)
  }

  ret
}

## Terminal effects - most of them are not supported ;)
## See http://en.wikipedia.org/wiki/ANSI_escape_code
##
## On OS X I see blink and conceal, in addition to the "usually
## supported" cases below.
ANSI_EFFECTS <- c(
  "reset"         = 0L,  "nothing"         = 0L,  # usually supported
  "bright"        = 1L,  "bold"            = 1L,  # usually supported
  "faint"         = 2L,
  "italic"        = 3L,
  "underline"     = 4L,                          # usually supported
  "blink"         = 5L,  "slow_blink"      = 5L,
  "rapid_blink"   = 6L,
  "inverse"       = 7L,  "swap"            = 7L,  # usually supported
  "conceal"       = 8L,  "hide"            = 9L,
  "default_font"  = 10L,
  "font0" = 10L, "font1" = 11L, "font2" = 12L, "font3" = 13L, "font4" = 14L,
  "font5" = 15L, "font6" = 16L, "font7" = 17L, "font8" = 18L, "font9" = 19L,
  "fraktur"       = 20L,
  "bright_off"    = 21L, "bold_off"        = 21L, "double_underline" = 21L,
  "clean"         = 22L,
  "italic_off"    = 23L, "fraktur_off"     = 23L,
  "underline_off" = 24L,
  "blink_off"     = 25L,
  "inverse_off"   = 26L, "positive"        = 26L,
  "conceal_off"   = 27L, "show"            = 27L, "reveal"           = 27L,
  "crossed_off"   = 29L, "crossed_out_off" = 29L,
  "frame"         = 51L,
  "encircle"      = 52L,
  "overline"      = 53L,
  "frame_off"     = 54L, "encircle_off"    = 54L,
  "overline_off"  = 55L
  )

##' Remove formatting from a string
##'
##' @title Remove Formatting From a String
##' @param string A string that was generated by \code{\link{paint}}.
##' @author Rich FitzJohn
##' @export
unpaint <- function(string) {
  gsub('\\e\\[(?:[0-9];?)+m', '', string)
}
