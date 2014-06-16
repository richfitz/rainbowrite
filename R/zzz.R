opts <- new.env(parent=emptyenv())
reset <- function() {
  opts$os <- 1
  opts$at <- 0
  opts$freq <- 0.1
  opts$spread <- 3.0
  opts$mode <- detect_mode()
}

lolcat_set_options <- function(freq, spread) {
  if (!missing(freq)) {
    if (!is.numeric(freq)) {
      stop("freq must be numeric")
    }
    opts$freq <- freq
  }
  if (!missing(spread)) {
    if (!is.numeric(spread)) {
      stop("spread must be numeric")
    }
    opts$spread <- spread
  }
}

increment_offset <- function() {
  opts$os <- opts$os + 1L
  invisible(NULL)
}

increase_at <- function(n) {
  opts$at <- opts$at + n
}

reset_at <- function() {
  opts$at <- 0L
}

set_offset <- function(x) {
  opts$os <- x
  invisible(NULL)
}


## This is modified from similar code in testthat:::colourise
detect_mode <- function() {
  rcmd_running <- function() {
    nchar(Sys.getenv("R_TESTS")) != 0
  }
  term <- Sys.getenv("TERM")
  colour_terms <- c("xterm-color", "xterm-256color", "screen",
                    "screen-256color")
  if (rcmd_running() || !any(term %in% colour_terms, na.rm = TRUE)) {
    0L
  } else {
    256L
  }
}

.onLoad <- function(lib, pkg) {
  reset()
}
