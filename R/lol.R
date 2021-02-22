##' @title lol engine
##'
##' @description Create a `lol` object to pass to [lolcat] and friends
##' @export
lol <- R6::R6Class(
  "lol",
  cloneable = FALSE,

  public = list(
    ##' @description Create a new lol engine
    ##'
    ##' @param rate The rate of colour progression in terms of fraction
    ##'   of the colour palette per horizontal line when `angle = 0`.
    ##'   Increasing values give increasing rate of colour change.
    ##'
    ##' @param angle The angle of the colour change. 0 is horizontal
    ##'   lines only, `pi / 4` (the default) runs the gradient top-left
    ##'  to bottom-right, `pi / 2` changes across columns and so on.
    ##'  Any numeric value is allowed.
    ##'
    ##' @param aspect The assumed aspect ratio of rows to columns (used
    ##'   in the treatment of `angle`).
    ##'
    ##' @param palette The colour palette to use. This should be a vector
    ##'   of hex colours that "wrap". We will interpolate along this
    ##'   vector to select a colour for the output. The default is a
    ##'   rainbow set of colours.
    initialize = function(rate = 0.02, angle = pi / 4, aspect = 3,
                          palette = NULL) {
      private$row <- 0L
      private$col <- 0L
      if (is.null(palette)) {
        palette <- lol_rainbow
      }
      private$palette <- make_palette(palette)
      private$rate_row <- cos(angle) * rate
      private$rate_col <- sin(angle) * rate / aspect
    },

    ##' @description Render text in colour
    ##'
    ##' @param text Character vector to render
    ##'
    ##' @param reset Reset the column on return (i.e, assume text is followed
    ##'   by a newline)
    render = function(text, reset = TRUE) {
      lines <- unlist(strsplit(text, "\n"), FALSE, FALSE)
      n <- length(lines)
      if (n == 0 && reset) {
        self$render_line("")
      } else {
        for (i in seq_along(lines)) {
          lines[[i]] <- self$render_line(lines[[i]], reset || i < n)
        }
      }
      lines
    },

    ##' @description Render a single line (i.e., single string) in colour
    ##'
    ##' @param text Single string to render
    ##'
    ##' @param reset Reset the column on return (i.e, assume text is followed
    ##'   by a newline)
    render_line = function(text, reset = TRUE) {
      nchar <- nchar(text)
      row <- private$row
      col <- private$col + seq_len(nchar) - 1L
      p <- (private$rate_row * row + private$rate_col * col) %% 1
      cols <- private$palette(p)
      if (reset) {
        private$col <- 0L
        private$row <- private$row + 1L
      } else {
        private$col <- private$col + nchar
      }
      ## This exists to support tests
      private$last <- list(row = row, col = col, p = p, reset = reset)
      apply_colour(text, cols)
    },

    ##' @description Set this object as the package default
    set_as_default = function() {
      prev <- pkg$default
      pkg$default <- self
      invisible(prev)
    }
  ),

  private = list(
    row = NULL,
    col = NULL,
    rate_row = NULL,
    rate_col = NULL,
    palette = NULL,
    last = NULL
  ))


lol_rainbow <- c("#80EE11", "#D1AB02", "#FD542E", "#EE117F", "#AB02D1",
                 "#542EFD", "#117FEE", "#02D1AB", "#2EFD54", "#80EE11")
