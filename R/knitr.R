##' Hook for use with knitr. Must have knitr and fansi installed to
##' use. Must be within a block that has `results = "asis"` (though
##' may be `echo = FALSE)` as we output some style.
##'
##' @title Hook for knitr
##'
##' @param background Colour to use for the background (any valid CSS
##'   specification). The default is quite dark grey, which works well
##'   with the default colour theme.
##'
##' @param plain Colour to use for unstyled text; practically this
##'   is any knitr pre-output prompt like `#>`. Should contrast
##'   against `background`. The default is very light grey.
##'
##' @return Invisibly, an argument-less function that will restore the
##'   previous state before running this function.
##' @export
lol_hooks <- function(background = "#222222", plain = "#eeeeee") {
  oo <- options(crayon.enabled = TRUE, crayon.colors = 256)
  crayon::num_colors(forget = TRUE)
  prev_hooks <- fansi::set_knit_hooks(
    knitr::knit_hooks,
    style = c(
      sprintf("PRE.fansi {background-color: %s;}", background),
      sprintf("PRE.fansi CODE {color: %s; background-color: transparent;}",
              plain)))
  reset <- function() {
    options(oo)
    crayon::num_colors(forget = TRUE)
    do.call(knitr::knit_hooks$set, prev_hooks)
  }
  invisible(reset)
}
