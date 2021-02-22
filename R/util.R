with_options <- function(opts, code) {
  oo <- options(opts)
  on.exit(options(oo))
  force(code)
}


squote <- function(x) {
  sprintf("'%s'", x)
}


apply_colour <- function(text, cols) {
  if (!crayon::has_color()) {
    return(text)
  }
  chars <- strsplit(text, NULL)[[1L]]
  dat <- Map(function(char, col) crayon::make_style(col, colors = 256)(char),
             chars, cols)
  paste(vapply(dat, identity, ""), collapse = "")
}


make_palette <- function(cols) {
  ramp <- grDevices::colorRamp(cols)
  function(p) {
    m <- ramp(p)
    grDevices::rgb(m[, 1], m[, 2], m[, 3], maxColorValue = 255L)
  }
}
