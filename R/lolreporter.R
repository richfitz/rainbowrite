##' This can be passed to test_dir and friends to produced rainbow
##' coloured outputs.  It formats the output very simlarly, but
##' slightly differently to \code{SummaryReporter}, which is the
##' default for things like \code{test_dir}.  The differences are: (1)
##' it breaks the string of dots close to the edge of the screen
##' (based on \code{getOption("width")} and (2) it produces a summary
##' saying how many successful and unsuccessful tests were performed.
##'
##' @title A Colourful testthat Reporter
##' @param ... Arguments passed through to \code{SummaryReporter}
##' @export
##' @importClassesFrom testthat Reporter SummaryReporter
##' @import methods
LolReporter <- setRefClass("LolReporter", contains="SummaryReporter",
                           fields=list(
                             count="integer",
                             at="integer",
                             at_context="integer",
                             width="integer"))

labels <- c(1:9, letters, LETTERS)
LolReporter$methods(initialize=function(...) {
  at <<- 0L
  at_context <<- 0L
  callSuper(...)
})
LolReporter$methods(start_reporter=function() {
  count <<- 0L
  callSuper()
})
LolReporter$methods(start_context=function(desc) {
  lolcat(desc, ": ")
  width <<- as.integer(getOption("width") - 4L)
  at_context <<- nchar(desc) + 3L - 1L
  at <<- at_context
})
LolReporter$methods(end_context=function() {
  lolcat("\n")
  rainbowrite:::increment_offset()
})
LolReporter$methods(add_result=function(result) {
  has_tests <<- TRUE
  count <<- count + 1L
  if (at >= width) {
    at <<- at_context
    lolcat(paste0("\n", strrep(" ", at_context)))
  } else {
    at <<- at + 1L
  }

  if (result$passed) {
    lolcat(".", bold=TRUE)
    return()
  }
  failed <<- TRUE
  if (n + 1 > length(labels) || n + 1 > max_reports) {
    cat(paint("F", "red", background=TRUE))
  } else {
    n <<- n + 1L
    result$test <- if (is.null(test)) "(unknown)" else test
    failures[[n]] <<- result
    cat(paint(labels[n], "red", background=TRUE))
  }
})
## I've not really customised the end_reporter method, but I've always
## wanted a total number count with the tests.
LolReporter$methods(end_reporter=function() {
  lolcat(sprintf("%d / %d tests successful", count - n, count))
  callSuper()
})

##' @export
##' @rdname LolReporter-class
lolreporter <- function(...) {
  LolReporter$new(...)
}
