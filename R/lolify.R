##' Convert a function to use rainbow-writing functions: replacing
##' [cat] with [rainbowrite::lolcat], etc.  This is done by rewriting
##' environments rather than rewriting code.
##'
##' The function `unlolify` reverses the process.
##'
##' @title Make Functions More Colourful
##'
##' @param fun A function to modify
##'
##' @param which Character vector indicating which types of output
##'   generating functions to modify.  Possible values are "cat",
##'   "message" and "print", indicating which functions to modify.
##'   Multiple values are OK (e.g, `c("cat", "message")`.  The special
##'   value "all" changes all values and is the default.
##'
##' @return A function with the environment rewritten to use lolifed
##'   versions of output functions
##'
##' @author Rich FitzJohn
##' @export
lolify <- function(fun, which = "all") {
  if (is_lolified(fun)) {
    stop("Function is already lolified")
  }

  valid <- list(
    cat = lolcat,
    message = lolmessage,
    print = lolprint)

  if (identical(which, "all")) {
    which <- names(valid)
  }

  if (any(!(which %in% names(valid)))) {
    stop("Invalid value for 'which': ",
         paste(squote(setdiff(which, names(valid))), collapse = ", "))
  }

  e <- new.env(parent = environment(fun))
  for (i in which) {
    e[[i]] <- valid[[i]]
  }
  attr(e, "lolified") <- TRUE

  environment(fun) <- e
  fun
}

##' @export
##' @rdname lolify
unlolify <- function(fun) {
  if (!is_lolified(fun)) {
    stop("This function does not look lolified")
  }
  environment(fun) <- parent.env(environment(fun))
  fun
}


is_lolified <- function(f) {
  isTRUE(attr(environment(f), "lolified"))
}
