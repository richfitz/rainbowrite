##' x1Convert a function to use rainbow-writing functions: replacing
##' \code{cat} with \code{lolcat}, etc.  This is done by rewriting
##' environments rather than rewriting code.
##'
##' The function \code{unlolify} reverses the process.
##'
##' @title Make Functions More Colourful
##' @param f A function to modify
##' @param which Character vector indicating which types of output
##' generating functions to modify.  Possible values are \code{"cat"},
##' \code{"message"}, \code{"warning"} and \code{"stop"}, indicating
##' which functions to modify.  Multiple values are OK (e.g,
##' \code{c("cat", "message")}.  The special value \code{"all"}
##' changes all values and is the default.
##' @return A function with the environent rewritten to use lolifed
##' versions of output functions
##' @author Rich FitzJohn
##' @export
lolify <- function(f, which="all") {
  valid <- c("cat", "message", "warning", "stop")
  if (identical(which, "all")) {
    which <- valid
  }
  if (any(!(which %in% valid))) {
    lolstop("Invalid entries:",
            paste(dQuote(setdiff(which, valid)), collapse=", "))
  }
  if (is_lolified(f)) {
    lolwarning("Function is already lolified - not doing anything else")
    return(f)
  }

  e <- new.env(parent=environment(f))
  if ("cat"     %in% which) assign("cat",     lolcat,     e)
  if ("message" %in% which) assign("message", lolmessage, e)
  if ("warning" %in% which) assign("warning", lolwarning, e)
  if ("stop"    %in% which) assign("stop",    lolstop,    e)
  e$.RAINBOWRITE <- TRUE

  environment(f) <- e
  f
}

##' @export
##' @rdname lolify
unlolify <- function(f) {
  if (is_lolified(f)) {
    environment(f) <- parent.env(environment(f))
  } else {
    lolwarning("This function does not look lolified")
  }
  f
}

##' Modify a function in another package to replace calls to
##' \code{cat} (etc) with lolified verisons.
##'
##' @section Warning:
##' This function is pretty evil, as it modifies aready-loaded code.
##' Aside from the general sillyness of the package, this would
##' probably keep the package off CRAN as they take a very dim view of
##' this.
##'
##' @title Lolify Functions in other packages.
##' @param sym a name object or character string, referring to the
##' function to modify (not the function it self, as in
##' \code{\link{lolify}}).
##' @param env The environment in which the function lives.  This
##' might be 'package:foo'
##' @param which As with \code{\link{lolify}}, a character vector
##' indicating the functions to replace.
##' @author Rich FitzJohn
##' @export
lolify_in_environment <- function(sym, env, which="all") {
  modify_in_environment(sym, env, lolify(get(sym, env), which))
}
##' @export
##' @rdname lolify_in_environment
unlolify_in_environment <- function(sym, env, which="all") {
  modify_in_environment(sym, env, unlolify(get(sym, env)))
}

modify_in_environment <- function(sym, env, target) {
  env <- as.environment(env)
  locked <- bindingIsLocked(sym, env)
  if (locked) {
    unlockBinding(sym, env)
    on.exit(lockBinding(sym, env))
  }
  assign(sym, target, pos=env)
  invisible(NULL)
}

is_lolified <- function(f) {
  exists(".RAINBOWRITE", environment(f))
}
