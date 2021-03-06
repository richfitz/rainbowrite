% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lolify.R
\name{lolify}
\alias{lolify}
\alias{unlolify}
\title{Make Functions More Colourful}
\usage{
lolify(fun, which = "all")

unlolify(fun)
}
\arguments{
\item{fun}{A function to modify}

\item{which}{Character vector indicating which types of output
generating functions to modify.  Possible values are "cat",
"message" and "print", indicating which functions to modify.
Multiple values are OK (e.g, \code{c("cat", "message")}.  The special
value "all" changes all values and is the default.}
}
\value{
A function with the environment rewritten to use lolifed
versions of output functions
}
\description{
Convert a function to use rainbow-writing functions: replacing
\link{cat} with \link{lolcat}, etc.  This is done by rewriting
environments rather than rewriting code.
}
\details{
The function \code{unlolify} reverses the process.
}
\author{
Rich FitzJohn
}
