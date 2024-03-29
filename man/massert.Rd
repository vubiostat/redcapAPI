% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/massert.R
\name{massert}
\alias{massert}
\title{Conduct Multiple Assertions}
\usage{
massert(formula, fun, ..., fixed = list())
}
\arguments{
\item{formula}{A one sided formula naming the arguments on which the assertion
will be performed.}

\item{fun}{An assertion function to perform.}

\item{...}{Additional lists. Each argument provided is a named list of \verb{assertion  arguments}. The name of each element in a list should match the name of a
\verb{function argument}. \code{lower = list(var1 = 0, var2 = 10)} sets
the \emph{assertion argument} \code{lower = 0} for \emph{function argument}
\code{var1}; and sets the \emph{assertion argument} \code{lower = 10} for
\verb{function argument} \code{var2}. The \emph{massert arguments} in \code{...}
may themselves be named or unnamed.}

\item{fixed}{A named list of arguments that are fixed across all assertions.}
}
\description{
This documentation attempts to describe arguments to make assertions
on arguments.  In order to prevent confusion, it is imperative to develop some
terminology up front. We will use \emph{function argument} to refer to an argument
of the function for which we are conducting assertions.  We will  use
\emph{assertion argument} to refer to arguments to pass to the assertion function
being applied to a function argument.  Lastly, we will use \emph{massert argument}
to refer to arguments to \code{massert}
}
\details{
Only one assert function may be utilized in each call to \code{massert}.
This allows for all numeric variables to be checked in one call, all logical
variables to be checked in a subsequent call, etc.
}
