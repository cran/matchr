% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match.R
\name{Matchply}
\alias{Matchply}
\title{Match Each Object in List or Vector}
\usage{
Matchply(x, ...)
}
\arguments{
\item{x}{a vector (including list) or expression object}

\item{...}{conditions and expressions for matching. See \code{\link{Match}} for details.}
}
\value{
vector depending on input \code{x}. By default, \code{sapply} is
used with \code{simplify = TRUE}. This could return a vector, matrix, list,
etc. When \code{simplify = FALSE} or a list is provided, the result will be
a list.
}
\description{
Applies \code{\link{Match}} to each individual object within the input rather than matching the entire object.
}
\details{
See \code{\link{Match}} for details on condition implementation. Default conditions using the
period \code{.} are highly recommended to prevent error.

\code{Matchply} is a wrapper to \code{lapply} and \code{sapply}, depending on the input object,
with \code{...} converted to a match statement for easy use.
}
\examples{
new_list <- list(
  hello = "World!",
  nice  = 2,
  meet  = "u"
)

Matchply(
  new_list,
  is.numeric  -> "found a number!",
  "{rld}"     -> "maybe found 'World'!",
  "u" | "z"   -> "found a letter",
  .  -> "found nothing"
)

}
