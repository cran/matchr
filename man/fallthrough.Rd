% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fallthrough.R
\name{fallthrough}
\alias{fallthrough}
\title{Fall Through Match}
\usage{
fallthrough()
}
\value{
Object of class 'fallthrough'
}
\description{
Stop execution of current return expression in \code{\link{Match}}, then continue attempting to match conditions.
}
\examples{

Match(
  "abc",
  is.character -> {
    print("Found a character.")
    fallthrough()
  },
  "abc" -> "start of the alphabet",
  .     -> "found nothing"
)
}
