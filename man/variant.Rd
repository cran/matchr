% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.R
\name{variant}
\alias{variant}
\title{Enum Variant}
\usage{
variant(x, ...)
}
\arguments{
\item{x}{Enum object}

\item{...}{objects passed to methods}
}
\value{
character with the name of the variant or \code{NULL}
}
\description{
Return the variant name of an enumerated type.
}
\examples{
x <- Result$Ok("hello world!")
variant(x)  # "Ok"
}
