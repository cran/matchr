#Result <- Enum(
#  "Result",
#  Ok(x),
#  Err(e)
#)
#save(Result, file = "data/Result.RData")
#
#Option <- Enum(
#  "Option",
#  Some(x),
#  None
#)
#save(Option, file = "data/Option.RData")
#
#None <- Option$None
#save(None, file = "data/None.RData")

#' Result
#'
#' An Enum that mimics Rust's "Result" type. This is used to denote whether
#' a function contained an error without stopping execution and allowing the
#' error result to be unwrapped.
#'
#' @format list with 2 Enum generators
#' \describe{
#'   \item{Ok(x)}{Wrap \code{x} in the 'Ok' variant.}
#'   \item{Err(e)}{Wrap \code{x} in the 'Err' variant.}
#' }
"Result"

#' Option
#'
#' An Enum that mimics Rust's "Option" type. This is used to denote whether
#' a function returned an object or not, rather than returning \code{NULL}.
#'
#' @format list with 1 Enum generators and 1 Enum variant
#' \describe{
#'   \item{Some(x)}{Wrap \code{x} in the 'Some' variant.}
#'   \item{None}{Variant denoting that nothing was returned.}
#' }
"Option"

#' None
#'
#' An Enum variant of \code{Option} used to denote that a function returned
#' no value.
#'
#' @format an empty list of classes \code{"Option"} and \code{"Enum"}
"None"

#' Create an 'Ok' Result
#'
#' Create an Enum variant of \code{Result} used to denote that function did not contain an error.
#' This allows the creation of safer functions.
#'
#' @param x Object to be wrapped in the Enum variant.
#'
#' @return a list with a single value \code{x} and classes \code{"Result} and \code{"Enum}
#' @export
#'
#' @examples
#' grepl_safe <- function(pattern, x)
#' {
#'   if (!is.character(pattern)){ return(Err("'pattern' in 'grepl_safe' was not a character value.")) }
#'   if (!is.character(x)){ return(Err("'x' in 'grepl_safe' was not a character value.")) }
#'   Ok(grepl(pattern, x))
#' }
#'
#' #grepl_safe(123, 1:5)
#'
Ok   <- function(x){
  structure(
    list(
      x = x
    ),
    enum    = "Result",
    variant = "Ok",
    class   = c("Result", "Enum")
  )
}

#' Create an 'Err' Result
#'
#' Create an Enum variant of \code{Result} used to denote that function contained an error.
#' This allows the creation of safer functions that do not automatically stop, without using
#' \code{try} or \code{tryCatch}.
#'
#' @param e Object to be wrapped in the Enum variant.
#'
#' @return a list with a single value \code{e} and classes \code{"Result} and \code{"Enum}
#' @export
#'
#' @examples
#' grepl_safe <- function(pattern, x)
#' {
#'   if (!is.character(pattern)){ return(Err("'pattern' in 'grepl_safe' was not a character value.")) }
#'   if (!is.character(x)){ return(Err("'x' in 'grepl_safe' was not a character value.")) }
#'   Ok(grepl(pattern, x))
#' }
#'
#' #grepl_safe(123, 1:5)
#'
Err  <- function(e){
  structure(
    list(
      e = e
    ),
    enum    = "Result",
    variant = "Err",
    class   = c("Result", "Enum")
  )
}

#' Create an 'Some' Option
#'
#' Create an Enum variant of \code{Option} used to denote that function returned a value.
#' This allows the creation of safer functions that extract values from other objects, without using
#' \code{try} or \code{tryCatch}.
#'
#' @param x Object to be wrapped in the Enum variant.
#'
#' @return a list with a single value \code{x} and classes \code{"Option} and \code{"Enum}
#' @export
#'
#' @examples
#' subset_safe <- function(x, index) {
#'   if (index > length(x)){ return(None) }
#'   Some(x[index])
#' }
#'
Some <- function(x){
  matchr::Option$Some(x)
}


#' @export
#' @importFrom utils capture.output
print.Result <- function(x, ...){
  if (is.ok(x)){ out <- paste0("Ok::\n", paste(capture.output(print(x[["x"]][[1]], ...)), collapse="\n") )
  } else { out <- paste0("Err::\n", paste(capture.output(print(x[["e"]][[1]], ...)), collapse="\n")) }
  cat(out)
}

#' @export
#' @importFrom utils capture.output
print.Option <- function(x, ...){
  if (is.ok(x)){ out <- paste0("Some::\n", paste(capture.output(print(x[["x"]][[1]], ...)), collapse="\n") )
  } else { out <- "None" }
  cat(out)
}

#' Convert Object into Result
#'
#' Create a \code{\link{Result}} out of an object. By default the object is wrapped in an \code{Ok} variant.
#' \code{Some} variants of \code{\link{Option}} are turned into \code{Ok} Results, while \code{None} variants are
#' turned into \code{Err} Results
#'
#' @param x Object to be converted
#' @param ... Objects passed to methods
#'
#' @return an Enum object of class \code{Result}
#' @export
#'
#' @examples
#' nothing <- Option$None
#' into_result(nothing) # Err
#'
into_result <- function(x, ...) UseMethod("into_result")
#' @export
#' @method into_result default
into_result.default <- function(x, ...) structure(list(x = x), enum = "Result", variant = "Ok", class = c("Result", "Enum"))
#' @export
#' @method into_result Result
into_result.Result <- function(x, ...) x
#' @export
#' @method into_result Option
into_result.Option <- function(x, ...){
  if (attr(x, "variant") == "None"){ return(structure(list(e = "No object found."), enum = "Result", variant = "Err", class = c("Result", "Enum"))) }
  attr(x, "variant") <- "Ok"
  attr(x, "enum") <- "Result"
  class(x) <- c("Result", "Enum")
  x
}

#' Convert Object into Option
#'
#' Create an \code{\link{Option}} out of an object. By default the object is wrapped in a \code{Some} variant.
#' \code{Ok} variants of \code{\link{Result}} are turned into \code{Some} Options, while \code{Err} variants are
#' turned into \code{None} Options.
#'
#' @param x Object to be converted
#' @param ... Objects passed to methods
#'
#' @return an Enum object of class \code{Option}
#' @export
#'
#' @examples
#' an_error <- Result$Err("hello world!")
#' into_option(an_error) # None
#'
into_option <- function(x, ...) UseMethod("into_result")
#' @export
#' @method into_option default
into_option.default <- function(x, ...) structure(list(x = x), enum = "Option", variant = "Some", class = c("Option", "Enum"))
#' @export
#' @method into_option NULL
into_option.NULL <- function(x, ...) structure(list(), enum = "Option", variant = "None", class = c("Option", "Enum"))
#' @export
#' @method into_option Option
into_option.Option <- function(x, ...) x
#' @export
#' @method into_option Result
into_option.Result <- function(x, ...){
  if (attr(x, "variant") == "Err"){ return(structure(list(), enum = "Option", variant = "None", class = c("Option", "Enum"))) }
  attr(x, "variant") <- "Some"
  attr(x, "enum") <- "Option"
  class(x) <- c("Option", "Enum")
  x
}


#' Execute Expression as Result
#'
#' Evaluates given expression returning an \code{Err} \link{Result} if there is an error, otherwise an \code{Ok} Result.
#'
#' @param expr expression to evaluate
#'
#' @return Result Enum of variant \code{Ok} or \code{Err}
#' @export
#'
#' @examples
#' # This returns an Err
#' Try(sqrt + 1)
#'
#' # This returns an Ok
#' Try(sqrt(5) + 1)
Try <- function(expr) {
  tryCatch({
    res <- eval(expr)
    structure(list(x = res), enum = "Result", variant = "Ok", class = c("Result", "Enum"))
  },
  error = function(e) structure(list(e = e), enum = "Result", variant = "Err", class = c("Result", "Enum")))
}


