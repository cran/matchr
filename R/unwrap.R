inside <- function(x) {
  typ <- attr(x, "enum")
  if (is.null(typ)) {
    w <- !(class(x) %in% "Enum")
  } else {
    w <- !(class(x) %in% c("Enum", typ))
    attr(x, "enum") <- NULL
  }
  class(x) <- class(x)[w]
  attr(x, "variant") <- NULL
  x
}

#' Extract the Value Contained in Enum
#'
#' Returns the value contained inside of an enum variant. The function strips all relevant attributes from the
#' object, returning its bare value.
#'
#' @param x Enumerated value to unwrap
#' @param alt Alternative value to be returned in case of failure
#' @param ... objects to be passed to methods.
#'
#' @details
#' \code{unwrap} is used to extract the inside objects of an \link{Enum}. Unless the Enum was assigned a specific
#' value, the returned value will be a list with names equal to those in the Enum declaration.
#'
#' \code{\link{Result}} and \code{\link{Option}} have associated \code{unwrap} methods that automatically
#' call an error and stop execution if the variant is either \code{Err(e)} or \code{None}, respectively.
#' \code{unwrap_or} allows the user to specify an alternative value in case of failure on the part of
#' \code{Result} or \code{Option}.
#'
#' @return an object of any class.
#' @export
#'
#' @examples
#' Color <- Enum(
#'   "Color",
#'   Black = c(0,0,0),
#'   Red   = c(255,0,0),
#'   Green = c(0, 255, 0),
#'   Blue  = c(0, 0, 255),
#'   White = c(255, 255, 255)
#' )
#'
#' red_rgb <- unwrap(Color$Red)
#' blue    <- rev(red_rgb)
#' blue
#'
#' new_err <- Err("hello world!")
#' unwrap_or(new_err, "this is not an error")
#'
unwrap <- function(x, ...) UseMethod("unwrap")
#' @method unwrap Enum
#' @export
unwrap.Enum <- function(x, ...) inside(x)
#' @method unwrap Option
#' @export
unwrap.Option <- function(x, ...) {
  if (variant(x) == "Some") { return(inside(x)) }
  stop("Option could not be unwrapped. No object found.")
}
#' @method unwrap Result
#' @export
unwrap.Result <- function(x, ...) {
  if (variant(x) == "Ok") { return(inside(x)) }
  stop(paste0("Result could not be unwrapped. Object contained Err.\n", inside(x)))
}

#' @describeIn unwrap Extract the inside of Enum. If variant is 'Err' or 'None', the alternative is returned.
#' @export
unwrap_or <- function(x, alt, ...) UseMethod("unwrap_or")
#' @method unwrap_or Enum
#' @export
unwrap_or.Enum <- function(x, alt, ...) inside(x)
#' @method unwrap_or Option
#' @export
unwrap_or.Option <- function(x, alt, ...) {
  if (variant(x) == "Some") { return(inside(x)) }
  alt
}
#' @method unwrap_or Result
#' @export
unwrap_or.Result <- function(x, alt, ...) {
  if (variant(x) == "Ok") { return(inside(x)) }
  alt
}
#' @method unwrap_or default
#' @export
unwrap_or.default <- function(x, alt, ...) alt


#' Extract Result or Return
#'
#' Returns the value contained inside of an \link{Result} or \link{Option} Enum or returns if failure.
#'
#' @param x Enumerated value of type \link{Result} or \link{Option} to unwrap
#' @param ... objects to be passed to methods.
#'
#' @details
#' This is similar to \code{\link{unwrap}} for \link{Result} and \link{Option} objects. However,
#' an \code{Err} or \code{None} variant does not cause execution to stop. Instead, the parent
#' function immediately returns the Enum intact. Inspired by the \code{?} operator in Rust.
#'
#' @return an object of any class or \code{x} if failure.
#' @name bang
#'
#' @examples
#' is_big <- function(x) {
#'   if (x > 10) return(Ok(x))
#'   Err("This is small!")
#' }
#'
#' # If 'x' is greater than 10, the value will be printed.
#' # Otherwise, an error is returned.
#' print_big <- function(x) {
#'   print(!is_big(x))
#' }
#'
NULL


#' @describeIn bang Unwrap Result if Ok, otherwise return the Err variant in the parent function.
#' @importFrom rlang expr eval_bare `!!`
#' @export
`!.Result` <- function(x, ...) {
  if (variant(x) == "Ok"){ return(inside(x)) }
  if (identical(parent.frame(), .GlobalEnv)){ stop("Object cannot be returned from the Global Environment.") }
  call <- expr(return(`!!`(x)))
  eval_bare(call, env = parent.frame())
}
#' @describeIn bang Unwrap Option if Some, otherwise return the None variant in the parent function.
#' @importFrom rlang expr eval_bare
#' @export
`!.Option` <- function(x, ...) {
  if (variant(x) == "Some"){ return(inside(x)) }
  if (identical(parent.frame(), .GlobalEnv)){ stop("Object cannot be returned from the Global Environment.") }
  call <- expr(return(matchr::None))
  eval_bare(call, env = parent.frame())
}


