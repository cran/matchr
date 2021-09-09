#' Is Object an Enum
#'
#' Test whether object has class \code{\link{Enum}}.
#'
#' @param x object to be tested
#'
#' @return \code{TRUE} if \code{x} is an Enum, \code{FALSE} otherwise
#' @export
#'
#' @examples
#' HelloEnum <- Enum(
#'   "HelloEnum",
#'   Hello,
#'   World
#' )
#'
#' # TRUE
#' is.enum(HelloEnum$Hello)
#'
#' # FALSE
#' is.enum(5)
#'
is.enum <- function(x) "Enum" %in% class(x)

#' Check Enum Type
#'
#' Test whether \code{\link{Enum}} is also of class \code{type}.
#'
#' @param x object to be tested
#' @param type character string denoting type to check.
#' @param ... objects passed to methods
#'
#' @return \code{TRUE} if \code{x} has enumerated type \code{type}, \code{FALSE} otherwise
#' @export
#'
#' @examples
#' HelloEnum <- Enum(
#'   "HelloEnum",
#'   Hello,
#'   World
#' )
#'
#' # TRUE
#' is.enum_type(HelloEnum$Hello, "HelloEnum")
#'
#' # FALSE
#' is.enum_type(HelloEnum$Hello, "Hello")
is.enum_type <- function(x, type, ...) UseMethod("is.enum_type")
#' @method is.enum_type Enum
#' @export
is.enum_type.Enum <- function(x, type, ...) {
  typ <- attr(x, "enum")
  if (!is.null(type)){ return(typ == type) }
  FALSE
}
#' @method is.enum_type EnumGenerator
#' @export
is.enum_type.EnumGenerator <- function(x, type, ...) {
  typ <- attr(x, "enum")
  if (!is.null(type)){ return(typ == type) }
  FALSE
}
#' @method is.enum_type default
#' @export
is.enum_type.default <- function(x, type, ...) stop("Object is not the variant of an enumerated type.")

#' Check Enum Variant
#'
#' Test whether \code{\link{Enum}} is variant \code{variant}.
#'
#' @param x object to be tested
#' @param variant character string denoting variant to check.
#' @param ... objects passed to methods
#'
#' @return \code{TRUE} if \code{x} is enumerated type of variant \code{variant}, \code{FALSE} otherwise
#' @export
#'
#' @examples
#' HelloEnum <- Enum(
#'   "HelloEnum",
#'   Hello,
#'   World
#' )
#'
#' # TRUE
#' is.variant(HelloEnum$Hello, "Hello")
#'
#' # FALSE
#' is.variant(HelloEnum$Hello, "World")
is.variant <- function(x, variant, ...) UseMethod("is.variant")
#' @method is.variant Enum
#' @export
is.variant.Enum <- function(x, variant, ...) {
  attr(x, "variant") == variant
}
#' @method is.variant EnumGenerator
#' @export
is.variant.EnumGenerator <- function(x, variant, ...) {
  attr(x, "variant") == variant
}
#' @method is.variant default
#' @export
is.variant.default <- function(x, variant, ...) stop("Object is not the variant of an enumerated type.")

#' Check if Result is an Err
#'
#' Test whether Result Enum is Ok or an Err.
#'
#' @param x object to be tested
#'
#' @return \code{TRUE} if \code{x} is enumerated type of variant \code{Err}, \code{FALSE} otherwise
#' @export
#'
#' @examples
#' sqrt_big <- function(x) {
#'   if (x > 1000){ return(Ok(sqrt(x))) }
#'   Err("Not large enough!")
#' }
#' x <- sqrt_big(250)
#' is.err(x)  # TRUE
#'
is.err <- function(x) {
  if (!is.enum(x)) return(FALSE)
  if (is.enum_type.Enum(x, "Result")) return(is.variant(x, "Err"))
  FALSE
}
#' Check if Result is Ok
#'
#' Test whether Result Enum is Ok or an Err.
#'
#' @param x object to be tested
#'
#' @return \code{TRUE} if \code{x} is enumerated type of variant \code{Ok}, \code{FALSE} otherwise
#' @export
#'
#' @examples
#' sqrt_big <- function(x) {
#'   if (x > 1000){ return(Ok(sqrt(x))) }
#'   Err("Not large enough!")
#' }
#' x <- sqrt_big(250)
#' is.ok(x)  # FALSE
#'
is.ok <- function(x) {
  if (!is.enum(x)) return(FALSE)
  if (is.enum_type.Enum(x, "Result")) return(is.variant(x, "Ok"))
  FALSE
}

#' Check if Option is Some
#'
#' Test whether Option Enum is Some or None.
#'
#' @param x object to be tested
#'
#' @return \code{TRUE} if \code{x} is enumerated type of variant \code{Some}, \code{FALSE} otherwise
#' @export
#'
#' @examples
#' x <- 1:5
#' get_n <- function(x, n) {
#'   if (n > length(x)) return(None)
#'   Some(x[n])
#' }
#' obj <- get_n(x, 6)
#' is.some(obj)  # FALSE
is.some <- function(x) {
  if (!is.enum(x)) return(FALSE)
  if (is.enum_type.Enum(x, "Option")) return(is.variant(x, "Some"))
  FALSE
}

#' Check if Option is None
#'
#' Test whether Option Enum is Some or None.
#'
#' @param x object to be tested
#'
#' @return \code{TRUE} if \code{x} is enumerated type of variant \code{None}, \code{FALSE} otherwise
#' @export
#'
#' @examples
#' x <- 1:5
#' get_n <- function(x, n) {
#'   if (n > length(x)) return(None)
#'   Some(x[n])
#' }
#' obj <- get_n(x, 6)
#' is.none(obj)  # TRUE
is.none <- function(x) {
  if (!is.enum(x)) return(FALSE)
  if (is.enum_type.Enum(x, "Option")) return(is.variant(x, "None"))
  FALSE
}


#' Enum Variant
#'
#' Return the variant name of an enumerated type.
#'
#' @param x Enum object
#' @param ... objects passed to methods
#'
#' @return character with the name of the variant or \code{NULL}
#' @export
#'
#' @examples
#' x <- Result$Ok("hello world!")
#' variant(x)  # "Ok"
variant <- function(x, ...) UseMethod("variant")
#' @export
#' @method variant Enum
variant.Enum <- function(x, ...) attr(x, "variant")
#' @export
#' @method variant default
variant.default <- function(x, ...) NULL

#' Enum Type
#'
#' Return the enumerated type name of an object, if a name was provided.
#'
#' @param x Enum object
#' @param ... objects passed to methods
#'
#' @return character with the name of the enumerated type or \code{NULL}
#' @export
#'
#' @examples
#' x <- Result$Ok("hello world!")
#' enum_type(x)  # "Result"
enum_type <- function(x, ...) UseMethod("enum_type")
#' @export
#' @method enum_type default
enum_type.default <- function(x, ...) NULL
#' @export
#' @method enum_type Enum
enum_type.Enum <- function(x, ...) attr(x, "enum")
