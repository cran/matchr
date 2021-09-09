#' Create Function
#'
#' Syntactic sugar for creating a single-variable function. Can be conveniently used in \code{\link{Match}} statements.
#'
#' @param lhs symbol used to denote the function argument
#' @param rhs expression that is converted to the function body. \code{rhs} may need to be surrounded
#' by parentheses is other infix operators are used due to precedence rules.
#'
#' @return a function
#' @export
#'
#' @examples
#' Match(
#'   "abc",
#'   is.numeric  -> -1,
#'   i %fn% grepl("bc", i) -> 0,
#'   is.character -> 1
#' )
#'
#' print_sq_log <- i %fn% print(sqrt(log(i)))
#' print_sq_log(10)
#'
`%fn%` <- function(lhs, rhs) {
  lhs <- substitute(lhs)
  rhs <- substitute(rhs)
  if (!is.symbol(lhs) && !is.character(lhs)) { stop("left-hand side must be a symbol") }
  f_list <- list(substitute(), rhs)
  names(f_list) <- c(as.character(lhs), "")
  as.function(f_list)
}

#' Compose Functions
#'
#' Combine two functions into a single function so that the \code{rhs} is called on the arguments first,
#' then the \code{lhs}.
#'
#' @param lhs function to be called second
#' @param rhs function to be called first
#'
#' @return a composed function
#' @export
#'
#' @examples
#' sq_log <- round %.% sqrt %.% log
#'
#' Match(
#'   10:20,
#'   i %fn% (sq_log(i) > 2) ->
#'     "big",
#'   . ->
#'     "small"
#' )
`%.%` <- function(lhs, rhs) {
  structure(
    function(x) {
      lhs <- attr(sys.function(), "lhs")
      rhs <- attr(sys.function(), "rhs")
      lhs(rhs(x))
    },
    lhs_name = paste(as.character(substitute(lhs)), collapse= ""),
    rhs_name = paste(as.character(substitute(rhs)), collapse= ""),
    lhs = lhs,
    rhs = rhs,
    class = c("ComposedFunction", "function")
  )
}

#' @export
print.ComposedFunction <- function(x, ...) {
  print(attr(x, "lhs"))
  cat("\t%.%\n")
  print(attr(x, "rhs"))
}
