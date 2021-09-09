#' Fall Through Match
#'
#' Stop execution of current return expression in \code{\link{Match}}, then continue attempting to match conditions.
#'
#' @return Object of class 'fallthrough'
#' @importFrom rlang expr eval_bare
#' @export
#'
#' @examples
#'
#' Match(
#'   "abc",
#'   is.character -> {
#'     print("Found a character.")
#'     fallthrough()
#'   },
#'   "abc" -> "start of the alphabet",
#'   .     -> "found nothing"
#' )
fallthrough <- function() {
  if (identical(parent.frame(), .GlobalEnv)){ stop("'fallthrough' cannot be called from the Global environment.") }
  call <- expr(return(structure(list(), class = "fallthrough")))
  eval_bare(call, env = parent.frame())
}
