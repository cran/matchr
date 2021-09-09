#' Match Value Against Multiple Values
#'
#' Functional programming style matching using \code{->} to separate
#' conditions from associated return values.
#'
#' @param x object to match
#' @param ... conditions used for matching, separated from the returned
#' value by \code{->}. Multiple conditions can be associated with the
#' same return value using \code{|}. Each matching statement must be
#' separated by a comma. See "Details" below. Use \code{.} to represent
#' the default *(else ...)* condition.
#'
#' @details
#' Unlike \code{\link{switch}}, \code{Match} accepts a variety of
#' different condition statements. These can character, numeric,
#' or logical values, functions, symbols, language objects, enums,
#' etc. For example, \code{"hello" -> 1} tests whether the object is
#' equal to \code{"hello"}. If so, the function returns \code{1},
#' otherwise the next condition is tested. \code{<-} can also be used.
#' If so, the condition & return expression are reversed: \code{1 <- "hello"}
#' also tests \code{"hello"} and returns \code{1}.
#'
#' Each condition is tested sequentially by calling the appropriate method
#' of \code{\link{match_cond}}. If the condition is a character value, then
#' \code{match_cond.character} is called, and so on. If a match is
#' confirmed, the right-hand side is evaluated and returned.
#'
#' For atomic vectors - numeric, logical, or character - \code{Match} will
#' check for equality. All resulting values must be \code{TRUE} to match.
#' Lists and environments are checked using \code{identical}. If a
#' function is placed within the condition, then the function will be evaluated
#' on object \code{x}. If the result is logical and \code{TRUE}, then it is
#' considered a match. A non-logical result will be checked again using
#' \code{\link{match_cond}}. Failed function calls with an error are treated
#' as a non-match rather than stopping \code{Match}. Expressions are evaluated
#' similar to functions.
#'
#' The period \code{.} is a special condition in \code{Match}. When alone,
#' it is treated as the "default" condition that always matches. When used
#' as a call, though, it matches values within object \code{x} and/or attaches
#' the individual items within \code{x} for use in the return expression.
#' For example, \code{x = c(1, 2)} will be matched with the condition
#' \code{.(1, second)}. This is because the first values are identical
#' \code{(1 == 1)}. Furthermore, \code{second = 2} for use in the return
#' expression. Preface a symbol with \code{..} to evaluate it and check
#' for equality. \code{...} can be used to denote any number of unspecified
#' objects.
#'
#' The period call \code{.()} can also be used to test named member of \code{x},
#' though all objects in \code{.()} must be named to do so. For example, the
#' condition \code{.(a = 5, b=)} tests whether \code{x} contains \code{"a"}
#' with a value of \code{5} and \code{"b"} with any value.
#'
#' If \code{function(...)} is used on the left hand side, then it may
#' need to be surrounded by parentheses for the parser to properly recognize
#' it. The \code{\%fn\%} infix function has be provided as syntactic sugar
#' for developing functions for matching.
#'
#' Similar to many functional languages, \code{(first:rest)} can be used
#' as a condition to extract the first element and the rest from any
#' vector as long as the vector is sufficiently long. Variables used on the
#' left hand side can be called on the right hand side expression.
#'
#' Matching an \code{\link{Enum}} causes symbols to represent possible
#' variants. For example, \code{None -> "none"} would try to match the
#' variant of \code{x} with \code{None}. If it succeeds, then \code{Match}
#' will return \code{"none"}. A function call on the left-hand side for an
#' Enum is treated as a variant and its inside arguments, which are made
#' available in the result expression. So, \code{Some(var) -> sqrt(var)} would
#' attempt to match on the variant \code{Some}. If it matches, then the
#' inside is exposed as the variable \code{var} for the right-hand side to
#' use. The number of objects in the variant on the left-hand side must
#' match the number of objects inside of \code{x} or else an error will
#' populate.
#'
#' Regex conditions can be used when matching strings by surrounding the
#' expression in braces. For example, the condition "{[ab]*}" is equivalent
#' to using \code{grepl("\[ab\]*", ...)}. The braces must be the first and
#' last characters to trigger a regex match.
#'
#' Call \code{\link{fallthrough}} within a return expression to stop evaluating
#' the expression and return to matching. This can be convenient for complex
#' matching conditions or to execute code for side-effects, such as printing.
#'
#' @return an object based on the matched clause. An Error is produced if no
#' match is found.
#' @export
#'
#' @examples
#' ## Matching to functions, characters, regex, and default
#' Match(
#'  "abc",
#'  is.numeric       -> "Not a character!",
#'  is.character     -> {
#'    print("Found a character!")
#'    fallthrough()
#'  },
#'  "a" | "b" | "c"  -> "It's a letter!",
#'  "{bc}"           -> "Contains 'bc'!",
#'  .                -> "Can be anything!"
#' )
#'
#'
#' ## Unwrapping a Result enum
#' val <- Result$Ok("hello world!")
#'
#' Match(
#'   val,
#'   Ok(w)  -> w,
#'   Err(s) -> s
#' )
#'
#' ## Using functions
#' # If 'function' is used on the lhs, surround in '()'
#' # Alternatively, use %fn% notation
#' Match(
#'   1:10,
#'   (function(i) mean(i) < 5)  -> TRUE,
#'   i %fn% (mean(i) >= 5) -> FALSE
#' )
#'
#' ## Extracting parts
#' x <- list(a = 5, b = 6, c = 7)
#' Match(
#'   x,
#'   .(a=, d=2)  -> "won't match, no 'd'",
#'   .(a=5, b=)  -> "will match, a == '5'",
#'   (x:xs)      -> {
#'     print(x)  # 5
#'     print(xs) # list(b=6, c=7)
#'     "will match, since not empty"
#'   },
#'   .           -> "this matches anything!"
#' )
#'
#' z     <- c(1,2,3,4)
#' first <- 1
#' Match(
#'   z,
#'   .(0, ...) -> "no match, first is 1 not 0",
#'   .(1, 2)   -> "no match, z has 4 elements",
#'   .(x, 2, ...) -> paste("match, x = ", x),
#'   .(..first, ...) -> "match, since 'first' == 1"
#' )
#'
#'
Match <- function(x, ...) {
  opts <- format_match_(...)
  for (i in opts) {
    m <- match_cond(i[[2]], x, i[[1]])
    if (m[[1]]){ return(m[[2]]) }
  }
  stop("No match found.")
}


#' Match Each Object in List or Vector
#'
#' Applies \code{\link{Match}} to each individual object within the input rather than matching the entire object.
#'
#' @param x a vector (including list) or expression object
#' @param ... conditions and expressions for matching. See \code{\link{Match}} for details.
#'
#' @details
#' See \code{\link{Match}} for details on condition implementation. Default conditions using the
#' period \code{.} are highly recommended to prevent error.
#'
#' \code{Matchply} is a wrapper to \code{lapply} and \code{sapply}, depending on the input object,
#' with \code{...} converted to a match statement for easy use.
#'
#' @return vector depending on input \code{x}. By default, \code{sapply} is
#' used with \code{simplify = TRUE}. This could return a vector, matrix, list,
#' etc. When \code{simplify = FALSE} or a list is provided, the result will be
#' a list.
#' @export
#'
#' @examples
#' new_list <- list(
#'   hello = "World!",
#'   nice  = 2,
#'   meet  = "u"
#' )
#'
#' Matchply(
#'   new_list,
#'   is.numeric  -> "found a number!",
#'   "{rld}"     -> "maybe found 'World'!",
#'   "u" | "z"   -> "found a letter",
#'   .  -> "found nothing"
#' )
#'
Matchply <- function(x, ...) UseMethod("Matchply")
#' @export
#' @method Matchply list
Matchply.list <- function(x, ...){
  lapply(x, function(val, match){
    for (i in match) {
      m <- match_cond(i[[2]], val, i[[1]])
      if (m[[1]]){ return(m[[2]]) }
    }
    stop("No match found.")
  }, match = format_match_(...))
}
#' @export
#' @method Matchply default
Matchply.default <- function(x, ..., simplify=TRUE, USE.NAMES=TRUE){
  sapply(x, function(val, match){
    for (i in match) {
      m <- match_cond(i[[2]], val, i[[1]])
      if (m[[1]]){ return(m[[2]]) }
    }
    stop("No match found.")
  }, match = format_match_(...), simplify=simplify, USE.NAMES = USE.NAMES)
}




format_match_ <- function(...) {
  opts <- eval(substitute(alist(...)))
  lapply(opts, function(i){
    if (!is.language(i)){ stop("Conditions and Expressions in \"Match\" must be separated by \"->\".") }
    if (as.character(i[[1]]) != "<-"){ stop("Conditions and Expressions in \"Match\" must be separated by \"->\".") }
    return(as.list(i[2:3]))
  })
}

mrepl <- function(pattern, x, ...) {
  if (substr(pattern, 1, 1) == "{") {
    nc <- nchar(pattern)
    if (substr(pattern, nc, nc) == "}") {
      return(grepl(substr(pattern, 2, nc-1), x, ...))
    }
    return(pattern == x)
  }
  pattern == x
}

eval_match <- function(do, ...) {
  x <- eval(do, ...)
  if ("fallthrough" %in% class(x)){ return(FALSE) }
  list(TRUE, x)
}

match_inside <- function(x, cond){
  # TODO: Allow more flexibility in specifying argument conditions
  chars <- as.character(cond)
  vars <- inside(x)
  if (length(chars) != length(vars)){ stop(paste0("Number of objects in variant does not match those specified by the match statement.")) }
  names(vars) = chars
  vars
}


#' Check and Evaluate Match Condition
#'
#' Called by \code{\link{Match}} the check whether a condition matches. Used to create custom methods for matching.
#'
#' @param cond match condition
#' @param x object being matched
#' @param do return expression associated with the condition. If \code{cond} is matched with \code{x}, then \code{do}
#' should be evaluated and returned in a list with \code{TRUE}: \code{list(TRUE, eval(do))}.
#' @param ... arguments passed to evaluation
#'
#' @details
#' See the \code{\link{Match}} details for explanations about provided methods.
#'
#' @return \code{FALSE} if no match, or a list containing \code{TRUE} and the evaluated expression
#' @export
#'
match_cond <- function(cond, x, do, ...) UseMethod("match_cond")
#' @method match_cond default
#' @export
match_cond.default <- function(cond, x, do, ...) FALSE
#' @method match_cond matrix
#' @export
match_cond.matrix  <- function(cond, x, do, ...) {
  if (identical(cond, x)) { return(eval_match(do, ...)) }
  FALSE
}
#' @method match_cond environment
#' @export
match_cond.environment <- function(cond, x, do, ...) {
  if (identical(cond, x)) { return(eval_match(do, ...)) }
  FALSE
}
#' @method match_cond character
#' @export
match_cond.character <- function(cond, x, do, ...) {
  if (!is.character(x)) { return(FALSE) }
  nx <- length(x)
  nc <- length(cond)
  if (nx == 1) {
    if (nc == 1 && mrepl(cond, x)) { return(eval_match(do, ...)) }
  } else if (nc == 1) {
    if (all(sapply(x, function(i) mrepl(cond, i)))) { return(eval_match(do, ...)) }
  } else if (nc == nx) {
    if (all(sapply(1:nc, function(i) mrepl(cond[[i]], x[[i]])))) { return(eval_match(do, ...)) }
  }
  FALSE
}
#' @method match_cond numeric
#' @export
match_cond.numeric <- function(cond, x, do, ...) {
  if (!is.numeric(x)) { return(FALSE) }
  if (all(x == cond)) { return(eval_match(do, ...)) }
  FALSE
}
#' @method match_cond logical
#' @export
match_cond.logical <- function(cond, x, do, ...) {
  if (identical(x, cond)) { return(eval_match(do, ...)) }
  FALSE
}
#' @method match_cond list
#' @export
match_cond.list <- function(cond, x, do, ...) {
  if (identical(x, cond)) { return(eval_match(do, ...)) }
  FALSE
}
#' @method match_cond name
#' @export
match_cond.name <- function(cond, x, do, ...) {
  char <- as.character(cond)
  if (char == "."){ return(eval_match(do, ...)) }
  if (is.enum(x)) {
    if (variant(x) == char){ return(eval_match(do, ...)) }
    if (substr(char, 1, 2) == "..") { return(match_cond(eval(as.symbol(substring(char,3))), x, do, ...)) }
    return(FALSE)
  }
  match_cond(eval(cond), x, do, ...)
}
#' @method match_cond (
#' @export
`match_cond.(` <- function(cond, x, do, ...) {
  cond2 <- cond[[2]]
  if (as.character(cond2[[1]] == ":")) {
    n <- length(x)
    if (n == 0) { return(FALSE) }
    if (n == 1) { chars <- list(x[[1]], list())
    } else { chars <- list(x[[1]], x[2:n]) }
    name_char <- c("", "")
    if (!is.symbol(cond2[[2]])) {
      res <- tryCatch(identical(eval(cond2[[2]]), chars[[1]]), error = function(e) FALSE)
      if (!res){ return(FALSE) }
    } else {
      first_char <- as.character(cond2[[2]])
      if (identical(substr(first_char, 1, 2), "..")){
        res <- tryCatch(identical(get(first_char), chars[[1]]), error = function(e) FALSE)
        if (!res){ return(FALSE) }
      } else { name_char[1] <- first_char}
    }
    if (!is.symbol(cond2[[3]])) {
      res <- tryCatch(identical(eval(cond2[[3]]), chars[[2]]), error = function(e) FALSE)
      if (!res){ return(FALSE) }
    } else {
      sec_char <- as.character(cond2[[3]])
      if (identical(substr(sec_char, 1, 2), "..")){
        res <- tryCatch(identical(get(substring(sec_char, 3)), chars[[2]]), error = function(e) FALSE)
        if (!res){ return(FALSE) }
      } else { name_char[2] <- sec_char}
    }
    names(chars) <- name_char
    wsub <- which(name_char == "")
    nw   <- length(wsub)
    if (nw == 2){ return(eval_match(do, ...)) }
    if (nw == 1){ chars <- chars[-wsub] }
    return(eval_match(do, envir=chars, enclos=parent.frame(), ...))
  }
  tryCatch(match_cond(eval(cond2), x, do, ...), error = function(e) FALSE)
}
#' @method match_cond function
#' @export
match_cond.function <- function(cond, x, do, ...) {
  res <- tryCatch(cond(x), error = function(e) FALSE)
  if (is.logical(res)) {
    if (all(res)) { return(eval_match(do, ...)) }
    return(FALSE)
  }
  match_cond(res, x, do, ...)
}
#' @method match_cond call
#' @export
match_cond.call <- function(cond, x, do, ...) {
  char <- as.character(cond[[1]])
  if (identical(char, "|")) {
    m <- match_cond(cond[[2]], x, do, ...)
    if (m[[1]]) { return(m) }
    return(match_cond(cond[[3]], x, do, ...))
  }
  if (identical(char, ".")) {
    nc <- length(cond)
    nx <- length(x)
    if (nx < nc-1){ return(FALSE)
    } else if (nx >= nc) {
      if (!identical(as.character(cond[[nc]]), "...")){ return(FALSE) }
    }
    if (nc == 1){ return(eval_match(do, ...)) }
    vals <- as.list(cond)[2:nc]
    name_vals <- names(vals)
    if (!is.null(name_vals)){
      name_x <- names(x)
      if (is.null(name_x)){ return(FALSE) }
      for (i in name_vals){
        if (i == ""){ stop("All values in .() must be named or unnamed when matching.") }
        if (i %in% name_x){
          if (vals[[i]] != substitute() && !identical(x[[i]], vals[[i]])) { return(FALSE) }
        }
      }
      return(eval_match(do, ...))
    }
    val_list <- list()
    for (i in 1:(nc-1)) {
      vali <- vals[[i]]
      if (is.symbol(vali)){
        char_val <- as.character(vali)
        if (char_val == "..."){ next }
        if (substr(char_val, 1, 2) == ".."){
          res <- tryCatch(identical(get(substring(char_val, 3)), x[[i]]), error = function(e) FALSE)
          if (!res){ return(FALSE) }
        } else {
          val_list[[char_val]] <- x[[i]]
        }
      } else {
        res <- tryCatch(identical(eval(vali), x[[i]]), error = function(e) FALSE)
        if (!res){ return(FALSE) }
      }
    }
    if (length(val_list) > 0){ return(eval_match(do, envir=val_list, enclos = parent.frame(), ...)) }
    return(eval_match(do, ...))
  }
  if (is.enum(x)){
    if (length(char) == 1 && char == "$") {
      if (!is.null(attr(x, "enum")) && attr(x, "enum") == as.character(cond[[2]]) && attr(x, "variant") == as.character(cond[[3]])){
        return(eval_match(do, ...))
      }
      return(FALSE)
    }
    if (length(char) > 1){
      if (char[[1]] == "$") {
        if (!is.null(attr(x, "enum")) && attr(x, "enum") == char[[2]] && attr(x, "variant") == char[[3]]) {
          vars <- match_inside(x, cond[2:length(cond)])
          return(eval_match(do, envir=vars, enclos = parent.frame(), ...))
        }
        return(FALSE)
      } else { return(tryCatch(match_cond(eval(cond), x, do, ...), error= function(e) FALSE)) }
    }
    if (char %in% attr(x, "variant")) {
      vars <- match_inside(x, cond[2:length(cond)])
      return(eval_match(do, envir=vars, enclos = parent.frame(), ...))
    }
    return(FALSE)
  }
  tryCatch(match_cond(eval(cond), x, do, ...), error= function(e) FALSE)
}


