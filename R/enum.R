#' Create Enumerated Type
#'
#' An object inspired by enums in Rust and types in other functional languages.
#'
#' @param ... Symbols specifying the named of the variant, or language call with the names and default values of objects
#' contained within the variant. Other values can be used as long as the variant is named. The first item in
#' \code{...} can optionally be a character string that names Enum.
#'
#' @details
#' The \code{Enum} function creates a list of objects of class "Enum" *or* functions that generate "Enum" objects
#' similar to those found in Rust of similar languages. Symbols or characters passed to \code{Enum} become the new
#' variants. Language objects, i.e. a name followed by parentheses \code{name(...)}, associate the name with the
#' variant and create a function based on the arguments passed in \code{...}. When function is called, the
#' passed arguments are converted into a named list of class "Enum" and associated variant. Like functions, default
#' values can be given to the variants.
#'
#' Variants can be assigned specific values using '\code{=}'. For example, \code{Enum( Hello = "world" )} creates
#' an enum variant named "Hello" with the underlying value of \code{"world"}. If the initial variant is assigned a
#' single numeric value, then subsequent variants are automatically assigned the next highest value if possible,
#' similar to using \code{iota()} in Go. Variant names are not allowed to be numeric values or other non-symbolic
#' values.
#'
#' @return a list of variants or variant generators
#' @export
#'
#' @examples
#' ### Create a Linked List
#'
#' # Node is an enum with two varieties: a link to the next node, and none
#' # 'Node$Some' is a function that accepts two values and generates the enum
#' # variant, while 'Node$Empty' is a variant
#' Node <- Enum(
#'   Some(Val, Next),
#'   Empty
#' )
#'
#' # Initialize an empty linked list, push values to the front
#' new_list <- Node$Empty
#' new_list <- Node$Some(1, new_list)
#' new_list <- Node$Some(2, new_list)
#' new_list
#'
#' # return the head of the list ('car') and tail ('cdr')
#' car <- new_list$Val
#' cdr <- new_list$Next
#'
#'
#' ### RGB Colors
#'
#' # The Color enum is provided with a named type "Color". All
#' # variants will have both "Enum" and "Color" as a class.
#' # Each variant is associated with a specific value.
#' Color <- Enum(
#'   "Color",
#'   Black = c(0,0,0),
#'   Red   = c(255,0,0),
#'   Green = c(0, 255, 0),
#'   Blue  = c(0, 0, 255),
#'   White = c(255, 255, 255)
#' )
#'
#' Color$Red
#'
#' # This will generate an error since it is not a function
#' # Color$Black()
#'
#'
#' ### Directions
#'
#' # This enum creates a sequence of numbers associated with
#' # a particular direction. Enum automatically increments the
#' # values if the initial variant is assigned a single number
#' Direction <- Enum(
#'   North = 1,
#'   East,
#'   South,
#'   West
#' )
#'
#' # This will result in '5' since North is '1' and West is '4'
#' Direction$North + Direction$West
#'
Enum <- function(...) {
  variants <- eval(substitute(alist(...)))
  nm <- names(variants)
  n  <- length(variants)
  if (n == 0) { stop("No variants provided.") }
  if (is.null(nm)){ nm <- rep("", length(variants)) }
  if (is.character(variants[[1]]) && nm[1] == "") {
    mod <- 1
    type  <- variants[[1]]
    if (n < 2) { stop("No variants provided.") }
    var_list <- vector("list", length = n-1)
    nm_list  <- character(n-1)
  } else {
    mod <- 0
    type  <- ""
    var_list <- vector("list", length = n-1)
    nm_list  <- character(n)
  }
  auto    <- FALSE
  autoval <- 0
  for (i in 1:(length(variants)-mod)) {
    variant <- variants[[i+mod]]
    nme <- nm[i+mod]
    if (nme == "") {
      if (is.symbol(variant) || is.character(variant)) {
        variant <- as.character(variant)
        nm_list[i]    <- variant
        if (auto) {
          inside  <- autoval
          autoval <- autoval + 1
        } else {
          inside <- list()
        }
        if (type == "") {
          var_list[[i]] <- structure( inside, class = "Enum", variant = variant)
        } else {
          var_list[[i]] <- structure( inside, class = c(type, "Enum"), variant = variant, enum = type)
        }
      } else if (is.language(variant)) {
        sym <- as.character(variant[[1]])
        nl  <- length(variant)-1
        if (nl == 0){
          # Perhaps change this to be same as "SYM", then continue
          stop(paste0("No objects found for variant '", sym, "'. Consider dropping the '()'."))
        }
        var_names   <- names(variant)
        arg_list    <- lapply(1:nl, function(i) substitute())
        arg_default <- character(0)
        if (is.null(var_names)){
          names(arg_list) <- sapply(variant[2:(nl+1)], function(j){
            if (is.character(j)) { return(j) }
            if (is.symbol(j)) { return(as.character(j)) }
            stop(paste0("Objects in variants can only specified by name."))
          })
        } else {
          arg_names   <- character(nl)
          for (j in 1:nl){
            var <- variant[[j+1]]
            if (var_names[j+1] == ""){
              if (is.symbol(var) || is.character(var)){arg_names[j] <- as.character(var)
              } else { stop("Objects within variant must be given a character or symbol name.") }
            } else {
              arg_names[j]  <- var_names[j+1]
              evaled_var    <- eval(var)
              arg_list[[j]] <- evaled_var
              arg_default   <- c(arg_default, arg_names[j])
            }
          }
          names(arg_list) <- arg_names
        }
        nm_list[i]    <- sym
        var_list[[i]] <- function(...) {
          args <- as.list(as.environment(-1))
          #args <- as.list(match.call(expand.dots = FALSE))[-1]
          ## TODO:: COME BACK HERE!
          #args <- lapply(as.list(match.call(expand.dots = FALSE))[-1], function(i){eval(i, envir = parent.frame(2L))})
          #form <- formals(sys.function())
          atr  <- attributes(sys.function())
          #for (i in atr$default) {
          #  if (is.null(args[[i]])){ args[[i]] <- form[[i]] }
          #}
          attr(args, "variant") = atr$variant
          if (atr$enum == ""){
            class(args) <- "Enum"
          } else {
            attr(args, "enum") <- atr$enum
            class(args) <- c(atr$enum, "Enum")
          }
          return(args)
        }
        formals(var_list[[i]]) <- arg_list
        attributes(var_list[[i]]) <- list(class = "VariantGenerator", variant = sym, enum= type, default = arg_default)
      }
    } else {
      nm_list[i] <- nme
      inside <- eval(variant)
      if (i == 1 && is.numeric(inside) && length(inside) == 1) {
        auto <- TRUE
        autoval <- inside + 1
      } else if (auto && is.numeric(inside) && length(inside) == 1) {
        autoval <- inside + 1
      }
      if (type == ""){
        var_list[[i]] <- structure( inside, class = "Enum", variant = nme )
      } else {
        var_list[[i]] <- structure( inside, class = c(type, "Enum"), variant = nme, enum = type )
      }
    }
  }
  names(var_list) <- nm_list
  class(var_list) <- "EnumGenerator"
  if (type != "") { attr(var_list, "enum") <- type }
  var_list
}


#' @export
print.Enum <- function(x, ...) {
  if (is.null(attr(x, "enum"))){
    print(paste0("Enum<", attr(x, "variant"), ">"), ...)
  } else {
    print(paste0(attr(x, "enum"), "<", attr(x, "variant"), ">"))
  }
}

#' @export
print.EnumGenerator <- function(x, ...) {
  if (is.null(attr(x, "enum"))){
    print(paste0("EnumGenerator: ", paste(names(x), collapse=", ")), ...)
  } else {
    print(paste0("EnumGenerator<", attr(x, "enum"), ">: ", paste(names(x), collapse=", ")), ...)
  }
}

#' @export
print.VariantGenerator <- function(x, ...) {
  if (is.null(attr(x, "enum"))){
    print(paste0("Enum<", attr(x, "variant"), ">(", paste(names(formals(x)), collapse=", "), ")"), ...)
  } else {
    print(paste0(attr(x, "enum"), "<", attr(x, "variant"), ">(", paste(names(formals(x)), collapse=", "), ")"))
  }
}
