`%||%` <- function(a, b) if (is.null(a)) b else a

is_named <- function(x) {
  length(x) > 0 && !any(is.null(x)) && all(names(x) != '')
}

names3 <- function(x) {
  names(x) %||% ''
}
