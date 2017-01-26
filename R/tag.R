#' Create an HTML Tag
#'
#' Internal helper function.
#'
#' @keywords internal
#' @export
tag <- function(element, attributes) {
  self <- new.env(parent = emptyenv())
  self$element <- element
  self$attributes <- attributes

  # self$validate <- function(..., .attr = list()) {
  #   new_attr <- c(list(...), .attr)
  #   if (!is_named(new_attr)) {
  #     FALSE
  #   } else if (!all(names(new_attr) %in% a))
  # }
  call('function', as.pairlist())
}
