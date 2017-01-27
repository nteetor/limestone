#' HTML Widgets
#'
#' HTML widgets are most commonly used under-the-hood of the common HTML form
#' \code{\link{inputs}}. Custom HTML widgets can be created.
#'
#' @param attrs Widget attributes.
#'
#' @export
widget <- function(attrs = NULL) {
  .__widget__$new(attrs)
}

#' @rdname widget
#' @export
is.widget <- function(x) {
  is.environment(x) && 'widget' %in% x[['__class']]
}

.__widget__ <- R6::R6Class(
  class = FALSE,
  cloneable = FALSE,
  public = list(
    `__attributes` = NULL,
    `__class` = 'widget',
    initialize = function(attrs = NULL) {
      self[['__attributes']] <- attrs %||% list()

      invisible(self)
    },
    render = function() {
      stop_not_implemented()
    }
  )
)
