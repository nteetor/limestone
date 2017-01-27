#' @rdname inputs
#' @export
text_area <- function() {
  .__text_area__$new()
}

.__text_area__ <- R6::R6Class(
  class = FALSE,
  inherit = .__widget__,
  public = list(
    initialize = function(attrs = NULL) {
      attrs <- attrs %||% list()
      attrs[['cols']] <- attrs[['cols']] %||% '40'
      attrs[['rows']] <- attrs[['rows']] %||% '10'

      super$initialize(attrs)
      invisible(self)
    },
    render = function(attrs = NULL) {
      attrs <- c(self[['__attributes']], attrs)
      format_text_area(attrs)
    }
  )
)