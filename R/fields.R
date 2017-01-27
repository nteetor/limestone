#' Create a Form Field
#'
#' @keywords internal
#' @export
field <- function(name, validate) {

}

.__field__ <- R6::R6Class(
  'field',
  public = list(
    `__widget` = NULL,
    initialize = function(widget = NULL) {
      self[['__widget']] <- widget %||% text_input
      invisible(self)
    },
    validate = function() {
      NULL
    }
  )
)
