#' Create a Form Field
#'
#' @keywords internal
#' @export
field <- function() {
  .__field__$new()
}



.__field__ <- R6::R6Class(
  'field',
  public = list(
    `__widget` = NULL,
    `__handlers` = NULL,
    `__required` = NULL,

    initialize = function(widget = NULL, handlers = NULL, required = TRUE) {
      self[['__widget']] <- widget %||% text_input
      handlers <- handlers %||% list()
      handlers[['required']] <- handlers[['required']] %||%
      self[['__handlers']] <-
      invisible(self)
    },
    validate = function() {
      NULL
    }
  )
)
