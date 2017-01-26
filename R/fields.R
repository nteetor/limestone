#' Create a Form Field
#'
#' @keywords internal
#' @export
field <- function(name, validate) {

}

.__field__ <- R6::R6Class(
  'field',
  public = list(
    initialize = function() {
      invisible(self)
    },
    validate = function() {
      NULL
    }
  )
)

