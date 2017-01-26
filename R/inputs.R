#' HTML Form Inputs
#'
#' Common HTML inputs.
#'
#' @name inputs
#' @export
NULL

#' @rdname inputs
#' @export
text_input <- function() {
  input('text')
}

#' @rdname inputs
#' @export
email_input <- function() {
  input('email')
}

input <- function(type) {
  .__input__$new(type)
}

.__input__ <- R6::R6Class(
  class = FALSE,
  inherit = .__widget__,
  public = list(
    `__type` = NULL,
    `__class` = c('input', 'widget'),
    initialize = function(type) {
      self[['__type']] <- type
      invisible(self)
    },
    render = function() {
      attrs <- self[['__attributes']]
      attrs[['type']] <- self[['__type']]
      format_input(attrs)
    }
  )
)
