#' HTML Form Inputs
#'
#' Common HTML form inputs.
#'
#' @name inputs
NULL

#' @rdname inputs
#' @export
text_input <- function() {
  .__text_input__$new()
}

#' @rdname inputs
#' @export
email_input <- function() {
  .__text_input__$new(list(type = 'email'))
}

#' @rdname inputs
#' @export
password_input <- function() {
  .__text_input__$new(list(type = 'password'))
}

#' @rdname inputs
#' @export
number_input <- function() {
  .__text_input__$new(list(type = 'number'))
}

#' @rdname inputs
#' @export
tel_input <- function() {
  .__text_input__$new(list(type = 'tel'))
}

#' @rdname inputs
#' @export
url_input <- function() {
  .__text_input__$new(list(type = 'url'))
}

#' @rdname inputs
#' @export
hidden_input <- function() {
  this <- .__input__$new()
  this[['type']] <- 'hidden'
  this
}

.__text_input__ <- R6::R6Class(
  class = FALSE,
  inherit = .__input__,
  public = list(
    initialize = function(attrs = NULL) {
      attrs <- attrs %||% list()
      self[['__type']] <- attrs[['type']] %||% 'text'
      attrs[['type']] <- NULL

      super$initialize(attrs)
      invisible(self)
    }
  )
)

.__input__ <- R6::R6Class(
  class = FALSE,
  inherit = .__widget__,
  public = list(
    `__type` = NULL,
    initialize = function(attrs = NULL) {
      super$initialize(attrs)
      self[['__class']] <- c('input', self[['__class']])
      invisible(self)
    },
    render = function(attrs = NULL) {
      attrs <- c(self[['__attributes']], attrs)
      attrs[['type']] <- self[['__type']]
      format_input(attrs)
    }
  ),
  active = list(
    type = function(value) {
      if (missing(value)) {
        self[['__type']]
      } else {
        self[['__type']] <- value
        invisible(self)
      }
    }
  )
)
