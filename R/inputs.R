#' HTML Form Inputs
#'
#' Common HTML form inputs.
#'
#' @name inputs
NULL

#' @rdname inputs
#' @export
text_input <- function(...) {
  args <- list(...)
  if (!is_named(args)) {
    stop('!!!')
  }
  .__text_input__$new(attrs = args)
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
      attrs <- attrs %||% list()
      attrs <- list(
        type = attribute(
          'type',
          attrs[['type']],
          'text',
          function(v) !is.null(v)
        ),
        autocapitalize = attribute(
          'autocapitalize',
          attrs[['autocapitalize']],
          'none',
          function(v) v %in% c('none', 'sentences', 'words', 'characters')
        ),
        autocomplete = attribute(
          'autocomplete',
          attrs[['autocomplete']],
          'off',
          function(v) v %in% autocomplete_values
        ),
        autocorrect = attribute(
          'autocorrect',
          attrs[['autocorrect']],
          'off',
          function(v) v %in% c('on', 'off')
        )
      )

      # if type=file, then attr accept
      self[['__class']] <- c('input', self[['__class']])
      invisible(self)
    },
    render = function() {

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
