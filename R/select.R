#' Dropdown Inputs
#'
#' Use select to include dropdown inputs in HTML forms
#'
#' @export
select <- function() {
  .__select__$new()
}

#' @rdname select
#' @export
option <- function() {
  .__option__$new()
}

.__select__ <- R6::R6Class(
  class = FALSE,
  inherit = .__widget__,
  public = list(
    `__choices` = NULL,
    initialize = function(attrs = NULL, choices = NULL) {
      super$initialize(attrs)

      self[['__choices']] <- choices %||% list()
      if (!all(vapply(self[['__choices']], is.option, logical(1)))) {
        stop('all choices must be option objects, see ?option', call. = FALSE)
      }

      self[['__class']] <- c('select', self[['__class']])
      invisible(self)
    },
    render = function(attrs = NULL) {
      attrs <- c(self[['__attributes']], attrs)
      choices <- lapply(self[['__choices']], function(ch) ch$render())
      format_select(attrs, choices)
    }
  )
)

is.option <- function(x) {
  is.environment(x) && 'option' %in% x[['__class']]
}

.__option__ <- R6::R6Class(
  class = FALSE,
  inherit = .__widget__,
  public = list(
    initialize = function(attrs = NULL) {
      super$initialize(attrs)
      self[['__class']] <- c('option', self[['__class']])
      invisible(self)
    },
    render = function(attrs = NULL) {
      attrs <- c(self[['__choices']], attrs)
      format_option(attrs)
    }
  )
)

