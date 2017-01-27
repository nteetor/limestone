#' Create an HTML Form
#'
#' An HTML form.
#'
#' @param action Where to forward the form information.
#' @param method One of \code{'get'} or \code{'post'}.
#' @param \ldots widget or input objects, see \code{\link{inputs}} and
#'   \code{\link{widgets}}.
#'
#' @export
form <- function(action, method = 'get', ...) {
  structure(
    list(
      action = action,
      method = method,
      widgets = list(...)
    ),
    class = 'form'
  )
}

.__form__ <- R6::R6Class(
  class = FALSE,
  public = list(
    initialize = function() {
      invisible(self)
    }
  )
)
