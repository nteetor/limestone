#' Create an HTML Form
#'
#' An HTML form.
#'
#' @export
form <- function(...) {
  args <- list(...)
  attrs <- args[which(names3(args) != '')]
  fields <- args[which(names3(args) == '')]
  .__form__$new(attrs = attrs)
}

is.form <- function(x) {
  is.environment(x) && 'form' %in% x[['__class']]
}

.__form__ <- R6::R6Class(
  class = FALSE,
  public = list(
    `__attributes` = NULL,
    `__fields` = NULL,
    `__class` = NULL,
    initialize = function(attrs = NULL, fields = NULL) {
      self[['__class']] <- 'form'
      self[['__fields']] <- fields %||% list()
      self[['__attributes']] <- attrs %||% list()

      invisible(self)
    },
    render = function() {
      self$validate()

      paste0(
        '<form ',
        paste0(
          names(self[['__attributes']]), '=',
          paste0('"', self[['__attributes']], '"'),
          collapse = ' '
        ),
        '>',
        '\n',
        '</form>'
      )
    },
    validate = function() {
      tryCatch(
        self[['__attributes']] <- lapply(self[['__attributes']], as.character),
        error = function(e) {
          stop('could not convert attributes to character', call. = FALSE)
        }
      )

      invisible(self)
    }
  )
)

attribute <- function(name, value, default, validate = function(v) TRUE,
                      parent = parent.frame()) {
  .__attribute__$new(name, value, default, validate,
                     parent[['self']][['__class']])
}

.__attribute__ <- R6::R6Class(
  class = FALSE,
  public = list(
    `__name` = NULL,
    `__value` = NULL,
    `__default` = NULL,
    `__validate` = NULL,
    `__parent` = NULL,
    initialize = function(name, value, default, validate, parent) {
      self[['__name']] <- name
      self[['__value']] <- value %||% default
      self[['__default']] <- default
      self[['__validate']] <- validate
      self[['__parent']] <- parent

      invisible(self)
    },
    render = function() {
      paste0(self[['__name']], '="', self[['__value']], '"')
    },
    validate = function() {
      if (is.null(self[['__value']])) {
        stop(missing_attribute(self[['__parent']], self[['__name']]))
      }
      if (!self[['__validate']](self[['__value']])) {
        stop(invalid_attribute(self[['__parent']], self[['__name']],
                               self[['__value']]))
      }
    },
    is_default = function() {
      isTRUE(all.equal(self[['__value']], self[['__default']]))
    },
    set = function(value) {
      stop(not_implemented('oops!'))
    }
  )
)

autocomplete_values <- c(
  "off",
  "on",
  "name",
  "honorific-prefix",
  "given-name",
  "additional-name",
  "family-name",
  "honorific-suffix",
  "nickname",
  "email",
  "username",
  "new-password",
  "current-password",
  "organization-title",
  "organization",
  "street-address",
  "address-line1",
  "address-line2",
  "address-line3",
  "address-level4",
  "address-level3",
  "address-level2",
  "address-level1",
  "country",
  "country-name",
  "postal-code",
  "cc-name",
  "cc-given-name",
  "cc-additional-name",
  "cc-family-name",
  "cc-number",
  "cc-exp",
  "cc-exp-month",
  "cc-exp-year",
  "cc-csc",
  "cc-type",
  "transaction-currency",
  "transaction-amount",
  "language",
  "bday",
  "bday-day",
  "bday-month",
  "bday-year",
  "sex",
  "tel",
  "url",
  "photo"
)

