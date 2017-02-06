#' Create an HTML Form
#'
#' An HTML form.
#'
#' @export
form <- function(...) {
  args <- list(...)
  attrs <- Filter(is.character, args)
  if (!is_named(attrs)) {
    unknown <- attrs[which(names3(attrs) == '')[1]]
    stop(invalid_argument('form', unknown))
  }
  .__form__$new(attrs = attrs)
}

fields <- function(x) {
  if (!is.form(x)) {
    stop('expecting form object', call. = FALSE)
  }
  x[['__fields']]
}

is.form <- function(x) {
  is.environment(x) && 'form' %in% x[['__class']]
}

`[.form` <- function(x, name) {
  x[['__fields']][[name]]
}

.__form__ <- R6::R6Class(
  classname = 'form',
  public = list(
    `__attributes` = NULL,
    `__fields` = NULL,
    `__class` = NULL,
    initialize = function(attrs = NULL) {
      self[['__class']] <- 'form'
      self[['__fields']] <- list()

      attrs <- attrs %||% list()
      self[['__attributes']] <- list(
        `accept-charset` = attribute(
          'accept-charset',
          attrs[['accept-charset']],
          'UNKNOWN'
        ),
        action = attribute(
          'action',
          attrs[['action']],
          NULL,
          function(v) !is.null(v)
        ),
        autocomplete = attribute(
          'autocomplete',
          attrs[['autocomplete']],
          'on',
          function(v) v == 'on' || v == 'off'
        ),
        enctype = attribute(
          'enctype',
          attrs[['enctype']],
          'application/x-www-form-urlencoded',
          function(v) v %in% c('application/x-www-form-urlencoded',
                               'multipart/form-data', 'text/plain')
        ),
        method = attribute(
          'method',
          attrs[['method']],
          'get',
          function(v) v == 'get' || v == 'post'
        ),
        name = attribute(
          'name',
          attrs[['name']],
          NULL,
          function(v) !is.null(v)
        ),
        novalidate = attribute(
          'novalidate',
          attrs[['novalidate']],
          'false',
          function(v) v == 'false' || v == 'true'
        ),
        target = attribute(
          'target',
          attrs[['target']],
          '_self',
          function(v) is.character(v)
        )
      )

      invisible(self)
    },
    render = function() {
      self$validate()
      paste0(
        '<form ',
        paste0(
          lapply(self$attributes, function(a) a$render()),
          collapse = ' '
        ),
        '>',
        '\n',
        '</form>'
      )
    },
    validate = function() {
      for (att in self[['__attributes']]) {
        tryCatch(
          att$validate(),
          invalid_attribute = function(e) {
            stop(invalid_attribute('form', attr(e, 'name'), attr(e, 'value')))
          }
        )
      }

      invisible(self)
    }
  ),
  active = list(
    attributes = function() {
      Filter(function(a) !a$is_default(), self[['__attributes']])
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

