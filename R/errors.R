# Interested in learning more about error handling and custom signals?
# check out http://adv-r.had.co.nz/Exceptions-Debugging.html
condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call),
    ...
  )
}

not_implemented <- function(message) {
  condition('not_implemented', message, call = NULL)
}

required <- function(message) {
  condition('required', message, call = NULL)
}

invalid_attribute <- function(tag, name, value) {
  msg <- sprintf('invalid value "%s" for %s attribute "%s"', value, tag, name)
  condition('invalid_attribute', msg, call = NULL, tag = tag, name = name,
            value = value)
}

missing_attribute <- function(tag, name) {
  msg <- sprintf('missing value for %s attribute "%s"', tag, name)
  condition('missing_attribute', msg, call = NULL, tag = tag, name = name)
}

invalid_argument <- function(fun, arg) {
  msg <- paste('invalid argument', arg, 'passed to', fun)
  condition('invalid_argument', msg, call = NULL)
}
