# Interested in learning more about error handling and custom signals, check out
# http://adv-r.had.co.nz/Exceptions-Debugging.html
condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call),
    ...
  )
}

stop_not_implemented <- function(message) {
  stop(condition('not_implemented', message, call = NULL))
}
