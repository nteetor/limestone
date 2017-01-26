str_attify <- function(attrs) {
  vapply(
    seq_along(attrs),
    function(i) {
      name <- names(attrs)[i]
      value <- attrs[[i]]

      if (name == '') {
        value
      } else {
        paste0(name, '="', value, '"')
      }
    },
    character(1)
  )
}

format_input <- function(attrs) {
  sprintf('<input %s />', str_attify(attrs))
}
