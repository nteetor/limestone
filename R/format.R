str_attify <- function(attrs) {
  paste(
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
    ),
    collapse = ' '
  )
}

format_input <- function(attrs) {
  sprintf('<input %s />', str_attify(attrs))
}

format_text_area <- function(attrs) {
  sprintf('<textarea %s></textarea>', str_attify(attrs))
}

format_select <- function(attrs, choices) {
  sprintf('<select %s>\n%s\n</select>', str_attify(attrs),
          paste0(choices, collapse = '\n'))
}

format_option <- function(attrs) {
  sprintf('<option %s></option>', str_attify(attrs))
}
