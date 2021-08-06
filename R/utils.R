format_ci <- function(est, lower, upper, n = 1, prefix = "") {
  case_when(
    is.na(est) ~ "-",
    is.na(lower) | is.na(upper) ~ format_numbers(est, n),
    TRUE ~ sprintf(
      "%s (%s%s - %s)",
      format_numbers(est, n),
      prefix,
      format_numbers(lower, n),
      format_numbers(upper, n)
    )
  )
}

format_numbers <- function(x, n = 1) {
  ifelse(
    is.na(x), NA_character_, gsub(" ", "", format(round(x, n), nsmall = n, big.mark = ","))
  )
}


string_to_formula <- function(x) {
  rlang::new_formula(NULL, rlang::sym(x))
}

is_na <- function(x) {
  isTRUE(is.na(x))
}

read_table <- function(file, ...) {
  tibble::as_tibble(
    data.table::fread(file = file, sep = " ", header = TRUE, ...)
  )
}
