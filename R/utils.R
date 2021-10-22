# Some disorders (e.g., chpater XV pregnancy) only have estimates for
# a particular sex, but we still want them displayed when "persons" is
# selected, so we just repeat the given row as if we truly do
# have the estimate for "persons"
repeat_sex_rows <- function(d) {
  dats <- split(d, paste(d$id, d$cause, sep = "-"))
  dplyr::bind_rows(lapply(dats, function(x) {
    sexes <- unique(x$sex)
    if ("persons" %in% sexes || length(sexes) > 1) {
      return(x)
    }
    y <- x
    y$sex <- "persons"
    dplyr::bind_rows(x, y)
  }))
}

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

wrap <- scales::wrap_format(60)
