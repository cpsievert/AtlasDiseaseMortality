summary_headline <- function(dx, dx_mrr, dx_lyl, sex_dropdown) {

  template <- "A total of {Diagnosed} {sex_dropdown} living in Denmark in 2000-2018 were diagnosed for the first time with {label} in a hospital in 1995-2018."

  if (!is_na(dx$age_dx50)) {
    template <- paste0(
      template, " The median age at diagnosis was {format_numbers(age_dx50, 1)} years, while 25% of the diagnosed were younger than {format_numbers(age_dx25, 1)} and 25% were older than {format_numbers(age_dx75, 1)} years at time of diagnosis. Among the diagnosed, {Deaths} {sex} died in 2000-2018"
    )
  }

  if (!is_na(dx$age_death50)) {
    template <- paste0(
      template, ", and the median age at time of death was {format_numbers(age_death50, 1)} years (25% were younger than {format_numbers(age_death25, 1)} years and 25% were older than {format_numbers(age_death75, 1)} years)."
    )
  } else {
    template <- paste0(template, ".")
  }

  mrr <- dx_mrr$est

  template_mrr <- if (isTRUE(mrr >= 2)) {
    " On average, mortality rates were {format_numbers(est, 2)} times higher"
  } else if (isTRUE(mrr > 1)) {
    " On average, mortality rates were {format_numbers(100 * (est - 1), 0)}% higher"
  } else if (isTRUE(mrr < 1)) {
    " On average, mortality rates were {format_numbers(abs(100 * (est - 1)), 0)}% lower"
  }

  template_mrr <- paste(template_mrr, "among the diagnosed compared to those of same age and sex without that diagnosis (MRR = {format_numbers(est, 2)} (95% CI: {format_numbers(lower, 2)}, {format_numbers(upper, 2)}))")

  lyl <- dx_lyl$est

  template_lyl <- if (!is_na(lyl)) {
    paste0(
      ", and the average reduction in life expectancy after diagnosis was {format_numbers(est, 2)} years compared to the entire Danish population of same age and sex",
      if (!is_na(dx_lyl$lower)) {
        " (LYL = {format_numbers(est, 2)} (95% CI: {format_numbers(lower, 2)}, {format_numbers(upper, 2)}))."
      },
      "<br/><br/><b>Note:</b> The estimate of life expectancy is based on the assumption that the diagnosed will experience the mortality rates
                                                   of the diagnosed during the entire life (after diagnosis), which might be plausible for chronic disorders but not for acute ones."
    )
  }

  txt <- paste0(
    glue::glue_data(dx, template),
    glue::glue_data(dx_mrr, template_mrr),
    glue::glue_data(dx_lyl, template_lyl)
  )

  div(class = "px-5 py-2", style = "font-weight: 300", HTML(txt))
}
