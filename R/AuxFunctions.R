ggplotly2 <- function(p, ..., tooltip = "text") {
  dims <- getDims()
  ggplotly(p, ..., width = dims$width, height = dims$height, tooltip = tooltip) %>%
    config(displayModeBar = FALSE) %>%
    layout(
      xaxis = list(fixedrange = TRUE),
      yaxis = list(fixedrange = TRUE),
      legend = list(
        title = "", orientation = "h",
        y = 1, yanchor = "bottom",
        x = 0.5, xanchor = "center"
      )
    )
}

subplot2 <- function(..., nrows = 1, margin = 0.01, shareX = FALSE,
                     shareY = FALSE, titleX = shareX, titleY = shareY) {
  plots <- lapply(rlang::list2(...), function(x) {
    if (ggplot2::is.ggplot(x)) ggplotly2(x) else x
  })
  dims <- getDims()
  sub <- subplot(
    plots, nrows = nrows, margin = margin,
    shareX = shareX, shareY = shareY,
    titleX = titleX, titleY = titleY
  )
  layout(sub, width = dims$width, height = dims$height)
}

hline <- function(y = 0, ...) {
  list(
    type = "line",
    x0 = 0, x1 = 1,
    xref = "paper",
    y0 = y, y1 = y,
    line = list(...)
  )
}

getDims <- function() {
  info <- shiny::getCurrentOutputInfo()
  list(
    height = if (length(info)) info$height(),
    width = if (length(info)) info$width()
  )
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
    is.na(x), NA, gsub(" ", "", format(round(x, n), nsmall = n))
  )
}

cis_by_category <- function(data, ytitle, show_ci = FALSE) {

  panel <- function(d) {
    error_y <- if (show_ci) {
      list(
        type = "data",
        symmetric = FALSE,
        array = ~(upper - est),
        arrayminus = ~(est - lower),
        thickness = 1
      )
    }
    plot_ly(d, showlegend = FALSE) %>%
      add_markers(
        y = ~est,
        x = ~jitter(rep(1, nrow(d))),
        text = ~text,
        hoverinfo = "text",
        color = I("black"),
        alpha = 0.2,
        error_y = error_y
      ) %>%
      # TODO: overlay chapter level summary
      #add_segments(
      #  x = ~min(-pos), xend = ~max(-pos),
      #  y = ~unique(LYL_chap), yend = ~unique(LYL_chap)
      #) %>%
      config(displayModeBar = FALSE) %>%
      layout(
        yaxis = list(
          zeroline = FALSE,
          title = ytitle
        ),
        xaxis = list(
          title = "",
          ticktext = ~unique(as.character(group)),
          tickvals = 1,
          tickangle = 45,
          zeroline = FALSE,
          showgrid = FALSE
        )
      )
  }

  data %>%
    group_by(group) %>%
    do(p = panel(.)) %>%
    subplot(
      nrows = 1, margin = 0.01,
      shareY = TRUE, titleY = TRUE,
      titleX = TRUE
    ) %>%
    layout(
      shapes = hline(y = 1, dash = "dash", color = toRGB("red"))
    )
}

string_to_formula <- function(x) {
  rlang::new_formula(NULL, rlang::sym(x))
}

read_table <- function(file, ...) {
  tibble::as_tibble(
    data.table::fread(file = file, sep = " ", header = TRUE, ...)
  )
}
