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


overview_plot <- function(counts, mrr, lyl, sex, show_ci, cause) {
  counts <- highlight_key(counts, ~y, "overview_plot")
  mrr <- highlight_key(mrr, ~y, "overview_plot")
  lyl <- highlight_key(lyl, ~y, "overview_plot")

  p_counts <- ggplot(counts, aes(y = y, x = n / 1000, text = desc_EN, customdata = id)) +
    scale_x_continuous(labels = scales::comma) +
    ylab(NULL) + xlab("Number of diagnosed in 1995-2018 (in thousands)")

  p_mrr <- ggplot(mrr, aes(x = est, y = y, text = text, customdata = id)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    scale_x_log10(breaks = c(0.2, 0.5, 0.8, 1, 1.2, 1.5, 3, 5, 10, 20, 40, 100, 150, 250)) +
    ylab(NULL) + xlab(paste0("Mortality Rate Ratios for ", tolower(cause), " causes"))

  p_lyl <- ggplot(lyl, aes(x = est, y = y, text = text, customdata = id)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    ylab(NULL) + xlab(paste0("Life Years Lost due to ", tolower(cause), " causes"))

  if (length(sex) == 2) {
    p_counts <- p_counts + geom_col(aes(color = sex), position = "dodge")
    p_mrr <- p_mrr + geom_point(aes(color = sex))
    p_lyl <- p_lyl + geom_point(aes(color = sex))
    if (show_ci) {
      p_mrr <- p_mrr + geom_errorbarh(aes(color = sex, xmin = lower, xmax = upper))
      p_lyl <- p_lyl + geom_errorbarh(aes(color = sex, xmin = lower, xmax = upper))
    }
  } else {
    p_counts <- p_counts + geom_col()
    p_mrr <- p_mrr + geom_point()
    p_lyl <- p_lyl + geom_point()
    if (show_ci) {
      p_mrr <- p_mrr + geom_errorbarh(aes(xmin = lower, xmax = upper))
      p_lyl <- p_lyl + geom_errorbarh(aes(xmin = lower, xmax = upper))
    }
  }

  gg <- subplot2(
    p_counts,
    ggplotly2(p_mrr) %>%
      style(showlegend = FALSE) %>%
      layout(yaxis = list(showticklabels = FALSE)),
    ggplotly2(p_lyl) %>%
      style(showlegend = FALSE) %>%
      layout(yaxis = list(showticklabels = FALSE)),
    titleX = TRUE
  ) %>%
    highlight("plotly_hover", debounce = 0.1)

  gg$x$source <- "display_disorder"
  gg
}


cis_by_category <- function(data, ytitle, show_ci = FALSE, jitter = TRUE) {
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
    plot_ly(d, showlegend = FALSE, source = "display_disorder") %>%
      add_markers(
        y = ~est,
        x = ~if (jitter) jitter(rep(1, nrow(d))) else 1,
        text = ~text,
        customdata = ~id,
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

mrr_by_age <- function(dat_rates, dat_ratios) {
  rates <- ggplot(
    dat_rates, aes(x = age_group + 2.5, y = death_rate, group = color, color = color)
  ) +
    geom_point(
      data = highlight_key(dat_rates, ~age_group, "disorder_mrr"),
      aes(customdata = text)
    ) +
    geom_errorbar(aes(ymin = death_rate_left, ymax = death_rate_right), width = 0.1) +
    xlab("Age in years") +
    ylab("Mortality rates (per 10,000 person-years)") +
    scale_y_log10()

  rates <- ggplotly2(rates) %>%
    style(mode = "markers+lines", line.dash = "dot", line.width = 1)

  ratios <- ggplot(
    dat_ratios,
    aes(x = age_group + 2.5, y = est)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    geom_point(
      data = highlight_key(dat_ratios, ~age_group, "disorder_mrr"),
      aes(customdata = format_ci(est, lower, upper, n = 1))
    ) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
    xlab("Age in years") +
    ylab("Mortality rate ratio") +
    scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
    scale_y_log10()

  ratios <- ggplotly2(ratios) %>%
    style(mode = "markers+lines", line.dash = "dot", line.width = 1)

  gg <- subplot2(rates, ratios, titleX = TRUE, titleY = TRUE, margin = 0.03) %>%
    style(hoverinfo = "none") %>%
    layout(
      hovermode = "x",
      legend = list(y = 1.15)
    )

  highlight(
    gg, "plotly_hover", debounce = 0.1,
    selected = attrs_selected(
      hovertemplate = "%{customdata}<extra></extra>"
    )
  )
}
