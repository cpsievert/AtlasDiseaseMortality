ggplotly2 <- function(p, ..., tooltip = "text") {
  dims <- getDims()
  g <- ggplotly(p, ..., width = dims$width, height = dims$height, tooltip = tooltip) %>%
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

  g$x$layout$shapes <- lapply(g$x$layout$shapes, function(s) {
    s$layer <- "below"
    s
  })

  g
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

lvls <- function(x) if (is.factor(x)) levels(x) else unique(x)


overview_plot <- function(counts, mrr, lyl, sex, show_ci, cause) {

  counts <- counts %>%
    mutate(
      y = factor(y, rev(lvls(y))),
      x = n / 1000,
      text = paste0(
        scales::wrap_format(40)(desc), "\n",
        format_numbers(n, 0)
      )
  )

  mrr <- mutate(mrr, y = factor(y, rev(lvls(y))))
  lyl <- mutate(lyl, y = factor(y, rev(lvls(y))))

  # TODO: highlighting not working for gmc view?
  #counts <- highlight_key(counts, ~y, "overview_plot")
  #mrr <- highlight_key(mrr, ~y, "overview_plot")
  #lyl <- highlight_key(lyl, ~y, "overview_plot")

  p_counts <- ggplot(counts, aes(y = y, x = x, text = text, customdata = id)) +
    scale_x_continuous(labels = scales::comma) +
    scale_color_identity() + scale_fill_identity() +
    ylab(NULL) + xlab("Number of diagnosed (in thousands)")

  p_mrr <- ggplot(mrr, aes(x = est, y = y, text = text, customdata = id)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    scale_x_log10(breaks = c(0.2, 0.5, 0.8, 1, 1.2, 1.5, 3, 5, 10, 20, 40, 100, 150, 250)) +
    scale_color_identity() + scale_fill_identity() +
    ylab(NULL) + xlab(paste0("Mortality Rate Ratios for ", tolower(cause), " causes"))

  p_lyl <- ggplot(lyl, aes(x = est, y = y, text = text, customdata = id)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    scale_color_identity() + scale_fill_identity() +
    ylab(NULL) + xlab(paste0("Life Years Lost due to ", tolower(cause), " causes"))

  if (length(sex) == 2) {
    p_counts <- p_counts + geom_col(aes(fill = color), position = "dodge", size = 1)
    p_mrr <- p_mrr + geom_point(aes(color = color))
    p_lyl <- p_lyl + geom_point(aes(color = color))
    if (show_ci) {
      p_mrr <- p_mrr + geom_errorbarh(aes(color = color, xmin = lower, xmax = upper))
      p_lyl <- p_lyl + geom_errorbarh(aes(color = color, xmin = lower, xmax = upper))
    }
  } else {
    p_counts <- p_counts + geom_col(aes(fill = color))
    p_mrr <- p_mrr + geom_point(aes(color = color))
    p_lyl <- p_lyl + geom_point(aes(color = color))
    if (show_ci) {
      p_mrr <- p_mrr + geom_errorbarh(aes(xmin = lower, xmax = upper, color = color), size = 1)
      p_lyl <- p_lyl + geom_errorbarh(aes(xmin = lower, xmax = upper, color = color), size = 1)
    }
  }

  ggcounts <- ggplotly2(p_counts)
  ggcounts$x$data[[1]]$name <- "women"
  ggcounts$x$data[[2]]$name <- "men"

  gg <- subplot2(
    ggcounts,
    ggplotly2(p_mrr) %>%
      style(showlegend = FALSE) %>%
      layout(yaxis = list(showticklabels = FALSE)),
    ggplotly2(p_lyl) %>%
      style(showlegend = FALSE) %>%
      layout(yaxis = list(showticklabels = FALSE)),
    titleX = TRUE
  ) %>%
    # TODO: not working for GMC view???
    #highlight(
    #  "plotly_hover", debounce = 0.01,
    #  selected = attrs_selected(showlegend = FALSE)
    #) %>%
    layout(
      showlegend = length(sex) == 2,
      annotations = list(
        text = "Hover/click for\nmore details",
        x = 0.25, y = 0.2,
        xref = "paper", yref = "paper",
        ax = 0, ay = -50,
        font = list(size = 11)
      )
    )

  gg$x$source <- "display_disorder"

  gg
}


cis_by_category <- function(data, ytitle, xtitle = NULL, ymin = NULL, yint = 1, show_ci = FALSE, jitter = TRUE) {

  panel <- function(d) {
    error_y <- if (show_ci) {
      list(
        type = "data",
        symmetric = FALSE,
        array = ~(upper - est),
        arrayminus = ~(est - lower),
        thickness = 1,
        marker = ~list(color = color)
      )
    }

    p <- plot_ly(d, showlegend = FALSE, source = "display_disorder") %>%
      config(displayModeBar = FALSE) %>%
      layout(
        yaxis = list(
          zeroline = FALSE,
          autorange = FALSE,
          title = ytitle
        ),
        xaxis = list(
          title = "",
          # ticks are only relevant when ymin is missing
          # (in that case we're faking the x-axis)
          showticklabels = length(ymin) == 0,
          ticktext = ~unique(as.character(group)),
          tickvals = 1,
          tickangle = 45,
          fixedrange = TRUE,
          zeroline = FALSE,
          showgrid = FALSE
        )
      )

    if (length(ymin)) {
      p <- add_text(p,
        text = ~unique(as.character(group)),
        customdata = ~sub("^.*:\\s*", "", unique(desc_broad)),
        hovertemplate = "%{customdata}<extra></extra>",
        x = 1, y = ymin,
        textposition = "bottom center",
        cliponaxis = FALSE
      )
    }

    for (col in unique(d$color)) {
      p <- add_markers(
        p, data = filter(d, color %in% col),
        y = ~est, x = ~x,
        text = ~text,
        customdata = ~id,
        hoverinfo = "text",
        color = ~I(color),
        alpha = if (jitter) 0.2 else 0.6,
        error_y = error_y
      )
    }

    p
  }

  # generate random x position for each unique id to be displayed
  if (jitter) {
    jit <- tibble::tibble(
      id = unique(data$id),
      x = withr::with_seed(101, jitter(rep(1, length(id))))
    )
    data <- left_join(data, jit, by = "id")
  } else {
    data$x <- 1
  }

  p <- data %>%
    group_by(group) %>%
    do(p = panel(.)) %>%
    subplot(
      nrows = 1, margin = 0.01,
      shareY = TRUE, titleY = TRUE,
      titleX = TRUE
    ) %>%
    layout(
      shapes = hline(y = yint, dash = "dash", color = toRGB("red")),
      font = list(family = "Roboto Slab")
    )

  if (!is.null(xtitle)) {
    p <- add_annotations(
      p, text = xtitle,
      x = 0.5, xref = "paper", xanchor = "center",
      y = 0, yref = "paper", yanchor = "top",
      yshift = -10, showarrow = FALSE
    )
  }

  p
}

mrr_by_age <- function(dat_rates, dat_ratios) {
  # Only display rates if we have the "with the disorder" estimate
  rates <- if (any(!is.na(filter(dat_rates, id == 1)$death_rate))) {
    ggplot() +
    geom_errorbar(
      data = highlight_key(dat_rates, ~age_group, "disorder_mrr"),
      aes(
        x = age_group + 2.5, y = death_rate,
        ymin = death_rate_left, ymax = death_rate_right,
        color = color,
        text = text,
        customdata = customdata
      ),
      width = 0.1
    ) +
    geom_text(
      data = highlight_key(dat_rates, ~age_group, "disorder_mrr"),
      aes(x = age_group + 2.5, y = death_rate, label = text),
      alpha = 0
    ) +
    xlab("Age in years") +
    ylab("Mortality rates (per 10,000 person-years)") +
    scale_y_log10()
  } else {
    plotly_empty() %>%
      add_annotations(
        text = "Estimates of mortality rates given age<br>are not available because there are less than 100<br>diagnosed who died during the study period",
        x = 0.05, y = 0.5,
        xanchor = "left",
        xref = "paper", yref = "paper",
        showarrow = FALSE
      )
  }

  ratios <- if (nrow(dat_ratios) > 0 && any(!is.na(dat_ratios$est))) {
    ggplot() +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      geom_errorbar(
        data = highlight_key(dat_ratios, ~age_group, "disorder_mrr"),
        aes(
          x = age_group + 2.5, y = est,
          ymin = lower, ymax = upper,
          text = text,
          customdata = customdata
        )
      ) +
      geom_text(
        data = highlight_key(dat_ratios, ~age_group, "disorder_mrr"),
        aes(x = age_group + 2.5, y = upper, label = paste0(text, "\n")),
        alpha = 0
      ) +
      xlab("Age in years") +
      ylab("Mortality rate ratio") +
      scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
      scale_y_log10()
  } else {
    plotly_empty() %>%
      add_annotations(
        text = "Estimates of mortality rate ratios given age<br>are not available because there are less than 100<br>diagnosed who died during the study period",
        x = 0.95, y = 0.5,
        xanchor = "right",
        xref = "paper", yref = "paper",
        showarrow = FALSE
      )
  }

  gg <- subplot2(
    ggplotly3(rates) %>%
      style_textposition_multiple(top = "top left", bottom = "bottom right"),
    ggplotly3(ratios) %>%
      style_textposition_single(left = "top center", right = "top center"),
    titleX = TRUE, titleY = TRUE, margin = 0.04
  ) %>%
    layout(
      showlegend = TRUE,
      legend = list(y = 1.15, x = 0.05, xanchor = "left")
    )

  add_crosstalk_hover(gg)
}


ggplotly3 <- function(p, ...) {
  p <- ggplotly2(p, tooltip = "text")
  p$x$data <- lapply(p$x$data, function(x) {
    if (length(x$error_y) && length(x$error_y$array)) {
      x$mode <- "markers+lines"
      x$line$dash <- "dot"
      x$line$color <- x$error_y$color
      x$marker$color <- x$error_y$color
    }
    x
  })
  p
}


show_customdata_offcanvas <- function(p) {
  htmlwidgets::onRender(
    p,
    "function(el) {
      el.on('plotly_click', function(d) {
        var cd = d.points[0].customdata;
        if (!cd) return;
        var $interpret = $('#interpret');
        $interpret.find('.offcanvas-body').html(cd);
        var canvas = new bootstrap.Offcanvas($interpret[0]);
        canvas.show();
      })
    }"
  )
}

add_crosstalk_hover <- function(p, left = 'top left', right = 'bottom right', opacityDim = 0.3, debounce = 0, ...) {
  p <- style(p, hoverinfo = "none")

  highlight(
    p, "plotly_hover", opacityDim = opacityDim,
    debounce = debounce,
    selected = attrs_selected(
      cliponaxis = FALSE, ...,
      showlegend = FALSE,
      textfont = list(color = "black", size = 10),
      marker = list(color = "black")
    )
  )
}


style_textposition_single <- function(p, left = "top left", right = "bottom right") {
  p$x$data <- lapply(p$x$data, function(tr) {
    if (!isTRUE(tr$mode == "text") || is.null(tr$set)) {
      return(tr)
    }
    xmid <- mean(range(tr$x, na.rm = TRUE), na.rm = TRUE)
    tr$textposition <- ifelse(tr$x > xmid, left, right)
    tr
  })
  p
}


style_textposition_multiple <- function(p, top = "top left", bottom = "bottom right", add_margin = TRUE) {
  p$x$data <- lapply(p$x$data, function(tr) {
    if (!isTRUE(tr$mode == "text") || is.null(tr$set)) {
      return(tr)
    }
    x <- tr$x
    y <- tr$y
    txt <- tr$text
    for (i in seq_along(x)) {
      ys <- y[which(x == x[i])]
      is_bottom <- isTRUE(y[i] == min(ys, na.rm = TRUE))
      tr$textposition[i] <- if (is_bottom) bottom else top
      if (add_margin) {
        tr$text[i] <- if (is_bottom) paste0("\n", txt[i]) else paste0(txt[i], "\n")
      }
    }
    tr
  })

  p
}
