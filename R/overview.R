overviewUI <- function(id, title1, title2, title3) {
  ns <- NS(id)
  accordion(
    accordion_item(
      title1,
      plotlyOutput(ns("overview"))
    ),
    accordion_item(
      title2,
      plotlyOutput(ns("mrr")), show = FALSE
    ),
    accordion_item(
      title3,
      plotlyOutput(ns("lyl")), show = FALSE
    )
  )
}

overviewServer <- function(id, counts, MRR_broad, LYL_broad, MRR_specific, LYL_specific, sex, cause, show_ci, jitter = TRUE) {
  moduleServer(id, function(input, output, session) {

    output$overview <- renderPlotly({
      overview_plot(
        counts(), MRR_broad(), LYL_broad(),
        sex(), show_ci(), cause()
      )
    })

    output$mrr <- renderPlotly({
      cis_by_category(
        MRR_specific(),
        ytitle = paste0(
          "Mortality Rate Ratios for ", cause(), " causes"
        ),
        show_ci(), jitter
      ) %>%
        layout(yaxis = list(type = "log"))
    })

    output$lyl <- renderPlotly({
      cis_by_category(
        LYL_specific(),
        ytitle = paste0(
          "Life Years Lost due to ", cause(), " causes"
        ),
        show_ci(), jitter
      ) %>%
        layout(yaxis = list(type = "log"))
    })

  })
}
