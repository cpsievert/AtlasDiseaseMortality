library(shiny)
library(bslib)
library(shinyWidgets)
library(htmltools)
library(dplyr)
library(plotly)
library(scales)
#library(shinyalert)
#library(egg)

source("R/AuxFunctions.R", local = TRUE)

theme_set(theme_bw(base_family = "Roboto Slab"))
thematic::thematic_shiny(font = "Roboto Slab")

# Load list of diseases
DX <- readRDS("data/DX.rds")

disorders <- DX %>%
  select(id, description, level) %>%
  distinct()

chapters <- disorders %>%
  filter(level == 1)

gmc_broad <- disorders %>%
  filter(id %in% (100000 + c(1, 9, 13, 16, 21, 24, 27, 30, 31, 39)))

gmc_specific <- disorders %>%
  filter(id %in% (100000 + c(2:8, 10:12, 14, 15, 17:20, 22, 23, 25, 26, 28:30, 32:39)))


MRR <- readRDS("data/MRR.rds")
MRRlagged <- readRDS("data/MRRlagged.rds")
MRRage <- readRDS("data/MRRage.rds")

LYL <- readRDS("data/LYL.rds")
LYLage <- readRDS("data/LYLage.rds")

ages <- readRDS("data/ages.rds")
incidence <- readRDS("data/incidence.rds")

# General population death rates
incidence0 <- filter(incidence, id == 0)


ui <- fluidPage(
  div(
    style = "display:flex;justify-content:space-around",
    selectInput("summary", "Summary type", c(
      "IDC Chapters" = "idc",
      "General Medical Conditions" = "gmc"
    )),
    selectInput("sex", "Sex", c("Persons" = "All", "Men Only" = "Males", "Women Only" = "Females", "Men vs Women" = "M/F")),
    selectInput("cause", "Cause", unique(MRR$cause)),
    #checkboxInput("air", "")
  ),
  div(
    style = "display:flex;flex-direction:column;align-items:center",
    plotlyOutput("overview", width = "80%"),
    br(),
    div(
      style = "display:flex;justify-content:center",
      checkboxInput("show_ci", "Show 95% CI", FALSE),
      selectInput(
        "disorder_id", NULL,
        c("Search for a disorder" = "", setNames(disorders$id, disorders$description)),
        selected = 1001
      )
    ),
    plotlyOutput("mrr",  width = "90%"),
    plotlyOutput("lyl",  width = "90%"),
    plotlyOutput("disorder_mrr", width = "70%")
  )
)

server <- function(input, output, session) {

  is_gmc_view <- reactive(identical(input$summary, "gmc"))

  ids_broad <- reactive({
    if (is_gmc_view()) gmc_broad$id else chapters$id
  })
  ids_specific <- reactive({
    if (is_gmc_view()) gmc_specific$id else disorders %>% filter(level != 1) %>% pull(id)
  })

  sex <- reactive({
    if (identical(input$sex, "M/F")) c("Males", "Females") else input$sex
  })

  output$overview <- renderPlotly({
    counts <- filter(DX, id %in% ids_broad(), sex %in% sex()) %>%
      mutate(y = if (is_gmc_view()) desc_EN else chapter_label)
    mrr <- filter(MRR, id %in% ids_broad(), sex %in% sex(), cause %in% input$cause) %>%
      mutate(y = if (is_gmc_view()) desc_EN else chapter_label)
    lyl <- filter(LYL, id %in% ids_broad(), sex %in% sex(), cause %in% input$cause) %>%
      mutate(y = if (is_gmc_view()) desc_EN else chapter_label)

    p_counts <- ggplot(counts, aes(y = y, x = n / 1000, text = desc_EN)) +
      scale_x_continuous(labels = scales::comma) +
      ylab(NULL) + xlab("Number of diagnosed in 1995-2018 (in thousands)")

    p_mrr <- ggplot(mrr, aes(x = est, y = y, text = text)) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
      scale_x_log10(breaks = c(0.2, 0.5, 0.8, 1, 1.2, 1.5, 3, 5, 10, 20, 40, 100, 150, 250)) +
      ylab(NULL) + xlab(paste0("Mortality Rate Ratios for ", tolower(input$cause), " causes"))

    p_lyl <- ggplot(lyl, aes(x = est, y = y,  text = text)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
      ylab(NULL) + xlab(paste0("Life Years Lost due to ", tolower(input$cause), " causes"))

    if (length(sex) == 2) {
      p_counts <- p_counts + geom_col(aes(color = sex), position = "dodge")
      # TODO: need to fix position?
      p_mrr <- p_mrr + geom_point(aes(color = sex))
      p_lyl <- p_lyl + geom_point(aes(color = sex))
      if (input$show_ci) {
        p_mrr <- p_mrr + geom_errorbarh(aes(color = sex, xmin = lower, xmax = upper))
        p_lyl <- p_lyl + geom_errorbarh(aes(color = sex, xmin = lower, xmax = upper))
      }
    } else {
      p_counts <- p_counts + geom_col()
      p_mrr <- p_mrr + geom_point()
      p_lyl <- p_lyl + geom_point()
      if (input$show_ci) {
        p_mrr <- p_mrr + geom_errorbarh(aes(xmin = lower, xmax = upper))
        p_lyl <- p_lyl + geom_errorbarh(aes(xmin = lower, xmax = upper))
      }
    }

    subplot2(
      p_counts,
      ggplotly2(p_mrr) %>%
        style(showlegend = FALSE) %>%
        layout(yaxis = list(showticklabels = FALSE)),
      ggplotly2(p_lyl) %>%
        style(showlegend = FALSE) %>%
        layout(yaxis = list(showticklabels = FALSE)),
      titleX = TRUE
    )
  })


  output$mrr <- renderPlotly({

    # TODO: how to handle the gmc case?
    dat <- MRR %>%
      filter(id %in% ids_specific(), sex %in% sex(), cause %in% input$cause) %>%
      mutate(group = as.roman(chapter)) %>%
      # TODO: why are there repeated values?
      distinct()

    ytitle <- paste0("Mortality Rate Ratios for ", tolower(input$cause), " causes")

    cis_by_category(dat, ytitle = ytitle, input$show_ci) %>%
      layout(
        yaxis = list(type = "log", range = log10(c(0.3, 180)))
      )
  })

  output$lyl <- renderPlotly({
    # TODO: how to handle the gmc case?
    dat <- LYL %>%
      filter(id %in% ids_specific(), sex %in% sex(), cause %in% input$cause) %>%
      mutate(group = as.roman(chapter)) %>%
      # TODO: why are there repeated values?
      distinct()

    ytitle <- paste0("Life Years Lost due to ", tolower(input$cause), " causes")

    cis_by_category(dat, ytitle = ytitle, input$show_ci) %>%
      layout(
        yaxis = list(type = "log", range = log10(c(0.3, 180)))
      )
  })

  output$disorder_mrr <- renderPlotly({
    req(input$disorder_id)

    incidence1 <- filter(incidence, id == input$disorder_id)

    d <- rbind(incidence0, incidence1) %>%
      filter(sex %in% sex()) %>%
      mutate(
        id = ifelse(id == 0, 0, 1),
        death_rate_left = ifelse(id == 0, NA, death_rate_left),
        death_rate_right = ifelse(id == 0, NA, death_rate_right)
      ) %>%
      filter(age_group <= 95) %>%
      mutate(
        death_rate = 10000 * death_rate,
        death_rate_left = 10000 * death_rate_left,
        death_rate_right = 10000 * death_rate_right,
        text = paste0(
          "Rate: ", format_ci(death_rate, death_rate_left, death_rate_right, n = 1), "\n",
          "Age: ", age_group + 2.5
        ),
        color = ifelse(id == 1, "Diagnosed with the disorder", "Entire Danish population")
      )

    rates <- ggplot(d, aes(x = age_group + 2.5, y = death_rate, group = color, color = color)) +
      geom_point(aes(customdata = text)) +
      geom_errorbar(aes(ymin = death_rate_left, ymax = death_rate_right), width = 0.1) +
      xlab("Age in years") +
      ylab("Mortality rates (per 10,000 person-years)") +
      scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100))

    rates <- ggplotly2(rates) %>%
      style(mode = "markers+lines", line.dash = "dot", line.width = 1)

    browser()
    ratios <- ggplot(
      filter(MRRage, id == input$disorder_id, sex %in% sex(), cause %in% input$cause),
      aes(x = age_group + 2.5, y = est)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      geom_point(aes(customdata = ci)) +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
      xlab("Age in years") +
      ylab("Mortality rate ratio") +
      scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
      scale_y_log10()

    ratios <- ggplotly2(ratios) %>%
      style(mode = "markers+lines", line.dash = "dot", line.width = 1)

    gg <- subplot2(rates, ratios, titleX = TRUE, titleY = TRUE, margin = 0.03) %>%
      layout(
        hovermode = "x",
        legend = list(y = 1.15)
      )

    gg$x$data <- lapply(gg$x$data, function(tr) {
      #if (!length(tr$error_y)) {
        tr$hovertemplate <- "%{customdata}<extra></extra>"
      #}
      tr
    })

    gg
  })

}

shinyApp(ui, server)
