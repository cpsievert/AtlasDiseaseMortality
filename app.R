library(shiny)
library(bslib)
library(shinyWidgets)
library(htmltools)
library(dplyr)
library(plotly)
library(scales)
library(rlang)
#library(shinyalert)
#library(egg)

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
  theme = bs_theme(
    version = 5,
    base_font = font_google("Roboto Slab", wght = "100..900")
  ) %>%
    # TODO: bslib should handle this
    bs_add_rules(".accordion h2 {margin-top: 0}"),
  navs_pill(
    id = "pills",
    header = div(
      style = "display:flex;justify-content:space-around;margin-top:2rem",
      selectInput("sex", "Sex", c("Persons" = "All", "Men Only" = "Males", "Women Only" = "Females", "Men vs Women" = "M/F")),
      selectInput("cause", "Cause", unique(MRR$cause)),
      checkboxInput("show_ci", "Show 95% CI", FALSE)
    ),
    nav_spacer(),
    nav(
      "IDC chapters", value = "idc",
      overviewUI(
        "idc",
        "Mortality measures for all IDC Chapters",
        "Mortality rate ratios within subchapters",
        "Life years lost within subchapters"
      )
    ),
    nav(
      "General conditions", value = "gmc",
      overviewUI(
        "gmc",
        "Mortality measures for all general medical conditions",
        "Mortality rate ratios within general medical conditions",
        "Life years lost within general medical conditions"
      )
    ),
    nav(
      "Individual", value = "individual",
      div(
        style = "display:flex;flex-direction:column;align-items:center",
        div(
          style = "display:flex;justify-content:center",
          selectInput(
            "disorder_id", NULL,
            c("Search for a disorder" = "", setNames(disorders$id, disorders$description))
          )
        )
      ),
      # TODO: why does crosstalk break on redraw with uiOutput()?
      conditionalPanel(
        "input.disorder_id !== ''",
        tagList(
          uiOutput("disorder_summary"),
          accordion(
            accordion_item(
              "Mortality rate by age", show = TRUE,
              plotlyOutput("disorder_mrr")
            )
          )
        )

      )
    ),
    nav_spacer()
  )
)


server <- function(input, output, session) {

  # ----------------------------------------------------
  # Reactive building blocks
  # ----------------------------------------------------

  is_gmc_view <- reactive(identical(input$pills, "gmc"))

  ids_broad <- reactive({
    if (is_gmc_view()) gmc_broad$id else chapters$id
  })
  ids_specific <- reactive({
    if (is_gmc_view()) gmc_specific$id else disorders %>% filter(level != 1) %>% pull(id)
  })

  sex <- reactive({
    if (identical(input$sex, "M/F")) c("Males", "Females") else input$sex
  })

  cause <- reactive(tolower(input$cause))
  show_ci <- reactive(isTRUE(input$show_ci))

  MRR_ <- reactive({
    filter(MRR, sex %in% sex(), cause %in% input$cause) %>%
      mutate(y = if (is_gmc_view()) desc else chapter_label) %>%
      distinct()
  })
  MRR_broad <- reactive(filter(MRR_(), id %in% ids_broad()))
  MRR_specific <- reactive({
    filter(MRR_(), id %in% ids_specific()) %>%
      mutate(group = if (is_gmc_view()) gmc else as.roman(chapter))
  })

  LYL_ <- reactive({
    filter(LYL, sex %in% sex(), cause %in% input$cause) %>%
      mutate(y = if (is_gmc_view()) desc else chapter_label) %>%
      distinct()
  })
  LYL_broad <- reactive(filter(LYL_(), id %in% ids_broad()))
  LYL_specific <- reactive({
    filter(LYL_(), id %in% ids_specific()) %>%
      mutate(group = if (is_gmc_view()) gmc else as.roman(chapter))
  })

  # TODO: does this need to be filtered by cause of death?
  counts <- reactive({
    filter(DX, id %in% ids_broad(), sex %in% sex()) %>%
      mutate(y = if (is_gmc_view()) desc_EN else chapter_label)
  })

  # ----------------------------------------------------
  # IDC and GMC overview tabs (implemented as modules in R/overview.R)
  # ----------------------------------------------------

  overviewServer(
    "gmc", counts, MRR_broad, LYL_broad, MRR_specific, LYL_specific,
    sex, cause, show_ci, jitter = FALSE
  )
  overviewServer(
    "idc", counts, MRR_broad, LYL_broad, MRR_specific, LYL_specific,
    sex, cause, show_ci,
  )

  # Clicking on any of the overview plots takes you to details # for an individual disorder
  observe({
    disorder_id <- event_data("plotly_click", source = "display_disorder")$customdata
    if (!length(disorder_id)) return()
    updateSelectizeInput(session, "disorder_id", selected = disorder_id)
    nav_select("pills", "individual")
  })


  # ----------------------------------------------------
  # Info for a particular disorder
  # ----------------------------------------------------


  output$disorder_summary <- renderUI({
    d <- filter(DX, id == input$disorder_id, sex == sex())
    txt <- glue::glue_data(
      d, "A total of {scales::comma(n)} {ifelse(sex == 'All', 'persons', tolower(sex))} living in Denmark in 2000-2018 were diagnosed for the first time with disorder '{desc_EN}' in a hospital in 1995-2018. The median age at diagnosis was {dx_median} years, while 25% of the diagnosed were younger than {p25}"
    )
    div(class = "lead py-1", txt)
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
          format_ci(death_rate, death_rate_left, death_rate_right, n = 1)
        ),
        color = ifelse(id == 1, "Diagnosed with the disorder", "Entire Danish population")
      )

    mrr_by_age(
      dat_rates = d,
      dat_ratios = filter(MRRage, id == input$disorder_id, sex %in% sex(), cause %in% input$cause)
    )
  })

}

shinyApp(ui, server)
