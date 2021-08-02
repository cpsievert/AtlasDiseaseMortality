library(shiny)
library(bslib)
library(shinyWidgets)
library(htmltools)
library(dplyr)
library(plotly)
library(scales)
library(rlang)
library(htmlwidgets)
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

lifeExp <- readRDS("data/lifeExp.rds")
lifeExp0 <- readRDS("data/lifeExp0.rds")

ages <- readRDS("data/ages.rds")
incidence <- readRDS("data/incidence.rds")

# General population death rates
incidence0 <- filter(incidence, id == 0)


ui <- fluidPage(
  theme = bs_theme(
    version = 5, primary = "#ED3A3B",
    "nav-pills-link-active-color" = "white",
    base_font = font_google("Roboto Slab", wght = "100..900")
  ) %>%
    # TODO: bslib should handle this
    bs_add_rules(".accordion h2 {margin-top: 0}"),
  navs_pill(
    id = "pills",
    header = conditionalPanel(
      "input.pills !== 'individual'",
      div(
        style = "display:flex;justify-content:space-around;margin-top:2rem",
        selectInput("sex", "Sex", c("Persons" = "All", "Men Only" = "Males", "Women Only" = "Females", "Men vs Women" = "M/F")),
        selectInput("cause", "Cause", unique(MRR$cause)),
        checkboxInput("show_ci", "Show 95% CI", FALSE)
      )
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
          style = "display:flex;justify-content:center;margin-top:1rem",
          selectInput(
            "disorder_id", NULL,
            c("Search for a disorder" = "", setNames(disorders$id, disorders$description))
          )
        ),
      conditionalPanel(
        "input.disorder_id !== ''",
        offcanvas("interpret", .title = "Interpretation"),
        accordion(
          accordion_item(
            "Summary statement",
            uiOutput("summary_headline")
          ),
          accordion_item(
            "Summaries split by sex",
            DT::dataTableOutput("summary_table")
          ),
          accordion_item(
            "Incidence rate given age", show = FALSE,
            plotlyOutput("incidence_age", height = 300)
          ),
          # TODO: add a control for sex?
          accordion_item(
            "Mortality rate given age", show = FALSE,
            plotlyOutput("mrr_age", height = 300)
          ),
          # TODO: add a control for sex?
          accordion_item(
            "Mortality rate given time since diagnosis", show = FALSE,
            plotlyOutput("mrr_time", height = 300, width = "67%")
          ),
          accordion_item(
            "Expected life remaining given age", show = FALSE,
            plotlyOutput("lyl_age", height = 300)
          ),
          accordion_item(
            "Survival curves", show = FALSE,
            plotlyOutput("survival")
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
  # Logic for 'individual' tab  (a particular disorder)
  # ----------------------------------------------------

  DX_ <- reactive({
    filter(DX, id %in% input$disorder_id) %>%
      mutate(Persons = ifelse(sex == 'All', 'persons', tolower(sex))) %>%
      mutate(
        Diagnosed = ifelse(!is.na(n), format(n, big.mark = ","), as.character(n_text)),
        Deaths = ifelse(!is.na(cases), format(cases, big.mark = ","), as.character(cases_text))
      ) %>%
      distinct()
  })

  DX_MRR <- reactive({
    select(DX_(), -MRR) %>%
      left_join(MRR, by = c("id", "sex")) %>%
      distinct()
  })

  DX_LYL <- reactive({
    select(DX_(), -LYL) %>%
      left_join(LYL, by = c("id", "sex")) %>%
      distinct()
  })

  output$summary_headline <- renderUI({
    summary_headline(
      filter(DX_()),
      filter(DX_MRR(), cause %in% input$cause),
      filter(DX_LYL(), cause %in% input$cause),
      input
    )
  })


  output$summary_table <- DT::renderDataTable({
    dx_mrr <- filter(DX_MRR(), cause %in% "All")
    dx_lyl <- filter(DX_LYL(), cause %in% "All") %>%
      select(id, sex, ci)

    dx_mrr %>%
      mutate(dx_death = format_ci(age_death50, age_death25, age_death75)) %>%
      select(
        id, sex, Diagnosed,
        `Age at Diagnosis [Median (IQR)]` = dx_median,
        Deaths = Deaths,
        `Age at Death [Median (IQR)]` = dx_death,
        `Mortality Rate Ratio` = ci
      ) %>%
      left_join(dx_lyl, by = c("id", "sex")) %>%
      select(-id) %>%
      rename(Sex = sex, `Life Years Lost` = ci) %>%
      DT::datatable(
        rownames = FALSE,
        options = list(
          pageLength = 20, info = FALSE,
          lengthChange = FALSE, searching = FALSE,
          scrollX = FALSE, scrollY = FALSE, paging = FALSE,
          initComplete = DT::JS(
            "function(settings) {
              var table = settings.oInstance.api();
              table.$('td').each(function(x, val) {
                var html = $(this).html();
                if (html === '' || html === '-') {
                  this.setAttribute('title', 'Estimate not available');
                  this.setAttribute('data-bs-toggle', 'tooltip');
                  this.setAttribute('data-bs-placement', 'bottom');
                  new bootstrap.Tooltip(this);
                }
              });
            }"
          )
        )
      )
  })

  output$incidence_age <- renderPlotly({
    req(input$disorder_id)

    d <- incidence %>%
      filter(id %in% input$disorder_id) %>%
      filter(age_group <= 95) %>%
      mutate(
        dx_rate = 10000 * dx_rate,
        dx_rate_left = 10000 * dx_rate_left,
        dx_rate_right = 10000 * dx_rate_right,
        text = paste0(
          "Rate: ", format_ci(dx_rate, dx_rate_left, dx_rate_right, n = 1), "\n",
          "Age: ", format_numbers(age_group + 2.5, 0)
        )
      )

    g <- ggplot(d, aes(x = age_group + 2.5, y = dx_rate, text = text)) +
      geom_line(linetype = "dashed") +
      geom_point() +
      geom_errorbar(aes(ymin = dx_rate_left, ymax = dx_rate_right)) +
      xlab("Age in years") +
      ylab("Incidence rate (per 10,000 person-years)") +
      scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
      facet_grid(~sex, drop = FALSE)

    ggplotly2(g) %>%
      onRender(
        paste0("function(el) {
          el.on('plotly_click', function(d) {
            var pt = d.points[0];
            var est = pt.y.toFixed(2);
            var lower = (pt.y - pt['error_y.arrayminus']).toFixed(2)
            var upper = (pt.y + pt['error_y.array']).toFixed(2)
            var sex = (pt.data.xaxis === 'x') ? 'persons' : pt.data.xaxis === 'x2' ? 'females' : 'males';
            var body = `If 10,000 ${sex} of age ${pt.x - 2.5} - ${pt.x + 2.5} years are followed for one year, on average ${est} (95% CI: ${lower} - ${upper}) of them will be diagnosed with '", unique(DX_()$desc_EN), "' for the first time.`;
            var $interpret = $('#interpret');
            $interpret.find('.offcanvas-body').html(body);
            var canvas = new bootstrap.Offcanvas($interpret[0]);
            canvas.show();
          })
        }")
      )
  })

  output$mrr_age <- renderPlotly({
    req(input$disorder_id)

    incidence1 <- filter(incidence, id == input$disorder_id)

    rates <- rbind(incidence0, incidence1) %>%
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

    ratios <- filter(MRRage, id %in% input$disorder_id, sex %in% sex(), cause %in% input$cause)

    validate(
      need(nrow(rates) > 0 && nrow(ratios), "No estimates available")
    )

    mrr_by_age(rates, ratios)
  })


  output$mrr_time <- renderPlotly({
    req(input$disorder_id)

    lag <- DX_() %>%
      left_join(MRRlagged, by = c("id", "sex")) %>%
      filter(exposure != 0)

    g1 <- ggplot(lag, aes(x = exposure, y = est)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      geom_point(aes(text = ci)) +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
      geom_line(linetype = "dashed") +
      facet_wrap(~sex, ncol = 3, drop = FALSE) +
      scale_x_continuous(breaks = 1:6, labels = c("0-6 months", "6-12 months", "1-2 years", "2-5 years", "5-10 years", "10+ years")) +
      scale_y_log10() +
      labs(x = NULL, y = NULL)

    g1 <- ggplotly2(g1) %>%
      layout(
        xaxis = list(title = "Time since first diagnosis"),
        yaxis = list(title = "Mortality rate ratio (95% CI)")
      )
  })

  output$lyl_age <- renderPlotly({

    d <- bind_rows(
      mutate(lifeExp0, group = "Entire Danish population", ci = format_numbers(est, 2)),
      lifeExp %>%
        filter(id %in% input$disorder_id) %>%
        mutate(group = "Diagnosed with the disorder")
    )

    g <- ggplot(d, aes(x = age, y = est, color = group)) +
      geom_ribbon(aes(
        ymin = ifelse(is.na(lower), est, lower),
        ymax = ifelse(is.na(upper), est, upper),
        fill = group), alpha = 0.5) +
      geom_line(aes(customdata = ci)) +
      scale_y_continuous(limits = c(0, NA)) +
      facet_grid(~sex) +
      xlab("Age in years") +
      ylab("Remaining life expectancy\nafter diagnosis (in years)")

    gg <- ggplotly2(g) %>%
      style(hovertemplate = "%{customdata}<extra></extra>") %>%
      layout(
        hovermode = "x",
        legend = list(y = 1.15),
        margin = list(l = 80)
      )

    gg$x$data <- lapply(gg$x$data, function(tr){
      if (isTRUE(tr$fill == "toself")) {
        tr$hovertemplate <- NULL
      } else {
        tr$showlegend <- FALSE
      }
      tr
    })

    gg
  })


  output$survival <- renderPlotly({

    chapter <- unique(DX_()$chapter)
    browser()
    survival <- vroom::vroom(file = paste0("data-raw/plot_chapter", chapter, ".txt"), delim = " ") %>%
      filter(id %in% input$disorder_id) %>%
      filter(time <= 100)

    # TODO: implement a dropdown for cod
    cause <- "All causes"

    if (cause == "All causes") {
      survival <- survival %>%
        mutate(
          cause = ifelse(cause == "Censored", "Censored", "Dead")
        ) %>%
        group_by(cause, group, sex, pct, time) %>%
        summarise(cip = sum(cip)) %>%
        ungroup() %>%
        mutate(
          cause = factor(cause, levels = c("Censored", "Dead"), labels = c("Alive", "Dead (all causes)")),
          group = factor(group, levels = c("Diagnosed", "All population"), labels = c("Diagnosed with the disorder", "Entire Danish population"))
        )
    } else {
      survival <- survival %>%
        mutate(
          cause = factor(cause, levels = c("Censored", "Natural", "External", "Dead"), labels = c("Alive", "Dead (natural causes)", "Dead (external causes)", "Dead (all causes)")),
          group = factor(group, levels = c("Diagnosed", "All population"), labels = c("Diagnosed with the disorder", "Entire Danish population"))
        )
    }

    survival <- survival %>%
      bind_rows(
        survival %>%
          group_by(cause, group, sex, pct) %>%
          slice(n()) %>% ungroup() %>%
          mutate(time = 100)
      ) %>%
      distinct()

    # TODO: radio buttons for choosing percentile
    input_pct <- 50

    survival_pct <- filter(survival, pct == input_pct)
    min_age <- min(survival_pct$time)

    g <- ggplot() +
      facet_wrap(~sex, drop = FALSE) +
      xlab("Age in years") +
      ylab("Percentage of persons dead") +
      scale_x_continuous(breaks = c(min_age, seq(0, 100, 10)))

    if (cause == "All causes") {
      g <- g + geom_line(
        data = filter(survival_pct, cause == "Dead (all causes)"),
        aes(x = time, y = 100 * cip, color = group)
      )
    } else {
      g <- g + geom_line(
        data = mutate(survival_pct, cause = recode(cause, "Dead (external causes)" = " External", "Dead (natural causes)" = " Natural")) %>%
          filter(cause %in% c(" External", " Natural")),
        aes(x = time, y = 100 * cip, color = group, linetype = cause)
      )
    }

    ggplotly2(g, tooltip = c("y", "x")) %>%
      style(hovertemplate = "%{y:.1f}%") %>%
      layout(
        hovermode = "x",
        xaxis = list(hoverformat = ".1f"),
        xaxis2 = list(hoverformat = ".1f"),
        legend = list(y = 1.1)
      )
  })

}

shinyApp(ui, server)
