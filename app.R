library(shiny)
library(bslib)
library(htmltools)
library(dplyr)
library(plotly)
library(scales)
library(rlang)
library(htmlwidgets)


# TODO: before deploying change this to
# shinyOptions(cache = cachem::cache_disk(file.path(dirname(tempdir()), "myapp-cache"))
bindCache <- function(x, ...) x


# https://sashamaps.net/docs/resources/20-colors/
cols20 <- c(
  '#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4',
  '#46f0f0', '#f032e6', '#bcf60c', '#808080', '#fabebe', '#008080',
  '#e6beff', '#9a6324',  '#800000', '#aaffc3', '#808000', '#ffd8b1',
  '#000075', '#000000'
  # only 20 chapters needed (so far)
  # '#ffffff',  '#fffac8'
)

theme_set(theme_bw(base_family = "Roboto Slab"))

# thematic theme is set in user session based on UI state
shiny::onStop(thematic::thematic_off)

# Load list of diseases
DX <- readRDS("data/DX.rds")

disorders <- DX %>%
  select(id, description, level, chapter) %>%
  distinct()

chapters_broad <- disorders %>%
  filter(level == 1)

chapter_broad_lvls <- DX %>%
  filter(level == 1) %>%
  pull(chapter_label) %>%
  unique() %>%
  as.character()


chapter_broad_lvls <- setNames(
  chapter_broad_lvls,
  sapply(strsplit(chapter_broad_lvls, " "), "[[", 1)
)

chapters_specific <- disorders %>%
  filter(level != 1, !chapter %in% c(998, 999)) %>%
  left_join(
    select(
      chapters_broad, chapter, desc_broad = description),
    by = "chapter"
  )

gmc_broad <- disorders %>%
  filter(id %in% (100000 + c(1, 9, 13, 16, 21, 24, 27, 30, 31, 39)))

gmc_specific <- disorders %>%
  filter(id %in% (100000 + c(2:8, 10:12, 14, 15, 17:20, 22, 23, 25, 26, 28:30, 32:39)))

gmc_ids <- c(gmc_broad$id, gmc_specific$id)

gmc_map <- readRDS("data/gmc_map.rds")

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

# -----------------------------------------------------------------
# User interface
# -----------------------------------------------------------------

ui <- function(request) {

  THEME <- bs_theme(
    version = 5, primary = "#ED3A3B",
    "nav-pills-link-active-color" = "white",
    base_font  = list(
      font_google("Roboto Slab", wght = "100..900"),
      font_google("Montserrat")
    )
  ) %>%
    # TODO: bslib should do better
    bs_add_rules(".accordion h2 {margin-top: 0}") %>%
    bs_add_rules(
      lapply(dir("scss", full.names = TRUE), sass::sass_file)
    )

  HEAD <- withTags(head(
    # TODO: remove noindex after publication
    meta(name = "robots", content = "noindex"),
    link(
      href = "https://stackpath.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css",
      rel = "stylesheet"
    ),
    #script(src = "js/down-arrow.js"),
    script(src = "js/scroll-to.js")
  ))

  down_arrow <- function(msg, href) {
    div(
      class = "py-3 down-arrow d-flex flex-row text-center text-primary",
      style = "justify-content: center",
      a(
        href = href,
        p(class = "lead", msg),
        #p(style = "font-size: 5rem", "﹀")
        tags$i(
          style = "font-size: 2rem",
          class = "fa fa-chevron-down lead"
        )
      )
    )
  }

  INTRO <- withTags(
    div(
      id = "intro",
      class = "container",
      #style = "height: 100vh; position:relative",
      h2(
        class = "mt-4 text-center",
        style = "width: 100%; max-width: 900px; margin-left:auto; margin-right:auto",
        "An analysis of mortality metrics associated with a comprehensive range of disorders: the Danish atlas of disease mortality"
      ),
      hr(style = "width: 100%; max-width: 600px", class = "mx-auto"),
      div(
        align = "center", class = "mt-4",
        img(
          style = "width: 100%; max-width: 600px",
          src = "images/people.png"
        )
      ),
      hr(style = "width: 100%; max-width: 600px", class = "mx-auto my-3"),
      #p(
      #  class = "my-3 text-center text-secondary",
      #  style = "width: 100%; max-width: 600px; margin-left:auto; margin-right:auto",
      #  "A study by Oleguer Plana-Ripoll et. al."
      #),
      #p(
      #  class = "lead my-3 text-center",
      #  style = "width: 100%; max-width: 700px; margin-left:auto; margin-right:auto",
      #  "Abstract coming soon"
      #),
      #div(
      #  align = "center", class = "mt-5",
      #  img(class = "img-fluid", src = "images/NBPlogo.jpg", width="250px")
      #),
      #down_arrow("Explore", "#pills")
    )
  )

  clickToInform <- helpText(
    "Click on a point for more information",
    class = "lead", style = "align-self:flex-end"
  )

  PILLS <- navs_pill(
    id = "pills",
    header = conditionalPanel(
      "input.pills !== 'individual'",
      tagList(
        div(
          class = "pt-4 d-flex justify-content-center",
          tags$span("Overview of ", class = "lead px-2"),
          selectInput("sex", NULL, c("persons", "men", "women", "men vs women" = "m/w"), selectize = FALSE, width = "auto"),
          tags$span("dying of ", class = "lead px-2"),
          selectInput("cause", NULL, c("any" = "All", "natural" = "Natural", "external" = "External"), selectize = FALSE, width = "auto"),
          tags$span("cause", class = "lead px-2"),
          tagAppendAttributes(
            switchInput("show_ci", "Show 95% CI", FALSE),
            style = "margin-left: 1.50rem"
          )
        )
      )
    ),
    nav_spacer(),
    nav(
      "ICD-10 Chapters", value = "icd",
      accordion(
        accordion_item(
          "Mortality measures for all ICD-10 Chapters",
          plotlyOutput("overview_icd")
        ),
        accordion_item(
          "Mortality rate ratios for disorders within ICD-10 Chapters",
          plotlyOutput("mrr_icd")
        ),
        accordion_item(
          "Life years lost for disorders within ICD-10 Chapters",
          plotlyOutput("lyl_icd")
        )
      )
    ),
    nav(
      "All ICD-10 disorders", value = "individual",
      div(
        style = "display:flex;flex-direction:column;align-items:center;margin-top:1rem;margin-bottom:1.5rem",
        selectizeInput(
          "disorder_id", NULL,
          c("Search for a disorder" = "", setNames(disorders$id, disorders$description)),
          options = list(
            score = I(sprintf("function(search) {
              var score = this.getScoreFunction(search);
              var gmc_ids = %s;
						  return function(item) {
							  return score(item) * (gmc_ids.indexOf(item.value) == -1);
						  };
            }", jsonlite::toJSON(as.character(gmc_ids))))
          )
        ),
        conditionalPanel(
          "input.disorder_id !== ''",
          helpText("To search for another disorder, click the dropdown above, then press ", tags$kbd("Backspace"), "(", tags$kbd("Delete"), "on Mac), then enter the disorder.")
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
            "Age at diagnosis", show = FALSE,
            plotlyOutput("diagnosis_age", height = 300)
          ),
          accordion_item(
            "Age at death", show = FALSE,
            plotlyOutput("death_age", height = 300)
          ),
          accordion_item(
            "Incidence rate given age", show = FALSE,
            tagList(
              plotlyOutput("incidence_age", height = 300),
              clickToInform
            )
          ),
          accordion_item(
            "Mortality rates and mortality rate ratio given age",
            show = FALSE,
            tagList(
              plotlyOutput("mrr_age", height = 300),
              clickToInform,
              uiOutput("mrr_age_sex")
            )
          ),
          # TODO: add a control for sex?
          accordion_item(
            "Mortality rate ratio given time since diagnosis", show = FALSE,
            tagList(
              tagAppendAttributes(
                plotlyOutput("mrr_time", height = 300, width = "67%")[[1]],
                style = "align-self:center;"
              ),
              clickToInform
            )
          ),
          accordion_item(
            "Expected remaining life expectancy at a given age",
            show = FALSE,
            tagAppendAttributes(
              plotlyOutput("lyl_age", height = 300, width = "80%")[[1]],
              style = "align-self:center;"
            )
          ),
          accordion_item(
            "Survival curves", show = FALSE,
            tagList(
              plotlyOutput("survival"),
              uiOutput("survival_controls")
            )
          )
        )
      )
    ),
    nav(
      "Selected disorders", value = "gmc",
      accordion(
        accordion_item(
          "Mortality measures for broad categories of selected disorders",
          plotlyOutput("overview_gmc")
        ),
        accordion_item(
          "Mortality rate ratios for selected disorders within each broad category",
          plotlyOutput("mrr_gmc")
        ),
        accordion_item(
          "Life years lost for selected disorders within each broad category",
          plotlyOutput("lyl_gmc")
        )
      )
    ),
    nav_spacer()
  )


  logo_img <- function(src, href = NULL, ...) {
    div(
      class = "col-lg-2 my-3 text-center",
      a(href = href, img(class = "img-fluid", src = src, ...))
    )
  }


  LOGO <- div(
    id = "logo",
    class = "container",
    h1("Brought to you by:", class = "my-5 text-center"),
    div(
      class = "row justify-content-center align-items-center my-5",
      logo_img(
        "images/NBPlogo.jpg",
        "http://econ.au.dk/the-national-centre-for-register-based-research/niels-bohr-professorship/",
        width = "250px"
      ),
      div(class = "w-100 my-3"),
      logo_img("images/aarhus_university_logo.png", "http://www.au.dk/en/"),
      logo_img("images/DNRF.png", "https://dg.dk/en/"),
      logo_img("images/LUF.png", "https://lundbeckfonden.com/", style = "height:125px;"),
      logo_img("images/UQlogo.png", "https://www.uq.edu.au"),
      logo_img("images/statensSerumInstitut.png", "https://en.ssi.dk/"),
      div(class = "w-100 my-3"),
      logo_img("images/harvard.png", "https://www.harvard.edu"),
      logo_img("images/EU_flag_yellow_high.jpg"),
      logo_img("images/WMH Namestyle.jpg", "https://www.westmoreton.health.qld.gov.au/")
    )
  )

  ABOUT <- div(
    id = "about",
    class = "bg-light text-center",
    style = "height: 600px; padding-top: 100px",
    div(
      class = "container",
      h1("Learn more"),
      p(
        class = "lead my-5",
        style = "width: 100%; max-width: 800px; margin-left:auto; margin-right:auto",
        "This website aims to provide more information on our scientific publication in ",
        "TO BE DETERMINED.",
        #"The study was ", a(
        #  "pre-registered at ClinicalTrials.gov. ",
        #  href = "https://clinicaltrials.gov/ct2/show/NCT03847753"
        #),
        "See the links below for information about this study (i.e., scientific publication, pre-registered protocol, code, and aggregated summary data), ",
        "our contact information, and more about our work on psychiatric  epidemiology at ",
        a(href = "https://www.nbepi.com", "nbepi.com", .noWS = "after"),
        ". To cite the paper use TO BE DETERMINED."
      ),
      div(
        class="row justify-content-center",
        # TODO: paper link
        a(class = "btn btn-primary btn-xl text-uppercase", href = "#", "Paper"),
        a(class = "btn btn-primary btn-xl text-uppercase", href = "https://osf.io/zafhu", "Protocol"),
        a(class = "btn btn-primary btn-xl text-uppercase", "data-bs-toggle"="modal", href="https://osf.io/zafhu", "Code and summary data"),
        a(class="btn btn-primary btn-xl text-uppercase", href="mailto:j.mcgrath@uq.edu.au", "Contact"),
        a(class="btn btn-primary btn-xl text-uppercase", href="https://www.nbepi.com", "Epi.com")
      )
    )
  )

  shiny::addResourcePath("data", "data-raw")

  ABOUT_MODAL <- modal(
    id = "dataDownload", title = "Data download",
    body = "Many datasets are used in this webpage, all available for download using the buttons below. ",
    buttons = list(
      "data/list_dx.txt" = "Condition info and stats",
      "data/MRR.txt" = "Mortality rate ratios (MRR)",
      "data/MRRage.txt" = "MRR given age",
      "data/MRRlagged.txt" = "MRR given time since diagnosis",
      "data/LYL.csv" = "Life years lost (LYL)",
      "data/LYLages.csv" = "LYL given age",
      "data/incidence.csv" = "Incidence rate given age"
    )
  )

  FOOTER <- withTags(footer(
    class = "bg-light",
    style = "width:100vw; height:100px; position:absolute; left:0;",
    div(
      class = "container my-5",
      div(
        class = "row",
        div(
          class = "col-auto",
          span(
            class = "copyright",
            HTML("Copyright &copy; Atlas of disease mortality 2021")
          )
        ),
        div(
          class="col-auto ml-auto ms-auto",
          ul(
            class="list-inline quicklinks",
            li(
              class="list-inline-item",
              a(href="#", "Privacy Policy")
            ),
            li(
              class="list-inline-item",
              a(href="#", "Terms of Use")
            )
          )
        )
      )
    )
  ))

  fluidPage(
    theme = THEME,
    HEAD,
    INTRO,
    PILLS,
    ABOUT,
    LOGO,
    ABOUT_MODAL,
    FOOTER
  )

}

# ----------------------------------------------------
# Server logic
# ----------------------------------------------------

server <- function(input, output, session) {

  # Before any other observers run, set the theme
  observe(priority = 10000, {
    if (!identical(input$pills, "individual") && length(sex()) < 2) {
      thematic::thematic_on(font = "Roboto Slab", qualitative = cols20)
    } else {
      thematic::thematic_on(font = "Roboto Slab")
    }
  })

  # ------------------------------------
  # Bookmarking logic
  # ------------------------------------

  plotly_events <- eval(formals(plotly::event_data)$event)

  setBookmarkExclude(c(
    ".clientValue-default-plotlyCrosstalkOpts",
    paste0(plotly_events, "-A"),
    paste0(plotly_events, "-display_disorder"),
    paste0("summary_table_", c("rows_selected", "state", "search", "cell_clicked", "columns_selected", "cells_selected", "rows_current", "rows_all"))
  ))

  # Update URL everytime any input changes
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })

  # For some reason input$disorder_id doesn't restore properly
  onRestored(function(state) {
    id <- state$input$disorder_id
    if (length(id)) {
      updateSelectizeInput(session, "disorder_id", selected = id)
    }
    session$sendCustomMessage("scroll-to", list(selector = "#pills"))
  })

  # ----------------------------------------------------
  # Reactive building blocks
  # ----------------------------------------------------

  is_gmc_view <- reactive(identical(input$pills, "gmc"))

  ids_broad <- reactive({
    if (is_gmc_view()) gmc_broad$id else chapters_broad$id
  })
  ids_specific <- reactive({
    if (is_gmc_view()) {
      return(gmc_specific$id)
    }
    chapters_specific$id
  })

  sex <- reactive({
    if (identical(input$sex, "m/w")) c("men", "women") else input$sex
  })

  cause <- reactive(tolower(input$cause))
  show_ci <- reactive(isTRUE(input$show_ci))

  MRR_ <- reactive({
    MRR %>%
      repeat_sex_rows() %>%
      filter(sex %in% sex(), cause %in% input$cause) %>%
      distinct()
  })

  MRR_broad <- reactive({
    MRR_() %>% filter(id %in% ids_broad()) %>%
      add_aes(broad = TRUE)
  })

  MRR_specific <- reactive({
    MRR_() %>%
      filter(id %in% ids_specific()) %>%
      add_aes(broad = FALSE) %>%
      left_join(select(chapters_specific, id, desc_broad), by = "id")
  })

  LYL_ <- reactive({
    LYL %>%
      repeat_sex_rows() %>%
      filter(sex %in% sex(), cause %in% input$cause) %>%
      distinct()
  })

  LYL_broad <- reactive({
    LYL_() %>% filter(id %in% ids_broad()) %>%
      add_aes(broad = TRUE)
  })

  LYL_specific <- reactive({
    LYL_() %>%
      filter(id %in% ids_specific()) %>%
      add_aes(broad = FALSE) %>%
      left_join(select(chapters_specific, id, desc_broad), by = "id")
  })

  add_aes <- function(d, broad = TRUE) {
    d <- if (broad) {
      mutate(
        d, y = if (is_gmc_view()) desc else chapter_label,
        y = factor(y, if (is_gmc_view()) names(gmc_map) else chapter_broad_lvls)
      )
    } else {
      mutate(
        d, group = if (is_gmc_view()) gmc else as.character(as.roman(chapter)),
        group = factor(group, if (is_gmc_view()) names(gmc_map) else names(chapter_broad_lvls))
      )
    }

    if (length(sex()) == 2) {
      okabe_pal <- scales::col_factor(thematic::okabe_ito(), domain = NULL)
      return(mutate(d, color = okabe_pal(sex)))
    }

    pal <- scales::col_factor(cols20, domain = NULL)
    mutate(d, color = if (broad) pal(y) else pal(group))
  }

  counts <- reactive({
    d <- filter(DX, id %in% ids_broad(), !is.na(n))
    d <- repeat_sex_rows(d)
    filter(d, sex %in% sex()) %>%
      rename(desc = desc_EN) %>%
      add_aes(broad = TRUE)
  })

  # ----------------------------------------------------
  # ICD and GMC overview tabs (implemented as modules in R/overview.R)
  # ----------------------------------------------------

  output$overview_icd <- renderPlotly({
    overview_plot(
      counts(), MRR_broad(), LYL_broad(),
      sex(), show_ci(), cause()
    )
  }) %>%
    bindCache("overview_icd", sex(), show_ci(), cause())

  output$overview_gmc <- renderPlotly({
    overview_plot(
      counts(), MRR_broad(), LYL_broad(),
      sex(), show_ci(), cause()
    )
  }) %>%
    bindCache("overview_gmc", sex(), show_ci(), cause())

  output$mrr_icd <- renderPlotly({
    d <- MRR_specific()
    cis_by_category(
      d,
      ytitle = paste0(
        "Mortality Rate Ratios for ", cause(), " causes"
      ),
      xtitle = "ICD-10 Chapters (hover for description)",
      ymin = min(d$est, na.rm = TRUE),
      show_ci = show_ci(),
      jitter = TRUE
    ) %>%
      add_annotations(
        text = "Hover/click for\nmore info",
        x = 0.8, y = 0.8,
        xref = "paper", yref = "paper",
        ax = -50, ay = -40,
        font = list(size = 11)
      ) %>%
      layout(
        yaxis = list(
          type = "log", tickformat = ".1r", nticks = 5,
          range = extendrange(log10(d$est))
        )
      )
  }) %>%
    bindCache("mrr_icd", sex(), cause(), show_ci())

  output$mrr_gmc <- renderPlotly({
    d <- MRR_specific()
    cis_by_category(
      d,
      ytitle = paste0(
        "Mortality Rate Ratios for ", cause(), " causes"
      ),
      show_ci = show_ci(),
      jitter = FALSE
    ) %>%
      layout(
        yaxis = list(
          type = "log", tickformat = ".1r", nticks = 5,
          range = extendrange(log10(d$est))
        )
      )
  }) %>%
    bindCache("mrr_gmc", sex(), cause(), show_ci())

  output$lyl_icd <- renderPlotly({
    d <- LYL_specific()
    cis_by_category(
      d,
      ytitle = paste0(
        "Life Years Lost due to ", cause(), " causes"
      ),
      xtitle = "ICD-10 Chapters (hover for description)",
      ymin = min(d$est, na.rm = TRUE),
      yint = 0,
      show_ci = show_ci(),
      jitter = TRUE
    ) %>%
      layout(
        yaxis = list(
          range = extendrange(d$est)
        )
      )
  }) %>%
    bindCache("lyl_icd", sex(), cause(), show_ci())

  output$lyl_gmc <- renderPlotly({
    d <- LYL_specific()
    cis_by_category(
      d,
      ytitle = paste0(
        "Life Years Lost due to ", cause(), " causes"
      ),
      yint = 0,
      show_ci = show_ci(),
      jitter = FALSE
    ) %>%
      layout(
        yaxis = list(
          range = extendrange(d$est)
        )
      )
  }) %>%
    bindCache("lyl_gmc", sex(), cause(), show_ci())

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
    req(nrow(DX_()) > 0)

    dx <- filter(DX_(), include == 1)
    sexes <- unique(dx$sex)
    default <- if ("persons" %in% sexes) "persons" else sexes[1]
    selected <- input$sex_headline %||% default

    summary_headline(
      filter(dx, sex %in% selected),
      filter(DX_MRR(), sex %in% selected, cause %in% "All"),
      filter(DX_LYL(), sex %in% selected, cause %in% "All"),
      dropdown('sex_headline', !!!sexes, selected = selected)
    )
  })

  output$summary_table <- DT::renderDataTable({
    req(nrow(DX_MRR()) > 0)

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

  output$diagnosis_age <- renderPlotly({
    req(input$disorder_id)

    d <- filter(ages, id == input$disorder_id)
    validate(
      need(nrow(d) > 0, message = "Estimates coming soon")
    )

    g <- ggplot(d, aes(x = dx, y = pct)) +
      geom_line(color = "black") +
      xlab("Age in years") +
      ylab("Percentage diagnosed") +
      scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
      facet_grid(
        ~factor(sex, levels = c("persons", "men", "women")),
        drop = TRUE
      )

    withr::with_options(
      list(digits = 1),
      ggplotly2(g) %>%
        style(hovertemplate = "%{y:.1f}% of the diagnosed have received\nthe diagnosis before age %{x:.0f}<extra></extra>")
    )
  })

  output$death_age <- renderPlotly({
    req(input$disorder_id)

    d <- filter(ages, id == input$disorder_id)
    validate(
      need(nrow(d) > 0, message = "Estimates coming soon")
    )

    g <- ggplot(d, aes(x = death, y = pct)) +
      geom_line(color = "black") +
      xlab("Age in years") +
      ylab("Percentage deceased") +
      scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
      facet_grid(
        ~factor(sex, levels = c("persons", "men", "women")),
        drop = TRUE
      )

    withr::with_options(
      list(digits = 1),
      ggplotly2(g) %>%
        style(hovertemplate = "%{y:.1f}% of the diagnosed\npassed away by age %{x:.0f}<extra></extra>")
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
          "Age: ", age_group, "-", age_group + 5, "\n",
          format_ci(dx_rate, dx_rate_left, dx_rate_right, n = 2)
        ),
        customdata = wrap(glue::glue(
          "If 10,000 {sex} of age {age_group}-{age_group+5} years are followed for one year, on average <b>{format_numbers(dx_rate, 2)}</b> of them will be diagnosed with the disorder for the first time (IR = {format_numbers(dx_rate, 2)} (95% CI: {format_numbers(dx_rate_left, 2)}-{format_numbers(dx_rate_right, 2)}))"
        ))
      )

    g <- ggplot(d, aes(x = age_group + 2.5, y = dx_rate, text = text)) +
      geom_line(linetype = "dashed") +
      geom_point(aes(customdata = customdata)) +
      geom_errorbar(aes(ymin = dx_rate_left, ymax = dx_rate_right)) +
      xlab("Age in years") +
      ylab("Incidence rate (per 10,000 person-years)") +
      scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
      facet_grid(
        ~factor(sex, levels = c("persons", "men", "women")),
        drop = TRUE
      )

    ggplotly2(g) %>%
      show_customdata_offcanvas()
  })


  DX_incidence <- reactive({
    filter(incidence, id == input$disorder_id)
  })

  output$mrr_age_sex <- renderUI({
    sexes <- unique(DX_incidence()$sex)
    drop <- selectInput("mrr_age_sex", NULL, sexes, selectize = FALSE, width = "fit-content")
    if (length(sexes) < 2) {
      drop <- tagAppendAttributes(drop, style = "display:none;")
    }
    drop
  })

  MRRage_ <- reactive({
    filter(MRRage, id %in% input$disorder_id, cause %in% "All")
  })

  output$mrr_age <- renderPlotly({
    req(input$disorder_id, input$mrr_age_sex)

    rates <- rbind(incidence0, DX_incidence()) %>%
      filter(sex %in% input$mrr_age_sex) %>%
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
        color = ifelse(id == 1, "Diagnosed with the disorder", "Entire Danish population"),
        text = paste0(
          "Age: ", age_group, "-", age_group + 5, "\n",
          format_ci(death_rate, death_rate_left, death_rate_right, n = 1)
        ),
        customdata = glue::glue(
          "If 10,000 {sex} of age {age_group}-{age_group+5} years are followed for one year{ifelse(id == 1, ' after diagnosis', '')}, on average <b>{format_numbers(death_rate, 1)}</b> of them will die (IR = {format_numbers(death_rate, 1)} (95% CI: {format_numbers(death_rate_left, 1)}-{format_numbers(death_rate_right, 1)}))"
        )
      )

    ratios <- filter(MRRage_(), sex %in% input$mrr_age_sex)

    if (nrow(ratios) > 0) {
      ratios <- ratios %>%
        mutate(
          text = paste0(
            "Age: ", age_group, "-", age_group + 5, "\n",
            format_ci(est, lower, upper, n = 1)
          ),
          est_display = case_when(
            est < 1 ~ paste0(format_numbers(100 * (1 - est)), "% lower"),
            est < 2 ~ paste0(format_numbers(100 * (est - 1)), "% higher"),
            TRUE ~ paste(format_numbers(est, 1), "times higher")
          ),
          customdata = glue::glue(
            "At age {age_group}-{age_group+5}, those with a diagnosis experience mortality rates {est_display} compared to those of same age and sex without that diagnosis (MRR = {format_numbers(est, 1)} (95% CI: {format_numbers(lower, 1)}-{format_numbers(upper, 1)}))"
          )
        )
    }

    validate(
      need(nrow(rates) > 0, "No estimates available")
    )

    mrr_by_age(rates, ratios) %>%
      show_customdata_offcanvas()
  })


  output$mrr_time <- renderPlotly({
    req(input$disorder_id)

    lag <- DX_() %>%
      left_join(MRRlagged, by = c("id", "sex")) %>%
      filter(exposure != 0)

    g1 <- ggplot(lag, aes(x = exposure, y = est)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      geom_point(aes(text = text, customdata = customdata)) +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
      geom_line(linetype = "dashed") +
      facet_wrap(
        ~factor(sex, levels = c("persons", "men", "women")),
        ncol = 3, drop = TRUE
      ) +
      scale_x_continuous(breaks = 1:6, labels = c("0-6 months", "6-12 months", "1-2 years", "2-5 years", "5-10 years", "10+ years")) +
      scale_y_log10() +
      labs(x = NULL, y = NULL)

    ggplotly2(g1) %>%
      layout(
        xaxis = list(title = "Time since first diagnosis"),
        yaxis = list(title = "Mortality rate ratio (95% CI)")
      ) %>%
      show_customdata_offcanvas()
  })

  output$lyl_age <- renderPlotly({

    d0 <- mutate(lifeExp0, group = "Entire Danish population", ci = format_numbers(est, 1))

    d1 <- lifeExp %>%
      filter(id %in% input$disorder_id) %>%
      mutate(group = "Diagnosed with the disorder")

    ages <- extendrange(d1$age)

    d <- bind_rows(d0, d1) %>%
      filter(between(age, ages[1], ages[2]))

    g <- ggplot(d, aes(x = age, y = est, color = group)) +
      geom_ribbon(aes(
        ymin = ifelse(is.na(lower), est, lower),
        ymax = ifelse(is.na(upper), est, upper),
        fill = group), alpha = 0.5) +
      geom_line(aes(customdata = ci)) +
      facet_grid(~sex) +
      xlab("Age in years") +
      ylab("Remaining life expectancy\nafter diagnosis (in years)")

    gg <- ggplotly2(g) %>%
      style(hovertemplate = "%{customdata}<extra></extra>") %>%
      layout(
        hovermode = "x",
        xaxis = list(hoverformat = ".1f"),
        xaxis2 = list(hoverformat = ".1f"),
        legend = list(y = 1.15),
        margin = list(l = 80)
      )

    gg$x$data <- lapply(gg$x$data, function(tr){
      if (isTRUE(tr$fill == "toself")) {
        # unfortunately setting this to NULL breaks hovermode="x"?
        tr$hovertemplate <- "<extra></extra>"
      } else {
        tr$showlegend <- FALSE
      }
      tr
    })

    gg
  })


  DX_survival <- reactive({
    chapter <- unique(DX_()$chapter)

    vroom::vroom(file = paste0("data-raw/plot_chapter", chapter, ".txt"), delim = " ") %>%
      filter(id %in% input$disorder_id) %>%
      filter(time <= 100) %>%
      mutate(sex = ifelse(sex == "All", "persons", ifelse(sex == "Males", "men", "women")))
  })

  output$survival_controls <- renderUI({
    survival <- DX_survival()

    ages <- distinct(survival, pct, age_specific)

    age <- selectInput(
      "age_survival",
      label = "Starting age",
      choices = setNames(ages$pct, glue::glue_data(ages, "{age_specific} years ({pct}th percentile)")),
      selected = 50,
      selectize = FALSE,
      width = "fit-content"
    )

    causes <- unique(survival$cause)
    if (!"Natural" %in% causes) {
      return(age)
    }

    cause <- selectInput(
      "cause_survival",
      label = "Estimates for all-causes of cause-specific mortality:",
      choices = c("All causes", "Natural and External causes"),
      selected = "All causes",
      selectize = FALSE,
      width = "fit-content"
    )

    div(
      style = "display:flex; justify-content:space-around; margin-top:1rem",
      age, cause
    )
  })


  output$survival <- renderPlotly({
    survival <- DX_survival()
    req(input$age_survival)

    all_causes <- (input$cause_survival %||% "All causes") %in% "All causes"

    if (all_causes) {
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

    survival_pct <- filter(survival, pct == input$age_survival)
    min_age <- min(survival_pct$time)

    g <- ggplot() +
      facet_wrap(
        ~factor(sex, levels = c("persons", "men", "women")),
        drop = TRUE
      ) +
      xlab("Age in years") +
      ylab("Percentage of persons dead") +
      scale_x_continuous(breaks = c(min_age, seq(0, 100, 10)))

    if (all_causes) {
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
      style(hovertemplate = "%{y:.1f}%<extra></extra>") %>%
      layout(
        hovermode = "x",
        xaxis = list(hoverformat = ".1f"),
        xaxis2 = list(hoverformat = ".1f"),
        legend = list(y = 1.1)
      )
  })

}

shinyApp(ui, server, enableBookmarking = "url")
