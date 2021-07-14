library(shiny)
library(bslib)
library(htmltools)
library(dplyr)
library(plotly)
library(scales)
#library(shinyalert)
#library(egg)

source("R/AuxFunctions.R", local = TRUE)

theme_set(theme_bw(base_family = "Roboto Slab"))
thematic::thematic_shiny(font = "Roboto Slab")

read_table <- function(file, ...) {
  tibble::as_tibble(
    data.table::fread(file = file, sep = " ", header = TRUE, ...)
  )
}

# Load list of diseases
DX <- read_table("data/list_dx.txt") %>%
  arrange(chapter, start, level) %>%
  mutate(
    start = as.character(start),
    end = as.character(end),
    desc_EN = as.character(desc_EN),
    chapter_label = paste0(as.roman(chapter), " (", start, "-", end, ")"),
    description = case_when(
      level == "1" ~ paste0("Chapter ", as.roman(chapter), " (", start, "-", end, "): ", desc_EN),
      level == "3" ~ paste0(" ---> ", start, ": ", desc_EN),
      substr(level, 1, 1) == "2" ~ paste0(" -> ", start, "-", end, ": ", desc_EN),
      TRUE ~ desc_EN
    ),
    dx_median = ifelse(
      is.na(age_dx50), "-", paste0(format_numbers(age_dx50, 1), " (", format_numbers(age_dx25, 1), "-", format_numbers(age_dx75, 1), ")")),
    death_median = ifelse(
      is.na(age_death50), "-", paste0(format_numbers(age_death50, 1), " (", format_numbers(age_death25, 1), "-", format_numbers(age_death75, 1), ")"))
  )


disorders <- DX %>%
  select(id, description, level) %>%
  distinct()

chapters <- disorders %>%
  filter(level == 1)

gmc_broad <- disorders %>%
  filter(id %in% (100000 + c(1, 9, 13, 16, 21, 24, 27, 30, 31, 39)))

gmc_specific <- disorders %>%
  filter(id %in% (100000 + c(2:8, 10:12, 14, 15, 17:20, 22, 23, 25, 26, 28:30, 32:39)))


MRR <- read_table("data/MRR.txt") %>%
  select(id, sex, cause = cod, est = HR, lower = CI_left, upper = CI_right) %>%
  mutate(ci = format_ci(est, lower, upper, n = 2)) %>%
  left_join(select(DX, id, desc = desc_EN, chapter, chapter_label), by = "id") %>%
  mutate(text = text)

MRRlagged <- read_table("data/MRRlagged.txt") %>%
  select(id, sex, cause = cod, exposure, est = HR, lower = CI_left, upper = CI_right) %>%
  mutate(ci = format_ci(est, lower, upper, n = 2)) %>%
  left_join(select(DX, id, desc_EN), by = "id")

MRRage <- read_table("data/MRRage.txt") %>%
  mutate(
    age_cat = paste0(age_group, "-", age_group + age_categories)
  ) %>%
  select(id, sex, cause = cod, age_cat, est = HR, lower = CI_left, upper = CI_right) %>%
  mutate(ci = format_ci(est, lower, upper, n = 2)) %>%
  left_join(select(DX, id, desc_EN), by = "id")

LYL <- read_table("data/LYL.txt")
LYL <- bind_rows(
  select(LYL, id, sex, est = LYL_Total, lower = LYL_Total_L, upper = LYL_Total_R) %>%
    mutate(cause = "All"),
  select(LYL, id, sex, est = LYL_Natural, lower = LYL_Natural_L, upper = LYL_Natural_R) %>%
    mutate(cause = "Natural"),
  select(LYL, id, sex, est = LYL_External, lower = LYL_External_L, upper = LYL_External_R) %>%
    mutate(cause = "External")
) %>%
  mutate(ci = format_ci(est, lower, upper, n = 2)) %>%
  left_join(select(DX, id, desc = desc_EN, chapter, chapter_label), by = "id") %>%
  mutate(text = text)

# TODO: cache this
#LYLage <- read_table("data/LYLages.txt")
#LYLage <- bind_rows(
#  select(LYLage, id, sex, age, est = LYL_Total, lower = LYL_Total_L, upper = LYL_Total_R) %>%
#    mutate(cause = "All"),
#  select(LYLage, id, sex, age, est = LYL_Natural, lower = LYL_Natural_L, upper = LYL_Natural_R) %>%
#    mutate(cause = "Natural"),
#  select(LYLage, id, sex, age, est = LYL_External, lower = LYL_External_L, upper = LYL_External_R) %>%
#    mutate(cause = "External")
#) %>%
#  mutate(ci = format_ci(est, lower, upper, n = 2)) %>%
#  left_join(select(DX, id, desc = desc_EN, chapter, chapter_label), by = "id")


ages <- read_table("data/ages.txt")

incidence <- read_table("data/incidence.txt")

ui <- fluidPage(
  div(
    style = "display:flex;justify-content:center",
    radioButtons("summary", "Choose summary", c(
      "IDC Chapters" = "idc",
      "General Medical Conditions" = "gmc"
    )),
    selectInput("sex", "Sex", c("Persons" = "All", "Men Only" = "Males", "Women Only" = "Females", "Men vs Women" = "M/F")),
    selectInput("cause", "Cause", unique(MRR$cause)),
    #checkboxInput("air", "")
  ),
  plotlyOutput("overview", width = "80%"),
  plotlyOutput("mrr"),
  plotlyOutput("lyl")
)

server <- function(input, output, session) {

  is_gmc_view <- reactive({
    identical(input$summary, "gmc")
  })

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
      ylab(NULL) + xlab(paste0("Mortality Rate Ratios for ", tolower(input$cause), " causes (95% CI)"))

    p_lyl <- ggplot(lyl, aes(x = est, y = y,  text = text)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
      ylab(NULL) + xlab(paste0("Life Years Lost due to ", tolower(input$cause), " causes"))

    if (length(sex) == 2) {
      p_counts <- p_counts + geom_col(aes(color = sex), position = "dodge")
      # TODO: need to fix position?
      p_mrr <- p_mrr + geom_point(aes(color = sex)) +
        geom_errorbarh(aes(color = sex, xmin = lower, xmax = upper))
      p_lyl <- p_lyl + geom_point(aes(color = sex)) +
        geom_errorbarh(aes(color = sex, xmin = lower, xmax = upper))
    } else {
      p_counts <- p_counts + geom_col()
      p_mrr <- p_mrr + geom_point() +
        geom_errorbarh(aes(xmin = lower, xmax = upper))
      p_lyl <- p_lyl + geom_point() +
        geom_errorbarh(aes(xmin = lower, xmax = upper))
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
      mutate(
        text = paste(ci, "\n", desc),
        group = as.roman(chapter)
      ) %>%
      # TODO: why are there repeated values?
      distinct()

    title <- paste0(
      "Mortality Rate Ratios for ",
      tolower(input$cause), " causes (95% CI)"
    )

    cis_by_category(dat, ytitle = title) %>%
      layout(
        yaxis = list(type = "log", range = log10(c(0.3, 180)))
      )
  })

  output$lyl <- renderPlotly({
    # TODO: how to handle the gmc case?
    dat <- LYL %>%
      filter(id %in% ids_specific(), sex %in% sex(), cause %in% input$cause) %>%
      mutate(
        text = paste(ci, "\n", desc),
        group = as.roman(chapter)
      ) %>%
      # TODO: why are there repeated values?
      distinct()

    title <- paste0(
      "Life Years Lost due to ",
      tolower(input$cause), " causes"
    )

    cis_by_category(dat, ytitle = title) %>%
      layout(
        yaxis = list(type = "log", range = log10(c(0.3, 180)))
      )
  })
}

shinyApp(ui, server)
