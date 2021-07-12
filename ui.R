library(shiny)
library(bslib)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(shinyjs)
library(shinydashboard)
library(shinyalert)
library(DT)
library(htmltools)

plotlyOutput2 <- function(..., height = 550) {
  plotlyOutput(..., height = height)
}

h6 <- function(...) {
  tags$p(class = "lead", style = "font-size:0.85rem", ...)
}

fixedPage(
  theme = bs_theme(
    version = 4,
    base_font = font_google("Roboto Slab", wght = c(300, 400))
  ),
  titlePanel("Atlas of Disease Mortality (beta version)"),
  useShinyjs(),
  useShinyalert(),
  tabsetPanel(
    type = "pills",
    tabPanel(
      "List of selected health disorders",
      hr(),
      fluidRow(
        column(
          9,
          prettyCheckboxGroup(
            inputId = "broad_selected999",
            label = "Choose specific disorders:",
            choices = c("Broad 10 categories of disorders", "Specific 31 disorders", "Additional 50 disorders"),
            inline = TRUE,
            status = "primary",
            selected = c("Broad 10 categories of disorders", "Specific 31 disorders"),
            icon = icon("check-square-o")
          )
        ),
        column(
          3,
          actionButton(
            inputId = "btn_list_disorders", label = "See list of disorders",
            icon = icon("info-circle"), width = "100%"
          )
        )
      ),
      bslib::navs_tab_card(

        tabPanel(
          "Summary of several disorders",
          hr(),
          fluidRow(
            column(
              5,
              pickerInput(
                inputId = "diseases_all999",
                label = "Select/deselect disorders",
                choices = NULL,
                selected = NULL,
                options = list(
                  `actions-box` = TRUE
                ),
                multiple = TRUE
              )
            ),
            column(
              4,
              radioGroupButtons(
                inputId = "sex999",
                label = "Sex",
                choices = c("All", "Males", "Females", "M vs. F"),
                status = "primary",
                selected = "All"
              )
            ),
            column(
              3,
              radioGroupButtons(
                inputId = "cod999",
                label = "Cause of death",
                choices = c("All", "Natural", "External"),
                status = "primary",
                selected = "All"
              )
            )
          ),
          hidden(div(
            id = "main_panel_all999",
            actionButton(
              inputId = "button_MRRLYLall999", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-down"), label = "Click to hide a panel of number of diagnosed, mortality rate ratios and life years lost"
            ),
            div(
              id = "MRRLYL_all999_show",
              plotlyOutput2("MRR_LYL_all999"),
              h6("This figures show the following estimates for the selected disorders: (i) the number of individuals living in Denmark in 2000-2018 and diagnosed in a
                                                          hospital during 1995-2018; (ii) mortality rate ratios for the selected sex and selected cause of death comparing those diagnosed with those not diagnosed with each specific
                                                              disorder; and (iii)
                                                              average Life Years Lost after diagnosis (difference in remaining life expectancy between those diagnosed and the age- and sex-matched
                                                              general Danish population). Mortality estimates are available for disorders with at least 100 individuals diagnosed and at least 20 deaths. For Life Years Lost, an additional
                                                              requirement is enough diagnosed
                                                              individuals at older ages of follow-up (the survival probability must be lower than 10% when there are less than 10 diagnosed individuals at risk).")
            ),
            actionButton(
              inputId = "button_MRRall999", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-right"), label = "Click to show mortality rate ratios only"
            ),
            hidden(div(
              id = "MRR_all999_show",
              plotlyOutput2("MRR_all999"),
              h6("Mortality rate ratios for the selected sex and selected cause of death comparing those diagnosed with those not diagnosed with each specific
                                                                     disorder for the selected disorders Estimates are available for disorders with at least 100 individuals diagnosed and at least 20 deaths.")
            )),
            actionButton(
              inputId = "button_LYLall999", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-right"), label = "Click to show life years lost only"
            ),
            hidden(div(
              id = "LYL_all999_show",
              plotlyOutput2("LYL_all999"),
              h6("Average Life Years Lost after diagnosis (difference in remaining life expectancy between those diagnosed and the age- and sex-matched
                                                                     general Danish population). Estimates available for disorders with at least 100 individuals diagnosed and at least 20 deaths, as long as there are enough diagnosed
                                                                     individuals at older ages of follow-up (the survival probability must be lower than 10% when there are less than 10 diagnosed individuals at risk).")
            )),
            actionButton(
              inputId = "button_airall999", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-right"), label = "Click to show mortality rate ratios after adjustment for air pollution"
            ),
            hidden(div(
              id = "air_all999_show",
              plotlyOutput2("MRR_air_plot999"),
              h6("Mortality rate ratios for all causes and both sexes combined comparing those diagnosed with those not diagnosed with each specific
                                                                     disorder for the selected disorders. One estimate is adjusted for air pollution, while the other is not (all estimates are adjusted
                                                                     for age, sex, and calendar time). Estimates are available for disorders with at least 100 individuals diagnosed and at least 20 deaths.")
            ))
          ))
        ),
        tabPanel(
          "One specific disorder",
          hr(),
          fluidRow(
            column(
              6,
              pickerInput(
                inputId = "dx_interest999",
                label = NULL,
                choices = c("--> Choose disorder of interest"),
                selected = NULL,
                options = list(
                  `live-search` = TRUE
                )
              )
            ),
            column(
              6,
              switchInput(
                inputId = "interpret999",
                label = "Text with interpretation",
                value = TRUE,
                labelWidth = "100%"
              )
            )
          ),
          hidden(div(
            id = "panel999",
            hidden(div(
              id = "not_all_estimates999",
              HTML("<b>Note:</b> Some results are not available (panel below disabled) due to small number of individuals with this disorder.")
            )),

            # Main table

            actionButton(
              inputId = "button_main999", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-down"), label = "Click to hide table with main results"
            ),
            div(
              id = "main_panel999",
              htmlOutput("Carson1"),
              # tableOutput("list999"),
              dataTableOutput("list999"),
              htmlOutput("text_main999"),
              hr()
            ),
            actionButton(
              inputId = "button_agedx999", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-right"), label = "Click to show age-specific incidence of the disorder and distribution of age of diagnosis"
            ),
            hidden(div(
              id = "agedx999_show",
              plotlyOutput2("incidence999", height = 300),
              htmlOutput("text_agedx999"),
              plotlyOutput2("ages999", height = 300),
              materialSwitch(
                inputId = "smooth999",
                label = "Smoothed estimates of age of diagnosis",
                value = TRUE,
                width = "100%",
                status = "primary",
                right = TRUE
              ),
              htmlOutput("text_agedx2999"),
              hr()
            )),
            actionButton(
              inputId = "button_agedeath999", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-right"), label = "Click to show age-specific mortality rates and distribution of age of death"
            ),
            hidden(div(
              id = "agedeath999_show",
              plotlyOutput2("mortality999", height = 300),
              htmlOutput("text_agedeath999"),
              plotlyOutput2("ages_death999", height = 300),
              materialSwitch(
                inputId = "smooth_death999",
                label = "Smoothed estimates of age of death",
                value = TRUE,
                width = "100%",
                status = "primary",
                right = TRUE
              ),
              htmlOutput("text_agedeath2999"),
              hr()
            )),
            actionButton(
              inputId = "button_lagged999", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-right"), label = "Click to show mortality rate ratios depending on age and time since diagnosis"
            ),
            hidden(div(
              id = "lagged999_show",
              plotlyOutput2("MRRlagged999", height = 300),
              htmlOutput("text_MRRlagged999"),
              hr()
            )),
            actionButton(
              inputId = "button_LYLages999", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-right"), label = "Click to show age-specific remaining life expectancy"
            ),
            hidden(div(
              id = "LYLages999_show",
              plotlyOutput2("LYLages999", height = 300),
              htmlOutput("text_LYLages999"),
              hr()
            )),
            actionButton(
              inputId = "button_MRR999", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-right"), label = "Click to show mortality due to specific causes of death"
            ),
            hidden(div(
              id = "MRR999_show",
              # tableOutput("MRR_LYL_causes999"),
              dataTableOutput("MRR_LYL_causes999"),
              htmlOutput("text_MRR_LYL_causes999"),
              hr()


              # fluidRow(
              #   column(5,
              #          tableOutput("MRR999")
              #   ),
              #   column(7,
              #          div(id = "LYLcauses_show999",
              #              tableOutput("LYL999")
              #          ),
              #          htmlOutput("text_MRR999"),
              #          hr()
              #   )
              # )
            )),
            actionButton(
              inputId = "button_survival999", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-right"), label = "Click to show age-specific and cause-specific survival curves"
            ),
            hidden(div(
              id = "survival999_show",
              fluidRow(
                column(
                  7,
                  radioGroupButtons(
                    inputId = "age_survival999",
                    label = "Age of estimation (percentile of the age-at-diagnosis distribution)",
                    choices = c("P25", "P50 (median)", "P75"),
                    status = "primary",
                    selected = "P50 (median)"
                  )
                ),
                column(
                  5,
                  radioGroupButtons(
                    inputId = "cause_survival999",
                    label = "Estimates for all-causes of cause-specific mortality:",
                    choices = c("All causes", "Natural and External causes"),
                    status = "primary",
                    selected = "All causes"
                  ),
                  hidden(div(
                    id = "cause_survival_not999",
                    "Estimates available only for all causes due to small number of individuals with this disorder dying of external causes"
                  ))
                )
              ),
              plotlyOutput2("LYLplot999"),
              htmlOutput("text_survival999"),
              hr()
            )),
            actionButton(
              inputId = "button_air999", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-right"), label = "Click to show mortality rate ratios after adjustment for air pollution"
            ),
            hidden(div(
              id = "air999_show",
              dataTableOutput("MRR_air999"),
              htmlOutput("text_MRRair999")
            ))
          ))
        )
      )
    ),
    tabPanel(
      "All ICD-10 diagnoses, groups and chapters",
      hr(),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Summary of several disorders",
          hr(),
          fluidRow(
            column(
              5,
              pickerInput(
                inputId = "diseases_all",
                label = "Select/deselect ICD-10 chapters",
                choices = NULL,
                selected = NULL,
                options = list(
                  `actions-box` = TRUE
                ),
                multiple = TRUE
              )
            ),
            column(
              4,
              radioGroupButtons(
                inputId = "sex",
                label = "Sex",
                choices = c("All", "Males", "Females", "M vs. F"),
                status = "primary",
                selected = "All"
              )
            ),
            column(
              3,
              radioGroupButtons(
                inputId = "cod",
                label = "Cause of death",
                choices = c("All", "Natural", "External"),
                status = "primary",
                selected = "All"
              )
            )
          ),
          hidden(div(
            id = "main_panel_all",
            actionButton(
              inputId = "button_MRRLYLall", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-down"), label = "Click to hide a panel of number of diagnosed, mortality rate ratios and life years lost"
            ),
            div(
              id = "MRRLYL_all_show",
              plotlyOutput2("MRR_LYL_all"),
              h6("This figures show the following estimates for the selected ICD-10 chapters: (i) the number of individuals living in Denmark in 2000-2018 and diagnosed in a
                                                          hospital during 1995-2018; (ii) mortality rate ratios for the selected sex and selected cause of death comparing those diagnosed with those not diagnosed with disorders in each specific
                                                              chapter; and (iii)
                                                              average Life Years Lost after diagnosis (difference in remaining life expectancy between those diagnosed and the age- and sex-matched
                                                              general Danish population). Mortality estimates available for chapters with at least 100 individuals diagnosed and at least 20 deaths. For Life Years Lost, an additional
                                                              requirement is enough diagnosed
                                                              individuals at older ages of follow-up (the survival probability must be lower than 10% when there are less than 10 diagnosed individuals at risk).")
            ),
            actionButton(
              inputId = "button_MRRall", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-right"), label = "Click to show mortality rate ratios only"
            ),
            hidden(div(
              id = "MRR_all_show",
              plotlyOutput2("MRR_all"),
              h6("Mortality rate ratios for the selected sex and selected cause of death comparing those diagnosed with those not diagnosed with each specific
                ICD-10 chapter (left figure) and disorder (right figure). Estimates are available for disorders with at least 100 individuals diagnosed and at least 20 deaths.")
            )),
            actionButton(
              inputId = "button_LYLall", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-right"), label = "Click to show life years lost only"
            ),
            hidden(div(
              id = "LYL_all_show",
              plotlyOutput2("LYL_all"),
              h6("Average Life Years Lost after diagnosis (difference in remaining life expectancy between those diagnosed and the age- and sex-matched
                general Danish population) for those diagnosed with each specific ICD-10 chapter (left figure) and disorder (right figure). Estimates available for disorders with at least 100 individuals diagnosed and at least 20 deaths, as long as there are enough diagnosed
                                                                     individuals at older ages of follow-up (the survival probability must be lower than 10% when there are less than 10 diagnosed individuals at risk).")
            )),
            actionButton(
              inputId = "button_airall", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-right"), label = "Click to show mortality rate ratios after adjustment for air pollution"
            ),
            hidden(div(
              id = "air_all_show",
              plotlyOutput2("MRR_air_plot"),
              # materialSwitch(
              #   inputId = "air_plot_CIs",
              #   label = "Confidence intervals for the ratio of MRRs",
              #   value = TRUE,
              #   width = "100%",
              #   status = "primary",
              #   right = TRUE
              # ),
              h6("Left figure: mortality rate ratios for all causes and both sexes combined comparing those diagnosed with those not diagnosed with a disorder in each specific
                chapter. One estimate is adjusted for air pollution, while the other is not (all estimates are adjusted
                                                                     for age, sex, and calendar time). Right figure: Ratio of mortality ratio ratios (adjusted by not-adjusted for air pollution) for all disorders in the specific ICD-10
                                                                     chapters. A ratio around 1 indicates similar estimates when adjusting or not adjusting for air pollution. The confidence interval for the ratio has been estimated as if the two
                                                                     estimates were independent of each other (which they weren't because they are based on the same population). Estimates are available for disorders with at least 100 individuals diagnosed and at least 20 deaths.")
            ))
          ))
        ),
        tabPanel(
          "One specific disorder",
          hr(),
          fluidRow(
            column(
              6,
              pickerInput(
                inputId = "dx_interest",
                label = NULL,
                choices = c("--> Choose disorder of interest"),
                selected = NULL,
                options = list(
                  `live-search` = TRUE
                )
              )
            ),
            column(
              6,
              switchInput(
                inputId = "interpret",
                label = "Text with interpretation",
                value = TRUE,
                labelWidth = "100%"
              )
            )
          ),
          hidden(div(
            id = "panel",
            hidden(div(
              id = "not_all_estimates",
              HTML("<b>Note:</b> Some results are not available (panel below disabled) due to small number of individuals with this disorder.")
            )),

            # Main table

            actionButton(
              inputId = "button_main", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-down"), label = "Click to hide table with main results"
            ),
            div(
              id = "main_panel",
              # tableOutput("list"),
              dataTableOutput("list"),
              htmlOutput("text_main"),
              hr()
            ),
            actionButton(
              inputId = "button_agedx", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-right"), label = "Click to show age-specific incidence of the disorder and distribution of age of diagnosis"
            ),
            hidden(div(
              id = "agedx_show",
              plotlyOutput2("incidence", height = 300),
              htmlOutput("text_agedx"),
              plotlyOutput2("ages", height = 300),
              materialSwitch(
                inputId = "smooth",
                label = "Smoothed estimates of age of diagnosis",
                value = TRUE,
                width = "100%",
                status = "primary",
                right = TRUE
              ),
              htmlOutput("text_agedx2"),
              hr()
            )),
            actionButton(
              inputId = "button_agedeath", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-right"), label = "Click to show age-specific mortality rates and distribution of age of death"
            ),
            hidden(div(
              id = "agedeath_show",
              plotlyOutput2("mortality", height = 300),
              htmlOutput("text_agedeath"),
              plotlyOutput2("ages_death", height = 300),
              materialSwitch(
                inputId = "smooth_death",
                label = "Smoothed estimates of age of death",
                value = TRUE,
                width = "100%",
                status = "primary",
                right = TRUE
              ),
              htmlOutput("text_agedeath2"),
              hr()
            )),
            actionButton(
              inputId = "button_lagged", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-right"), label = "Click to show mortality rate ratios depending on age and time since diagnosis"
            ),
            hidden(div(
              id = "lagged_show",
              plotlyOutput2("MRRlagged", height = 300),
              htmlOutput("text_MRRlagged"),
              hr()
            )),
            actionButton(
              inputId = "button_LYLages", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-right"), label = "Click to show age-specific remaining life expectancy"
            ),
            hidden(div(
              id = "LYLages_show",
              plotlyOutput2("LYLages", height = 300),
              htmlOutput("text_LYLages"),
              hr()
            )),
            actionButton(
              inputId = "button_MRR", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-right"), label = "Click to show mortality due to specific causes of death"
            ),
            hidden(div(
              id = "MRR_show",
              # tableOutput("MRR_LYL_causes"),
              dataTableOutput("MRR_LYL_causes"),
              htmlOutput("text_MRR_LYL_causes"),
              hr()
              # fluidRow(
              #   column(5,
              #          tableOutput("MRR")
              #   ),
              #   column(7,
              #          div(id = "LYLcauses_show",
              #              tableOutput("LYL")
              #          ),
              #          htmlOutput("text_MRR"),
              #          hr()
              #          )
              #     )
            )),
            actionButton(
              inputId = "button_survival", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-right"), label = "Click to show age-specific and cause-specific survival curves"
            ),
            hidden(div(
              id = "survival_show",
              fluidRow(
                column(
                  7,
                  radioGroupButtons(
                    inputId = "age_survival",
                    label = "Age of estimation (percentile of the age-at-diagnosis distribution)",
                    choices = c("P25", "P50 (median)", "P75"),
                    status = "primary",
                    selected = "P50 (median)"
                  )
                ),
                column(
                  5,
                  radioGroupButtons(
                    inputId = "cause_survival",
                    label = "Estimates for all-causes of cause-specific mortality:",
                    choices = c("All causes", "Natural and External causes"),
                    status = "primary",
                    selected = "All causes"
                  ),
                  hidden(div(
                    id = "cause_survival_not",
                    "Estimates available only for all causes due to small number of individuals with this disorder dying of external causes"
                  ))
                )
              ),
              plotlyOutput2("LYLplot"),
              htmlOutput("text_survival"),
              hr()
            )),
            actionButton(
              inputId = "button_air", width = "100%", style = "color: white; background-color: grey",
              icon = icon("chevron-right"), label = "Click to show mortality rate ratios after adjustment for air pollution"
            ),
            hidden(div(
              id = "air_show",
              dataTableOutput("MRR_air"),
              htmlOutput("text_MRRair")
            ))
          ))
        )
      )
    ),
    tabPanel(
      "About",
      hr(),
      "xxx"
    )
  )
)
