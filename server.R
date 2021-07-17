library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyjs)
library(shinyalert)
library(egg)
library(scales)
library(DT)
library(htmltools)

source("R/AuxFunctions.R", local = TRUE)

theme_set(theme_bw(base_family = "Roboto Slab"))
thematic::thematic_shiny(font = "Roboto Slab")

read_table <- function(file, ...) {
  as.data.frame(
    data.table::fread(file = file, sep = " ", header = TRUE, ...)
  )
}

# Load list of diseases
list_dx <- read_table("data/list_dx.txt")
list_dx <- list_dx %>%
  arrange(chapter, start, level) %>%
  mutate(
    start = as.character(start),
    end = as.character(end),
    desc_EN = as.character(desc_EN),
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
list_unique <- unique(list_dx[!(list_dx$chapter %in% c(998, 999)), c("id", "description")])
list_unique999 <- unique(list_dx[list_dx$chapter %in% c(998, 999), c("id", "description")] %>% arrange(id))
chapters_unique <- unique(list_dx[list_dx$level == 1, c("id", "description", "chapter", "start", "end")])

ages <- read_table("data/ages.txt")

incidence <- read_table("data/incidence.txt")


MRR <- read_table("data/MRR.txt") %>%
  mutate(
    rate = format_numbers(1000 * rate, 2),
    rateDK_std = format_numbers(1000 * rate_DK_std, 2),
    MRR = format_ci(HR, CI_left, CI_right, n = 2),
    MRR_95CI = format_ci(HR, CI_left, CI_right, n = 2, prefix = "95% CI: ")
  )

LYL <- read_table("data/LYL.txt") %>%
  mutate(
    Total = format_ci(LYL_Total, LYL_Total_L, LYL_Total_R, n = 2),
    Total_95CI = format_ci(LYL_Total, LYL_Total_L, LYL_Total_R, n = 2, prefix = "95% CI: "),
    Natural = format_ci(LYL_Natural, LYL_Natural_L, LYL_Natural_R, n = 2),
    Natural_95CI = format_ci(LYL_Natural, LYL_Natural_L, LYL_Natural_R, n = 2, prefix = "95% CI: "),
    External = format_ci(LYL_External, LYL_External_L, LYL_External_R, n = 2),
    External_95CI = format_ci(LYL_External, LYL_External_L, LYL_External_R, n = 2, prefix = "95% CI: ")
  )

LYLages <- read_table("data/LYLages.txt")
MRRlagged <- read_table("data/MRRlagged.txt")
MRRage <- read_table("data/MRRage.txt") %>%
  mutate(
    age_cat = paste0(age_group, "-", age_group + age_categories)
  )


pd <- ggstance::position_dodgev(height = 0.5)

id_broad <- 100000 + c(1, 9, 13, 16, 21, 24, 27, 30, 31, 39)
id_specific <- 100000 + c(2:8, 10:12, 14, 15, 17:20, 22, 23, 25, 26, 28:30, 32:39)
id_additional <- 100000 + c(40:89)

function(input, output, session) {
  output$Carson1 <- renderText({
    HTML("<div style='color:#FF0000'>", "Carson: It would be ideal if the explainations on \"Estimates not available\" below the table could be hidden
    and display if you hover the mouse over NA estimates")
  })


  ###################
  ###################
  ### GMCs: one disorder

  ### For list of GMCs (chapters 998 and 999)
  observeEvent(input$broad_selected999,
               {
                 x <- input$broad_selected999
                 list <- data.frame()

                 if ("Broad 10 categories of disorders" %in% x) {
                   list <- list %>%
                     bind_rows(list_unique999 %>% filter(id %in% (id_broad))) %>%
                     distinct()
                 }

                 if ("Specific 31 disorders" %in% x) {
                   list <- list %>%
                     bind_rows(list_unique999 %>% filter(id %in% (id_specific))) %>%
                     distinct()
                   if ("Broad 10 categories of disorders" %in% x) {
                     list <- list %>%
                       mutate(
                         description = ifelse(id %in% id_specific & !(id %in% id_broad), paste0("- ", description), description)
                       )
                   }
                 }

                 if ("Additional 50 disorders" %in% x) {
                   y <- list_unique999 %>% filter(id %in% (id_additional))
                   if (nrow(list > 0)) {
                     y <- y %>%
                       mutate(description = paste0("Additional: ", description))
                   }
                   list <- list %>% bind_rows(y)
                 }

                 list <- list %>% arrange(id)

                 # Keep the selected disorder if still available
                 selected <- "--> Choose disorder of interest"

                 if (!is.null(x)) {
                   dx_selected <- list$description[gsub("- ", "", gsub("Additional: ", "", list$description)) == gsub("- ", "", gsub("Additional: ", "", input$dx_interest999))]

                   if (length(dx_selected) > 0) {
                     selected <- dx_selected
                   }

                   updatePickerInput(session,
                                     inputId = "dx_interest999",
                                     choices = c("--> Choose disorder of interest", list$description),
                                     selected = selected
                   )
                 } else {
                   updatePickerInput(session,
                                     inputId = "dx_interest999",
                                     choices = "--> Choose disorder of interest",
                                     selected = "--> Choose disorder of interest"
                   )
                 }



                 # Keep the selected disorders if still available
                 selected <- list$description

                 if (!is.null(x)) {
                   dx_selected <- list$description[gsub("- ", "", gsub("Additional: ", "", list$description)) %in% gsub("- ", "", gsub("Additional: ", "", input$diseases_all999))]

                   if (length(dx_selected) > 0) {
                     selected <- dx_selected
                   }
                   updatePickerInput(session,
                                     inputId = "diseases_all999",
                                     choices = list$description,
                                     selected = selected
                   )
                 } else {
                   updatePickerInput(session,
                                     inputId = "diseases_all999",
                                     choices = "",
                                     selected = NULL
                   )
                 }
               },
               ignoreNULL = FALSE
  )

  observeEvent(input$dx_interest999, {
    if (input$dx_interest999 == "--> Choose disorder of interest") {
      shinyjs::hide(id = "panel999")
    } else {
      shinyjs::show(id = "panel999")
    }
  })

  observeEvent(input$btn_list_disorders, {
    # Show a simple modal
    shinyalert(
      title = "List of disorders", type = "info",
      text = "To be done - add list of disorders in each category",
      html = TRUE
    )
  })


  ### Main Table
  observeEvent(input$button_main999, {
    hide_show_arrows(
      button = "button_main999", button2 = input$button_main999, panel = "main_panel999", session = session,
      text = "table with main results"
    )
  })

  output$list999 <- renderDataTable({
    datatable(
      table_main(dx = gsub("- ", "", gsub("Additional: ", "", input$dx_interest999))),
      options = list(pageLength = 20, info = FALSE, lengthChange = FALSE, searching = FALSE, scrollX = FALSE, scrollY = FALSE, paging = FALSE),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: bottom;", htmltools::strong("Estimates not available: Age at diagnosis"), " if 'Diagnosed<100'; ", htmltools::strong("Age at death"), " if 'Deaths<100'; ", htmltools::strong("Mortality rate ratio"), " if 'Diagnosed<100' or 'Deaths<20'; ", htmltools::strong("Life years lost"), " if 'Diagnosed<100', 'Deaths<20' or not enough individuals at risk
                at old ages; ", htmltools::strong("Life years lost"), " for persons if not available for both males and females. A range for ", htmltools::strong("Diagnosed"), " and ", htmltools::strong("Deaths"), " is provided
                when less than 5 individuals could be identified."
      )
    )
  })

  ### Age-specific incidences
  observeEvent(input$button_agedx999, {
    show_hide_arrows(
      button = "button_agedx999", button2 = input$button_agedx999, panel = "agedx999_show", session = session,
      text = "age-specific incidence of the disorder and distribution of age of diagnosis"
    )
  })

  output$incidence999 <- renderPlotly({
    plot_incidence(dx = gsub("- ", "", gsub("Additional: ", "", input$dx_interest999)))
  })

  output$ages999 <- renderPlotly({
    plot_age_dx(dx = gsub("- ", "", gsub("Additional: ", "", input$dx_interest999)), smooth = input$smooth999)
  })


  ### Age-specific mortality rates
  observeEvent(input$button_agedeath999, {
    show_hide_arrows(
      button = "button_agedeath999", button2 = input$button_agedeath999, panel = "agedeath999_show", session = session,
      text = "age-specific mortality rates and distribution of age of death"
    )
  })

  output$mortality999 <- renderPlotly({
    plot_mortality(dx = gsub("- ", "", gsub("Additional: ", "", input$dx_interest999)))
  })

  output$ages_death999 <- renderPlotly({
    plot_age_death(dx = gsub("- ", "", gsub("Additional: ", "", input$dx_interest999)), smooth = input$smooth_death999)
  })


  ### Age-specific and lagged HR
  observeEvent(input$button_lagged999, {
    show_hide_arrows(
      button = "button_lagged999", button2 = input$button_lagged999, panel = "lagged999_show", session = session,
      text = "mortality rate ratios depending on age and time since diagnosis"
    )
  })

  output$MRRlagged999 <- renderPlotly({
    plot_MRRlagged(dx = gsub("- ", "", gsub("Additional: ", "", input$dx_interest999)))
  })

  output$MRRlagged <- renderPlotly({
    plot_MRRlagged(dx = input$dx_interest)
  })

  ### Age-specific LYLs
  observeEvent(input$button_LYLages999, {
    show_hide_arrows(
      button = "button_LYLages999", button2 = input$button_LYLages999, panel = "LYLages999_show", session = session,
      text = "age-specific remaining life expectancy"
    )
  })

  output$LYLages999 <- renderPlotly({
    plot_LYLages(dx = gsub("- ", "", gsub("Additional: ", "", input$dx_interest999)))
  })

  ### MRRs and LYLs
  observeEvent(input$button_MRR999, {
    show_hide_arrows(
      button = "button_MRR999", button2 = input$button_MRR999, panel = "MRR999_show", session = session,
      text = "mortality due to specific causes of death"
    )
  })

  #  output$MRR_LYL_causes999 <- renderTable({
  output$MRR_LYL_causes999 <- renderDataTable({
    datatable(table_causes(dx = gsub("- ", "", gsub("Additional: ", "", input$dx_interest999))),
              options = list(pageLength = 20, info = FALSE, lengthChange = FALSE, searching = FALSE, scrollX = FALSE, scrollY = FALSE, paging = FALSE),
              rownames = FALSE, container = causes_structure(dx = gsub("- ", "", gsub("Additional: ", "", input$dx_interest999)))
    )
  })


  ### Survival curves
  observeEvent(input$button_survival999, {
    show_hide_arrows(
      button = "button_survival999", button2 = input$button_survival999, panel = "survival999_show", session = session,
      text = "age-specific and cause-specific survival curves"
    )
  })

  observeEvent(input$dx_interest999, {
    x <- list_dx %>%
      filter(description == gsub("- ", "", gsub("Additional: ", "", !!input$dx_interest999)), LYL == TRUE) %>%
      slice(1)

    updateRadioGroupButtons(
      session,
      inputId = "age_survival999",
      choices = c(
        paste0("P25: ", x[1, "p25"], " years"),
        paste0("P50 (median): ", x[1, "p50"], " years"),
        paste0("P75: ", x[1, "p75"], " years")
      ),
      status = "primary",
      selected = paste0("P50 (median): ", x[1, "p50"], " years")
    )
  })

  output$LYLplot999 <- renderPlotly({
    res <- plot_LYLplot(dx = gsub("- ", "", gsub("Additional: ", "", input$dx_interest999)), age = input$age_survival999, cause = input$cause_survival999)
    text_survival999 <<- res$text
    res$g
  })

  text_survival999 <- NULL
  output$text_survival999 <- renderText(text_survival999)

  ### Air pollution
  observeEvent(input$button_air999, {
    show_hide_arrows(
      button = "button_air999", button2 = input$button_air999, panel = "air999_show", session = session,
      text = "mortality rate ratios after adjustment for air pollution"
    )
  })

  output$MRR_air999 <- renderDataTable({
    datatable(table_MRRair(dx = gsub("- ", "", gsub("Additional: ", "", input$dx_interest999))),
              options = list(pageLength = 20, info = FALSE, lengthChange = FALSE, searching = FALSE, scrollX = FALSE, scrollY = FALSE, paging = FALSE),
              rownames = FALSE, container = MRRair_structure
    )
  })


  ###################
  ###################
  ### GMCs: all disorders
  observeEvent(input$diseases_all999,
               {
                 if (is.null(input$diseases_all999)) {
                   shinyjs::hide(id = "main_panel_all999")
                   shinyjs::disable(id = "sex999")
                   shinyjs::disable(id = "cod999")
                 } else {
                   if ("" %in% input$diseases_all999) {
                     shinyjs::hide(id = "main_panel_all999")
                     shinyjs::disable(id = "sex999")
                     shinyjs::disable(id = "cod999")
                   } else {
                     shinyjs::show(id = "main_panel_all999")
                     shinyjs::enable(id = "sex999")
                     shinyjs::enable(id = "cod999")
                   }
                 }
               },
               ignoreNULL = FALSE
  )


  ### MRRs & LYLs

  observeEvent(input$button_MRRLYLall999, {
    hide_show_arrows(
      button = "button_MRRLYLall999", button2 = input$button_MRRLYLall999, panel = "MRRLYL_all999_show", session = session,
      text = "a panel of number of diagnosed, mortality rate ratios and life years lost"
    )
  })


  observeEvent(input$button_MRRLYLall, {
    hide_show_arrows(
      button = "button_MRRLYLall", button2 = input$button_MRRLYLall, panel = "MRRLYL_all_show", session = session,
      text = "a panel of number of diagnosed, mortality rate ratios and life years lost"
    )
  })

  check_plot_all999 <- reactive({
    list(input$sex999, input$diseases_all999, input$cod999)
  })

  observeEvent(check_plot_all999(), {
    sex <- input$sex999
    if (sex == "M vs. F") {
      sex <- c("Males", "Females")
    }

    labels <- list_unique999 %>%
      filter(
        description %in% gsub("- ", "", gsub("Additional: ", "", !!input$diseases_all999))
      )

    x <- list_dx %>%
      filter(
        level == 4,
        desc_EN %in% gsub("- ", "", gsub("Additional: ", "", !!input$diseases_all999))
      ) %>%
      arrange(id) %>%
      mutate(
        pos = factor(id, levels = rev(labels$id), labels = rev(labels$description))
      )

    browser()

    output$MRR_LYL_all999 <- renderPlotly({
      plot_all(
        dx = x, sex = sex, causes = input$cod999, list_dx_included = input$diseases_all999,
        list_clean = gsub("- ", "", gsub("Additional: ", "", input$diseases_all999))
      )
    })

    output$MRR_all999 <- renderPlotly({
      plot_mrr_all(
        dx = x, sex = sex, causes = input$cod999, list_dx_included = input$diseases_all999,
        list_clean = gsub("- ", "", gsub("Additional: ", "", input$diseases_all999))
      )
    })

    output$LYL_all999 <- renderPlotly({
      plot_lyl_all(
        dx = x, sex = sex, causes = input$cod999, list_dx_included = input$diseases_all999,
        list_clean = gsub("- ", "", gsub("Additional: ", "", input$diseases_all999))
      )
    })

    output$MRR_air_plot999 <- renderPlotly({
      plot_mrr_air(
        dx = x, list_dx_included = input$diseases_all999,
        list_clean = gsub("- ", "", gsub("Additional: ", "", input$diseases_all999))
      )
    })
  })

  check_plot_all <- reactive({
    list(input$sex, input$diseases_all, input$cod)
  })

  observeEvent(check_plot_all(), {
    sex <- input$sex
    if (sex == "M vs. F") {
      sex <- c("Males", "Females")
    }

    labels <- chapters_unique %>%
      filter(
        description %in% !!input$diseases_all
      ) %>%
      mutate(
        lab = paste0(as.roman(chapter), " (", start, "-", end, ")")
      )

    x <- list_dx %>%
      filter(
        level == 1,
        description %in% !!input$diseases_all
      ) %>%
      arrange(id) %>%
      mutate(
        pos = factor(id, levels = rev(labels$id), labels = rev(labels$description))
      )

    chapters_included <- labels$chapter

    estimates <- list_dx %>%
      filter(
        level == 3,
        chapter %in% !!chapters_included
      )

    estimates_sex <- estimates %>% filter(sex %in% !!sex)

    # Include M/F if sex == "all" and not estimates for everyone
    if ("All" %in% sex) {
      out <- estimates_sex %>%
        filter(include == 0) %>%
        select(id) %>%
        left_join(
          estimates %>% filter(sex != "All") %>% filter(include == 1),
          by = "id"
        )
      estimates <- estimates_sex %>% bind_rows(out)
    } else {
      estimates <- estimates_sex
    }

    if (nrow(estimates) > 0) {
      estimates <- estimates %>%
        select(id, chapter, sex) %>%
        arrange(chapter, id) %>%
        mutate(
          pos = 1:n()
        ) %>%
        left_join(labels %>% select(chapter, lab), by = "chapter") %>%
        mutate(description = factor(lab, levels = labels$lab))

      min <- min(estimates$pos, na.rm = T) - 0.5
      max <- max(estimates$pos, na.rm = T) + 0.5

      shaded <- estimates %>%
        group_by(chapter) %>%
        summarise(
          min = min(pos) - 0.5,
          max = max(pos) + 0.5
        ) %>%
        ungroup() %>%
        arrange(chapter) %>%
        mutate(num = 1:n()) %>%
        filter(num %% 2 == 0)

      chapters_labels <- estimates %>%
        group_by(chapter) %>%
        summarise(
          med = median(pos)
        ) %>%
        ungroup()
    }


    output$MRR_LYL_all <- renderPlotly({
      plot_all(
        dx = x, sex = sex, causes = input$cod, list_dx_included = labels$lab,
        list_clean = labels$description
      )
    })

    output$MRR_all <- renderPlotly({
      estimates_mrr <- estimates %>%
        left_join(MRR, by = c("id", "sex")) %>%
        filter(cod == !!input$cod) %>%
        left_join(select(list_dx, id, desc = desc_EN), by = "id") %>%
        mutate(
          text = paste0(
            format_ci(HR, CI_left, CI_right, n = 1),
            "\n", desc
          )
        )

      cis_by_chapter(
        estimates_mrr,
        "HR", "CI_left", "CI_right", "text",
        ytitle = paste0(
          "Mortality Rate Ratios for ",
          tolower(input$cod), " causes (95% CI)"
        )
      ) %>%
        layout(
          yaxis = list(type = "log", range = log10(c(0.3, 180)))
        )
    })

    output$LYL_all <- renderPlotly({
      LYL_aux <- LYL %>%
        select(
          id, sex, LYL = LYL_Total, LYL_L = LYL_Total_L, LYL_R = LYL_Total_R
        ) %>%
        mutate(cod = "All") %>%
        bind_rows(
          LYL %>% select(id, sex, LYL = LYL_Natural, LYL_L = LYL_Natural_L, LYL_R = LYL_Natural_R) %>% mutate(cod = "Natural")
        ) %>%
        bind_rows(
          LYL %>% select(id, sex, LYL = LYL_External, LYL_L = LYL_External_L, LYL_R = LYL_External_R) %>% mutate(cod = "External")
        )

      estimates_lyl <- estimates %>%
        left_join(LYL_aux, by = c("id", "sex")) %>%
        filter(cod == !!input$cod) %>%
        left_join(select(list_dx, id, desc = desc_EN), by = "id") %>%
        mutate(
          text = paste0(
            format_ci(LYL, LYL_L, LYL_R, n = 1),
            "\n", desc
          )
        )

      cis_by_chapter(
        estimates_lyl, "LYL", "LYL_L", "LYL_R", "text",
        ytitle = paste0(
          "Life Years Lost due to ",
          tolower(input$cod), " causes"
        )
      )
    })

    output$MRR_air_plot <- renderPlotly({

      estimates_mrr_air <- estimates %>%
        left_join(MRR, by = c("id", "sex")) %>%
        filter(cod %in% c("Air_adjusted", "Air_not_adjust"))

      estimates_mrr_air <- estimates_mrr_air %>%
        filter(cod == "Air_adjusted") %>%
        mutate(se = (log(CI_right) - log(HR)) / qnorm(0.975)) %>%
        select(id, chapter, description, pos, HR, se) %>%
        left_join(estimates_mrr_air %>% filter(cod == "Air_not_adjust") %>%
                    mutate(se0 = (log(CI_right) - log(HR)) / qnorm(0.975)) %>%
                    select(id, HR0 = HR, se0),
                  by = "id"
        ) %>%
        mutate(
          HR = HR / HR0,
          se_comb = sqrt(se^2 + se0^2),
          CI_left = ifelse(is.na(HR), NA, exp(log(HR) - qnorm(0.975) * se_comb)),
          CI_right = ifelse(is.na(HR), NA, exp(log(HR) + qnorm(0.975) * se_comb))
        )

      stop("TODO: split by color for both air and sex? ")
    })
  })

  ### MRRs
  observeEvent(input$button_MRRall999, {
    show_hide_arrows(
      button = "button_MRRall999", button2 = input$button_MRRall999, panel = "MRR_all999_show", session = session,
      text = "mortality rate ratios only"
    )
  })

  observeEvent(input$button_MRRall, {
    show_hide_arrows(
      button = "button_MRRall", button2 = input$button_MRRall, panel = "MRR_all_show", session = session,
      text = "mortality rate ratios only"
    )
  })

  ### LYL
  observeEvent(input$button_LYLall999, {
    show_hide_arrows(
      button = "button_LYLall999", button2 = input$button_LYLall999, panel = "LYL_all999_show", session = session,
      text = "life years lost only"
    )
  })

  observeEvent(input$button_LYLall, {
    show_hide_arrows(
      button = "button_LYLall", button2 = input$button_LYLall, panel = "LYL_all_show", session = session,
      text = "life years lost only"
    )
  })

  ### Air pollution
  observeEvent(input$button_airall999, {
    show_hide_arrows(
      button = "button_airall999", button2 = input$button_airall999, panel = "air_all999_show", session = session,
      text = "mortality rate ratios after adjustment for air pollution"
    )
  })

  observeEvent(input$button_airall, {
    show_hide_arrows(
      button = "button_airall", button2 = input$button_airall, panel = "air_all_show", session = session,
      text = "mortality rate ratios after adjustment for air pollution"
    )
  })

  ###################
  ###################
  ### All ICD-10: one disorder

  ### For list of GMCs (chapters 998 and 999)
  observe({
    updatePickerInput(session,
                      inputId = "dx_interest",
                      choices = c("--> Choose disorder of interest", list_unique$description)
    )

    updatePickerInput(session,
                      inputId = "diseases_all",
                      choices = chapters_unique$description,
                      selected = chapters_unique$description
    )
  })

  observeEvent(input$dx_interest, {
    if (input$dx_interest == "--> Choose disorder of interest") {
      shinyjs::hide(id = "panel")
    } else {
      shinyjs::show(id = "panel")
    }
  })

  ### Main Table
  observeEvent(input$button_main, {
    hide_show_arrows(
      button = "button_main", button2 = input$button_main, panel = "main_panel", session = session,
      text = "table with main results"
    )
  })

  output$list <- renderDataTable({
    datatable(table_main(dx = input$dx_interest),
              options = list(pageLength = 20, info = FALSE, lengthChange = FALSE, searching = FALSE, scrollX = FALSE, scrollY = FALSE, paging = FALSE),
              rownames = FALSE,
              caption = htmltools::tags$caption(
                style = "caption-side: bottom;", htmltools::strong("Estimates not available: Age at diagnosis"), " if 'Diagnosed<100'; ", htmltools::strong("Age at death"), " if 'Deaths<100'; ", htmltools::strong("Mortality rate ratio"), " if
              'Diagnosed<100' or 'Deaths<20'; ", htmltools::strong("Life years lost"), " if 'Diagnosed<100', 'Deaths<20' or not enough individuals at risk
              at old ages; ", htmltools::strong("Life years lost"), " for persons if not available for both males and females. A range for ", htmltools::strong("Diagnosed"), " and ", htmltools::strong("Deaths"), " is provided
              when less than 5 individuals could be identified."
              )
    )
  })

  ### Age-specific incidences
  observeEvent(input$button_agedx, {
    show_hide_arrows(
      button = "button_agedx", button2 = input$button_agedx, panel = "agedx_show", session = session,
      text = "age-specific incidence of the disorder and distribution of age of diagnosis"
    )
  })

  output$incidence <- renderPlotly({
    plot_incidence(dx = input$dx_interest)
  })

  output$ages <- renderPlotly({
    plot_age_dx(dx = input$dx_interest, smooth = input$smooth)
  })


  ### Age-specific mortality rates
  observeEvent(input$button_agedeath, {
    show_hide_arrows(
      button = "button_agedeath", button2 = input$button_agedeath, panel = "agedeath_show", session = session,
      text = "age-specific mortality rates and distribution of age of death"
    )
  })

  output$mortality <- renderPlotly({
    plot_mortality(dx = input$dx_interest)
  })

  output$ages_death <- renderPlotly({
    plot_age_death(dx = input$dx_interest, smooth = input$smooth_death)
  })


  ### Age-specific and lagged HR
  observeEvent(input$button_lagged, {
    show_hide_arrows(
      button = "button_lagged", button2 = input$button_lagged, panel = "lagged_show", session = session,
      text = "mortality rate ratios depending on age and time since diagnosis"
    )
  })


  ### Age-specific LYLs
  observeEvent(input$button_LYLages, {
    show_hide_arrows(
      button = "button_LYLages", button2 = input$button_LYLages, panel = "LYLages_show", session = session,
      text = "age-specific remaining life expectancy"
    )
  })

  output$LYLages <- renderPlotly({
    plot_LYLages(dx = input$dx_interest)
  })

  ### MRRs and LYLs
  observeEvent(input$button_MRR, {
    show_hide_arrows(
      button = "button_MRR", button2 = input$button_MRR, panel = "MRR_show", session = session,
      text = "mortality due to specific causes of death"
    )
  })

  output$MRR_LYL_causes <- renderDataTable({
    datatable(table_causes(dx = input$dx_interest),
              options = list(pageLength = 20, info = FALSE, lengthChange = FALSE, searching = FALSE, scrollX = FALSE, scrollY = FALSE, paging = FALSE),
              rownames = FALSE, container = causes_structure(dx = input$dx_interest)
    )
  })

  ### Survival curves
  observeEvent(input$button_survival, {
    show_hide_arrows(
      button = "button_survival", button2 = input$button_survival, panel = "survival_show", session = session,
      text = "age-specific and cause-specific survival curves"
    )
  })

  observeEvent(input$dx_interest, {
    x <- list_dx %>%
      filter(description == !!input$dx_interest, LYL == TRUE) %>%
      slice(1)

    updateRadioGroupButtons(session,
                            inputId = "age_survival",
                            choices = c(
                              paste0("P25: ", x[1, "p25"], " years"),
                              paste0("P50 (median): ", x[1, "p50"], " years"),
                              paste0("P75: ", x[1, "p75"], " years")
                            ),
                            status = "primary",
                            selected = paste0("P50 (median): ", x[1, "p50"], " years")
    )
  })


  output$LYLplot <- renderPlotly({
    res <- plot_LYLplot(dx = input$dx_interest, age = input$age_survival, cause = input$cause_survival)
    text_survival <<- res$text
    res$g
  })

  text_survival <- NULL
  output$text_survival <- renderText(text_survival)

  ### Air pollution
  observeEvent(input$button_air, {
    show_hide_arrows(
      button = "button_air", button2 = input$button_air, panel = "air_show", session = session,
      text = "mortality rate ratios after adjustment for air pollution"
    )
  })

  output$MRR_air <- renderDataTable({
    datatable(table_MRRair(dx = input$dx_interest),
              options = list(pageLength = 20, info = FALSE, lengthChange = FALSE, searching = FALSE, scrollX = FALSE, scrollY = FALSE, paging = FALSE),
              rownames = FALSE, container = MRRair_structure
    )
  })


  #### Interpretation
  observeEvent(input$dx_interest, {
    x <- interpretation_main(dx = input$dx_interest)
    output$text_main <- renderText({
      x$main
    })
    output$text_agedx <- renderText({
      x$incidence
    })
    output$text_agedx2 <- renderText({
      x$incidence2
    })
    output$text_agedeath <- renderText({
      x$mortality
    })
    output$text_agedeath2 <- renderText({
      x$mortality2
    })
    output$text_MRRlagged <- renderText({
      x$lagged
    })
    output$text_LYLages <- renderText({
      x$LYLages
    })
    output$text_MRR_LYL_causes <- renderText({
      x$MRR
    })

    output$text_MRRair <- renderText({
      x$MRRair
    })

    closed_panels <- 0

    ## Close LYL panels if LYL not estimated
    LYLs <- nrow(list_dx %>% filter(description == !!input$dx_interest, LYL == TRUE))
    close_panels(n = LYLs, button = "button_survival", button2 = input$button_survival, panel = "survival_show", session = session)
    close_panels(n = LYLs, button = "button_LYLages", button2 = input$button_LYLages, panel = "LYLages_show", session = session)
    if (LYLs == 0) {
      closed_panels <- 1
    }
    ## Close causes of death when LYL not available
    LYLs <- nrow(list_dx %>% filter(description == !!input$dx_interest, LYLcauses == TRUE))
    if (LYLs == 0) {
      shinyjs::disable(id = "cause_survival")
      shinyjs::show(id = "cause_survival_not")
    } else {
      shinyjs::enable(id = "cause_survival")
      shinyjs::hide(id = "cause_survival_not")
    }

    ## Close MRR panels if MRR not estimated
    MRRs <- nrow(list_dx %>% filter(description == !!input$dx_interest, MRR == TRUE))
    close_panels(n = MRRs, button = "button_lagged", button2 = input$button_lagged, panel = "lagged_show", session = session)
    close_panels(n = MRRs, button = "button_MRR", button2 = input$button_MRR, panel = "MRR_show", session = session)
    close_panels(n = MRRs, button = "button_air", button2 = input$button_air, panel = "air_show", session = session)
    if (MRRs == 0) {
      closed_panels <- 1
    }

    ## Close age panels if age not estimated
    age_dx <- nrow(list_dx %>% filter(description == !!input$dx_interest, age_dx == TRUE))
    close_panels(n = age_dx, button = "button_agedx", button2 = input$button_agedx, panel = "agedx_show", session = session)
    if (age_dx == 0) {
      closed_panels <- 1
    }
    age_death <- nrow(list_dx %>% filter(description == !!input$dx_interest, age_death == TRUE))
    close_panels(n = age_death, button = "button_agedeath", button2 = input$button_agedeath, panel = "agedeath_show", session = session)
    if (age_death == 0) {
      closed_panels <- 1
    }

    if (closed_panels == 1) {
      shinyjs::show(id = "not_all_estimates")
    } else {
      shinyjs::hide(id = "not_all_estimates")
    }
  })


  observeEvent(input$interpret, {
    ids <- c(
      "main", "agedx", "agedx2", "agedeath", "agedeath2",
      "MRRlagged", "LYLages", "MRR_LYL_causes", "survival", "MRRair"
    )
    func <- if (input$interpret) shinyjs::show else shinyjs::hide
    lapply(paste0("text_", ids), func)
  })

  observeEvent(input$interpret999, {
    ids <- c(
      "main999","agedx999","agedx2999","agedeath999","agedeath2999","MRRlagged999",
      "LYLages999","MRR_LYL_causes999","survival999","MRRair999"
    )
    func <- if (input$interpret) shinyjs::show else shinyjs::hide
    lapply(paste0("text_", ids), func)
  })

  observeEvent(input$dx_interest999, {
    x <- interpretation_main(dx = gsub("- ", "", gsub("Additional: ", "", input$dx_interest999)))
    output$text_main999 <- renderText({
      x$main
    })

    output$text_agedx999 <- renderText({
      x$incidence
    })
    output$text_agedx2999 <- renderText({
      x$incidence2
    })

    output$text_agedeath999 <- renderText({
      x$mortality
    })
    output$text_agedeath2999 <- renderText({
      x$mortality2
    })
    output$text_MRRlagged999 <- renderText({
      x$lagged
    })
    output$text_LYLages999 <- renderText({
      x$LYLages
    })

    output$text_MRR_LYL_causes999 <- renderText({
      x$MRR
    })

    output$text_MRRair999 <- renderText({
      x$MRRair
    })

    closed_panels <- 0

    ## Close LYL panels if LYL not estimated
    LYLs <- nrow(list_dx %>% filter(description == gsub("- ", "", gsub("Additional: ", "", !!input$dx_interest999)), LYL == TRUE))
    close_panels(n = LYLs, button = "button_survival999", button2 = input$button_survival999, panel = "survival_show999", session = session)
    close_panels(n = LYLs, button = "button_LYLages999", button2 = input$button_LYLages999, panel = "LYLages_show999", session = session)
    if (LYLs == 0) {
      closed_panels <- 1
    }

    ## Close causes of death when LYL not available
    LYLs <- nrow(list_dx %>% filter(description == gsub("- ", "", gsub("Additional: ", "", !!input$dx_interest999)), LYLcauses == TRUE))
    if (LYLs == 0) {
      shinyjs::disable(id = "cause_survival999")
      shinyjs::show(id = "cause_survival_not999")
    } else {
      shinyjs::enable(id = "cause_survival999")
      shinyjs::hide(id = "cause_survival_not999")
    }

    ## Close MRR panels if MRR not estimated
    MRRs <- nrow(list_dx %>% filter(description == gsub("- ", "", gsub("Additional: ", "", !!input$dx_interest999)), MRR == TRUE))
    close_panels(n = MRRs, button = "button_lagged999", button2 = input$button_lagged999, panel = "lagged_show999", session = session)
    close_panels(n = MRRs, button = "button_MRR999", button2 = input$button_MRR999, panel = "MRR_show999", session = session)
    close_panels(n = MRRs, button = "button_air999", button2 = input$button_air999, panel = "air_show999", session = session)
    if (MRRs == 0) {
      closed_panels <- 1
    }
    ## Close age panels if age not estimated
    age_dx <- nrow(list_dx %>% filter(description == gsub("- ", "", gsub("Additional: ", "", !!input$dx_interest999)), age_dx == TRUE))
    close_panels(n = age_dx, button = "button_agedx999", button2 = input$button_agedx999, panel = "agedx_show999", session = session)
    if (age_dx == 0) {
      closed_panels <- 1
    }

    age_death <- nrow(list_dx %>% filter(description == gsub("- ", "", gsub("Additional: ", "", !!input$dx_interest999)), age_death == TRUE))
    close_panels(n = age_death, button = "button_agedeath999", button2 = input$button_agedeath999, panel = "agedeath_show999", session = session)
    if (age_death == 0) {
      closed_panels <- 1
    }

    if (closed_panels == 1) {
      shinyjs::show(id = "not_all_estimates999")
    } else {
      shinyjs::hide(id = "not_all_estimates999")
    }
  })

  observeEvent(
    input$diseases_all,
    {
      if (is.null(input$diseases_all)) {
        shinyjs::hide(id = "main_panel_all")
        shinyjs::disable(id = "sex")
        shinyjs::disable(id = "cod")
      } else {
        if ("" %in% input$diseases_all) {
          shinyjs::hide(id = "main_panel_all")
          shinyjs::disable(id = "sex")
          shinyjs::disable(id = "cod")
        } else {
          shinyjs::show(id = "main_panel_all")
          shinyjs::enable(id = "sex")
          shinyjs::enable(id = "cod")
        }
      }
    },
    ignoreNULL = FALSE
  )
}
