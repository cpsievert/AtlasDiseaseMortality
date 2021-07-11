format_numbers <- function(x, n) {
  ifelse(is.na(x), NA, gsub(" ", "", format(round(x, n), nsmall = n)))
}



plot_incidence <- function(dx) {
  x <- list_dx %>%
    filter(description == !!dx) %>%
    select(id) %>%
    distinct() %>%
    left_join(incidence, by = "id") %>%
    filter(age_group <= 95)

  if (dx != "--> Choose disorder of interest") {
    g <- ggplot(x, aes(x = age_group + 2.5, y = 10000 * dx_rate)) +
      geom_line(linetype = "dashed") +
      geom_point() +
      geom_errorbar(aes(ymin = 10000 * dx_rate_left, ymax = 10000 * dx_rate_right)) +
      xlab("Age in years") +
      ylab("Incidence rate (per 10,000 person-years)") +
      scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
      facet_grid(~sex, drop = FALSE) +
      theme_bw()
    return(g)
  }
}




table_main <- function(dx) {
  t <- list_dx %>%
    filter(description == !!dx) %>%
    select(-MRR, -LYL) %>%
    left_join(MRR %>% filter(cod == "All"), by = c("id", "sex")) %>%
    left_join(LYL, by = c("id", "sex")) %>%
    mutate(
      Diagnosed = ifelse(!is.na(n), format(n, big.mark = ","), as.character(n_text)),
      Deaths = ifelse(!is.na(cases), format(cases, big.mark = ","), as.character(cases_text)),
      MRR = ifelse(is.na(MRR), "-", MRR),
      Total = ifelse(is.na(Total), "-", Total)
    ) %>%
    select(Sex = sex, Diagnosed, "Age at diagnosis [Median (IQR)]" = dx_median, Deaths, "Age at death [Median (IQR)]" = death_median, "Mortality Rate Ratio" = MRR, "Life Years Lost" = Total)
  return(t)
}





plot_mortality <- function(dx) {
  x <- list_dx %>%
    filter(description == !!dx) %>%
    select(id) %>%
    add_row(id = 0) %>%
    distinct() %>%
    left_join(incidence, by = "id") %>%
    mutate(
      id = ifelse(id == 0, 0, 1),
      death_rate_left = ifelse(id == 0, NA, death_rate_left),
      death_rate_right = ifelse(id == 0, NA, death_rate_right)
    ) %>%
    filter(age_group <= 95)

  if (dx != "--> Choose disorder of interest") {
    g <- ggplot(x, aes(x = age_group + 2.5, y = 10000 * death_rate, group = factor(id), color = factor(id))) +
      geom_line(linetype = "dashed") +
      geom_point() +
      geom_errorbar(aes(ymin = 10000 * death_rate_left, ymax = 10000 * death_rate_right)) +
      xlab("Age in years") +
      ylab("Mortality rates (per 10,000 person-years)") +
      scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
      facet_grid(~sex, drop = FALSE) +
      theme_bw() +
      scale_color_manual(
        name = NULL,
        breaks = c(1, 0),
        labels = c("Diagnosed with the disorder", "Entire Danish population"),
        values = c("red", "black")
      ) +
      theme(
        legend.position = "top",
        legend.text = element_text(size = 11)
      )
    return(g)
  }
}

plot_age_dx <- function(dx, smooth) {
  x <- list_dx %>%
    filter(description == !!dx) %>%
    select(id) %>%
    distinct() %>%
    left_join(ages, by = "id")

  if (dx != "--> Choose disorder of interest") {
    if (smooth == FALSE) {
      g <- ggplot(x, aes(x = age, y = density_dx)) +
        geom_line(color = "black") +
        xlab("Age in years") +
        ylab("Distribution of age of diagnosis (density)") +
        scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
        facet_grid(~sex, drop = FALSE) +
        theme_bw()
    } else {
      g <- ggplot(x, aes(x = age, y = density_dx)) +
        geom_smooth(color = "black", se = FALSE) +
        xlab("Age in years") +
        ylab("Distribution of age of diagnosis (density)") +
        scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
        facet_grid(~sex, drop = FALSE) +
        scale_y_continuous(limit = c(0, NA)) +
        theme_bw()
    }
    return(g)
  }
}

plot_age_death <- function(dx, smooth) {
  x <- list_dx %>%
    filter(description == !!dx) %>%
    select(id) %>%
    distinct() %>%
    left_join(ages, by = "id")

  if (dx != "--> Choose disorder of interest") {
    if (smooth == FALSE) {
      g <- ggplot(x, aes(x = age, y = density_death)) +
        geom_line(color = "black") +
        xlab("Age in years") +
        ylab("Distribution of age of death among the diagnosed (density)") +
        scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
        facet_grid(~sex, drop = FALSE) +
        theme_bw()
    } else {
      g <- ggplot(x, aes(x = age, y = density_death)) +
        geom_smooth(color = "black", se = FALSE) +
        xlab("Age in years") +
        ylab("Distribution of age of death among the diagnosed (density)") +
        scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
        facet_grid(~sex, drop = FALSE) +
        scale_y_continuous(limit = c(0, NA)) +
        theme_bw()
    }
    return(g)
  }
}


plot_MRRlagged <- function(dx) {
  if (dx != "--> Choose disorder of interest") {
    x <- list_dx %>%
      filter(description == !!dx) %>%
      select(id) %>%
      distinct() %>%
      left_join(MRRlagged, by = "id") %>%
      filter(exposure != 0)

    g1 <- ggplot(x, aes(x = exposure, y = HR)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      geom_point() +
      geom_errorbar(aes(ymin = CI_left, ymax = CI_right), width = 0.1) +
      geom_line(linetype = "dashed") +
      facet_grid(~sex, drop = TRUE) +
      scale_x_continuous(breaks = 1:6, labels = c("0-6 months", "6-12 months", "1-2 years", "2-5 years", "5-10 years", "10+ years")) +
      scale_y_log10() +
      xlab("Time since first diagnosis") +
      ylab("Mortality rate ratio (95% CI)") +
      theme_bw()

    x <- list_dx %>%
      filter(description == !!dx) %>%
      select(id) %>%
      distinct() %>%
      left_join(MRRage, by = "id") %>%
      filter(age_group < 100)
    age_categories <- unique(x$age_categories)

    g2 <- ggplot(x, aes(x = age_group, y = HR)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      geom_point() +
      geom_errorbar(aes(ymin = CI_left, ymax = CI_right), width = 1.5) +
      geom_line(linetype = "dashed") +
      facet_grid(~sex, drop = TRUE) +
      scale_x_continuous(
        breaks = seq(0, 95, age_categories),
        labels = paste0(seq(0, 95, age_categories), "-", seq(age_categories, 95 + age_categories, age_categories))
      ) +
      scale_y_log10() +
      xlab("Age in years") +
      ylab("Mortality rate ratio (95% CI)") +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
      )

    ggarrange(g1, g2, ncol = 2)
  }
}

# table_causesMRR <- function(dx) {
#   t <- list_dx %>% filter(description == !!dx) %>%
#     select(id) %>% distinct() %>% left_join(MRR, by="id") %>%
#     filter(
#       !(cod %in% c("Air_adjusted", "Air_not_adjust"))
#     ) %>%
#     mutate(
#       mr = ifelse(is.na(rate), "-", paste0(rate, " vs. ", rateDK_std))
#     ) %>%
#     select('Sex' = sex, 'Cause of death' = cod, 'Mortality rates' = mr,
#            'Mortality Rate Ratio' = MRR)
#   return(t)
# }
#
# table_causesLYL <- function(dx) {
#   t <- list_dx %>% filter(description == !!dx) %>%
#     select(id) %>% distinct() %>% left_join(LYL, by="id") %>%
#     mutate(
#       LE = ifelse(is.na(life_exp), "-", paste0(format_numbers(life_exp, 1), " vs. ", format_numbers(life_exp0, 1), " years"))
#     ) %>%
#     select('Sex' = sex, 'Life expectancy after diagnosis' = LE, 'Life Years Lost' = Total, 'Natural Causes' = Natural, 'External Causes' = External)
#
#   return(t)
#
#
# }

table_causes <- function(dx) {

  # t1 <- list_dx %>% filter(description == !!dx) %>%
  #   select(id) %>% distinct() %>% left_join(MRR, by="id") %>%
  #   filter(
  #     !(cod %in% c("Air_adjusted", "Air_not_adjust"))
  #   ) %>%
  #   mutate(
  #     mr = ifelse(is.na(rate), "-", paste0(rate, " vs. ", rateDK_std))
  #   )
  #
  # LYLs <- nrow(list_dx %>% filter(description == !!dx, LYL==TRUE))
  #
  # if(LYLs > 0) {
  #   t2 <- list_dx %>% filter(description == !!dx) %>%
  #     select(id) %>% distinct() %>% left_join(LYL, by="id") %>%
  #     mutate(
  #       LE = ifelse(is.na(life_exp), "-", paste0(format_numbers(life_exp, 1), " vs. ", format_numbers(life_exp0, 1), " years"))
  #     )
  #
  #   t3 <- t2 %>%
  #     select(sex, LE, LYL=Total) %>%
  #     mutate(cod = "All") %>%
  #     bind_rows(t2 %>%
  #                 select(sex, LYL=Natural) %>%
  #                 mutate(LE = "-", cod = "Natural")) %>%
  #     bind_rows(t2 %>%
  #                 select(sex, LYL=External) %>%
  #                 mutate(LE = "-", cod = "External")) %>%
  #     mutate(
  #       LE = ifelse(is.na(LE), "-", LE),
  #       LYL = ifelse(is.na(LYL), "-", LYL)
  #     )
  #
  #   t <- t1 %>%
  #     left_join(t3) %>%
  #     select('Sex' = sex, 'Cause of death' = cod, 'Mortality rates' = mr,
  #            'Mortality Rate Ratio' = MRR, 'Life expectancy after diagnosis' = LE, 'Life Years Lost' = LYL)
  # } else {
  #   t <- t1 %>%
  #     select('Sex' = sex, 'Cause of death' = cod, 'Mortality rates' = mr,
  #            'Mortality Rate Ratio' = MRR)
  # }

  t1 <- list_dx %>%
    filter(description == !!dx) %>%
    select(id) %>%
    distinct() %>%
    left_join(MRR, by = "id") %>%
    filter(
      !(cod %in% c("Air_adjusted", "Air_not_adjust"))
    )

  LYLs <- nrow(list_dx %>% filter(description == !!dx, LYL == TRUE))

  if (LYLs > 0) {
    t2 <- list_dx %>%
      filter(description == !!dx) %>%
      select(id) %>%
      distinct() %>%
      left_join(LYL, by = "id")

    t3 <- t2 %>%
      select(sex, life_exp, life_exp0, LYL = Total) %>%
      mutate(
        cod = "All",
        life_exp = ifelse(is.na(life_exp), "-", paste0(as.character(format_numbers(life_exp, 1)), " years")),
        life_exp0 = ifelse(is.na(life_exp0), "-", paste0(as.character(format_numbers(life_exp0, 1)), " years"))
      ) %>%
      bind_rows(t2 %>%
        select(sex, LYL = Natural) %>%
        mutate(life_exp = "", life_exp0 = "", cod = "Natural")) %>%
      bind_rows(t2 %>%
        select(sex, LYL = External) %>%
        mutate(life_exp = "", life_exp0 = "", cod = "External")) %>%
      mutate(
        LYL = ifelse(is.na(LYL), "-", LYL)
      )

    t <- t1 %>%
      left_join(t3, by = c("sex", "cod")) %>%
      select(
        "Sex" = sex, "Cause of death" = cod, "Mortality rates diagnosed" = rate, "Mortality rates Denmark" = rateDK_std,
        "Mortality Rate Ratio" = MRR, "Life expectancy after diagnosis" = life_exp, "Life expectancy Denmark" = life_exp0, "Life Years Lost" = LYL
      )
  } else {
    t <- t1 %>%
      select(
        "Sex" = sex, "Cause of death" = cod, "Mortality rates diagnosed" = rate, "Mortality rates Denmark" = rateDK_std,
        "Mortality Rate Ratio" = MRR
      )
  }

  return(t)
}

causes_structure <- function(dx) {
  LYLs <- nrow(list_dx %>% filter(description == !!dx, LYL == TRUE))

  if (LYLs > 0) {
    sketch <- htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          th(rowspan = 2, "Sex"),
          th(rowspan = 2, "Cause of death"),
          th(colspan = 2, "Mortality rates (per 1,000)"),
          th(rowspan = 2, "Mortality Rate Ratios (95% CI)"),
          th(colspan = 2, "Life expectancy after diagnosis"),
          th(rowspan = 2, "Life Years Lost (95% CI)")
        ),
        tr(
          lapply(rep(c("Diagnosed", "Danish population"), 2), th)
        )
      )
    ))
  } else {
    sketch <- htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          th(rowspan = 2, "Sex"),
          th(rowspan = 2, "Cause of death"),
          th(colspan = 2, "Mortality rates (per 1,000)"),
          th(rowspan = 2, "Mortality Rate Ratios (95% CI)")
        ),
        tr(
          lapply(rep(c("Diagnosed", "Danish population"), 1), th)
        )
      )
    ))
  }

  return(sketch)
}




table_MRRair <- function(dx) {
  t <- list_dx %>%
    filter(description == !!dx) %>%
    select(id) %>%
    distinct() %>%
    left_join(MRR, by = "id") %>%
    filter(
      cod == "Air_not_adjust"
    ) %>%
    mutate(cod = "All") %>%
    select(id, sex, cod, rate, rateDK_std, MRR) %>%
    left_join(MRR %>% filter(cod == "Air_adjusted") %>% select(id, MRR_adjusted = MRR), by = "id") %>%
    mutate(
      mr = paste0(rate, " vs. ", rateDK_std)
    ) %>%
    select(-id) %>%
    mutate(empty = "") %>%
    select(
      "Sex" = sex, rate, rateDK_std, empty,
      "Mortality Rate Ratio (MRR)" = MRR, "Adjusted MRR" = MRR_adjusted
    )
  return(t)
}

MRRair_structure <- htmltools::withTags(table(
  class = "display",
  thead(
    tr(
      th(rowspan = 2, "Sex"),
      th(colspan = 2, "Mortality rates (per 1,000)"),
      th(rowspan = 2, ""),
      th(colspan = 2, "Mortality Rate Ratios (95% CI)")
    ),
    tr(
      lapply(c("Diagnosed", "Danish population", "Not pollution-adjusted", "Pollution-adjusted"), th)
    )
  )
))

plot_LYLages <- function(dx) {
  x <- list_dx %>%
    filter(description == !!dx) %>%
    select(id) %>%
    distinct() %>%
    left_join(LYLages, by = "id")

  x <- x %>%
    select(-life_exp0) %>%
    mutate(group = "Diagnosed with the disorder") %>%
    bind_rows(
      x %>% select(-life_exp, -life_exp_L, -life_exp_R) %>% rename(life_exp = life_exp0) %>% mutate(group = "Entire Danish population")
    )

  if (dx != "--> Choose disorder of interest") {
    # ggplot(x, aes(x=age, y=life_exp, linetype=group))+
    #   geom_ribbon(aes(ymin=life_exp_L, ymax=life_exp_R), alpha=0.3, show.legend = FALSE)+
    #   geom_line()+
    #   scale_y_continuous(limits=c(0, NA))+
    #   facet_grid(~sex, drop = FALSE)+
    #   xlab("Age in years") + ylab("Remaining life expectancy after diagnosis (in years)")+
    #   theme_bw()+
    #   scale_linetype_manual(
    #     name=NULL,
    #     breaks = c("Diagnosed with the disorder", "Entire Danish population"),
    #     labels = c("Diagnosed with the disorder", "Entire Danish population"),
    #     values= c("solid", "dashed")
    #   )+
    #   theme(
    #     legend.position = "top",
    #     legend.text=element_text(size=11)
    #   )

    ggplot(x, aes(x = age, y = life_exp)) +
      geom_ribbon(aes(ymin = life_exp_L, ymax = life_exp_R, fill = group), alpha = 0.3, show.legend = FALSE) +
      geom_line(aes(color = group)) +
      scale_y_continuous(limits = c(0, NA)) +
      facet_grid(~sex, drop = FALSE) +
      xlab("Age in years") +
      ylab("Remaining life expectancy after diagnosis (in years)") +
      theme_bw() +
      scale_color_manual(
        name = NULL,
        breaks = c("Diagnosed with the disorder", "Entire Danish population"),
        labels = c("Diagnosed with the disorder", "Entire Danish population"),
        values = c("red", "black")
      ) +
      theme(
        legend.position = "top",
        legend.text = element_text(size = 11)
      )
  }
}

show_hide_arrows <- function(button, button2, panel, session, text) {
  if (button2 %% 2 == 1) {
    shinyjs::show(id = panel)
    updateActionButton(session,
      inputId = button,
      icon = icon("chevron-down"),
      label = paste0("Click to hide ", text)
    )
  } else {
    shinyjs::hide(id = panel)
    updateActionButton(session,
      inputId = button,
      icon = icon("chevron-right"),
      label = paste0("Click to show ", text)
    )
  }
}

hide_show_arrows <- function(button, button2, panel, session, text) {
  if (button2 %% 2 == 1) {
    shinyjs::hide(id = panel)
    updateActionButton(session,
      inputId = button,
      icon = icon("chevron-right"),
      label = paste0("Click to show ", text)
    )
  } else {
    shinyjs::show(id = panel)
    updateActionButton(session,
      inputId = button,
      icon = icon("chevron-down"),
      label = paste0("Click to hide ", text)
    )
  }
}



plot_LYLplot <- function(dx, age, cause) {
  if (dx != "--> Choose disorder of interest") {
    x <- list_dx %>%
      filter(description == !!dx) %>%
      select(id, chapter) %>%
      distinct()
    LYLplot <- read.table(file = paste0("data/plot_chapter", x$chapter, ".txt"), header = T) %>%
      filter(id == !!x$id) %>%
      filter(time <= 100)

    if (cause == "All causes") {
      LYLplot <- LYLplot %>%
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
      LYLplot <- LYLplot %>%
        mutate(
          cause = factor(cause, levels = c("Censored", "Natural", "External", "Dead"), labels = c("Alive", "Dead (natural causes)", "Dead (external causes)", "Dead (all causes)")),
          group = factor(group, levels = c("Diagnosed", "All population"), labels = c("Diagnosed with the disorder", "Entire Danish population"))
        )
    }

    LYLplot <- LYLplot %>%
      bind_rows(
        LYLplot %>% group_by(cause, group, sex, pct) %>% slice(n()) %>% ungroup() %>% mutate(time = 100)
      ) %>%
      distinct()

    LYLplot2 <- LYLplot %>% filter(pct == as.numeric(substr(age, 2, 3)))
    min_age <- min(LYLplot2$time)

    ref <- LYLplot2 %>%
      filter(cause == "Alive") %>%
      mutate(
        group2 = ifelse(group == "Entire Danish population", "Diagnosed with the disorder", "Entire Danish population")
      ) %>%
      mutate(group = group2, linetype = "Survival curve in\nthe other group") %>%
      select(-group2)


    g <- ggplot(LYLplot2, aes(x = time, y = 100 * cip, fill = cause)) +
      geom_area(alpha = 0.6, size = 0.3, color = "black", position = position_stack(rev = T)) +
      geom_line(data = ref, aes(x = time, y = 100 * cip, linetype = linetype)) +
      facet_grid(group ~ sex, drop = FALSE) +
      xlab("Age in years") +
      ylab("Percentage of persons alive") +
      theme_bw() +
      scale_x_continuous(breaks = c(min_age, seq(0, 100, 10))) +
      scale_linetype_manual(
        name = "",
        values = "dashed"
      )

    if (cause == "All causes") {
      g <- g +
        scale_fill_manual(
          name = "",
          breaks = c("Alive", "Dead (all causes)"),
          values = c("white", "red")
        )
    } else {
      g <- g +
        scale_fill_manual(
          name = "",
          breaks = c("Alive", "Dead (natural causes)", "Dead (external causes)", "Dead (all causes)"),
          values = c("white", "#7fc97f", "#beaed4", "red")
        )
    }

    g <- g +
      guides(linetype = guide_legend(order = 2), fill = guide_legend(order = 1))



    # Text for interpretation
    check <- list_dx %>%
      filter(description == !!dx) %>%
      filter(LYL == TRUE) %>%
      select(id, p25, p50, p75) %>%
      distinct()
    point <- seq(0, 100, 10)
    point <- point[point > as.numeric(check[, paste0("p", as.numeric(substr(age, 2, 3)))])]
    point <- point[ceiling(length(point) / 2)]

    data <- LYLplot2 %>%
      filter(time <= !!point) %>%
      group_by(cause, group, sex) %>%
      arrange(-time) %>%
      slice(1) %>%
      ungroup()


    causes <- FALSE
    text <- paste0(
      "<b>Interpretation:</b> When focusing on one specific age, it is possible to look at the survival curves from that age, as well as the cumulative incidences of dying.
    Here we present the survival curves and stacked cumulative incidences for 3 specific ages, corresponding to the median age at diagnosis (",
      format_numbers(as.numeric(check$p50), 0), " years), and
                   percentiles 25 (", format_numbers(as.numeric(check$p25), 0), " years) and 75 (", format_numbers(as.numeric(check$p75), 0), " years)."
    )

    LYL_plot_sex <- data %>% filter(sex == "Females")
    if (nrow(LYL_plot_sex) > 0) {
      sex <- "Females"
    } else {
      LYL_plot_sex <- data %>% filter(sex == "Males")
      if (nrow(LYL_plot_sex) > 0) {
        sex <- "Males"
      }
    }

    if (nrow(LYL_plot_sex) > 0) {
      surv <- format_numbers(100 * as.numeric(LYL_plot_sex %>% filter(sex == !!sex, cause == "Alive", group == "Diagnosed with the disorder") %>% select(cip)), 0)
      surv0 <- format_numbers(100 * as.numeric(LYL_plot_sex %>% filter(sex == !!sex, cause == "Alive", group != "Diagnosed with the disorder") %>% select(cip)), 0)

      text <- paste0(
        text, " When looking at ", tolower(sex), " with a diagnosis at age ", min_age, " years, ", surv, "% of them are alive at age ", point,
        ", compared to ", surv0, "% in the general population of Danish ", tolower(sex), " of the same age."
      )

      if ("Dead (natural causes)" %in% LYL_plot_sex$cause) {
        causes <- TRUE
        nat <- format_numbers(100 * as.numeric(LYL_plot_sex %>% filter(sex == !!sex, cause == "Dead (natural causes)", group == "Diagnosed with the disorder") %>% select(cip)), 0)
        nat0 <- format_numbers(100 * as.numeric(LYL_plot_sex %>% filter(sex == !!sex, cause == "Dead (natural causes)", group != "Diagnosed with the disorder") %>% select(cip)), 0)
        ext <- format_numbers(100 * as.numeric(LYL_plot_sex %>% filter(sex == !!sex, cause == "Dead (external causes)", group == "Diagnosed with the disorder") %>% select(cip)), 0)
        ext0 <- format_numbers(100 * as.numeric(LYL_plot_sex %>% filter(sex == !!sex, cause == "Dead (external causes)", group != "Diagnosed with the disorder") %>% select(cip)), 0)

        text <- paste0(
          text, " More specifically, ", nat, "% die of natural causes and ", ext, "% of external causes (compared to ",
          nat0, "% and ", ext0, "% in the general population of Danish ", tolower(sex), " of the same age)."
        )
      }

      LYL_plot_sex <- LYL_plot_sex %>%
        mutate(id = as.numeric(check$id)) %>%
        select(id, sex) %>%
        distinct() %>%
        left_join(LYLages, by = c("id", "sex")) %>%
        filter(age == !!min_age)

      text <- paste0(
        text, " When looking at remaining life expectancy (corresponding to the area under the survival curve), ", tolower(sex),
        " with a diagnosis at age ", min_age, ", live on average an additional ", format_numbers(LYL_plot_sex$life_exp, 1), " years, compared to ",
        format_numbers(LYL_plot_sex$life_exp0, 1), " years in Danish ", tolower(sex), " from the general population. Consequently, ", tolower(sex),
        " with a diagnosis at age ", min_age, " years experience an average life years lost of ",
        format_numbers(LYL_plot_sex$life_exp0 - LYL_plot_sex$life_exp, 1), " years"
      )

      if (causes == TRUE) {
        text <- paste0(
          text, ", which can be decomposed into ", format_numbers(LYL_plot_sex$LYL_Natural, 1), " years due to natural causes and ",
          format_numbers(LYL_plot_sex$LYL_External, 1), " years due to external causes.<br/><b>Note:</b> These estimates are based on the assumption that the diagnosed will experience the mortality rates
                                                   of the diagnosed during the entire life (after diagnosis), which might be plausible for chronic disorders but not for acute ones."
        )
      } else {
        text <- paste0(text, ".<br/><b>Note:</b> These estimates are based on the assumption that the diagnosed will experience the mortality rates
                                                   of the diagnosed during the entire life (after diagnosis), which might be plausible for chronic disorders but not for acute ones.")
      }
    }

    text <- HTML("<div style='background-color:#E5E4E2'>", text)

    return(list(g = g, text = text))
  }
}



interpretation_main <- function(dx) {
  # Prepare interpretation and what to show
  check <- list_dx %>% filter(description == !!dx)

  # N diagnosed
  N <- check %>%
    filter(include == 1) %>%
    mutate(
      Diagnosed = ifelse(!is.na(n), format(n, big.mark = ","), as.character(n_text)),
      Deaths = ifelse(!is.na(cases), format(cases, big.mark = ","), as.character(cases_text))
    )
  if (nrow(N %>% filter(sex == "All")) == 1) {
    N <- N %>% filter(sex == "All")
  } else {
    N <- N %>% filter(sex != "All")
  }
  sex <- case_when(
    N[1, "sex"] == "All" ~ "persons",
    N[1, "sex"] == "Males" ~ "males",
    N[1, "sex"] == "Females" ~ "females"
  )
  N_text <- paste0(
    "<b>Interpretation:</b> A total of ", N[1, "Diagnosed"], " ", sex, " living in Denmark in 2000-2018 were diagnosed for the first time with disorder \"", N[1, "desc_EN"],
    "\" in a hospital in 1995-2018."
  )

  age <- N[1, "age_dx50"]
  if (!is.na(age)) {
    N_text <- paste0(
      N_text, " The median age at diagnosis was ", format_numbers(age, 1), " years, while 25% of the diagnosed were
                     younger than ", format_numbers(N[1, "age_dx25"], 1), " years and 25% were older than ", format_numbers(N[1, "age_dx75"], 1),
      " years at time of diagnosis."
    )
  }

  N_text <- paste0(N_text, " Among the diagnosed, ", N[1, "Deaths"], " ", sex, " died in 2000-2018")

  age <- N[1, "age_death50"]
  if (!is.na(age)) {
    N_text <- paste0(
      N_text, ", and the median age at time of death was ", format_numbers(age, 1), " years (25% were
                     younger than ", format_numbers(N[1, "age_death25"], 1), " years and 25% were older than ", format_numbers(N[1, "age_death75"], 1),
      " years)."
    )
  } else {
    N_text <- paste0(N_text, ".")
  }

  N <- N %>%
    select(-MRR, -LYL) %>%
    left_join(MRR, by = c("id", "sex")) %>%
    filter(cod == "All") %>%
    left_join(LYL, by = c("id", "sex"))

  HR <- N[1, "HR"]
  LYLnum <- N[1, "LYL_Total"]
  if (!is.na(HR)) {
    if (HR < 1) {
      mortality_text <- paste0(" On average, mortality rates were ", format_numbers(100 * (1 - HR), 0), "% lower among the diagnosed compared to those of same age and sex without that diagnosis (MRR = ", N[1, "MRR_95CI"], ")")
    } else {
      mortality_text <- paste0(" On average, mortality rates were ", format_numbers(100 * (HR - 1), 0), "% higher among the diagnosed compared to those of same age and sex without that diagnosis (MRR = ", N[1, "MRR_95CI"], ")")
      if (HR >= 2) {
        mortality_text <- paste0(" On average, mortality rates were ", format_numbers(HR, 1), " times higher among the diagnosed compared to those of same age and sex without that diagnosis (MRR = ", N[1, "MRR_95CI"], ")")
      }
    }
    if (!is.na(LYLnum)) {
      mortality_text <- paste0(mortality_text, ", and the average reduction in life expectancy after diagnosis was ", format_numbers(LYLnum, 1), " years compared to the entire Danish population of same age and sex")
      if (!is.na(N[1, "LYL_Total_L"])) {
        mortality_text <- paste0(mortality_text, " (LYL = ", N[1, "Total_95CI"], ").<br/><b>Note:</b> The estimate of life expectancy is based on the assumption that the diagnosed will experience the mortality rates
                                                   of the diagnosed during the entire life (after diagnosis), which might be plausible for chronic disorders but not for acute ones.")
      } else {
        mortality_text <- paste0(mortality_text, ".<br/><b>Note:</b> The estimate of life expectancy is based on the assumption that the diagnosed will experience the mortality rates
                                                   of the diagnosed during the entire life (after diagnosis), which might be plausible for chronic disorders but not for acute ones.")
      }
    } else {
      mortality_text <- paste0(mortality_text, ".")
    }
    N_text <- paste0(N_text, mortality_text)
  }
  main <- HTML("<div style='background-color:#E5E4E2'>", N_text)

  ###### Incidence
  x <- list_dx %>%
    filter(description == dx) %>%
    select(id) %>%
    distinct() %>%
    left_join(incidence, by = "id") %>%
    filter(age_group <= 95)

  if (!is.na(sex) & (sex == "persons")) {
    x <- x %>% filter(sex == "All", !is.na(dx_rate))
    x <- x[c(floor(nrow(x) / 2), floor(3 * nrow(x) / 4)), ]
    rate_sex <- "persons"
  } else {
    x <- x %>% filter(sex != "All")
    w <- x %>% filter(sex == "Females", !is.na(dx_rate))
    if (nrow(w) > 0) {
      x <- w[c(floor(nrow(w) / 2), floor(3 * nrow(w) / 4)), ]
      rate_sex <- "females"
    } else {
      x <- x %>% filter(sex == "Males", !is.na(dx_rate))
      x <- x[c(floor(nrow(x) / 2), floor(3 * nrow(x) / 4)), ]
      rate_sex <- "males"
    }
  }

  if (nrow(x) != 0) {
    age <- x[1, "age_group"]
    rate <- format_numbers(10000 * x[1, "dx_rate"], 1)
    rate_left <- format_numbers(10000 * x[1, "dx_rate_left"], 1)
    rate_right <- format_numbers(10000 * x[1, "dx_rate_right"], 1)

    text_incidence <- paste0(
      "<b>Interpretation:</b> These figures show the age-specific incidence rates (per 10,000 person-years) of being diagnosed for the first time with disorder \"", N[1, "desc_EN"],
      "\". As example, if 10,000 ", rate_sex, " of age ", age, "-", age + 5,
      " years are followed for one year, on average ", rate, " of them will be diagnosed with the disorder for the first time (IR = ", rate, " (95% CI: ", rate_left,
      "-", rate_right, "))"
    )

    age2 <- x[2, "age_group"]
    rate <- format_numbers(10000 * x[2, "dx_rate"], 1)
    rate_left <- format_numbers(10000 * x[2, "dx_rate_left"], 1)
    rate_right <- format_numbers(10000 * x[2, "dx_rate_right"], 1)

    if (age == age2) {
      text_incidence <- paste0(text_incidence, ".")
    } else {
      text_incidence <- paste0(
        text_incidence, ", while it will be ", rate, " among ", rate_sex, " of age ", age2, "-", age2 + 5, " years  (IR = ", rate, " (95% CI: ", rate_left,
        "-", rate_right, "))."
      )
    }
  } else {
    text_incidence <- ""
  }

  text_incidence <- HTML("<div style='background-color:#E5E4E2'>", text_incidence)

  incidence2 <- "<b>Interpretation:</b> These figures show the distribution of age-of-onset, without taking into account whether
  a disorder is more or less likely to occur."

  sex <- ifelse(sex == "persons", "persons (males and females combined)", sex)

  age <- N[1, "age_dx50"]
  if (!is.na(age)) {
    incidence2 <- paste0(
      incidence2, " The median age at diagnosis for ", sex, " was ", format_numbers(age, 1), " years, while 25% of the diagnosed were
                     younger than ", format_numbers(N[1, "age_dx25"], 1), " years and 25% were older than ", format_numbers(N[1, "age_dx75"], 1),
      " years at time of diagnosis."
    )
  }

  incidence2 <- HTML("<div style='background-color:#E5E4E2'>", incidence2)


  ###### Mortality rates
  x <- list_dx %>%
    filter(description == dx) %>%
    select(id) %>%
    add_row(id = 0) %>%
    distinct() %>%
    left_join(incidence, by = "id") %>%
    mutate(
      id = ifelse(id == 0, 0, 1)
    ) %>%
    filter(age_group <= 95)

  x <- x %>%
    filter(id == 1) %>%
    left_join(x %>% filter(id == 0) %>%
      select(age_group, sex, death_rate0 = death_rate, death_rate_left0 = death_rate_left, death_rate_right0 = death_rate_right),
    by = c("sex", "age_group")
    )

  if (!is.na(sex) & (sex == "persons")) {
    x <- x %>% filter(sex == "All", !is.na(death_rate))
    x <- x[c(floor(nrow(x) / 2), floor(3 * nrow(x) / 4)), ]
    rate_sex <- "persons"
  } else {
    x <- x %>% filter(sex != "All")
    w <- x %>% filter(sex == "Females", !is.na(death_rate))
    if (nrow(w) > 0) {
      x <- w[c(floor(nrow(w) / 2), floor(3 * nrow(w) / 4)), ]
      rate_sex <- "females"
    } else {
      x <- x %>% filter(sex == "Males", !is.na(death_rate))
      x <- x[c(floor(nrow(x) / 2), floor(3 * nrow(x) / 4)), ]
      rate_sex <- "males"
    }
  }

  if (nrow(x) != 0) {
    age <- x[1, "age_group"]
    rate <- format_numbers(10000 * x[1, "death_rate"], 1)
    rate_left <- format_numbers(10000 * x[1, "death_rate_left"], 1)
    rate_right <- format_numbers(10000 * x[1, "death_rate_right"], 1)
    rate0 <- format_numbers(10000 * x[1, "death_rate0"], 1)
    rate_left0 <- format_numbers(10000 * x[1, "death_rate_left0"], 1)
    rate_right0 <- format_numbers(10000 * x[1, "death_rate_right0"], 1)


    mortality <- paste0(
      "<b>Interpretation:</b> These figures show the age-specific mortality rates (per 10,000 person-years) among those diagnosed for the first time with disorder \"", N[1, "desc_EN"],
      "\" and the general population of Denmark. As example, an average of ", rate, " deaths are expected to be observed if 10,000 ", rate_sex, " of age ", age, "-", age + 5,
      " years diagnosed for the first time with disorder \"", N[1, "desc_EN"],
      "\" are followed for one year (MR = ", rate, " (95% CI: ", rate_left,
      "-", rate_right, ")), compared to ", rate0, " in the general population of same age (MR = ", rate0, " (95% CI: ", rate_left0,
      "-", rate_right0, "))"
    )

    age2 <- x[2, "age_group"]
    rate <- format_numbers(10000 * x[2, "death_rate"], 1)
    rate_left <- format_numbers(10000 * x[2, "death_rate_left"], 1)
    rate_right <- format_numbers(10000 * x[2, "death_rate_right"], 1)
    rate0 <- format_numbers(10000 * x[2, "death_rate0"], 1)
    rate_left0 <- format_numbers(10000 * x[2, "death_rate_left0"], 1)
    rate_right0 <- format_numbers(10000 * x[2, "death_rate_right0"], 1)

    if (age == age2) {
      mortality <- paste0(mortality, ".")
    } else {
      mortality <- paste0(
        mortality, ", while an average of ", rate, " deaths are expected to be observed if 10,000 ", rate_sex, " of age ", age2, "-", age2 + 5, " years diagnosed for the first time with disorder \"", N[1, "desc_EN"],
        "\" are followed for one year (MR = ", rate, " (95% CI: ", rate_left,
        "-", rate_right, ")), compared to ", rate0, " in the general population of same age (MR = ", rate0, " (95% CI: ", rate_left0,
        "-", rate_right0, "))."
      )
    }
  } else {
    mortality <- ""
  }

  mortality <- HTML("<div style='background-color:#E5E4E2'>", mortality)


  mortality2 <- paste0(
    "<b>Interpretation:</b> These figures show the distribution of age-of-death among those diagnosed for the first time with disorder \"", N[1, "desc_EN"],
    "\" who died during the follow-up, without taking into account whether mortality was likely or unlikely."
  )

  age <- N[1, "age_death50"]
  if (!is.na(age)) {
    mortality2 <- paste0(
      mortality2, " Among those ", sex, " who died with a previous diagnosis, the median age at time of death was ", format_numbers(age, 1), " years (25% were
                     younger than ", format_numbers(N[1, "age_death25"], 1), " years and 25% were older than ", format_numbers(N[1, "age_death75"], 1),
      " years)."
    )
  }

  mortality2 <- HTML("<div style='background-color:#E5E4E2'>", mortality2)



  # Among persons diagnosed with disorders of the circulatory system who died, the median age at time of death was 81.6 years (25% were younger than 73.3 years and 25% were older than 88.0 years).

  #### Lagged HR
  x <- list_dx %>%
    filter(description == dx) %>%
    select(id) %>%
    distinct() %>%
    left_join(MRRlagged, by = "id") %>%
    filter(exposure %in% c(1, 4))


  lagged <- paste0(
    "<b>Interpretation:</b> The left figure shows the mortality rate ratios comparing those with and without a diagnosis of disorder \"", N[1, "desc_EN"],
    "\" depending on time since the first diagnosis. In the first 6
    months after an initial diagnosis, the diagnosed had an average mortality rate "
  )

  MRR_age <- NA

  if (nrow(x %>% filter(exposure == 1)) > 0) {
    MRR_age <- x[x$exposure == 1, "HR"]
    CI_left <- x[x$exposure == 1, "CI_left"]
    CI_right <- x[x$exposure == 1, "CI_right"]
  }

  if (!is.na(MRR_age)) {
    if (MRR_age < 1) {
      lagged <- paste0(lagged, format_numbers(100 * (1 - MRR_age), 0), "% lower ")
    } else {
      if (MRR_age < 2) {
        lagged <- paste0(lagged, format_numbers(100 * (MRR_age - 1), 0), "% higher ")
      } else {
        lagged <- paste0(lagged, format_numbers(MRR_age, 1), " times higher ")
      }
    }

    lagged <- paste0(
      lagged, "compared to those of same age and sex without that diagnosis (MRR = ", format_numbers(MRR_age, 2), " (95% CI: ", format_numbers(CI_left, 2),
      "-", format_numbers(CI_right, 2), ")), while
                       the average mortality rate of the diagnosed was "
    )
  }


  MRR_age <- NA

  if (nrow(x %>% filter(exposure == 4)) > 0) {
    MRR_age <- x[x$exposure == 4, "HR"]
    CI_left <- x[x$exposure == 4, "CI_left"]
    CI_right <- x[x$exposure == 4, "CI_right"]
  }



  if (!is.na(MRR_age)) {
    if (MRR_age < 1) {
      lagged <- paste0(lagged, format_numbers(100 * (1 - MRR_age), 0), "% lower ")
    } else {
      if (MRR_age < 2) {
        lagged <- paste0(lagged, format_numbers(100 * (MRR_age - 1), 0), "% higher ")
      } else {
        lagged <- paste0(lagged, format_numbers(MRR_age, 1), " times higher ")
      }
    }

    lagged <- paste0(
      lagged, "compared to those without that diagnosis (MRR = ", format_numbers(MRR_age, 2), " (95% CI: ", format_numbers(CI_left, 2),
      "-", format_numbers(CI_right, 2), ")) between 2 and 5 years after the initial diagnosis."
    )
  }

  #### Age-specific MRR
  x <- list_dx %>%
    filter(description == dx) %>%
    select(id) %>%
    distinct() %>%
    left_join(MRRage, by = "id") %>%
    filter(age_group < 100, !is.na(HR))
  age_categories <- unique(x$age_categories)

  if (nrow(x) > 0) {
    lagged <- paste0(
      lagged, " The right figure shows the mortality rate ratios comparing those with and without a diagnosis of disorder \"", N[1, "desc_EN"],
      "\" depending on age. "
    )

    x <- unique(x[c(floor(nrow(x) / 2), floor(3 * nrow(x) / 4)), ])

    age <- x[1, "age_group"]
    MRR_age <- x[1, "HR"]
    CI_left <- x[1, "CI_left"]
    CI_right <- x[1, "CI_right"]

    lagged <- paste0(lagged, "At age ", age, "-", age + age_categories, " years, those with a diagnosis experience mortality rates ")
    if (MRR_age < 1) {
      lagged <- paste0(lagged, format_numbers(100 * (1 - MRR_age), 0), "% lower ")
    } else {
      if (MRR_age < 2) {
        lagged <- paste0(lagged, format_numbers(100 * (MRR_age - 1), 0), "% higher ")
      } else {
        lagged <- paste0(lagged, format_numbers(MRR_age, 1), " times higher ")
      }
    }

    lagged <- paste0(
      lagged, "compared to those of same age and sex without that diagnosis (MRR = ", format_numbers(MRR_age, 2), " (95% CI: ", format_numbers(CI_left, 2),
      "-", format_numbers(CI_right, 2), "))"
    )

    if (nrow(x) == 1) {
      lagged <- paste0(lagged, ".")
    } else {
      lagged <- paste0(lagged, ", while the average mortality rate of the diagnosed was ")
      age <- x[2, "age_group"]
      MRR_age <- x[2, "HR"]
      CI_left <- x[2, "CI_left"]
      CI_right <- x[2, "CI_right"]

      if (MRR_age < 1) {
        lagged <- paste0(lagged, format_numbers(100 * (1 - MRR_age), 0), "% lower ")
      } else {
        if (MRR_age < 2) {
          lagged <- paste0(lagged, format_numbers(100 * (MRR_age - 1), 0), "% higher ")
        } else {
          lagged <- paste0(lagged, format_numbers(MRR_age, 1), " times higher ")
        }
      }

      lagged <- paste0(
        lagged, "at age ", age, "-", age + age_categories, " years compared to those of same age and sex without that diagnosis (MRR = ", format_numbers(MRR_age, 2), " (95% CI: ", format_numbers(CI_left, 2),
        "-", format_numbers(CI_right, 2), "))."
      )
    }
  }

  lagged <- HTML("<div style='background-color:#E5E4E2'>", lagged)


  ## LYL ages
  x <- list_dx %>%
    filter(description == dx) %>%
    select(id, p5, p95) %>%
    filter(!is.na(p5)) %>%
    distinct() %>%
    left_join(LYLages, by = "id")

  x <- x %>%
    filter(!is.na(life_exp))

  w <- x %>% filter(sex == "Females")
  if (nrow(w) > 0) {
    sex <- "females"
    x <- w
  } else {
    sex <- "males"
    x <- x %>% filter(sex == "Males")
  }

  x <- unique(x[c(floor(nrow(x) / 2), floor(3 * nrow(x) / 4)), ])

  if (nrow(x) > 0) {
    LYLages <- paste0(
      "<b>Interpretation:</b> This figure shows the remaining life expectancies for each single age of a range that includes 90% of the diagnoses: from percentile 5 (",
      x[1, "p5"], " years) to percentile 95 (", x[1, "p95"], " years) of the distribution of age at diagnosis of disorder \"", N[1, "desc_EN"], "\"."
    )

    age <- x[1, "age"]
    life_exp <- x[1, "life_exp"]
    left <- x[1, "life_exp_L"]
    right <- x[1, "life_exp_R"]
    life_exp0 <- x[1, "life_exp0"]

    LYLages <- paste0(LYLages, " For ", sex, " with a diagnosis at age ", age, " years, the average remaining life expectancy is ", format_numbers(life_exp, 1), " years ")

    if (!is.na(left)) {
      LYLages <- paste0(LYLages, "(95% CI: ", format_numbers(left, 1), "-", format_numbers(right, 1), ") ")
    }

    LYLages <- paste0(
      LYLages, "compared to ",
      format_numbers(life_exp0, 1), " years in the general population of ", sex, " of same age"
    )

    if (nrow(x) == 1) {
      LYLages <- paste0(LYLages, ".")
    } else {
      age <- x[2, "age"]
      life_exp <- x[2, "life_exp"]
      life_exp <- x[2, "life_exp"]
      left <- x[2, "life_exp_L"]
      life_exp0 <- x[2, "life_exp0"]
      LYLages <- paste0(LYLages, ", while for ", sex, " with a diagnosis at age ", age, " years, the average remaining life expectancy is ", format_numbers(life_exp, 1), " years ")

      if (!is.na(left)) {
        LYLages <- paste0(LYLages, "(95% CI: ", format_numbers(left, 1), "-", format_numbers(right, 1), ") ")
      }

      LYLages <- paste0(
        LYLages, "compared to ",
        format_numbers(life_exp0, 1), " years in the general population of ", sex, " of same age."
      )
    }

    LYLages <- paste0(LYLages, "<br/><b>Note:</b> These estimates are based on the assumption that the diagnosed will experience the mortality rates
      of the diagnosed during the entire life (after diagnosis), which might be plausible for chronic disorders but not for acute ones.")
  } else {
    LYLages <- ""
  }

  LYLages <- HTML("<div style='background-color:#E5E4E2'>", LYLages)


  ##### MRRs
  t <- list_dx %>%
    filter(description == dx) %>%
    select(id) %>%
    distinct() %>%
    left_join(MRR, by = "id")

  x <- t %>%
    filter(
      !(cod %in% c("Air_adjusted", "Air_not_adjust")),
      !is.na(HR)
    ) %>%
    mutate(
      order1 = case_when(
        sex == "All" ~ 3,
        sex == "Females" ~ 1,
        sex == "Males" ~ 2
      ),
      order2 = case_when(
        cod == "All" ~ 2,
        cod == "Natural" ~ 1,
        cod == "External" ~ 3
      )
    )

  text_MRR <- paste0("<b>Interpretation:</b> The table shows mortality rates (per 1,000 person-years) for those diagnosed for the first time with disorder \"", N[1, "desc_EN"], "\" and for the age- and sex-standardized
           general Danish population for each sex and group of causes of death (all, natural and external causes), as well as Mortality Rate Ratios (95% confidence interval) comparing those diagnosed with those not diagnosed.")

  x <- x %>%
    mutate(!is.na(HR)) %>%
    arrange(order1, order2) %>%
    slice(1)

  sex <- ifelse(x$sex[1] == "All", "persons", tolower(x$sex[1]))

  if (nrow(x) > 0) {
    text_MRR <- paste0(text_MRR, " For ", sex, ", mortality rates of dying of ", tolower(x[1, "cod"]), " causes were ", x[1, "rate"], " per 1,000 person-years for the diagnosed and ", x[1, "rateDK_std"], " per 1,000 person-years for the general population of same sex and age.
                         Mortality rates of dying of ", tolower(x[1, "cod"]), " causes were ")

    MRR_age <- x[1, "HR"]
    if (MRR_age < 1) {
      text_MRR <- paste0(text_MRR, format_numbers(100 * (1 - MRR_age), 0), "% lower ")
    } else {
      if (MRR_age < 2) {
        text_MRR <- paste0(text_MRR, format_numbers(100 * (MRR_age - 1), 0), "% higher ")
      } else {
        text_MRR <- paste0(text_MRR, format_numbers(MRR_age, 1), " times higher ")
      }
    }

    text_MRR <- paste0(text_MRR, "for those with a diagnosis than those without (MRR = ", x[1, "MRR_95CI"], ").")
  }

  x <- list_dx %>%
    filter(description == dx) %>%
    select(id) %>%
    distinct() %>%
    left_join(LYL, by = "id") %>%
    mutate(
      order1 = case_when(
        sex == "All" ~ 3,
        sex == "Females" ~ 1,
        sex == "Males" ~ 2
      )
    ) %>%
    filter(Total != "-") %>%
    arrange(order1) %>%
    slice(1)


  if (nrow(x) == 0) {
    text_MRR <- paste0(text_MRR, " Estimates are not available for causes with less than 20 deaths.")
  } else {
    sex <- ifelse(x$sex[1] == "All", "persons", tolower(x$sex[1]))
    text_MRR <- paste0(text_MRR, " Additionally, the table above shows remaining life expectancy after disease diagnosis for those diagnosed for the first time with disorder \"", N[1, "desc_EN"], "\" and for the general population of same age and sex. Total life years lost represents the average reduction
                         in life expectancy, which can be decomposed into natural and external causes of death. For ", sex, ", reduction in life
                         expectancy was ", x[1, "Total_95CI"], " years")
    if (x[1, "Natural"] == "-") {
      text_MRR <- paste0(text_MRR, ". Estimates are not available for causes with less than 20 deaths. Additionally, life expectancy (and life years lost) for males and females combined is not available when estimates
                          are not available for one of the sexes.")
    } else {
      text_MRR <- paste0(text_MRR, ": ", x[1, "Natural_95CI"], " years due to natural causes and ", x[1, "External_95CI"], " years due to external causes of death.
                           Estimates are not available for causes with less than 20 deaths. Additionally, life expectancy (and life years lost) for males and females combined is not available when estimates
                          are not available for one of the sexes.")
    }
    text_MRR <- paste0(text_MRR, "<br/><b>Note:</b> The estimate of life expectancty is based on the assumption that the diagnosed will experience the mortality rates
         of the diagnosed during the entire life (after diagnosis), which might be plausible for chronic disorders but not for acute ones.")
  }

  text_MRR <- HTML("<div style='background-color:#E5E4E2'>", text_MRR)

  ##### Air
  x <- t %>%
    filter(
      cod %in% c("Air_adjusted", "Air_not_adjust"),
      !is.na(HR)
    )

  MRR0 <- x[x$cod == "Air_not_adjust", "MRR_95CI"]
  MRR1 <- x[x$cod == "Air_adjusted", "MRR_95CI"]

  y <- x %>%
    select(sex) %>%
    distinct() %>%
    mutate(cod = "All") %>%
    left_join(
      t,
      by = c("sex", "cod")
    )

  MRR00 <- y[1, "MRR_95CI"]

  if (nrow(x) > 0) {
    MRRair <- paste0("<b>Interpretation:</b> There is evidence that air pollution has an effect on both health and mortality. In order to assess whether the association between being diagnosed with the disorder \"", N[1, "desc_EN"], "\" and mortality could be explained by air pollution,
mortality rate ratios for all-cause mortality have been estimated after further adjustment for air pollution. This table shows mortality rates (per 1,000 person-years) for those diagnosed for the first time with disorder \"", N[1, "desc_EN"], "\" and the age- and sex-standardized
      general Danish population; and Mortality Rate Ratios (MRR; 95% confidence interval) with and without adjustment for air pollution. The unadjusted estimate (MRR = ", MRR0, ") is restricted to individuals with
      information on air pollution (N = 6,990,131; 94.7% of the total sample), and might differ from the main
      results (MRR = ", MRR00, ") based on the entire population (N = 7,378,598). The adjusted estimate (MRR = ", MRR1, ") adjusts for air pollution in addition to sex, age and calendar time.")
  } else {
    MRRair <- ""
  }

  MRRair <- HTML("<div style='background-color:#E5E4E2'>", MRRair)


  return(list(
    main = main, incidence = text_incidence, incidence2 = incidence2, mortality = mortality, mortality2 = mortality2,
    lagged = lagged, LYLages = LYLages, MRR = text_MRR, MRRair = MRRair
  ))
}


### Close tabs

close_panels <- function(n, button, button2, panel, session) {
  if (n == 0) {
    shinyjs::hide(id = panel)
    if (is.null(button)) {
      return()
    }
    shinyjs::disable(id = button)
  } else {
    if (is.null(button)) {
      shinyjs::show(id = panel)
      return()
    }
    shinyjs::enable(id = button)
    if (button2 %% 2 == 1) {
      shinyjs::show(id = panel)
    }
  }
}





plot_all <- function(dx, sex, causes, list_dx_included, list_clean) {
  x_sex <- dx %>% filter(sex %in% !!sex)

  # Include M/F if sex == "all" and not estimates for everyone
  if ("All" %in% sex) {
    out <- x_sex %>%
      filter(include == 0) %>%
      select(id) %>%
      left_join(
        dx %>% filter(sex != "All") %>% filter(include == 1),
        by = "id"
      )
    dx <- x_sex %>% bind_rows(out)
  } else {
    dx <- x_sex
  }

  if (length(sex) == 2) {
    g <- ggplot(dx, aes(y = pos, x = n / 1000, color = sex, fill = sex)) +
      geom_col(position = "dodge")
  } else {
    g <- ggplot(dx, aes(y = pos, x = n / 1000)) +
      geom_col()
  }

  g00 <- g +
    scale_x_continuous(labels = scales::comma) +
    scale_y_discrete(drop = FALSE, breaks = list_clean, labels = list_dx_included) +
    ylab(NULL) + xlab("Number of diagnosed in 1995-2018 (in thousands)") +
    theme_bw() +
    theme(
      legend.title = element_blank(),
      legend.position = "top",
      axis.text.y = element_text(hjust = 0, size = 11, vjust = 0.36)
    )

  x <- dx %>%
    select(id, sex, level, chapter, desc_EN, pos) %>%
    left_join(
      MRR,
      by = c("id", "sex")
    ) %>%
    filter(cod == !!causes)

  if (length(sex) == 2) {
    g <- ggplot(data = x, aes(x = HR, y = pos, color = sex)) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
      geom_point(position = pd) +
      geom_errorbarh(aes(xmin = CI_left, xmax = CI_right), height = 0.13, position = pd)
  } else {
    g <- ggplot(data = x, aes(x = HR, y = pos)) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
      geom_point() +
      geom_errorbarh(aes(xmin = CI_left, xmax = CI_right), height = 0.13)
  }

  g0 <- g +
    scale_x_log10(breaks = c(0.2, 0.5, 0.8, 1, 1.2, 1.5, 3, 5, 10, 20, 40, 100, 150, 250)) +
    scale_y_discrete(drop = FALSE, breaks = list_clean, labels = list_dx_included) +
    ylab(NULL) + xlab(paste0("Mortality Rate Ratios for ", tolower(causes), " causes (95% CI)")) +
    theme_bw() +
    theme(
      legend.title = element_blank(),
      legend.position = "top",
      axis.text.y = element_blank()
    )

  LYL_aux <- LYL %>%
    select(id, sex, LYL = LYL_Total, LYL_L = LYL_Total_L, LYL_R = LYL_Total_R) %>%
    mutate(cod = "All") %>%
    bind_rows(LYL %>% select(id, sex, LYL = LYL_Natural, LYL_L = LYL_Natural_L, LYL_R = LYL_Natural_R) %>% mutate(cod = "Natural")) %>%
    bind_rows(LYL %>% select(id, sex, LYL = LYL_External, LYL_L = LYL_External_L, LYL_R = LYL_External_R) %>% mutate(cod = "External"))


  x2 <- dx %>%
    select(id, sex, level, chapter, desc_EN, pos) %>%
    left_join(
      LYL_aux,
      by = c("id", "sex")
    ) %>%
    filter(cod == !!causes)

  # if(causes == "External") {
  #   breaks_LYL <- seq(-10, 10, 0.5)
  # } else {
  #   breaks_LYL <- seq(-20, 60, 2)
  # }

  if (length(sex) == 2) {
    g <- ggplot(data = x2, aes(x = LYL, y = pos, color = sex)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
      geom_point(position = pd) +
      geom_errorbarh(aes(xmin = LYL_L, xmax = LYL_R), height = 0.13, position = pd)
  } else {
    g <- ggplot(data = x2, aes(x = LYL, y = pos)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
      geom_point() +
      geom_errorbarh(aes(xmin = LYL_L, xmax = LYL_R), height = 0.13)
  }

  g1 <- g +
    # scale_x_continuous(breaks = breaks_LYL)+
    scale_y_discrete(drop = FALSE, breaks = list_clean, labels = list_dx_included) +
    ylab(NULL) + xlab(paste0("Life Years Lost due to ", tolower(causes), " causes")) +
    theme_bw() +
    theme(
      legend.title = element_blank(),
      legend.position = "top",
      axis.text.y = element_blank()
    )

  return(ggarrange(g00, g0, g1, ncol = 3))
}


plot_mrr_all <- function(dx, sex, causes, list_dx_included, list_clean) {
  x_sex <- dx %>% filter(sex %in% !!sex)

  # Include M/F if sex == "all" and not estimates for everyone
  if ("All" %in% sex) {
    out <- x_sex %>%
      filter(include == 0) %>%
      select(id) %>%
      left_join(
        dx %>% filter(sex != "All") %>% filter(include == 1),
        by = "id"
      )
    dx <- x_sex %>% bind_rows(out)
  } else {
    dx <- x_sex
  }

  x <- dx %>%
    select(id, sex, level, chapter, desc_EN, pos) %>%
    left_join(
      MRR,
      by = c("id", "sex")
    ) %>%
    filter(cod == !!causes)


  if (length(sex) == 2) {
    g <- ggplot(data = x, aes(x = HR, y = pos, color = sex)) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
      geom_point(position = pd) +
      geom_errorbarh(aes(xmin = CI_left, xmax = CI_right), height = 0.13, position = pd)
  } else {
    g <- ggplot(data = x, aes(x = HR, y = pos)) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
      geom_point() +
      geom_errorbarh(aes(xmin = CI_left, xmax = CI_right), height = 0.13)
  }

  g +
    scale_x_log10(breaks = c(0.2, 0.5, 0.8, 1, 1.2, 1.5, 3, 5, 10, 20, 40, 100, 150, 250)) +
    scale_y_discrete(drop = FALSE, breaks = list_clean, labels = list_dx_included) +
    ylab(NULL) + xlab(paste0("Mortality Rate Ratios for ", tolower(causes), " causes (95% CI)")) +
    theme_bw() +
    theme(
      legend.title = element_blank(),
      axis.text.y = element_text(hjust = 0, size = 11, vjust = 0.36)
    )
}

plot_lyl_all <- function(dx, sex, causes, list_dx_included, list_clean) {
  x_sex <- dx %>% filter(sex %in% !!sex)

  # Include M/F if sex == "all" and not estimates for everyone
  if ("All" %in% sex) {
    out <- x_sex %>%
      filter(include == 0) %>%
      select(id) %>%
      left_join(
        dx %>% filter(sex != "All") %>% filter(include == 1),
        by = "id"
      )
    dx <- x_sex %>% bind_rows(out)
  } else {
    dx <- x_sex
  }

  LYL_aux <- LYL %>%
    select(id, sex, LYL = LYL_Total, LYL_L = LYL_Total_L, LYL_R = LYL_Total_R) %>%
    mutate(cod = "All") %>%
    bind_rows(LYL %>% select(id, sex, LYL = LYL_Natural, LYL_L = LYL_Natural_L, LYL_R = LYL_Natural_R) %>% mutate(cod = "Natural")) %>%
    bind_rows(LYL %>% select(id, sex, LYL = LYL_External, LYL_L = LYL_External_L, LYL_R = LYL_External_R) %>% mutate(cod = "External"))


  x2 <- dx %>%
    select(id, sex, level, chapter, desc_EN, pos) %>%
    left_join(
      LYL_aux,
      by = c("id", "sex")
    ) %>%
    filter(cod == !!causes)

  if (length(sex) == 2) {
    g <- ggplot(data = x2, aes(x = LYL, y = pos, color = sex)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
      geom_point(position = pd) +
      geom_errorbarh(aes(xmin = LYL_L, xmax = LYL_R), height = 0.13, position = pd)
  } else {
    g <- ggplot(data = x2, aes(x = LYL, y = pos)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
      geom_point() +
      geom_errorbarh(aes(xmin = LYL_L, xmax = LYL_R), height = 0.13)
  }

  g +
    scale_y_discrete(drop = FALSE, breaks = list_clean, labels = list_dx_included) +
    ylab(NULL) + xlab(paste0("Life Years Lost due to ", tolower(causes), " causes")) +
    theme_bw() +
    theme(
      legend.title = element_blank(),
      axis.text.y = element_text(hjust = 0, size = 11, vjust = 0.36)
    )
}


plot_mrr_air <- function(dx, list_dx_included, list_clean) {
  x <- dx %>%
    select(id, sex, level, chapter, desc_EN, pos) %>%
    left_join(
      MRR %>% filter(cod %in% c("Air_adjusted", "Air_not_adjust")),
      by = c("id")
    ) %>%
    mutate(
      cod = factor(cod, levels = c("Air_adjusted", "Air_not_adjust"), labels = c("Adjusted for air pollution", "Not adjusted"))
    )

  ggplot(data = x, aes(y = pos, color = cod)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    geom_point(aes(x = HR), position = pd) +
    geom_errorbarh(aes(xmin = CI_left, xmax = CI_right), position = pd) +
    scale_x_log10(breaks = c(0.2, 0.5, 0.8, 1, 1.2, 1.5, 3, 5, 10, 20, 40, 100, 150, 250)) +
    scale_y_discrete(drop = FALSE, breaks = list_clean, labels = list_dx_included) +
    ylab(NULL) +
    xlab(paste0("Mortality Rate Ratios for all causes (95% CI)")) +
    theme_bw() +
    theme(
      legend.title = element_blank(),
      legend.position = "top",
      axis.text.y = element_text(hjust = 0, size = 11, vjust = 0.36)
    )
}
