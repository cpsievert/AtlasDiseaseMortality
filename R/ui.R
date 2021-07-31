accordion_item <- function(header, body, show = TRUE, id = header) {
  id <- gsub("\\s+|\\(|\\)", "-", id)
  btn <- tags$button(
    class = "accordion-button", type = "button",
    "data-bs-toggle" = "collapse",
    "data-bs-target" = paste0("#", id),
    header
  )

  div(
    class = "accordion-item",
    h2(class = "accordion-header", btn),
    div(
      id = id,
      class = "accordion-collapse collapse",
      class = if (show) "show",
      div(class = "accordion-body", body)
    )
  )
}


accordion <- function(...) {
  div(class = "accordion", ...)
}
