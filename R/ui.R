accordion <- function(...) {
  div(class = "accordion container", ...)
}

accordion_item <- function(header, body, show = TRUE, id = header) {
  id <- gsub("\\s+|\\(|\\)", "-", id)
  btn <- tags$button(
    class = "accordion-button",
    class = if (!show) "collapsed",
    type = "button",
    "data-bs-toggle" = "collapse",
    "data-bs-target" = paste0("#", id),
    header
  )

  div(
    class = "accordion-item",
    h2(
      class = "accordion-header", btn
    ),
    div(
      id = id,
      class = "accordion-collapse collapse",
      class = if (show) "show",
      div(
        class = "accordion-body",
        style = "display: flex;justify-content: center;",
        body
      )
    )
  )
}

offcanvas <- function(id, ..., .title = NULL) {
  div(
    class = "offcanvas offcanvas-bottom",
    id = id, tabindex = "-1",
    "aria-labelledby" = paste0(id, "-label"),
    div(
      class = "offcanvas-header",
      if (length(.title)) h5(class = "offcanvas-title", id = paste(id, "-label"), .title),
      tags$button(
        type = "button", class = "btn-close text-reset",
        "data-bs-dismiss" = "offcanvas",
        "aria-label" = "Close"
      )
    ),
    div(
      class = "offcanvas-body small",
      ...
    )
  )
}


dropdown <- function(id, ..., selected = NULL) {
  items <- rlang::list2(...)
  item_tags <- lapply(items, function(x) {
    a(class = "dropdown-item", id = sub("Model ", "", x), x)
  })
  item_tags[[1]] <- tagAppendAttributes(item_tags[[1]], class = "active")

  id <- sub("\\s+", "-", id)

  # Update the dropdown label
  jquery <- sprintf(
    '$("#%s").on("click", ".dropdown-item", function() {
       $(this).addClass("active");
       $(this).siblings().removeClass("active");
       var btn = $("#%s").find(".btn:first-child");
       var val = $(this).text();
       btn.text(val);
       btn.val(val);
       Shiny.setInputValue("%s", val);
     });', id, id, id
  )
  jquery <- paste("$(function(){", jquery, "});")

  withTags({
    div(
      script(HTML(jquery)),
      class = "dropdown",
      id = id,
      style = "display: inline",
      button(
        class = "btn btn-outline-secondary btn-sm dropdown-toggle",
        style = "padding: .05rem .5rem;",
        type = "button",
        "data-bs-toggle" = "dropdown",
        "aria-haspopup" = "true",
        "aria-expanded" = "false",
        selected %||% items[[1]]
      ),
      div(
        class = "dropdown-menu",
        "aria-labelledby" = id,
        item_tags
      )
    )
  })
}



switchInput <- function(id, label, value = FALSE) {
  div(
    class = "form-check form-switch",
    tags$input(
      id = id, checked = if (value) NA,
      type = "checkbox",
      class = "form-check-input",
      onclick = HTML(sprintf(
        "Shiny.setInputValue(
          '%s',
          document.getElementById('%s').value
        );", id, id
      ))
    ),
    tags$label(
      label,
      `for` = id,
      class = "form-check-label lead"
    )
  )
}


modal <- function(id, title, body, buttons = list()) {
  div(
    class = "modal fade",
    id = id,
    tabindex = "-1",
    role = "dialog",
    "aria-hidden" = "true",
    div(
      class = "modal-dialog modal-dialog-centered modal-lg",
      div(
        class="modal-content",
        div(
          class="row justify-content-center",
          style="padding: 70px; text-align: center;",
          h1(title),
          p(body),
          div(
            Map(
              function(label, file) {
                a(
                  class="btn btn-secondary btn-md text-uppercase",
                  href = file,
                  download = basename(file),
                  label
                )
              }, buttons, names(buttons)
            )
          )
        )
      )
    )
  )
}

