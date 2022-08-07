#' Generate a shiny pill.

#' @name pill
#' @param id  \code{[character]}  All value must be unique.
#' @param label  \code{[character]}  Must have an exact length of \code{length(id)}.  All value must be unique.
#' @param type  \code{[any]}  Defaults to \code{'none'}
#' @param data_icon  \code{[any]}  Defaults to \code{NULL}
#' @param class  \code{[any]}  Defaults to \code{NULL}
#' @param ...
#' @return \code{pill}: \code{[HTML]}
#' @examples

#'  pill(id = 'id', label = 'ds', type = 'none', data_icon = NULL,
#'  class = NULL)
#' @export
pill <- function(id, label, type = "none", data_icon = NULL, class = NULL, ...) {
    # Generate a shiny pill
    assert_character(id, unique = TRUE)
    assert_character(label, len = length(id), unique = TRUE)
    assert_any(type, check_character(len = 1), check_character(len = length(id)))
    assert_any(data_icon, check_character(len = 1, null.ok = T), check_character(len = length(id)))
    assert_any(class, check_character(len = 1, null.ok = T), check_character(len = length(id)))
    data = list(id = id, label = label, ...)
    class = class %or% ""
    class = glue("pill-item pill-{type} {class%sep%' '}")
    if (nnull(data_icon)) {
        div(class = "pill-data-icon flex-nowrap", icon_mdi("{icon}"))
    }
    data_init = lapply(as_data_attrs(data), function(x) {
        if (is.logical(x))
            x <- js_logical(x)
        if (is.numeric(x))
            x <- as.character(x)
        x[is.na(x)] = "NA"
        glue("'{x}'")
    })
    data_attributes = map2(names(data_init), data_init, function(x, y) paste0("`", x, "`", "=", y)) %>%
        reduce(function(x, y) {
            paste(x, y, sep = ",")
        })
    idn = paste0("id_", id)
    out <- exprs_eval(expr_glue(tags$div(id = "{idn}", {
        data_attributes
    }, class = "d-flex flex-row {class} align-items-center", div(class = "pill-area d-flex flex-fill", `data-id` = "{id}",
        tags$div("{label}", class = "pill-label flex-fill align-items-center rounded", `data-id` = "{id}"), div(class = "px-1",
            `data-id` = "{id}", icon_mdi("menu-down", class = "icon-right dropdown-handle float-right", `data-id` = "{id}"))))))
    out
    # Returns: \code{[HTML]}
}
