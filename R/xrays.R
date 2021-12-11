new_xrays <- function(data, value, col_types) {
  out <- structure(data,
                   class = c("xrays", "table"))
  attr(out, "value") <- value
  attr(out, "col_types") <- col_types
  out
}

#' @export
xrays <- function(data, axes, ...) {
  UseMethod("xrays")
}

#' @export
xrays.table <- function(data, axes = NULL, value = "value", col_types = NULL, ...) {
  ellipsis::check_dots_empty()

  new_xrays(data, value, col_types)
}

#' @export
xrays.data.frame <- function(data, axes = NULL, value = -1, ...) {
  ellipsis::check_dots_empty()

  nms <- names(data)
  value <- tidyselect::vars_pull(nms, value)
  rhs <- stringr::str_c(nms[nms != value], collapse = ' + ')
  form <- as.formula(stringr::str_glue("{value} ~ {rhs}"))

  col_types <- as.list(vctrs::vec_slice(data, 0))

  xrays(xtabs(form, data),
        axes = axes,
        value = value,
        col_types = col_types)
}


#' @export
xrays.default <- function(data, axes, value = "value", ...) {
  ellipsis::check_dots_empty()


}

#' @export
xrays.array <- function(data, axes, value = "value", ...) {
  ellipsis::check_dots_empty()
}

#' @export
xrays.xrays <- function(data, axes, ...) {
  ellipsis::check_dots_empty()
}

#' @export
as.data.frame.xrays <- function(x, row.names = NULL, stringsAsFactors = FALSE, ...) {
  out <- as_tibble.xrays(x)
  as.data.frame(out)
}

#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble

#' @export
as_tibble.xrays <- function(x, ...) {
  out <- NextMethod(n = attr(x, "value"),
                    ...)
  col_types <- attr(x, "col_types")
  for (i in names(col_types)) {
    to <- col_types[[i]]

    if (is.numeric(to)) {
      out[[i]] <- as.numeric(out[[i]])
    } else {
      out[[i]] <- vctrs::vec_cast(out[[i]], to)
    }
  }
  out
}
