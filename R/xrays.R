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

  attrs <- attributes(data)
  attributes(data) <- attrs[names(attrs) != "call"]
  new_xrays(data, value, col_types)
}

#' @export
xrays.data.frame <- function(data, axes = NULL, value = -1, ...) {
  ellipsis::check_dots_empty()

  nms <- names(data)
  value <- tidyselect::vars_pull(nms, value)
  nms <- vctrs::vec_slice(nms, nms != value)

  rhs <- stringr::str_c(nms, collapse = " + ")
  form <- as.formula(stringr::str_c(value, rhs,
                                    sep = " ~ "))

  col_types <- as.list(vctrs::vec_slice(data[nms], 0))

  xrays.table(xtabs(form, data),
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
as.table.xrays <- function(x, ...) {
  ellipsis::check_dots_empty()

  attrs <- attributes(x)
  attributes(x) <- attrs[!names(attrs) %in% c("value", "col_types")]
  class(x) <- "table"
  x
}

#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble

#' @export
as_tibble.xrays <- function(x, ...) {
  out <- as_tibble(as.table(x),
                   n = attr(x, "value"))
  col_types <- attr(x, "col_types")
  for (i in names(col_types)) {
    to <- col_types[[i]]

    if (is.factor(to)) {
      out[[i]] <- factor(out[[i]],
                         levels = levels(to))
    } else {
      out[[i]] <- as(out[[i]], class(to)[[1]])
    }
  }
  out
}

#' @export
as.data.frame.xrays <- function(x, row.names = NULL, stringsAsFactors = FALSE, ...) {
  out <- as_tibble.xrays(x)
  as.data.frame(out)
}
