#' @importFrom rlang %||%
new_xrays <- function(x, dimensions, types,
                      section = NULL) {
  out <- structure(x,
                   class = c("xrays", "table"))
  attr(out, "types") <- types
  attr(out, "flexible") <- FALSE
  out
}

#' @export
xrays <- function(x, dimensions, ...) {
  UseMethod("xrays")
}

#' @export
xrays.table <- function(x, dimensions = NULL, types = NULL, ...) {
  ellipsis::check_dots_empty()

  attrs <- attributes(x)
  attributes(x) <- attrs[names(attrs) != "call"]
  new_xrays(x = x,
            dimensions = dimensions,
            types = types)
}

#' @export
xrays.data.frame <- function(x, dimensions = NULL, value = -1, ...) {
  ellipsis::check_dots_empty()

  nms <- names(x)
  value <- tidyselect::vars_pull(nms, value)
  nms <- vctrs::vec_slice(nms, nms != value)

  rhs <- stringr::str_c(nms, collapse = " + ")
  form <- as.formula(stringr::str_c(value, rhs,
                                    sep = " ~ "))
  if (is.null(dimensions)) {
    types <- as.list(vctrs::vec_slice(x[nms], 0))
  } else {
    types <- lapply(dimensions,
                    function(x) vctrs::vec_slice(x, 0))
  }

  xrays.table(xtabs(form, x),
              dimensions = dimensions,
              types = types)
}

#' @export
xrays.default <- function(x, dimensions, ...) {
  ellipsis::check_dots_empty()

  if (is.array(dimensions)) {
    dimensions <- dimnames(dimensions)
    dm <- dim(dimensions)
  } else {
    dm <- vapply(dimensions, vctrs::vec_size,
                 FUN.VALUE = integer(1),
                 USE.NAMES = FALSE)
  }

  out <- as.table(array(vctrs::vec_recycle(x, prod(dm)),
                        dim = dm,
                        dimnames = dimensions))

  types <- lapply(dimensions,
                  function(x) vctrs::vec_slice(x, 0))

  new_xrays(x = out,
            dimensions = dimensions,
            types = types)
}

#' @export
xrays.array <- function(x, dimensions, ...) {
  ellipsis::check_dots_empty()
}

#' @export
xrays.xrays <- function(x, dimensions, ...) {
  ellipsis::check_dots_empty()
}

#' @export
as.table.xrays <- function(x, ...) {
  attrs <- attributes(x)
  attributes(x) <- attrs[!names(attrs) %in% c("types", "flexible", "groups")]
  class(x) <- "table"
  x
}

#' @export
as.array.xrays <- function(x, ...) {
  out <- as.table.xrays(x)
  class(out) <- "array"
  out
}

#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble

#' @export
as_tibble.xrays <- function(x, ...) {
  out <- as_tibble(as.table.xrays(x),
                   n = "value")
  types <- attr(x, "types")
  for (i in names(types)) {
    to <- types[[i]]

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

#' @export
print.xrays <- function(x, ...) {
  cat(stringr::str_glue("<xrays<{typeof(x)}>{pillar::size_sum(x)}>"), "\n")
  print(as.table.xrays(x))

  cat("\nDimensions:\n")
  dimensions <- names(dimnames(x))
  types <- attr(x, "types")
  print(vctrs::data_frame(name = dimensions,
                          type = vapply(types[dimensions],
                                        pillar::type_sum,
                                        FUN.VALUE = character(1),
                                        USE.NAMES = FALSE),
                          size = dim(x)))
  cat("\n")
  attrs <- c(Flexible = attr(x, "flexible"))
  groups <- attr(x, "groups")
  if (!is.null(groups)) {
    nms <- stringr::str_c(names(groups),
                          collapse = ", ")
    groups <- stringr::str_glue("{nms} [{vctrs::vec_size(groups)}]")
    attrs <- c(attrs,
               Groups = groups)
  }
  nms <- stringr::str_c(names(attrs), ":")
  nms <- stringr::str_pad(nms, max(stringi::stri_width(nms)), "right")
  cat(stringr::str_glue("{nms} {attrs}"),
      sep = "\n")
  invisible(x)
}
