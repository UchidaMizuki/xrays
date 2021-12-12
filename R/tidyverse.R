xrays_tidyverse <- function(f) {
  function(.data, ...) {
    .data <- as_tibble.xrays(.data)
    .data <- f(.data, ...)
    xrays(.data)
  }
}

#' @importFrom dplyr filter
#' @export
dplyr::filter

#' @export
filter.xrays <- function(.data, ..., .preserve = FALSE) {
  xrays_tidyverse(filter)(.data, ...,
                          .preserve = .preserve)
}

#' @importFrom dplyr mutate
#' @export
dplyr::mutate

#' @export
mutate.xrays <- function(.data, ...) {
  xrays_tidyverse(mutate)(.data, ...)
}

#' @importFrom dplyr transmute
#' @export
dplyr::transmute

#' @export
transmute.xrays <- function(.data, ...) {
  xrays_tidyverse(transmute)(.data, ...)
}

#' @importFrom dplyr select
#' @export
dplyr::select

#' @export
select.xrays <- function(.data, ...) {
  xrays_tidyverse(select)(.data, ...)
}

#' @importFrom dplyr rename
#' @export
dplyr::rename

#' @export
rename.xrays <- function(.data, ...) {
  xrays_tidyverse(rename)(.data, ...)
}

#' @importFrom dplyr pull
#' @export
dplyr::pull

#' @export
pull.xrays <- function(.data, var = -1, name = NULL, ...) {
  pull(as_tibble.xrays(.data),
       var = var,
       name = name,
       ...)
}

#' @importFrom dplyr slice
#' @export
dplyr::slice

#' @export
slice.xrays <- function(.data, ..., .preserve = FALSE) {
  xrays_tidyverse(slice)(.data, ...,
                         .preserve = .preserve)
}

#' @importFrom tidyr replace_na
#' @export
tidyr::replace_na

#' @export
replace_na.xrays <- function(data, replace, ...) {
  xrays_tidyverse(replace_na)(data,
                              replace = replace,
                              ...)
}

#' @importFrom dplyr group_by
#' @export
dplyr::group_by

#' @export
group_by.xrays <- function(.data, ..., .add = FALSE, .drop = group_by_drop_default(.data)) {
  x <- group_by(as_tibble.xrays(.data), ...,
                .add = .add,
                .drop = .drop)
  groups <- dplyr::group_data(x)
  attr(.data, "groups") <- groups[names(groups) != ".rows"]
  .data
}

#' @importFrom dplyr summarise
#' @export
dplyr::summarise

#' @export
summarise.xrays <- function(.data, ..., .groups = NULL) {
  groups <- names(attr(.data, "groups"))
  x <- group_by(as_tibble.xrays(.data),
                dplyr::across(dplyr::all_of(groups)))
  x <- summarise(x, ...,
                 .groups = .groups)
  xrays(x)
}
