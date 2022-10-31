#' @importFrom dplyr .data
calc_rr <- function(treatment, y, n) {
  dplyr::tibble(treatment, y, n) %>%
    dplyr::group_by(treatment) %>%
    dplyr::summarise(
      risk = n[2] / sum(n)
    ) %>%
    dplyr::summarise(
      rr = .data$risk[2] / .data$risk[1]
    ) %>%
    dplyr::pull(.data$rr)
}

#' @importFrom dplyr .data
dcalc_rr <- function(data, treatment, y, group = NULL) {
  data %>%
    dplyr::select({{ treatment }}, {{ y }}, {{ group }}) %>%
    dplyr::group_by({{ group }}) %>%
    dplyr::count({{ treatment }}, {{ y }}) %>%
    dplyr::rename("x" = {{ treatment }}, "y" = {{ y }}) %>%
    dplyr::group_by({{ group }}) %>%
    dplyr::summarise(
      rr = calc_rr(.data$x, .data$y, .data$n)
    )
}


#' @importFrom dplyr .data
dcalc_or <- function(data, treatment, y, group = NULL) {
  data %>%
    dplyr::select({{ treatment }}, {{ y }}, {{ group }}) %>%
    dplyr::group_by({{ group }}) %>%
    dplyr::count({{ treatment }}, {{ y }}) %>%
    dplyr::rename("x" = {{ treatment }}, "y" = {{ y }}) %>%
    dplyr::summarise(
      or = calc_or(.data$x, .data$y, .data$n)
    )
}

#' @importFrom dplyr .data
calc_or <- function(treatment, y, n) {
  dplyr::tibble(treatment, y, n) %>%
    dplyr::summarise(
      or = (n[4] * n[1]) / (n[2] * n[3])
    ) %>%
    dplyr::pull(.data$or)
}

