calc_risk <- function(x) {
  risk = x[2] / sum(x)
}


#' @importFrom dplyr .data
calc_rr <- function(treatment, y, n) {
  dplyr::tibble(treatment, y, n) %>%
    dplyr::group_by(treatment) %>%
    dplyr::summarise(
      risk = calc_risk(.data$n)
    ) %>%
    dplyr::summarise(
      rr = .data$risk[2] / .data$risk[1]
    ) %>%
    dplyr::pull(.data$rr)
}

#' @importFrom dplyr .data
dcalc_rr <- function(data, treatment, y, group = NULL) {
  check_data(data)
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
  check_data(data)
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
      or = (.data$n[4] * .data$n[1]) / (.data$n[2] * .data$n[3])
    ) %>%
    dplyr::pull(.data$or)
}

#' @importFrom dplyr .data
gen_pseudo_popn <- function(data, treatment, y, group = NULL) {
  data %>%
    dplyr::count({{ group }}, {{ treatment }}, {{ y }}) %>%
    dplyr::group_by({{ group }}, {{ treatment }}) %>%
    dplyr::mutate(treatment_size = sum(.data$n)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{ group }}) %>%
    dplyr::mutate(
      p = treatment_size / sum(n),
      weight = 1 / p,
      ns = weight * n
    ) %>%
    dplyr::ungroup()
}



# TODO: need to write more test for checking input data quality.

check_data <- function(x) {
  if(!is.data.frame(x)) stop("`data` must be a data.frame", call. = FALSE)
}
