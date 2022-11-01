#' @importFrom dplyr .data
calc_or <- function(treatment, y, n, treatment_ref_lvl = NULL,
                    y_ref_lvl = NULL) {
  check_param_null(treatment_ref_lvl, "treatment reference level")
  check_param_null(y_ref_lvl, "y reference level")
  dplyr::tibble(treatment, y, n) %>%
    dplyr::mutate(
      treatment = dplyr::if_else(treatment == treatment_ref_lvl, 0, 1),
      y = dplyr::if_else(y == y_ref_lvl, 0, 1)
    ) %>%
    dplyr::summarise(
      or = (.data$n[4] * .data$n[1]) / (.data$n[2] * .data$n[3])
    ) %>%
    dplyr::pull(.data$or)
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
gen_pseudo_popn <- function(data, treatment, y, group = NULL) {
  data %>%
    dplyr::count({{ group }}, {{ treatment }}, {{ y }}) %>%
    dplyr::group_by({{ group }}, {{ treatment }}) %>%
    dplyr::mutate(treatment_size = sum(.data$n)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{ group }}) %>%
    dplyr::mutate(
      p = .data$treatment_size / sum(.data$n),
      weight = 1 / .data$p,
      ns = .data$weight * .data$n
    ) %>%
    dplyr::ungroup()
}




