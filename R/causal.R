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




