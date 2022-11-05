stdd_rr <- function(data, treatment, y, group = NULL) {
  total = nrow(data)
  data %>%
    dplyr::group_by({{ group }}) %>%
    dplyr::count({{ treatment }}, {{ y }}) %>%
    dplyr::mutate(pL = sum(.data$n) / total) %>%
    dplyr::group_by({{ group }}, {{ treatment }}) %>%
    dplyr::summarise(pr = unique(calc_risk(.data$n)) * pL) %>%
    dplyr::group_by({{ treatment }}) %>%
    dplyr::summarise(sums = sum(pr)) %>%
    dplyr::summarise(st_rr = sums[2] / sums[1])
}
