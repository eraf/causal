cal_rr <- function(trt, y, n) {
  dplyr::tibble(trt, y, n) %>%
    dplyr::group_by(trt) %>%
    dplyr::summarise(
      risk= n[2] / sum(n)
    ) %>%
    dplyr::summarise(
      RR = risk[2] / risk[1]
    ) %>%
    dplyr::pull(RR)
}


dcal_rr <- function(data, trt, y, group = NULL) {
  data %>%
    dplyr::select({{trt}}, {{y}}, {{group}}) %>%
    dplyr::group_by({{group}}) %>%
    dplyr::count({{trt}}, {{y}}) %>%
    dplyr::rename("x" = {{trt}}, "y" = {{y}}) %>%
    dplyr::group_by({{group}}) %>%
    dplyr::summarise(
      rr = cal_rr_default(x, y, n)
    )
}


cal_or <- function(data, trt, y, group = NULL) {
  data %>%
    dplyr::select({{trt}}, {{y}}, {{group}}) %>%
    dplyr::group_by({{group}}) %>%
    dplyr::count({{trt}}, {{y}}) %>%
    dplyr::rename("x" = {{trt}}, "y" = {{y}}) %>%
    dplyr::summarise(
      or = cal_or_default(x, y, n)
    )
}

cal_or_default <- function(trt, y, n) {
  dplyr::tibble(trt, y, n) %>%
    dplyr::summarise(
      or = (n[4] * n[1]) / (n[2] * n[3])
    ) %>%
    dplyr::pull(or)
}

