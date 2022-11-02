#' Calculates Risk Ratio.
#'
#' \code{calc_rr} calculates risk-ratio for Binary Outcome (y) and Binary
#' treatment variable, for given a vector of treatment levels, a vector of
#' possible outcome y and the count for each combination of treatment and y.
#' \code{calc_rr} and supports tidy-selection.
#'
#'
#' @param treatment A vector with two unique level or value.
#'
#' @param y A vector with two unique level or value.
#' @param n count for each combination of treatment and y.
#' @param treatment_ref_lvl Reference Level of treatment variable.
#' @param y_ref_lvl Reference level of y variable.
#'
#' @return A numeric value of risk ratio
#' @export
#' @importFrom dplyr .data
calc_or <- function(treatment, y, n, treatment_ref_lvl = NULL,
                    y_ref_lvl = NULL) {
  check_level(treatment, "treatment")
  check_level(y, "outcome(y)")
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



#' Calculates Risk Ratio from the Data.
#'
#' \code{dcalc_rr} calculates the risk ratio directly from data and supports
#' tidy-selection.
#'
#'
#' @param data A data frame containing treatment and outcome variable (y)
#' @param treatment A vector with two unique level or value.
#' @param y A vector with two unique level or value.
#' @param treatment_ref_lvl Reference Level of treatment variable.
#' @param y_ref_lvl Reference level of y variable.
#' @param group group by variable.
#'
#' @return A tibble containing the risk ratio
#' @export
#' @importFrom dplyr .data
dcalc_or <- function(data, treatment, y, treatment_ref_lvl = NULL,
                     y_ref_lvl = NULL, group = NULL) {

  check_data(data)
  trt <- dplyr::pull(data, {{ treatment }})
  outcome <- dplyr::pull(data, {{ y }})
  check_level(trt, "treatment")
  check_level(outcome, "outcome(y)")
  check_param_null(treatment_ref_lvl, "treatment reference level")
  check_param_null(y_ref_lvl, "y reference level")

  data %>%
    dplyr::select({{ treatment }}, {{ y }}, {{ group }}) %>%
    dplyr::group_by({{ group }}) %>%
    dplyr::count({{ treatment }}, {{ y }}) %>%
    dplyr::rename("x" = {{ treatment }}, "y" = {{ y }}) %>%
    dplyr::summarise(
      or = calc_or(.data$x, .data$y, .data$n,
                   treatment_ref_lvl = treatment_ref_lvl,
                   y_ref_lvl = y_ref_lvl)
    )
}
