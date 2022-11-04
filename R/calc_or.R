#' Calculates Odds Ratio
#'
#' \code{calc_or} calculates odds ratio for Binary Outcome (y) and Binary
#' treatment variable, for given a vector of treatment levels, a vector of
#' possible outcome y and the count for each combination of treatment and y.
#' and \code{calc_or} supports tidy-selection.
#'
#'
#' @param treatment A vector with two unique level or value.
#' @param y A vector with two unique level or value.
#' @param n count for each combination of treatment and y.
#' @param treatment_ref_lvl Reference Level of treatment variable.
#' @param y_ref_lvl Reference level of y variable.
#'
#' @return A numeric value of odds ratio
#' @export
#' @importFrom dplyr .data
calc_or <- function(treatment, y, n, treatment_ref_lvl = NULL,
                    y_ref_lvl = NULL) {
  trt_colname <- deparse(substitute(treatment))
  y_colname <- deparse(substitute(y))
  trt_colname_msg <- paste0("treatment variable `", trt_colname, "`")
  y_colname_msg <- paste0("outcome (y) variable `", y_colname, "`")
  check_na(treatment, trt_colname_msg)
  check_na(y, y_colname_msg)
  check_level(treatment, trt_colname_msg)
  check_level(y, y_colname_msg)
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



#' Calculates Odds Ratio from a given data frame
#'
#' \code{dcalc_or} calculates the odds ratio directly from a data frame and supports
#' tidy-selection.
#'
#'
#' @param data A data frame containing treatment and outcome variable (y)
#'
#' @param treatment A vector with two unique level or value.
#' @param y A vector with two unique level or value.
#' @param treatment_ref_lvl Reference Level of treatment variable.
#' @param y_ref_lvl Reference level of y variable.
#' @param group group by variable.
#'
#' @return A tibble containing the odds ratio
#' @export
#' @importFrom dplyr .data
dcalc_or <- function(data, treatment, y, treatment_ref_lvl = NULL,
                     y_ref_lvl = NULL, group = NULL) {

  check_data(data)
  trt_colname <- deparse(substitute(treatment))
  y_colname <- deparse(substitute(y))
  check_col_exist(trt_colname, data)
  check_col_exist(y_colname, data)
  trt <- dplyr::pull(data, {{ treatment }})
  outcome <- dplyr::pull(data, {{ y }})
  trt_colname_msg <- paste0("treatment variable `", trt_colname, "`")
  y_colname_msg <- paste0("outcome (y) variable `", y_colname, "`")
  check_na(trt, trt_colname_msg)
  check_na(outcome, y_colname_msg)
  if (!is.null(substitute(group))) {
    grp_colname <- deparse(substitute(group))
    grp_colname_msg <- paste0("group variable `", grp_colname, "`")
    check_col_exist(grp_colname, data)
    grp <- dplyr::pull(data, {{ group }})
    check_na(grp, grp_colname_msg)
  }
  check_level(trt, trt_colname_msg)
  check_level(outcome, y_colname_msg)
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
