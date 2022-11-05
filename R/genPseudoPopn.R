#' Generate Pseudo Population
#'
#' \code{gen_pseudo_popn} generates Pseudo Population using the Inverse Probability
#' method given a data frame with treatment and outcome (y) column. And it also possible
#' to create pseudo population grouped by another variable which exists in the given
#' frame.
#'
#' @param data a data frame.
#'
#' @param treatment treatment column of the data frame.
#' @param y outcome column of the data frame.
#' @param treatment_ref_lvl Reference Level of treatment column.
#' @param y_ref_lvl Reference level of outcome (y) column.
#' @param group group column of the data frame by which pseudo population
#' need to be generated.
#'
#' @return A tibble with a `ns` column which contains the pseudo population count,
#'
#' @export
#' @importFrom dplyr .data
gen_pseudo_popn <- function(data, treatment, y, treatment_ref_lvl = NULL,
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


