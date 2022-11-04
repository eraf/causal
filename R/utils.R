#' for calculating risk from given frequencies.
#'
#' @param x a vector of length 2 where second value corresponds
#' to frequency of "yes" level of outcome.
#'
#' @noRd
calc_risk <- function(x) {
  risk = x[2] / sum(x)
}

#' Check function for checking whether a data frame was provided.
#'
#' @param x a data frame.
#'
#' @noRd
check_data <- function(x) {
  if(is.null(x)) {
    stop("argument `data` is missing, with no default", call. = FALSE)
  }

  if(!is.data.frame(x)) {
    stop("`data` must be a data.frame", call. = FALSE)
  }
}

#' Check function for checking whether a data frame was provided.
#'
#' @param x a character value (reference level).
#' @param param_name a character value for naming the parameter for which we want to
#' check for NULL.
#'
#' @noRd
check_param_null <- function(x, param_name) {
  if(is.null(x)) {
    stop(paste0("Could not determine the ", param_name), call. = FALSE)
  }
}

#' Check function for checking whether a vector only has 2 unique values.
#'
#' @param x a vector.
#' @param param_name a character value for constructing the error message.
#'
#' @noRd
check_level <- function(x, param_name) {
  level <- length(unique(x))
  if(level != 2) {
    level_name <- paste(unique(x), " ", collapse = "\n")
    msg = paste0("\n", param_name, " must be binary,\ninstead it has ",
                 level, " levels \n",
                 level_name, collapse = "")
    stop(msg, call. = FALSE)
  }
}

#' Give an error if there are any NA values
#'
#' @param x a vector.
#'
#' @noRd
check_na <- function(x, param_name) {
  if(sum(is.na(x)) > 0) {
    stop(paste0("There are missing values in ", param_name), call. = FALSE)
  }
}

#' Give an error if column doesn't exist in the data set
#'
#' @param colname column name for which we need to check
#' @param data a data frame
#' @param dataname name of the data frame
#'
#' @noRd
check_col_exist <- function(colname, data) {
  if (!(colname %in% names(data))) {
    stop(
      paste0("Column `", colname, "` doesn't exist in the data"),
      call. = FALSE
    )
  }
}

