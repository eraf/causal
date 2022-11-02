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
  if(!is.data.frame(x)) stop("`data` must be a data.frame", call. = FALSE)
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
check_level <- function(x, param_name = NULL) {
  level <- length(unique(x))
  if(level != 2) {
    level_name <- paste(unique(x), " ", collapse = "")
    msg = paste0(param_name, " must be binary, instead it has ",
                 level, " levels ",
                 level_name, collapse = "")
    stop(msg, call. = FALSE)
  }
}
