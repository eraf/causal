# TODO: need to write more test for checking input data quality.

calc_risk <- function(x) {
  risk = x[2] / sum(x)
}


check_data <- function(x) {
  if(!is.data.frame(x)) stop("`data` must be a data.frame", call. = FALSE)
}


check_param_null <- function(x, param_name) {
  if(is.null(x)) {
    stop(paste0("Could not determine the ", param_name), call. = FALSE)
  }
}
