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

check_level <- function(x, param_name = NULL) {
  level <- length(unique(x))
  if(level != 2) {
    level_name <- paste(unique(x), " ", collapse = "")
    msg = paste0(param_name, " must be binary,
                 instead it has ", level, " levels ",
                 level_name, collapse = "")
    stop(msg, call. = FALSE)
  }
}
