#' Generates a random number to represent gestational age in months
#'
#' @return a random integer between 1 and 9
gen_pregmonth <- function() {
  n_months <- sample(x = c(1:9), size = 1)

  return(n_months)
}
