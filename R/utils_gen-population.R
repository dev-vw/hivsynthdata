create_unique_ids <- function(n_char, n_ids) {
  set.seed(12345)

  unique_ids <- c()

  while (length(unique(unique_ids)) != n_ids) {
    unique_ids <- replicate(n_ids,
                            paste0(sample(c(LETTERS, 0:9), n_char, replace = TRUE),
                                   collapse = ""))
  }

  return(unique_ids)
}

#' Given age_group, chooses age between the boundaries of the age group
#' @param anchor_date YYYY-MM-DD
#'
#' @import lubridate
#' @return list decimal_age, rounded_age, birthday
calc_age <- function(age_group, anchor_date) {

  # calculate decimal age
  if (age_group == "65+") {
    decimal_age <- runif(1, 65, 105)
  } else if (age_group == "<01") {
    decimal_age <- runif(1, 0, 1)
  } else {
    bound_vec <- as.numeric(strsplit(age_group, "-")[[1]])
    decimal_age <- runif(1, bound_vec[1], bound_vec[2])
  }

  return(decimal_age)
}

calc_age <- Vectorize(calc_age)

calc_birthday <- function(decimal_age, anchor_date) {
  # calculate birthday
  anchor_date <- as.Date(anchor_date)
  whole_years <- floor(decimal_age)
  fraction_years <- decimal_age - whole_years

  birthday <- anchor_date - years(whole_years)
  birthday <- birthday - days(round(365.2425 * fraction_years))

  return(birthday)
}

calc_birthday <- Vectorize(calc_birthday)

gen_phonenumber <- function() {
  pn <- paste0("76", paste0(sample(0:9, 7, replace = TRUE), collapse = ""))
  return(pn)
}
