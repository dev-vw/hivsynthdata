#' Given age_group, chooses age between the boundaries of the age group
#'
#' @param anchor_date YYYY-MM-DD
#' @param age_group a vector of age groups
#'
#' @import lubridate
#' @return list decimal_age, rounded_age, birthday
assign_age <- Vectorize(function(age_group, anchor_date) {

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
})

#' Calculates birthday
#'
#' @description
#' Given decimal age and date of refence (anchor_date), calculates birthday
#'
#' @param decimal_age dbl
#'
#' @return a Date object birthday
#' @export
#'
#' @examples
#' calc_birthday(13.4, "2024-01-01")
calc_birthday <- Vectorize(function(decimal_age, anchor_date) {
  # calculate birthday
  anchor_date <- as.Date(anchor_date)
  whole_years <- floor(decimal_age)
  fraction_years <- decimal_age - whole_years

  birthday <- anchor_date - lubridate::years(whole_years)
  birthday <- birthday - lubridate::days(round(365.2425 * fraction_years))

  return(birthday)
  # return(format(birthday, "%Y-%m-%d"))
})

#' Generates a vector of random phone numbers (in the Eswatini format)
#'
#' TODO Allow this function to have an input that specifies which country
#'      format to use.
#'
#' @param n_pns integer; number of phone numbers
gen_phonenumber <- function(n_pns) {
  set.seed(12345)

  pn <- c()

  while (length(unique(pn)) != n_pns) {
    pn <- replicate(n_pns,
                    paste0("76",
                           paste0(sample(0:9, 7, replace = TRUE), collapse = "")))
  }

  return(pn)
}
