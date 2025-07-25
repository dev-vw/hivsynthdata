#' Common first names given country
#'
#' This dataset contains common first names generated by ChatGPT
#' for selected countries.
#'
#' @format A data frame with 1380 rows and 3 variables:
#' \describe{
#'   \item{Country}{Country}
#'   \item{First_Name}{First name}
#'   \item{Sex}{Sex, Male or Female}
#' }
"first_names"

#' Common last names given country
#'
#' This dataset contains common last names generated by ChatGPT
#' for selected countries.
#'
#' @format A data frame with 449 rows and 2 variables:
#' \describe{
#'   \item{Country}{Country}
#'   \item{Last_Name}{Last name}
#' }
"last_names"

#' Patient initialization table
#'
#' There are a series of rows that describe a unique and mutually exclusive
#' category of individuals based on age group, sex, and pregnancy status. This
#' data is transformed from the TX_CURR indicator from the 2023 PEPFAR MSDs.
#'
#' @format A data frame with 2010 rows and 5 variables:
#' \describe{
#'   \item{operatingunit}{Country name}
#'   \item{sex}{Sex, Male or Female}
#'   \item{agegroup}{Age Group, "<15", "15+", "<01", "65+", and five-year increments from 1-65}
#'   \item{preg_satus}{Prenancy Status, "pregnant" or "not pregnant"}
#'   \item{prob}{Probability}
#' }
"age_sex_preg"

#' Interaction type probabilities
#'
#' Probabilities associated with each interaction type
#'
#' @format A data frame with 4 rows and 2 variables:
#' \describe{
#'   \item{interaction_type}{Interaction type}
#'   \item{prob}{Probabilities}
#' }
"interaction_type_probs"

#' Laboratory names
#'
#' A data frame of laboratory names
#'
#' @format A data frame with 1080 rows and 2 variables:
#' \describe{
#'   \item{country}{Country Name}
#'   \item{lab_name}{Laboratory Name}
#' }
"lab_names"
