#' Initializes the patient data
#'
#' @param country String
#'
#' @importFrom dplyr filter select rowwise mutate ungroup
#' @return a df
#'
#' TODO Change the param, country, to using ISO3 code for reference
#' TODO Negative probs exist in age_sex_preg; contact Florence to change

#' @export
#' @examples
#' init_population("Eswatini")
init_population <- function(country) {

  print("Initializing unique patients...")
  first_names <- first_names |> dplyr::filter(Country == country)
  last_names <- last_names |> dplyr::filter(Country == country)

  patient_lookup <- age_sex_preg[age_sex_preg$operatingunit == country, ]

  # Create an index for sampling based on probabilities
  sample_indices <- sample(1:nrow(patient_lookup),
                           size = 2500,
                           replace = TRUE,
                           prob = patient_lookup$prob)

  # Create the patient population
  patient_data <- patient_lookup[sample_indices, ] |> dplyr::select(-prob)

  # Reset row names
  rownames(patient_data) <- NULL

  # generate attributes -----------------------------------------------------

  print("Adding variable: patient_id")
  # ids
  patient_data$patient_id <- create_unique_ids(7, 2500)

  print("Adding variable: age")
  # age (dbl)
  patient_data$age_dec <- assign_age(patient_data$agegroup)

  # age (int)
  patient_data$age <- floor(patient_data$age_dec)

  # dob
  print("Adding variable: dob")
  # patient_data <- patient_data |>
  #   mutate(dob = calc_birthday(patient_data$age, "2024-12-31"))
  patient_data$dob <- calc_birthday(patient_data$age_dec, "2024-12-31")
  patient_data$dob <- as.Date(patient_data$dob, "1970-01-01")

  print("Adding variables: first_name, last_name")
  # first and last names
  patient_data <- patient_data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      first_name = sample(first_names$First_Name[first_names$Sex == sex], 1),
      last_name = sample(last_names$Last_Name, 1)
    ) |>
    dplyr::ungroup()

  print("Adding variable: phone_number")
  # phone numbers
  patient_data$phone_number <- gen_phonenumber(2500)

  # cleans up initial patient data
  # NOTE: This is NOT the final patient data: age_dec will need to be removed
  patient_data <- patient_data |> dplyr::select(operatingunit, patient_id,
                                                first_name, last_name, sex,
                                                agegroup, age_dec, age, dob,
                                                preg_status, phone_number)

  # write_csv(patient_data, paste0("outputs/patient_data_", format(Sys.Date(), "%m%Y"), ".csv"))
  return(patient_data)
}
