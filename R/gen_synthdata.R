#' Generate the synthetic dataset
#'
#' @param country string
#'
#' @importFrom dplyr left_join
#'
#' @returns a synthetic dataset
#' @export
gen_synthdata <- function(country) {
  patient_data <- init_population(country)
  interactions <- init_interactions(patient_data)
  lab_data <- init_lab(interactions, country)

  # initial join of patient data and interactions
  init_join <- dplyr::left_join(patient_data, interactions, by = "patient_id")

  # join of lab and interactions
  lab_join <- dplyr::left_join(interactions,
                               lab_data,
                               by = c("patient_id", "interaction_date", "interaction_id"))

  return(lab_join)
}
