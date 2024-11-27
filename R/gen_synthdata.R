gen_synthdata <- function(country) {
  patient_data <- init_population(country)
  interactions <- init_interactions(patient_data)

  # initial join of patient data and interactions
  init_join <- dplyr::left_join(patient_data, interactions, by = "patient_id")

  # add calculated ages to each interaction

  # add dynamic pregnancy status
}
