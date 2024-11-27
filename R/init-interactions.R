init_interactions <- function(patient_data) {

  # step 0 ------------------------------------------------------------------

  # throw an error if patient ids are not unique in patient_data!


  # step 1 ------------------------------------------------------------------

  # extract unique patient ids and assign an n_interactions row
  print("Adding variable: interaction_date")
  n_interactions_tbl <- patient_data |>
    select(patient_id) |>
    rowwise() |>
    mutate(n_interactions = gen_n_interactions())

  interactions_dates_list <- lapply(1:nrow(n_interactions_tbl), function(i) {
    gen_interaction_dates_ran(n_interactions = n_interactions_tbl$n_interactions[i])
  })

  interactions_dates_list <- setNames(interactions_dates_list, n_interactions_tbl$patient_id)


  # step 2 ------------------------------------------------------------------

  # creates a table of patient ids and interaction dates
  interactions <- do.call(rbind,
                          lapply(names(interactions_dates_list),
                                 function(patient_id) {
                                   data.frame(
                                     patient_id = patient_id,
                                     interaction_date = unlist(interactions_dates_list[[patient_id]])
                                   )
                                 }))

  # step 3 ------------------------------------------------------------------
  # add variables

  # adds interaction type ---
  print("Adding variable: interaction_type")
  interactions <- assign_interaction_type(interactions)

  # adds interaction source ---
  print("Adding variable: interaction_source")
  interactions <- assign_interaction_source(interactions)

  # adds interaction ids ---
  print("Adding variable: interaction_ids")
  interactions$interaction_id <- create_unique_ids(8, nrow(interactions))

  # adds interaction age ---
  # joins patient data with interactions
  print("Adding variable: interaction_age")
  patients_interactions <- dplyr::left_join(patient_data, interactions, by = "patient_id")

  patients_interactions$interaction_age <- calc_age(patients_interactions$interaction_date,
                                                    patients_interactions$dob)

  # filter out negative interaction ages
  patients_interactions <- patients_interactions |> filter(interaction_age > 0)

  # assigns agegroup_interaction ---
  patients_interactions <- assign_agegroup(patients_interactions)

  # assigns pregnancy status ---
  patients_interactions <- assign_pregstatus(patients_interactions)


  return(patients_interactions)
}
