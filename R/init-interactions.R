init_interactions <- function(patient_data) {
  # throw an error if patient ids are not unique in patient_data!

  # step 1: extract unique patient ids and assign an n_interactions row
  n_interactions_tbl <- patient_data |>
    select(patient_id) |>
    rowwise() |>
    mutate(n_interactions = gen_n_interactions())

  interactions_list <- lapply(1:nrow(n_interactions_tbl), function(i) {
    gen_interaction_dates_ran(n_interactions = n_interactions_tbl$n_interactions[i])
  })

  interactions_list <- setNames(interactions_list, n_interactions_tbl$patient_id)

  # interactions <- lapply(names(interactions_list),
  #        function(patient_id) {
  #          data.frame(
  #            patient_id = patient_id,
  #            interaction_dat = unlist(interactions_list[[patient_id]])
  #          )
  #        })

  interactions <- do.call(rbind,
                          lapply(names(interactions_list),
                                 function(patient_id) {
                                   data.frame(
                                     patient_id = patient_id,
                                     interaction_date = unlist(interactions_list[[patient_id]])
                                   )
                                 }))

  # interaction ids
  interactions$interaction_id <- create_unique_ids(8, nrow(interactions))

  # interaction type and source
  interactions <- cbind(interactions,
                        assign_interaction_type_source(nrow(interactions)))

  return(interactions)
}
