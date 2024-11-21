# birthday generation
decimal_age <- 34.5678
anchor_date <- as.Date("2024-12-31")
whole_years <- floor(decimal_age)
fraction_years <- decimal_age - whole_years

birthday <- anchor_date - years(whole_years)
birthday <- birthday - days(round(365.2425 * fraction_years))

# interaction initialization test -----------------------------------------
patient_data <- init_population("Eswatini")

n_interactions_tbl <- patient_data |>
  select(patient_id) |>
  rowwise() |>
  mutate(n_interactions = gen_n_interactions())

interactions_list <- lapply(1:nrow(n_interactions_tbl), function(i) {
  gen_interaction_dates_ran(n_interactions = n_interactions_tbl$n_interactions[i])
})

interactions_list <- setNames(interactions_list, n_interactions_tbl$patient_id)

interactions <- lapply(names(interactions_list),
                       function(patient_id) {
                         data.frame(
                           patient_id = patient_id,
                           interaction_dat = unlist(interactions_list[[patient_id]])
                         )
                       })

interactions <- do.call(rbind,
                        lapply(names(interactions_list),
                               function(patient_id) {
                                 data.frame(
                                   patient_id = patient_id,
                                   interaction_dat = unlist(interactions_list[[patient_id]])
                                 )
                               }))


# interaction probability assignments -------------------------------------
test <- sample(interaction_type_probs$interaction_type,
               size = 1000,
               replace = TRUE,
               prob = interaction_type_probs$prob)

patient_data <- init_population("Eswatini")
interactions <- init_interactions(patient_data)


