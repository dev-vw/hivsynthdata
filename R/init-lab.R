#' Initialize the laboratory dataset
#'
#' @param interactions the full interaction data frame
#' @param country string
#'
#' @importFrom dplyr filter select mutate rowwise
#'
#' @returns a dataset of laboratory interaction details
#' @export
init_lab <- function(interactions, country) {

  # find all interactions that are labs and select, patient_id
  # interaction_id, interaction_date
  lab_df_init <- interactions |>
    dplyr::filter(interaction_type == "lab") |>
    dplyr::select(patient_id, interaction_id, interaction_date)

  lab_names_df <- lab_names |> dplyr::filter(country == country)

  # assign site_name
  lab_df_init$site_name <- sample(lab_names_df$lab_name,
                                  size = nrow(lab_df_init),
                                  replace = TRUE)

  # assign test_type
  lab_df_init$test_type <- sample(c("vl", "cd4"),
                                  size = nrow(lab_df_init),
                                  replace = TRUE,
                                  prob = c(0.7, 0.3))

  # assign test_date
  lab_df_init$test_date <- as.Date(lab_df_init$interaction_date) + sample(-14:14,
                                                                          size = nrow(lab_df_init),
                                                                          replace = TRUE)


  # assign test_result
  # if test_type == "vl", then assign from three separate sampling schemes
  # if test_type == "cd4", then assign from a single sample scheme
  lab_df_init <- lab_df_init |>
    dplyr::rowwise() |>
    dplyr::mutate(test_result = ifelse(test_type == "vl",
                                sample(c(sample(100:999, size = 1, replace = TRUE),
                                         sample(1000:1000000, size = 1, replace = TRUE),
                                         NA),
                                       size = 1,
                                       replace = TRUE,
                                       prob = c(0.85, 0.14, 0.01)),
                                sample(500:1600, size = 1, replace = TRUE)))

  return(lab_df_init)
}
