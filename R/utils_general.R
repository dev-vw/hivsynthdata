#' Creates unique ids for synthetic dataset
#'
#' @param n_char the number of characters in the ID
#' @param n_ids the number of IDs needed
#'
#' @return a vector of IDs
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

#' Tests if a vector is unique
#'
#' @param vec a vector
#'
#' @return bool
is_unique <- function(vec) {
    return(!any(duplicated(vec)))
}
