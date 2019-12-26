#' Get high level sections of form
#'
#' Get the high level sections from the form for a given
#' submission. Only returns sections that are not all NA.
#' Requires data in the form returned from `make_tidier_table()`.
#'
#' @export
#' @param data Dataframe with at least three columns,
#'  `form_data_id`, `section`, and `response`.
#' @param id form_data_id of the submission.
#' @return A vector of the high level sections from
#'   the form for which the submission is not all NA.
#'   If `id` is not in the `form_data_id`
#'   column, then returns `NULL`.
#' @examples
#' data <- tibble::tribble(
#'   ~variables, ~sub1, ~sub2,
#'   "naming.first_name", "Mel", "Jimothy",
#'   "naming.last_name", "Jovin", "Smithery",
#'   "exercise.pushups.min", "10", NA,
#'   "exercise.pushups.max", "20", NA,
#'   "exercise.pullups.reps.min", "1", NA,
#'   "exercise.pullups.reps.max", "2", NA
#' )
#' data <- make_tidier_table(data)
#' get_main_sections(data, "sub1")
#' get_main_sections(data, "sub2")
get_main_sections <- function(data, id) {
  if (!id %in% data$form_data_id) {
    return(NULL)
  }
  data <- data[which(data$form_data_id == id), ]
  sections <- unique(data$section)
  section_indices <- purrr::map(
    sections,
    function(x) {
      which(data$section == x)
    }
  )
  all_na <- purrr::map(
    section_indices,
    function(x) {
      all(
        is.na(
          data$response[x]
        )
      )
    }
  )
  sections[!unlist(all_na)]
}

#' Get all submission ids
#'
#' Get all submission ids. Requires data in the form
#' returned from `make_tidier_table()`.
#'
#' @export
#' @inheritParams get_main_sections
#' @return A vector of the submission names.
#' @examples
#' data <- tibble::tribble(
#'   ~variables, ~sub1, ~sub2,
#'   "naming.first_name", "Mel", "Jimothy",
#'   "naming.last_name", "Jovin", "Smithery",
#'   "exercise.pushups.min", "10", NA,
#'   "exercise.pushups.max", "20", NA,
#'   "exercise.pullups.reps.min", "1", NA,
#'   "exercise.pullups.reps.max", "2", NA
#' )
#' data <- make_tidier_table(data)
#' get_ids(data)
get_submission_ids <- function(data) {
  all_ids <- unique(data$form_data_id)
  all_ids
}
