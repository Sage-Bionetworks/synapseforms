#' Get high level sections of form
#'
#' Get the high level sections from the form for a given
#' submission. Only returns sections that are not all NA.
#' Requires data in the form returned from `make_tidier_table()`.
#'
#' @export
#' @param data Dataframe with at least three columns,
#'  `submission`, `section`, and `response`.
#' @param submission_name Name of the submission.
#' @return A vector of the high level sections from
#'   the form for which the submission is not all NA.
#'   If `submission_name` is not in the `submission`
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
get_main_sections <- function(data, submission_name) {
  if (!submission_name %in% data$submission) {
    return(NULL)
  }
  data <- data[which(data$submission == submission_name), ]
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

#' Get all submission names
#'
#' Get all submission names. Requires data in the form
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
#' get_submission_names(data)
get_submission_names <- function(data) {
  all_names <- unique(data$submission)
  all_names
}
