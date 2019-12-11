#' Get high level sections of form
#'
#' Get the high level sections from the form for a given
#' submission. Only returns sections that are not all NA.
#'
#' @export
#' @param data Dataframe with at least two columns,
#'   `variables` and `submission_name` if untidy. If `is_tidier`
#'   is `TRUE`, then there should be columns `submission`, `section`,
#'   and `response`.
#' @param submission_name Name of the submission.
#' @param is_tidier True if `data` is in the form given by
#'   [synapseforms::make_tidier_table()].
#' @return A vector of the high level sections from
#'   the form for which the submission is not all NA.
#'   If `submission_name` is not in `names(data)` or `submission`
#'   column, then returns `NULL`.
get_main_sections <- function(data, submission_name, is_tidier = FALSE) {
  if (is_tidier) {
    sections <- get_main_sections_tidier(data, submission_name)
  } else {
    if (!submission_name %in% names(data)) {
      return(NULL)
    }
    spread_table <- main_section_column(data)
    sections <- unique(spread_table$section)
    section_indices <- purrr::map(
      sections,
      function(x) {
        which(spread_table$section == x)
      }
    )
    all_na <- purrr::map(
      section_indices,
      function(x) {
        all(
          is.na(
            spread_table[x, which(names(spread_table) == submission_name)]
          )
        )
      }
    )
    sections <- sections[!unlist(all_na)]
  }
  sections
}

#' Get main sections from tidier data
#'
#' Get the high level sections from the form for a given
#' submission. Only returns sections that are not all NA.
#'
#' @inheritParams get_main_sections
#' @keywords internal
get_main_sections_tidier <- function(data, submission_name) {
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
#' Get all submission names.
#'
#' @export
#' @inheritParams get_main_sections
#' @return A vector of the submission names.
get_submission_names <- function(data, is_tidier = FALSE) {
  if (is_tidier) {
    all_names <- unique(data$submission)
  } else {
    all_names <- names(data)
    all_names <- all_names[all_names != "variables"]
  }
  all_names
}
