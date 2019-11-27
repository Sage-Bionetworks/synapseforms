#' Get high level sections of form
#'
#' Get the high level sections from the form for a given
#' submission. Only returns sections that are not all NA.
#'
#' @export
#' @param data Dataframe with at least two columns,
#'   `variables` and `submission_name`.
#' @param submission_name Name of the submission.
#' @return A vector of the high level sections from
#'   the form for which the submission is not all NA.
#'   If `submission_name` is not in `names(data)`, then
#'   returns `NULL`.
get_main_sections <- function(data, submission_name) {
  if (!submission_name %in% names(data)) {
    return(NULL)
  }
  spread_table <- main_section_column(data)
  sections <- unique(spread_table$main_section)
  section_indices <- purrr::map(
    sections,
    function(x) {
      which(spread_table$main_section == x)
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
  sections[!unlist(all_na)]
}

#' Get all submission names
#'
#' Get all submission names.
#'
#' @export
#' @param data Dataframe or tibble with at least two columns,
#'   `variables` and a submission with the name of the
#'   submission as the column name.
#' @return A vector of the submission names.
get_submission_names <- function(data) {
  all_names <- names(data)
  all_names <- all_names[all_names != "variables"]
  all_names
}
