#' Download all submissions, process into a single table, and output
#' the table to a csv file.
#'
#' @param state_filter The filter that is desired to gather submissions by.
#' @param group The groupID.
#' @param output_dir The directory to output the submissions and csv file to.
download_all_and_output_to_csv <- function(state_filter = "SUBMITTED_WAITING_FOR_REVIEW", # nolint
                                           group, output_dir) {
  download_all_submissions(state_filter, group, output_dir)
  data <- convert_all_forms_to_table(output_dir)
  output_submission_csv(data, output_dir)
}
