#' Write the data to a csv file.
#'
#' @param data The data.
#' @param output_dir The directory to write the file to.
output_submission_csv <- function(data, output_dir) {
  filename <- glue::glue("{output_dir}submissions.csv")
  utils::write.csv(data, filename, row.names = FALSE)
}
