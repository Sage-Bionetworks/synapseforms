#' Make submissions table more tidy
#'
#' Make the submissions table more tidy. New table will have
#' columns "form_data_id", "section", "variable", "sub_variable",
#' "response". Submission ids are moved to the "form_data_id"
#' column from the original column names. Submission responses
#' are moved to the "response" column, and "variables" that
#' correspond to the responses are broken into the three
#' columns "section", "variable", "sub_variable". These three
#' columns fill from the left, leaving `NA` in the right-most
#' column(s) if there are not enough pieces to the variable or
#' merging the remaining variable pieces if there are more
#' than three pieces to the variable. See the example for
#' clarification.
#'
#' @export
#' @param data Data frame with the submissions in the form
#'   given by `download_all_and_get_table()`, which has a
#'   "variables" column and a column for each submission
#'   with the form_data_id as the column name.
#' @examples
#' data <- tibble::tribble(
#'   ~variables, ~sub1, ~sub2,
#'   "naming.first_name", "Mel", "Jimothy",
#'   "naming.last_name", "Jovin", "Smithery",
#'   "exercise.pushups.min", "10", "3",
#'   "exercise.pushups.max", "20", "12",
#'   "exercise.pullups.reps.min", "1", "1",
#'   "exercise.pullups.reps.max", "2", "3"
#' )
#' tidier_data <- make_tidier_table(data)
make_tidier_table <- function(data) {
  data <- tidyr::gather(
    data,
    colnames(data)[-which(colnames(data) == "variables")],
    key = "form_data_id",
    value = "response"
  )
  data <- tidyr::separate(
    data,
    variables,
    into = c("section", "variable", "sub_variable"),
    sep = "[.]",
    remove = TRUE,
    extra = "merge",
    fill = "right"
  )
  data <- data[, c(
    "form_data_id",
    "section",
    "variable",
    "sub_variable",
    "response"
  )]
  data
}
