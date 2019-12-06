#' Make submissions table more tidy
#'
#' Make the submissions table more tidy by moving
#' column names to a key column and spreading out
#' the varaibles into three sections, "section",
#' "variable", and "sub_variable".
#'
#' @export
#' @param data Data frame with the submissions.
make_tidier_table <- function(data) {
  data <- tidyr::gather(
    data,
    colnames(data)[-which(colnames(data) == "variables")],
    key = "submission",
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
    "submission",
    "section",
    "variable",
    "sub_variable",
    "response")
  ]
  data
}
