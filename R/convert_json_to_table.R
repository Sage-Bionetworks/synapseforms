#' Convert all json files in a directory to a single table.
#'
#' @param output_dir The directory with the JSON files.
#' @return A table with the column 'variables' that corresponds
#'   to all variable names across all JSON files, and each file's
#'   values are listed in a column of the table under the filename.
convert_all_forms_to_table <- function(output_dir) {
  all_file_names <- list.files(output_dir, pattern = ".json")

  sub_data <- NULL
  sub_tables <- purrr::map(
    all_file_names,
    function(file) {
      get_json_unlisted_table(output_dir, file)
    }
  )
  if (length(sub) > 0) {
    if (length(sub_tables) == 1) {
      sub_data <- sub_tables[[1]]
    } else {
      sub_data <- sub_tables[[1]]
      for (table in 2:length(sub_tables)) {
        sub_data <- dplyr::full_join(
          sub_data,
          sub_tables[[table]],
          by = "variables"
        )
      }
    }
  }
  sub_data
}

#' Open a JSON file and return as a tibble.
#'
#' @param output_dir The directory the JSON file is in.
#' @param filename The name of the file.
#' @return A tibble with the columns: variables, filename, where
#'   the variables are concatenated names from the JSON form,
#'   and the values for each variable are in the column filename.
get_json_unlisted_table <- function(output_dir, filename) {
  sub <- jsonlite::fromJSON(glue::glue("{output_dir}{filename}"))
  sub_table <- as.data.frame(unlist(sub), stringsAsFactors = FALSE)
  colnames(sub_table) <- filename
  sub_table <- tibble::rownames_to_column(sub_table, var = "variables")
}
