#' Convert all json file data to single table
#'
#' Reads all the files given (or in `directory`) and gathers
#' the data into a single table by variable, where the variable
#' is the unlisted json form variable.
#'
#' @param file_list A named or unnamed list of files. If `NULL`, will
#'   gather list of files from `output_dir`. If named, the names will
#'   be the column names for the submissions; otherwise the names
#'   will be generic rubbish.
#' @param directory The directory with the JSON files. If `NULL`,
#'   then `file_list` is used.
#' @return A table with the column 'variables' that corresponds
#'   to all variable names across all JSON files, and each file's
#'   values are listed in a column of the table. The column names
#'   are either a generic name or the name assigned to the file in
#'   file_list. Will return `NULL` if both `file_list` and `directory`
#'   are `NULL`,
convert_all_forms_to_table <- function(file_list = NULL, directory = NULL) {
  if (is.null(file_list) && !is.null(directory)) {
    all_files <- list.files(directory, pattern = ".json")
    file_list <- purrr::map(all_files, function(x) {
      fs::path(directory, x)
    })
  } else if (is.null(file_list) && is.null(directory)) {
    return(NULL)
  }

  sub_data <- NULL
  if (is.null(names(file_list))) {
    sub_tables <- purrr::map(
      file_list,
      function(file) {
        get_json_unlisted_table(filepath = file)
      }
    )
  } else {
    sub_tables <- purrr::map2(
      file_list,
      names(file_list),
      function(file, name) {
        get_json_unlisted_table(filepath = file, name = name)
      }
    )
  }
  if (length(sub_tables) > 0) {
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

#' Get data from JSON file
#'
#' Open a JSON file and return as a tibble with two columns,
#' the variables and the data. The data column name is `name` or
#' a generic name if not provided.
#'
#' @param filepath The path to the file, including name and extension.
#' @param name The desired column name for the submission data.
#'   If `NULL`, the column will have a generic name.
#' @return A tibble with the columns: variables, filename, where
#'   the variables are concatenated names from the JSON form,
#'   and the values for each variable are in the column filename.
get_json_unlisted_table <- function(filepath, name = NULL) {
  sub <- jsonlite::fromJSON(filepath)
  sub_table <- as.data.frame(unlist(sub), stringsAsFactors = FALSE)
  if (!is.null(name)) {
    names(sub_table) <- name
  }
  sub_table <- tibble::rownames_to_column(sub_table, var = "variables")
}

#' Get a table with main section as column
#'
#' Get a table with the main section as a separate column.
#' This assumes the data is the unlisted data with variables
#' that are separated by periods. It takes the first string
#' before the period and puts it in a new column, `section`,
#' and puts the rest of the variable string in a `variable`
#' column.
#'
#' @param data The dataframe or tibble with the `variables`
#'   column, where each variable is a string separated by
#'   periods. The first string before the period is assumed
#'   to indicate the main section.
#' @return A dataframe or tibble where the `variables`
#'   have been broken into two columns, `section` and
#'   `variable`.
main_section_column <- function(data) {
  new_table <- tidyr::separate(
    data, "variables",
    into = c("section", "variable"),
    sep = "[.]",
    remove = TRUE,
    extra = "merge"
  )
  new_table
}
