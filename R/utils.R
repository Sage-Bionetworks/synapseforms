#' Write the data to a csv file
#'
#' Write the data to a csv file.
#'
#' @param data The data.
#' @param output_dir The directory to write the file to.
output_submission_csv <- function(data, output_dir) {
  filename <- glue::glue("{output_dir}submissions.csv")
  utils::write.csv(data, filename, row.names = FALSE)
}

#' Log into Synapse
#'
#' Log into Synapse. Assumes credentials are stored.
#'
#' @export
#' @return Synapse login object from
log_into_synapse <- function() {
  synapse <- reticulate::import("synapseclient")
  syn <- synapse$login()
  syn
}

#' Check that a value is a positive integer
#'
#' Check that a value is a positive integer. If the value is a string, will
#' return `TRUE` if it can be coerced to a positive integer.
#'
#' @param data A single value to check whether it is a positive integer.
#' @return `TRUE`` if data is an integer or a string that is coercible to a
#' positive integer; else `FALSE`
is_positive_integer <- function(data) {
  numeric_data <- suppressWarnings(as.numeric(data))
  if (is.na(data) || is.null(data) || is.na(numeric_data) || data == Inf) {
    return(FALSE)
  }
  if ((inherits(data, "numeric") || inherits(data, "character") ||
       inherits(data, "integer")) &&
      numeric_data > 0 && numeric_data %% 1 == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
