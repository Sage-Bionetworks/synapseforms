#' Retrieve forms from a FormGroup and append to a Synapse Table
#'
#' Appends newly submitted FormData to a Synapse Table. The contents of
#' the FormData object must be a valid JSON blob.
#'
#' @param syn A Synapse object (see `log_into_synapse`).
#' @param form_group_id The `groupId` of the FormGroup
#' @param output The Synapse ID of either the table to append the contents of
#' the retrieved FormData to (if such a table has already been created) or the
#' Synapse ID of the project to instantiate such a table and append the
#' retrieved FormData to.
#' @param submission_state Only retrieve FormData with this specific
#' state. Set to `NULL` to ignore state when retrieving FormData.
#' @export
export_forms_to_synapse <- function(syn, form_group_id, output = NULL,
                             submission_state = "ACCEPTED") {
  return()
}

get_form <- function(syn, form, as_list = TRUE) {
  fpath <- synapseforms:::get_form_temp(
    syn, form$dataFileHandleId, form$formDataId)
  if (as_list) {
    return(jsonlite::read_json(fpath))
  }
  return(fpath)
}

#' Email a Synapse user/group in response to a form event
#'
#' @param syn A Synapse object (see `log_into_synapse`).
#' @param recipients A vector of recipients to send an email alert to.
#' @param form_group_id The form group to check for the form event.
#' @param form_event The form event to check for. Possible values are
#' "create" or "submit". "create" will consider when the form was added
#' to the form group whereas "submit" will consider when the form was submitted.
#' Has no effect if time_duration is Inf.
#' @param time_duration The time period (as an integer in seconds or a string
#' representation parseable by lubridate::duration) to consider a
#' form event "recent" and hence requiring an email alert. This serves
#' two purposes. First, if there is no file view specified (we are not exporting
#' form data to Synapse), this allows us to determine which forms events are recent
#' and which are not. Secondly, if there is a file view specified but exporting
#' the form data fails for some reason, this prevents the caller from repeatedly
#' emailing a user/group past a certain time window (when used with a cron job).
#' @param file_view_reference Synapse file view to query to check if form has
#' already been exported. If the form has already been exported there is no alert.
#' @param as_reviewer Request forms using the /form/data/list/reviewer endpoint.
#' If FALSE, request forms using the /form/data/list endpoint. See the Synapse
#' REST docs for additional information.
#' @export
email_alert <- function(syn, recipients, form_group_id, form_event = "submit",
                        time_duration = 60, file_view_reference = NULL,
                        as_reviewer = TRUE, export_to_synapse = TRUE) {
  validate_form_event_params(time_duration = time_duration,
                             form_event = form_event)
  exportable_forms <- get_exportable_forms(syn = syn,
                                           form_group_id = form_group_id,
                                           form_event = form_event,
                                           time_duration = time_duration,
                                           file_view_reference = file_view_reference,
                                           as_reviewer = as_reviewer)
  if (export_to_synapse) {
    exported_forms <- export_forms_to_synapse(syn = syn)
  }

}

#' Check for recent form events and return those forms
#'
#' Checks for either recent "create" events (when a form has been added to a
#' form group) or "submit" events (when a form has been submitted) and returns
#' forms which had the given event type occurr within a certain time_duration of
#' the current time. Due to how the Forms API behaves, we can only filter on the
#' created on time for forms that have already been submitted if the caller is
#' not the owner (creator) of a form.
#'
#' @param syn A Synapse object (see `log_into_synapse`).
#' @param form_group_id The form group to check for the form event.
#' @param time_duration The time period (as an integer in seconds or a string
#' representation parseable by lubridate::duration) to consider a
#' form event "recent".
#' @param form_event The form event to check for. Possible values are
#' "create" or "submit". "create" will consider when the form was added
#' to the form group whereas "submit" will consider when the form was submitted.
#' Has no effect if time_duration is Inf.
#' @param as_reviewer Request forms using the /form/data/list/reviewer endpoint.
#' If False, request forms using the /form/data/list endpoint. See the Synapse
#' REST docs for additional information.
#' @return A dataframe where each record is a recent form.
#' @export
get_recent_forms <- function(syn, form_group_id, time_duration,
                             form_event = "create", as_reviewer = TRUE) {
  current_time <- lubridate::now(tzone = "UTC")
  validate_form_event_params(time_duration = time_duration,
                             form_event = form_event)
  time_duration <- lubridate::duration(time_duration)
  if (form_event == "create" && as_reviewer) {
    warning("Fetching forms as reviewer. Only submitted forms will be considered.")
  }
  if (form_event == "submit" || as_reviewer) {
    state_filter <- "SUBMITTED_WAITING_FOR_REVIEW"
  } else if (form_event == "create") {
    state_filter <- c("WAITING_FOR_SUBMISSION", "SUBMITTED_WAITING_FOR_REVIEW")
  }
  if (form_event == "submit") {
    timestamp_col <- rlang::sym("submissionStatus_submittedOn")
  } else if (form_event == "create") {
    timestamp_col <- rlang::sym("createdOn")
  }
  all_forms <- get_submissions_metadata(syn,
                                        group = form_group_id,
                                        state_filter = state_filter,
                                        all_users = as_reviewer)
  if (!(rlang::as_string(timestamp_col) %in% names(all_forms))) {
    # this should only happen if no forms have been submitted and form_event
    # is "submit"
    return(all_forms[0,]) # empty dataframe but retain column names
  }
  recent_forms <- all_forms %>%
    dplyr::mutate(time_since_event = current_time - lubridate::as_datetime(!!timestamp_col)) %>%
      dplyr::filter(time_since_event <= time_duration)
  return(recent_forms)
}


#' Get forms that don't yet exist in a file view on Synapse
#'
#' @param syn A Synapse object (see `log_into_synapse`).
#' @param form_group_id The form group to check for the form event.
#' @param file_view_reference Synapse file view to query to check if form has
#' already been exported.
#' @param time_duration The time period (as an integer in seconds or a string
#' representation parseable by lubridate::duration) to consider a
#' form event "recent". By default it is Inf -- all forms are fetched.
#' @param form_event The form event to check for. Possible values are
#' "create" or "submit". "create" will consider when the form was added
#' to the form group whereas "submit" will consider when the form was submitted.
#' Has no effect if time_duration is Inf.
#' @param as_reviewer Request forms using the /form/data/list/reviewer endpoint.
#' If False, request forms using the /form/data/list endpoint. See the Synapse
#' REST docs for additional information.
#' @return A dataframe where each record is an exportable form.
#' @export
get_exportable_forms <- function(syn, form_group_id, file_view_reference,
                                 time_duration = Inf, form_event = "submit",
                                 as_reviewer = TRUE) {
  forms <- get_recent_forms(syn = syn,
                            form_group_id = form_group_id,
                            time_duration = time_duration,
                            form_event = form_event,
                            as_reviewer = as_reviewer)
  if (is.null(file_view_reference)) {
    return(forms)
  }
  form_file_view <- synTableQuery(
    paste("SELECT * FROM", file_view_reference))$asDataFrame()
  exportable_forms <- dplyr::anti_join(forms, form_file_view, by = "formDataId")
  return(exportable_forms)
}


#' Validate common parameters when filtering forms
#'
#' @param time_duration The time period (as an integer in seconds or a string
#' representation parseable by lubridate::duration) to consider a
#' form event "recent".
#' @param form_event The recent event to check for. Possible values are
#' "create" or "submit". "create" will trigger upon a new form being added
#' to the form group whereas "submit" will trigger upon an existing form being
#' submitted.
validate_form_event_params <- function(time_duration, form_event) {
  # Check time_duration for a valid value
  if (is.na(lubridate::duration(time_duration))) {
    stop(paste(time_duration, "of type", typeof(time_duration),
               "is not an allowed `time_duration` value."))
  }
  # Check form_event for allowed values
  if (form_event == "submit") {
    timestamp_col <- "submissionStatus_submittedOn"
  } else if (form_event == "create") {
    timestamp_col <- "create"
  } else {
    stop("`form_event` must be either \"submit\" or \"create\"")
  }
}
