#' Retrieve forms from a FormGroup and export as YAML to Synapse.
#'
#' @param syn A Synapse object (see `log_into_synapse`).
#' @param form_group_id The `groupId` of the FormGroup.
#' @param output The Synapse ID of the parent folder to export the forms to.
#' @param submission_state Only retrieve FormData with this specific
#' state. Set to `NULL` to ignore state when retrieving FormData.
#' @param form_data_id A vector of specific forms to export.
#' @param ... additional arguments to pass to `get_exportable_forms`.
#' @export
export_forms_to_synapse <- function(syn, form_group_id, output,
                                    file_view_reference = NULL,
                                    submission_state = "ACCEPTED",
                                    form_data_id = NULL, ...) {
  exportable_forms <- get_exportable_forms(syn = syn,
                                           form_group_id = form_group_id,
                                           file_view_reference = file_view_reference,
                                           submission_state = submission_state,
                                           ...)
  if (!is.null(form_data_id)) {
    exportable_forms <- exportable_forms %>%
      filter(formDataId %in% form_data_id)
  }
  form_contents <- get_forms(
    syn = syn,
    data_file_handle_id = exportable_forms$dataFileHandleId,
    form_data_id = exportable_forms$formDataId,
    as_list=TRUE)
  synapseclient <- reticulate::import("synapseclient") # needed to create File objects
  form_file_handles <- purrr::map2(
    form_contents, exportable_forms$formDataId, function(form, fdi) {
    temp_f <- tempfile(pattern = glue::glue("form_{fdi}"), fileext = ".yaml")
    yaml::write_yaml(form, temp_f)
    syn_f <- synapseclient$File(temp_f, parent=output)
    syn$store(path = temp_f, parent=file_view_reference)
  })
  # TODO download all exportable forms, export to Synapse as YAML, and annotate with properties
  return()
}

#' Download a form's contents
get_forms <- function(syn, form_data_id, data_file_handle_id, as_list=FALSE) {
  fpath <- synapseforms:::get_form_temp(
    syn, file_handle_id = data_file_handle_id, form_data_id = form_data_id)
  if (as_list) {
    forms_as_lists <- purrr::map(fpath, jsonlite::read_json)
    return(forms_as_lists)
  }
  return(fpath)
}

#' Email a Synapse user/group in response to a form event
#'
#' @param syn A Synapse object (see `log_into_synapse`).
#' @param recipients A vector of recipients (Synapse user IDs) to send
#' an email alert to.
#' @param form_group_id The form group to check for the form event.
#' @param form_event The form event to check for. Possible values are
#' "create" or "submit". "create" will consider when the form was added
#' to the form group whereas "submit" will consider when the form was submitted.
#' @param time_duration The time period (as an integer in seconds or a string
#' representation parseable by lubridate::duration) to consider a
#' form event "recent" and hence requiring an email alert. This serves
#' two purposes. First, if there is no file view specified (we are not exporting
#' form data to Synapse), this allows us to determine which forms events are recent
#' and which are not. Secondly, if there is a file view specified but exporting
#' the form data fails for some reason, this prevents the caller from repeatedly
#' emailing a user/group past a certain time window (when used with a cron job).
#' Set to Inf to fetch all qualifying events.
#' @param file_view_reference Synapse file view to query to check if form has
#' already been exported. If the form has already been exported there is
#' no email alert sent out.
#' @param as_reviewer Request forms using the /form/data/list/reviewer endpoint.
#' If FALSE, request forms using the /form/data/list endpoint. See the Synapse
#' REST docs for additional information.
#' @export
email_alert <- function(syn, recipients, form_group_id, form_event = "submit",
                        time_duration = 60, file_view_reference = NULL,
                        as_reviewer = TRUE, submission_state = NULL) {
  validate_form_event_params(time_duration = time_duration,
                             form_event = form_event)
  exportable_forms <- get_exportable_forms(syn = syn,
                                           form_group_id = form_group_id,
                                           form_event = form_event,
                                           time_duration = time_duration,
                                           file_view_reference = file_view_reference,
                                           as_reviewer = as_reviewer,
                                           submission_state = submission_state)
  return(exportable_forms)
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
#' form event "recent". Set to Inf to fetch all qualifying events.
#' @param form_event The form event to check for. Possible values are
#' "create", "submit", or "review". "create" will consider when the form was added
#' to the form group, "submit" will consider when the form was submitted, and
#' "review" will consider when the form was reviewed.
#' @param as_reviewer Request forms using the /form/data/list/reviewer endpoint.
#' If False, request forms using the /form/data/list endpoint. See the Synapse
#' REST docs for additional information.
#' @param submission_state The submission state of the submissions.
#' Pass a list of submission states to match on any state in the list.
#' Submission states are: `WAITING_FOR_SUBMISSION`, `SUBMITTED_WAITING_FOR_REVIEW`,
#' `ACCEPTED`, `REJECTED`. Set to NULL (default) to ignore submission state.
#' @return A dataframe where each record is a recent form.
#' @export
get_recent_forms <- function(syn, form_group_id, time_duration,
                             form_event = "create", as_reviewer = TRUE,
                             submission_state = NULL) {
  current_time <- lubridate::now(tzone = "UTC")
  validate_form_event_params(time_duration = time_duration,
                             form_event = form_event)
  # Properly configure submission_state
  if (as_reviewer && "WAITING_FOR_SUBMISSION" %in% submission_state) {
    submission_state <- submission_state[
      !(submission_state == "SUBMITTED_WAITING_FOR_REVIEW")]
    warning("Fetching forms as reviewer. Only submitted forms will be fetched.")
  } else if (!as_reviewer) {
    warning(paste("Not fetching forms as a reviewer.",
                  "Only forms owned by the current user will be fetched."))
  }
  time_duration <- lubridate::duration(time_duration)
  # Set column containing timestamp of event
  if (form_event == "submit") {
    timestamp_col <- rlang::sym("submissionStatus_submittedOn")
  } else if (form_event == "create") {
    timestamp_col <- rlang::sym("createdOn")
  } else if (form_event == "review") {
    timestamp_col <- rlang::sym("submissionStatus_reviewedOn")
  }
  all_forms <- get_submissions_metadata(syn,
                                        group = form_group_id,
                                        state_filter = submission_state,
                                        all_users = as_reviewer)
  if (!(rlang::as_string(timestamp_col) %in% names(all_forms))) {
    # this will occurr if no forms satisfy the form event
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
#' already been exported. If NULL, this function behaves likes `get_recent_forms`
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
                                 submission_state = "ACCEPTED", time_duration = Inf,
                                 form_event = "submit", as_reviewer = TRUE) {
  forms <- get_recent_forms(syn = syn,
                            form_group_id = form_group_id,
                            time_duration = time_duration,
                            form_event = form_event,
                            as_reviewer = as_reviewer,
                            submission_state = submission_state)
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
  allowed_events <- c("create", "submit", "review")
  # Check time_duration for a valid value
  if (is.na(lubridate::duration(time_duration))) {
    stop(paste(time_duration, "of type", typeof(time_duration),
               "is not an allowed `time_duration` value."))
  } else if (!(form_event %in% allowed_events)) {
    stop(paste("form_event must be one of",
               paste(allowed_events, collapse = ", ")))
  }
}
