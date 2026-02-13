#!/usr/bin/env python3
"""Send an email in response to a form event on a form group.

This is a Python implementation of `email_alert.R`.

A command line tool for sending email alerts in response to form events
on a Synapse form group. This script is meant to be run within the docker
environment provided with this package.

To avoid redundant emails, the frequency of the cron job invoking this
script should be set to a frequency between one and two times the
--time-duration argument passed here.
"""

import argparse
import sys
from datetime import datetime, timedelta, timezone
from typing import List, Optional, Dict, Any
import pandas as pd
import synapseclient
from synapseclient import Synapse
from synapseclient.models import FormGroup, FormData


def parse_args() -> argparse.Namespace:
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        description=(
            "A command line tool for sending email alerts in response to "
            "form events on a Synapse form group. This script is meant to "
            "be run within the docker environment provided with this package. "
            "To avoid redundant emails, the frequency of the cron job invoking "
            "this script should be set to a frequency between one and two times "
            "the --time-duration argument passed here."
        )
    )

    parser.add_argument(
        "--synapse-auth-token",
        type=str,
        help="[optional] Your Synapse access token. Uses `~/.synapseConfig` if not specified.",
    )
    parser.add_argument(
        "--form-group-id", type=str, required=True, help="[required] The form group ID."
    )
    parser.add_argument(
        "--form-group-name",
        type=str,
        required=True,
        help="[required] The form group name.",
    )
    parser.add_argument(
        "--recipients",
        type=str,
        required=True,
        help=(
            "[required] A comma-delimited list of Synapse user IDs "
            "to notify in response to a recent form event."
        ),
    )
    parser.add_argument(
        "--form-event",
        type=str,
        default="submit",
        choices=["create", "submit", "review"],
        help=(
            "The form group event to monitor for. The possible values are "
            "'create', 'submit', and 'review'. The default value is 'submit'."
        ),
    )
    parser.add_argument(
        "--time-duration",
        type=str,
        default="inf",
        help=(
            "The period of time (in seconds) from the present moment to "
            "consider a form event 'recent' and hence requiring an email alert. "
            "If this flag is not included or set to 'inf', any form event "
            "satisfying other specified criteria will trigger an alert, "
            "regardless of when the form event occurred."
        ),
    )
    parser.add_argument(
        "--file-view-reference",
        type=str,
        default=None,
        help=(
            "The Synapse ID of a file view with at least a `formDataId` "
            "column where all forms from this form group are tracked."
        ),
    )
    parser.add_argument(
        "--action-link",
        type=str,
        default=None,
        help=(
            "A hyperlink to include as part of the email for reviewers "
            "to access submitted forms."
        ),
    )
    parser.add_argument(
        "--as-reviewer",
        action="store_true",
        default=True,
        help=(
            "Whether to use the /form/data/list/reviewer endpoint. " "Defaults to True."
        ),
    )
    parser.add_argument(
        "--submission-state",
        type=str,
        default=None,
        help=(
            "Filter results by submission state. Pass a comma-delimited "
            "list to filter by more than one submission state. Valid states: "
            "'waiting_for_submission', 'submitted_waiting_for_review', "
            "'accepted', 'rejected'. By default, submission state is not considered."
        ),
    )

    return parser.parse_args()


def log_into_synapse(auth_token: str) -> Synapse:
    """Log into Synapse.

    Args:
        auth_token: Your Synapse access/authentication token

    Returns:
        Synapse client object
    """
    syn = synapseclient.login(authToken=auth_token)
    return syn


def parse_time_duration(time_duration_str: str) -> Optional[timedelta]:
    """Parse time duration from string.

    Args:
        time_duration_str: Time duration as string (seconds or 'inf')

    Returns:
        timedelta object or None if 'inf'
    """
    if time_duration_str.lower() == "inf":
        return None

    try:
        seconds = float(time_duration_str)
        return timedelta(seconds=seconds)
    except ValueError:
        raise ValueError(
            f"time_duration must be a number (seconds) or 'inf', "
            f"got: {time_duration_str}"
        )


def validate_form_event_params(
    time_duration: Optional[timedelta], form_event: str
) -> None:
    """Validate form event parameters.

    Args:
        time_duration: Time duration as timedelta or None for infinite
        form_event: Form event type ('create', 'submit', or 'review')

    Raises:
        ValueError: If parameters are invalid
    """
    allowed_events = ["create", "submit", "review"]

    if form_event not in allowed_events:
        raise ValueError(f"form_event must be one of {', '.join(allowed_events)}")


def get_submissions_metadata(
    syn: Synapse,
    group: str,
    all_users: bool = True,
    state_filter: Optional[List[str]] = None,
) -> Optional[pd.DataFrame]:
    """Get all submission metadata by state filter.

    Args:
        syn: Synapse login object
        group: The group ID
        all_users: True to get all submissions in group (as reviewer); False to get
            group submissions from caller only
        state_filter: List of submission states to filter by. Valid states:
            'waiting_for_submission', 'submitted_waiting_for_review',
            'accepted', 'rejected'. None to use defaults.

    Returns:
        DataFrame of submission metadata, or None if no submissions
    """
    # Use default states if not specified
    if state_filter is None:
        if all_users:
            # Use reviewer default states
            state_filter = list(FormGroup.DEFAULT_REVIEWER_STATES)
        else:
            # Use owner default states
            state_filter = list(FormGroup.DEFAULT_OWNER_STATES)

    # Create FormGroup instance and list form data
    form_group = FormGroup(group_id=group)

    try:
        # Get all forms using the native list method
        forms = list(
            form_group.list(
                filter_by_state=state_filter,
                synapse_client=syn,
                as_reviewer=all_users,
            )
        )

        if not forms:
            return None

        # Convert FormData objects to dictionary records
        all_metadata = []
        for form in forms:
            record = {
                "formDataId": form.form_data_id,
                "etag": form.etag,
                "groupId": form.group_id,
                "name": form.name,
                "createdBy": form.created_by,
                "createdOn": form.created_on,
                "modifiedOn": form.modified_on,
                "dataFileHandleId": form.data_file_handle_id,
            }

            # Extract submission status fields if present
            if form.submission_status:
                record["submissionStatus_submittedOn"] = (
                    form.submission_status.submitted_on
                )
                record["submissionStatus_reviewedOn"] = (
                    form.submission_status.reviewed_on
                )
                record["submissionStatus_state"] = form.submission_status.state

            all_metadata.append(record)

        return pd.DataFrame(all_metadata)

    except Exception as e:
        print(f"Error fetching submissions metadata: {e}")
        return None


def get_recent_forms(
    syn: Synapse,
    form_group_id: str,
    time_duration: Optional[timedelta] = None,
    form_event: str = "create",
    as_reviewer: bool = True,
    submission_state: Optional[List[str]] = None,
) -> pd.DataFrame:
    """Check for recent form events and return those forms.

    Args:
        syn: Synapse client object
        form_group_id: The form group to check for the form event
        time_duration: Time period to consider a form event "recent".
            None for all forms regardless of when they were submitted.
        form_event: Form event to check for ('create', 'submit', or 'review')
        as_reviewer: Request forms using the /form/data/list/reviewer endpoint
        submission_state: List of submission states to filter by

    Returns:
        DataFrame where each record is a recent form
    """
    current_time = datetime.now(timezone.utc)
    validate_form_event_params(time_duration, form_event)

    # Configure submission_state
    # Configure submission_state - normalize to lowercase with underscores
    if submission_state:
        submission_state = [
            s.lower().replace(" ", "_") if isinstance(s, str) else s
            for s in submission_state
        ]

    if (
        as_reviewer
        and submission_state
        and "waiting_for_submission" in submission_state
    ):
        submission_state = [
            s for s in submission_state if s != "waiting_for_submission"
        ]
        print(
            "Warning: Fetching forms as reviewer. Forms in 'waiting_for_submission' state will be excluded."
        )
    elif not as_reviewer:
        print(
            "Warning: Not fetching forms as a reviewer. "
            "Only forms owned by the current user will be fetched."
        )

    # Set column containing timestamp of event
    if form_event == "submit":
        timestamp_col = "submissionStatus_submittedOn"
    elif form_event == "create":
        timestamp_col = "createdOn"
    elif form_event == "review":
        timestamp_col = "submissionStatus_reviewedOn"

    all_forms = get_submissions_metadata(
        syn, group=form_group_id, state_filter=submission_state, all_users=as_reviewer
    )

    if all_forms is None or all_forms.empty:
        return pd.DataFrame()

    if timestamp_col not in all_forms.columns:
        # This will occur if no forms satisfy the form event
        return pd.DataFrame(columns=all_forms.columns)

    # Filter by time if duration specified
    if time_duration is not None:
        all_forms[timestamp_col] = pd.to_datetime(all_forms[timestamp_col])
        all_forms["time_since_event"] = current_time - all_forms[timestamp_col]
        recent_forms = all_forms[all_forms["time_since_event"] <= time_duration].copy()
        return recent_forms

    return all_forms


def get_exportable_forms(
    syn: Synapse,
    form_group_id: str,
    file_view_reference: Optional[str] = None,
    submission_state: Optional[List[str]] = None,
    time_duration: Optional[timedelta] = None,
    form_event: str = "submit",
    as_reviewer: bool = True,
) -> pd.DataFrame:
    """Get forms that don't yet exist in a file view on Synapse.

    Args:
        syn: Synapse client object
        form_group_id: The form group to check for the form event
        file_view_reference: Synapse file view to query. If None, this
            function behaves like get_recent_forms
        submission_state: List of submission states to filter by
        time_duration: Time period to consider a form event "recent"
        form_event: Form event to check for
        as_reviewer: Request forms using the reviewer endpoint

    Returns:
        DataFrame where each record is an exportable form
    """
    forms = get_recent_forms(
        syn=syn,
        form_group_id=form_group_id,
        time_duration=time_duration,
        form_event=form_event,
        as_reviewer=as_reviewer,
        submission_state=submission_state,
    )

    if file_view_reference is None:
        return forms

    # Query the file view
    form_file_view_query = syn.tableQuery(f"SELECT * FROM {file_view_reference}")
    form_file_view = form_file_view_query.asDataFrame()
    form_file_view["formDataId"] = form_file_view["formDataId"].astype(str)

    if forms.empty:
        return forms

    forms["formDataId"] = forms["formDataId"].astype(str)

    # Get forms not in file view (anti-join)
    exportable_forms = forms[~forms["formDataId"].isin(form_file_view["formDataId"])]

    return exportable_forms


def draft_message(
    exportable_forms: pd.DataFrame,
    form_group_name: str,
    form_event: str,
    action_link: Optional[str] = None,
) -> str:
    """Draft the body of an email message notifying of a form event.

    Args:
        exportable_forms: DataFrame as returned from get_exportable_forms
        form_group_name: Name of the form group where the form event took place
        form_event: The form event ('create', 'submit', or 'review')
        action_link: Hyperlink for reviewers to access submitted forms

    Returns:
        Email message body as string
    """
    number_of_forms = len(exportable_forms)

    if number_of_forms == 1:
        number_of_forms_text = "A new form has"
    else:
        number_of_forms_text = f"{number_of_forms} new forms have"

    if form_event == "submit":
        form_event_verb = "submitted to"
    elif form_event == "create":
        form_event_verb = "created in"
    elif form_event == "review":
        form_event_verb = "reviewed from"

    questions_text = (
        "If you have questions about why you are receiving this email, "
        "please reach out to the current form group or project administrator."
    )

    email_body = (
        f"Hello, {number_of_forms_text} been {form_event_verb} "
        f"form group `{form_group_name}`."
    )

    if action_link:
        email_body += f" Submitted forms may be accessed here: {action_link}"

    email_body += f"\n\n{questions_text}"

    return email_body


def email_alert(
    syn: Synapse,
    recipients: List[str],
    form_group_id: str,
    form_group_name: str,
    form_event: str = "submit",
    time_duration: Optional[timedelta] = None,
    file_view_reference: Optional[str] = None,
    action_link: Optional[str] = None,
    as_reviewer: bool = True,
    submission_state: Optional[List[str]] = None,
) -> None:
    """Send an email alert for recent form events.

    Args:
        syn: Synapse client object
        recipients: List of Synapse user IDs to notify
        form_group_id: The form group ID
        form_group_name: The form group name
        form_event: Form event to monitor ('create', 'submit', or 'review')
        time_duration: Time period to consider a form event "recent"
        file_view_reference: Synapse file view to check if form already exported
        action_link: Hyperlink for reviewers to access submitted forms
        as_reviewer: Use the reviewer endpoint
        submission_state: List of submission states to filter by
    """
    validate_form_event_params(time_duration, form_event)

    exportable_forms = get_exportable_forms(
        syn=syn,
        form_group_id=form_group_id,
        form_event=form_event,
        time_duration=time_duration,
        file_view_reference=file_view_reference,
        as_reviewer=as_reviewer,
        submission_state=submission_state,
    )

    if not exportable_forms.empty:
        print(f"Found {len(exportable_forms)} exportable forms")

        email_body = draft_message(
            exportable_forms=exportable_forms,
            form_group_name=form_group_name,
            form_event=form_event,
            action_link=action_link,
        )

        try:
            syn.sendMessage(
                userIds=recipients,
                messageSubject=f"New form event on form group {form_group_name}",
                messageBody=email_body,
                contentType="text/plain",
            )
            print(f"Email sent successfully to {len(recipients)} recipient(s)")
        except Exception as e:
            # Send notification of failure to the caller
            print(f"Failed to send email: {e}")
            user_profile = syn.getUserProfile()

            duration_str = "all time" if time_duration is None else str(time_duration)

            syn.sendMessage(
                userIds=[user_profile["ownerId"]],
                messageSubject="Form event notification failure",
                messageBody=(
                    f"A form event notification failed to deliver for form group "
                    f"{form_group_name} (form group ID: {form_group_id}) after "
                    f"attempting to notify Synapse users or groups "
                    f"{', '.join(recipients)} of a {form_event} event that had "
                    f"occurred in the prior {duration_str}."
                ),
                contentType="text/plain",
            )
    else:
        print("Did not find any exportable forms")


def main():
    """Main entry point for the script."""
    args = parse_args()

    # Parse time duration
    time_duration = parse_time_duration(args.time_duration)

    # Parse recipients
    recipients = [r.strip() for r in args.recipients.split(",")]

    # Parse submission state
    if args.submission_state:
        submission_state = [s.strip() for s in args.submission_state.split(",")]
    else:
        submission_state = None

    # Log into Synapse
    syn = log_into_synapse(args.synapse_auth_token)

    # Send email alert
    email_alert(
        syn=syn,
        recipients=recipients,
        form_group_id=args.form_group_id,
        form_group_name=args.form_group_name,
        form_event=args.form_event,
        time_duration=time_duration,
        file_view_reference=args.file_view_reference,
        action_link=args.action_link,
        as_reviewer=args.as_reviewer,
        submission_state=submission_state,
    )


if __name__ == "__main__":
    sys.exit(main())
