#!/usr/bin/env python3
"""
Export forms as YAML to Synapse as they are submitted to a form group.

This script retrieves forms from a Synapse form group based on submission state
and exports them as YAML files to a specified Synapse folder, tracking already
exported forms via a file view to avoid duplicates.
"""

import argparse
import sys
import tempfile
import json
import yaml
import urllib.request
from pathlib import Path
from datetime import datetime, timezone
from typing import List, Optional, Set

from synapseclient import Synapse, login, File
from synapseclient.models import FormGroup, FormData


def parse_args():
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        description=(
            "A command line tool for exporting submitted forms to Synapse. "
            "This script retrieves forms from a form group, filters by submission "
            "state, and exports them as YAML files to a Synapse folder."
        )
    )
    parser.add_argument(
        "--synapse-auth-token",
        type=str,
        help="[required] Your Synapse access token",
    )
    parser.add_argument(
        "--form-group-id",
        type=str,
        required=True,
        help="[required] The form group ID",
    )
    parser.add_argument(
        "--form-group-name",
        type=str,
        required=True,
        help="[required] The form group name",
    )
    parser.add_argument(
        "--synapse-parent",
        type=str,
        required=True,
        help="[required] Synapse ID of the parent folder to export to",
    )
    parser.add_argument(
        "--file-view-reference",
        type=str,
        required=True,
        help=(
            "[required] The Synapse ID of a file view with at least a "
            "'formDataId' column where all forms from this form group are tracked"
        ),
    )
    parser.add_argument(
        "--submission-state",
        type=str,
        default=None,
        help=(
            "Filter results by submission state. Pass a comma-delimited list to "
            "filter by more than one submission state. By default, submission state "
            "is not considered (all relevant states are fetched based on reviewer status). "
            "Valid states: waiting_for_submission, submitted_waiting_for_review, "
            "accepted, rejected"
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
        "--no-as-reviewer",
        action="store_false",
        dest="as_reviewer",
        help="Use the /form/data/list endpoint instead of /reviewer endpoint",
    )
    return parser.parse_args()


def get_submitted_forms(
    syn: Synapse,
    form_group_id: str,
    submission_states: List[str],
    as_reviewer: bool = True,
) -> List[FormData]:
    """
    Retrieve forms from a form group filtered by submission state.

    Args:
        syn: Authenticated Synapse client
        form_group_id: The form group ID to retrieve forms from
        submission_states: List of submission states to filter by
        as_reviewer: Whether to retrieve forms as a reviewer

    Returns:
        List of FormData objects matching the criteria
    """
    # Normalize state strings to lowercase with underscores
    filter_states = [s.strip().lower().replace(" ", "_") for s in submission_states]

    # Create FormGroup instance
    form_group = FormGroup(group_id=form_group_id)

    # List all forms using the native method
    forms = list(
        form_group.list(
            filter_by_state=filter_states,
            synapse_client=syn,
            as_reviewer=as_reviewer,
        )
    )

    return forms


def get_already_exported_form_ids(
    syn: Synapse,
    file_view_id: str,
) -> Set[str]:
    """
    Query a file view to get the set of form data IDs that have already been exported.

    Args:
        syn: Authenticated Synapse client
        file_view_id: The Synapse ID of the file view to query

    Returns:
        Set of form data IDs that already exist in the file view
    """
    query = f"SELECT formDataId FROM {file_view_id}"
    results = syn.tableQuery(query)

    exported_ids = set()
    for row in results:
        if row[0]:  # formDataId might be None
            exported_ids.add(str(row[0]))

    return exported_ids


def get_exportable_forms(
    syn: Synapse,
    form_group_id: str,
    file_view_reference: str,
    submission_states: List[str],
    as_reviewer: bool = True,
) -> List[FormData]:
    """
    Get forms that haven't been exported yet based on file view tracking.

    Args:
        syn: Authenticated Synapse client
        form_group_id: The form group ID to retrieve forms from
        file_view_reference: Synapse ID of file view tracking exported forms
        submission_states: List of submission states to filter by
        as_reviewer: Whether to retrieve forms as a reviewer

    Returns:
        List of FormData objects that need to be exported
    """
    # Get all forms matching the criteria
    all_forms = get_submitted_forms(
        syn=syn,
        form_group_id=form_group_id,
        submission_states=submission_states,
        as_reviewer=as_reviewer,
    )

    # Get already exported form IDs
    exported_ids = get_already_exported_form_ids(syn, file_view_reference)

    # Filter to only forms not yet exported
    exportable_forms = [
        form for form in all_forms if form.form_data_id not in exported_ids
    ]

    return exportable_forms


def download_form_content(syn: Synapse, form: FormData) -> dict:
    """
    Download the JSON content of a form using direct API calls.

    This approach uses the /file/v1/fileHandle/batch endpoint to get a presigned URL,
    then downloads the file directly. This is necessary because FormData objects do not
    have traditional Synapse entity IDs, so FormData.download() cannot work.

    Args:
        syn: Authenticated Synapse client
        form: FormData object to download

    Returns:
        Dictionary containing the form's JSON data
    """
    # Build the request body for the batch file handle API
    request_body = {
        "requestedFiles": [
            {
                "fileHandleId": form.data_file_handle_id,
                "associateObjectId": form.form_data_id,
                "associateObjectType": "FormData",
            }
        ],
        "includePreSignedURLs": True,
        "includeFileHandles": False,
    }

    # Request the presigned URL
    response = syn.restPOST(
        uri="https://repo-prod.prod.sagebase.org/file/v1/fileHandle/batch",
        body=json.dumps(request_body),
    )

    # Extract the presigned URL
    presigned_url = response["requestedFiles"][0]["preSignedURL"]

    # Create a temporary file for download
    with tempfile.NamedTemporaryFile(mode="w+b", delete=False, suffix=".json") as tmp:
        tmp_path = tmp.name

    try:
        # Download the file from the presigned URL
        urllib.request.urlretrieve(presigned_url, tmp_path)

        # Read the JSON content
        with open(tmp_path, "r") as f:
            form_content = json.load(f)

        return form_content

    finally:
        # Clean up temp file
        Path(tmp_path).unlink(missing_ok=True)


def export_forms_to_synapse(
    syn: Synapse,
    forms: List[FormData],
    synapse_parent: str,
) -> None:
    """
    Export forms as YAML files to Synapse.

    Args:
        syn: Authenticated Synapse client
        forms: List of FormData objects to export
        synapse_parent: Synapse ID of the parent folder to store files in
    """
    for form in forms:
        print(f"Exporting form {form.form_data_id}...")

        # Download form content
        form_content = download_form_content(syn, form)

        # Write to temporary YAML file
        yaml_filename = f"form_{form.form_data_id}.yaml"
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".yaml", delete=False, prefix=f"form_{form.form_data_id}_"
        ) as tmp:
            yaml.dump(form_content, tmp, default_flow_style=False, allow_unicode=True)
            tmp_path = tmp.name

        try:
            # Create Synapse File object
            syn_file = File(
                path=tmp_path,
                name=yaml_filename,
                parent=synapse_parent,
            )

            # Add formDataId as an annotation
            syn_file.formDataId = str(form.form_data_id)

            # Store to Synapse
            syn.store(syn_file)
            print(f"  ✓ Exported form {form.form_data_id} as {yaml_filename}")

        finally:
            # Clean up temp file
            Path(tmp_path).unlink()


def main():
    """Main entry point."""
    args = parse_args()

    # Validate required arguments
    if not all(
        [
            args.form_group_id,
            args.form_group_name,
            args.synapse_parent,
            args.file_view_reference,
        ]
    ):
        print(
            "Error: All required command line arguments must be provided. "
            "Include the --help flag for more information.",
            file=sys.stderr,
        )
        return 1

    # Login to Synapse
    print("Logging into Synapse...")
    if args.synapse_auth_token:
        syn = Synapse()
        syn.login(authToken=args.synapse_auth_token)
    else:
        syn = login()

    user = syn.getUserProfile()
    print(f"Logged in as: {user.get('userName', 'Unknown')}")
    print()

    # Parse submission states
    if args.submission_state:
        submission_states = [s.strip() for s in args.submission_state.split(",")]
    else:
        # Default behavior: get all relevant states based on reviewer status
        # This matches the R implementation's default behavior
        if args.as_reviewer:
            # As reviewer, we cannot get forms that are waiting_for_submission
            submission_states = ["submitted_waiting_for_review", "accepted", "rejected"]
        else:
            submission_states = [
                "waiting_for_submission",
                "submitted_waiting_for_review",
                "accepted",
                "rejected",
            ]

    print(f"Form group ID: {args.form_group_id}")
    print(f"Form group name: {args.form_group_name}")
    print(f"Submission states: {', '.join(submission_states)}")
    print(f"Export destination: {args.synapse_parent}")
    print(f"File view reference: {args.file_view_reference}")
    print(f"As reviewer: {args.as_reviewer}")
    print()

    try:
        # Get exportable forms
        print("Retrieving exportable forms...")
        exportable_forms = get_exportable_forms(
            syn=syn,
            form_group_id=args.form_group_id,
            file_view_reference=args.file_view_reference,
            submission_states=submission_states,
            as_reviewer=args.as_reviewer,
        )

        if not exportable_forms:
            print("Did not find any exportable forms.")
            return 0

        print(f"Found {len(exportable_forms)} exportable form(s)")
        print()

        # Export forms to Synapse
        print("Exporting forms to Synapse...")
        export_forms_to_synapse(
            syn=syn,
            forms=exportable_forms,
            synapse_parent=args.synapse_parent,
        )

        print()
        print(f"✓ Successfully exported {len(exportable_forms)} form(s)")
        return 0

    except Exception as e:
        print(f"\n❌ Error: {e}", file=sys.stderr)
        import traceback

        traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main())
