# Python Scripts for Synapse Forms

This directory contains Python scripts for working with Synapse forms and is meant to be run within the python Dockerfile included in this repo.

## Prerequisites

The scripts are designed to run within the Docker container. Build the image first:

```bash
docker build --platform linux/amd64 -t synapseforms:python -f docker/python/Dockerfile .
```

## Available Scripts

### form_export.py

Export submitted forms from a Synapse form group as YAML files to a Synapse folder. This script tracks already-exported forms via a file view to avoid duplicates and supports filtering by submission state.

#### Basic Usage

```bash
docker run --platform linux/amd64 --rm \
  synapseforms:python \
  python /root/synapseforms/scripts/python/form_export.py \
    --synapse-auth-token YOUR_TOKEN \
    --form-group-id YOUR_FORM_GROUP_ID \
    --form-group-name 'My Form Group' \
    --synapse-parent YOUR_EXPORT_LOCATION \
    --file-view-reference syn25461396
```

#### Arguments

**Required:**
- `--form-group-id`: The numeric ID of the form group to export from
- `--form-group-name`: Display name of the form group
- `--synapse-parent`: Synapse ID of the parent folder where form data files will be exported
- `--file-view-reference`: Synapse ID of a file view with a `formDataId` column to track exported forms

**Optional:**
- `--synapse-auth-token`: Your Synapse personal access token. Alternatively, you can mount a `.synapseConfig` to the default directory.
- `--submission-state`: Filter by submission state(s). Options: `waiting_for_submission`, `submitted_waiting_for_review`, `accepted`, `rejected`. Comma-delimited for multiple states. Default: all relevant states based on reviewer status.
- `--as-reviewer`: Use the reviewer endpoint (default: `True`)
- `--no-as-reviewer`: Use the owner endpoint instead

#### How It Works

1. Queries the form group for forms matching the specified submission state(s)
2. Checks the file view to identify forms that haven't been exported yet
3. Downloads each form's JSON data using the Synapse REST API
4. Converts JSON to YAML format
5. Uploads YAML files to the specified Synapse folder with `formDataId` annotations
6. File view automatically tracks newly exported forms to prevent duplicates on subsequent runs


---

### email_alert.py

Send email alerts in response to form events on a Synapse form group. This script monitors for form creation, submission, or review events and sends notifications to specified recipients.

#### Basic Usage

```bash
docker run --platform linux/amd64 --rm \
  synapseforms:python \
  python /root/synapseforms/scripts/python/email_alert.py \
    --synapse-auth-token YOUR_TOKEN \
    --form-group-id MY_FORM_GROUP_ID \
    --form-group-name 'My Form Group' \
    --recipients COMMA_SEPERATED_SYNAPSE_USER_IDS \
    --time-duration SECONDS_SINCE_FORM_SUBMISSION
```

#### Arguments

**Required:**
- `--synapse-auth-token`: Your Synapse personal access token (optional if using `~/.synapseConfig`)
- `--form-group-id`: The numeric ID of the form group to monitor
- `--form-group-name`: Display name of the form group
- `--recipients`: Comma-delimited list of Synapse user IDs to notify

**Optional:**
- `--form-event`: Event type to monitor (`create`, `submit`, or `review`). Default: `submit`
- `--time-duration`: Time window in seconds to consider events "recent". Use `inf` for all events. Default: `inf`
- `--file-view-reference`: Synapse ID of a file view (e.g., `syn25461396`) to track which forms have already been processed
- `--action-link`: URL to include in the email for easy access to forms
- `--as-reviewer`: Use the reviewer endpoint (default: `True`)
- `--submission-state`: Filter by submission state(s). Options: `waiting_for_submission`, `submitted_waiting_for_review`, `accepted`, `rejected`. Comma-delimited for multiple states.


#### Scheduling with Cron

To avoid duplicate emails, set the cron frequency to 1-2x the `--time-duration` value.

Example crontab entry (runs once every hour, checks last 60 minutes):
```cron
0 * * * * docker run --platform linux/amd64 --rm synapseforms:python python /root/synapseforms/scripts/python/email_alert.py --synapse-auth-token $SYNAPSE_TOKEN --form-group-id 13 --form-group-name 'My Forms' --recipients 3342492 --time-duration 3600
```