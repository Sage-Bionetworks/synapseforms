#' Send an email in response to a form event on a form group
library(optparse)
library(synapseforms)

read_args <- function() {
  option_list <- list(
    make_option("--synapse-username", type = "character",
                help = "Your Synapse username."),
    make_option("--synapse-password", type = "character",
                help = "Your Synapse password."),
    make_option("--form-group-id", type = "character",
                help = "The form group ID."),
    make_option("--form-group-name", type = "character",
                help = "The form group name."),
    make_option("--form-event", type = "character", default = "submit",
                help = paste("The form group event to monitor for.",
                             "The possible values are 'create', 'submit',",
                             "and 'review'. The default value is 'submit'.")),
    make_option("--time-duration", type = "character",
                help = paste("The period of time (in seconds or as a string",
                             "parseable by `lubridate::as.duration`) from the",
                             "present moment to consider an event 'recent' and",
                             "hence requiring an email alert.")),
    make_option("--recipients", type = "character",
                help = paste("A comma-delimited list of Synapse user IDs",
                             "in response to a recent form event.")),
    make_option("--file-view-reference", type="character",
                help= paste("The Synapse ID of a file view with at least a",
                            "`formDataId` column where all forms from this",
                            "form group are tracked.")),
    make_option("--as-reviewer", action = "store_true", default = T,
                help = paste("Whether to use the /form/data/list/reviewer",
                             "endpoint. Defaults to TRUE.")),
    make_option("--submission-state", type = "character", default=NULL,
                help = paste("Filter results by submission state. Pass a",
                             "comma-delimited list to filter by more than",
                             "one submission state. By default,",
                             "submission state is not considered.")))
  parser <- OptionParser(
    option_list = option_list,
    description = paste("A command line tool for sending email alerts in",
                        "response to form events on a Synapse form group.",
                        "This script is meant to be run within the docker",
                        "environment provided with this package.",
                        "To avoid redundant emails, the frequency of the cron",
                        "job invoking this script should be set to a frequency",
                        "between one and two times the --time-duration argument",
                        "passed here. See the `email_alert` function docstring",
                        "for more detailed information."))
  opt <- parse_args(OptionParser(option_list = option_list),
                    convert_hyphens_to_underscores = TRUE)
  return(opt)
}

main <- function() {
  args <- read_args()
  # If time_duration is an integer in seconds, it can only be parsed as an int
  formatted_time_duration <- tryCatch({
    args$time_duration <- as.integer(args$time_duration)
  }, error = function(e) {
    # If the above failed, assume that time_duration is a parseable string
    # If not, it will fail on script execution (and give a helpful error message)
    return(args$time_duration)
  })
  recipients <- strsplit(args$recipients, split=",")[[1]]
  submission_state <- strsplit(args$submission_state, split=",")[[1]]
  syn <- log_into_synapse(
    username=args$synapse_username,
    password=args$synapse_password)
  email_alert(
    syn = syn,
    recipients = recipients,
    form_group_id = args$form_group_id,
    form_group_name = args$form_group_name,
    form_event = args$form_event,
    time_duration = formatted_time_duration,
    file_view_reference = args$file_view_reference,
    as_reviewer = args$as_reviewer,
    submission_state = submission_state)
}

main()
