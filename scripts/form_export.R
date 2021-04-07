#' Export a form as YAML to Synapse as it is submitted to a form group
library(optparse)
library(synapseforms)

read_args <- function() {
  option_list <- list(
    make_option("--synapse-username", type = "character",
                help = "[required] Your Synapse username."),
    make_option("--synapse-password", type = "character",
                help = "[required] Your Synapse password."),
    make_option("--form-group-id", type = "character",
                help = "[required] The form group ID."),
    make_option("--form-group-name", type = "character",
                help = "[required] The form group name."),
    make_option("--synapse-parent", type = "character",
                help = "[required] Synapse ID of the parent folder to export to."),
    make_option("--file-view-reference", type="character",
                help = paste("[required] The Synapse ID of a file view with at",
                            "least a `formDataId` column where all forms from",
                            "this form group are tracked.")),
    make_option("--submission-state", type = "character", default=NULL,
                help = paste("Filter results by submission state. Pass a",
                             "comma-delimited list to filter by more than",
                             "one submission state. By default,",
                             "submission state is not considered.")),
    make_option("--as-reviewer", action = "store_true", default = T,
                help = paste("Whether to use the /form/data/list/reviewer",
                             "endpoint. Defaults to TRUE.")),
    make_option("--python-path", type = "character", default=NULL,
                help = paste("Path to the python installation (for reticulate)",
                             "If omitted, the system python is used by default.")))
  parser <- OptionParser(
    option_list = option_list,
    description = paste("A command line tool for exporting submitted forms",
                        "to Synapse.",
                        "This script is meant to be run within the docker",
                        "environment provided with this package."))
  opt <- parse_args(OptionParser(option_list = option_list),
                    convert_hyphens_to_underscores = TRUE)
  return(opt)
}

main <- function() {
  args <- read_args()
  if (any(is.null(args$synapse_username),
          is.null(args$synapse_password),
          is.null(args$form_group_id),
          is.null(args$form_group_name),
          is.null(args$synapse_parent),
          is.null(args$file_view_reference))) {
    stop(paste("All required command line arguments must be provided.",
               "Include the --help flag for more information."))
  }
  syn <- log_into_synapse(
    username = args$synapse_username,
    password = args$synapse_password,
    python_path = args$python_path)
  export_forms_to_synapse(
    syn = syn,
    form_group_id = args$form_group_id,
    output = args$synapse_parent,
    file_view_reference = args$file_view_reference,
    submission_state = args$submission_state)
}

main()
