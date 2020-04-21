# synapseforms

![R-CMD-check](https://github.com/Sage-Bionetworks/synapseforms/workflows/R-CMD-check/badge.svg?branch=master)

Wrappers for the [Synapse Forms Services API](https://docs.synapse.org/rest/#org.sagebionetworks.repo.web.controller.FormController).

## Requirements

This package uses reticulate with the python Synapse client. See the
[reticulate documentation](https://rstudio.github.io/reticulate/), and the
[Synapse python client documentation](https://python-docs.synapse.org/build/html/index.html)
for setting these up. Using the Synapse client requires having a Synapse
account. See [manage Synapse credentials](https://python-docs.synapse.org/build/html/Credentials.html)
to learn how to log in through the client.

## Installation

The package can be installed with
`remotes::install_github("Sage-Bionetworks/synapseforms")`.
