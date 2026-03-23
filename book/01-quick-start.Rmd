---
output: html_document
---

# Quick Start

## Installation

Run the following commands in your R console to install clusteR:

```
install.packages("devtools")
devtools::install_github("alectries/clusteR")
```

You can update clusteR by running `devtools::update_packages()` and selecting
the option for clusteR.

## Setting up

Each survey should be run in a unique folder. clusteR will set up a survey
environment in this folder with a custom file structure. You may add and modify
files and folders within this structure as needed. clusteR will ignore files
that it did not create or does not need.

To load clusteR, run:

```
library(clusteR)
```

R is case-sensitive, so the capital `R` matters! On first load in a new folder,
clusteR will give a warning:

```
Warning message:
clusteR environment not configured!
! Please run clusteR::setup() to set up your survey environment. 
```

You will need to run [`setup()`][Setup] to create the survey environment in your working
directory. Use either `setwd()` or create an R project in RStudio in your
desired folder before running `setup()`.

## Workflow

Your project will likely go through three phases: setup, collection, and
data analysis. Below is an overview of the functions you are most likely to use
in each phase.

### Setup and sampling

- [`make_clusters`][make_clusters] to randomly select U.S. Census blocks,
weighted by their population, to serve as clusters.
  - [`view_map`][view_map] to view a simple graphical map of sampled
  clusters.
- [`setup`][Setup] to set up the clusteR environment.
  - [`setup_get_alc`][Alchemer data source] to establish a connection to an
  Alchemer survey.
  - [`setup_get_csv`][Comma-delimited (.csv) data source] to establish a
  connection to a CSV file with survey response data.

### Collection

- [`make_email`][make_email] to generate a PDF or CSV with information\
needed to send emails to cohort members.
- [`make_mailing`][make_mailing] to generate a PDF or CSV with information
needed to send physical mailings to cohort members.
- [`make_phone`][make_phone] to generate a PDF or CSV with information
needed to make calls to cohort members.
- [`make_groups`][make_groups] to automatically group clusters by physical
proximity and output a file to make manual adjustments for door-to-door
interviews.
- [`make_walklist`][make_walklist] to generate PDF walk lists for groups
assigned by `make_groups`, taking into account manual adjustments.
  - `custom_walklist` to save a starter custom template for walk lists.
- [`update_cohort`][update_cohort] to obtain, join, and view updated cohort
information from the data source and the Editor.
  - `update_confirm` to apply updates to the cohort file.
  - [`view_cohort`][view_cohort] to view the full cohort file.
  - [`search_cohort`][search_cohort] to view filtered results from the
  cohort file.
  - [`restore_cohort`][restore_cohort] to restore an archived version of the
  cohort file.
- [`view_progress`][view_progress] to generate a report on survey
completion by cluster.
- [`get_data`][get_data] to download and archive survey responses from a
pre-specified data source.
  - `get_alc`, generally used by `get_data`, to retrieve data from
  [Alchemer](https://www.alchemer.com).
  - `get_csv`, generally used by `get_data`, to retrieve data from a CSV.

### Data analysis

- [`clean_data`][Cleaning] to clean downloaded, standardized survey data.
  - [`clean_weight`][clean_weight], within `clean_data`, to calculate and save
  survey weights.
- [`view_topline`][view_topline] to analyze responses and view or export
overall results.
  - `custom_topline` to save a starter custom template for topline reports.
- [`view_breakdown`][view_breakdown] to analyze responses and break them
down by a stratification variable.
  - `custom_breakdown` to save a starter custom template for breakdown
  reports.
- [`export_cohort`][export_cohort] to export the cohort file to a CSV.
- [`export_data`][export_data] to export survey responses to a CSV.
