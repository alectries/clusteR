---
title: "clusteR Documentation"
author: "Alec Higgins"
date: "2026-03-23"
site: bookdown::bookdown_site
documentclass: book
description: "R package for local epidemiologists to manage cluster-sampled cohort surveys."
---

# Introduction

[clusteR]("https://github.com/alectries/clusteR") is an R package that assists
epidemiologists (or data scientists or analysts or...) in local, regional, or
state health departments in managing a cluster-sampled cohort survey similar to
[CDC's CASPER]("https://www.cdc.gov/casper/php/overview/index.html").

## What clusteR can do

In short, clusteR is built to give epidemiologists a framework to manage and
analyze a cluster-sampled cohort survey. clusteR is built to handle most data
management tasks so epidemiologists don't need to rely on a difficult-to-
maintain (and nearly impossible to share) set of custom scripts.

clusteR can:

1. Given information about your state and county/counties of interest, randomly
select U.S. Census blocks for participation and display simple maps.
1. Standardize, manage, update, and export a cohort file with key data and
status information on your participants.
1. Export PDF and CSV lists to contact participants via mail, phone, and email.
1. Filter groups by aggregate status of participants, group selected clusters
(by proximity), and produce customizable walk lists for door-to-door interviews.
1. Produce reports on completion in your cohort.
1. Establish a data connection, retrieve data, and standardize it.
1. Customizably clean and weight standardized data.
1. Produce analytic reports from weighted or unweighted responses.
1. Export cohort data, raw or cleaned data, and analytic products.

clusteR *cannot*:

1. **Replace a trained epidemiologist.**
1. Obtain a random sample of participants or addresses in clusters of interest,
even when clusteR selects U.S. Census blocks for you.
1. Build, maintain, or host a survey platform.
1. Build, maintain, or host a dashboard or other web platform.
1. Host cohort files or survey data for collaboration.
1. Secure cohort files or survey data.

<!--chapter:end:index.Rmd-->

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

<!--chapter:end:01-quick-start.Rmd-->

---
output: html_document
---

# Setup

Setup is the trickiest and least forgiving step in using clusteR. The most
critical information is available in R with `?setup`. This page goes into
additional detail.

## Location

`setup` generates an environment for clusteR. Before beginning setup, you need
to set your working directory to a dedicated folder for your project. You
should not reuse the same folder as past clusteR projects. You can, however,
include additional files and folders in this dedicated folder.

The easiest way to do this is to create an R project using RStudio. At the top
right of RStudio, click the project dropdown and select *New Project...*. If
you have already created a folder, select *Existing Directory* and specify it;
otherwise, select *New Directory* and place it somewhere safe. clusteR does not
encrypt or obscure cohort files or survey data, and does not implement any
HIPAA, FERPA, or other requirements, so you will need to set up clusteR in
an appropriately secure location that your R installation can access.

clusteR creates (but does not overwrite, if they already exist) Cohort,
Contacts, Scripts, and Survey Data folders in the main working directory.
clusteR will only write or modify files inside these folders, except a handful
of HTML outputs written to the main working directory.

The most important file for your clusteR setup is `Cohort/config.rds`. Unless
you wish to reset your clusteR configuration and start over, you should not
delete or tamper with this file.

## name, short_name

These are your project identifiers. Both should be strings. `name` will be used
primarily for formatted titles on reports, while `short_name` will be used
primarily for file name prefixes.

Importantly, your cohort data will be saved as a plain text file in the
Cohort folder named after `short_name`. It is rarely, if ever, a good idea
to tamper directly with the cohort file.

These are the only variables required for `setup` to run and exit successfully.
However, because so many other configured options are required for clusteR to
actually work, `setup` will provide warnings for other options you do not set.

## state, county

These variables allow you to specify FIPS/INCITS codes, which clusteR uses to
download U.S. Census TIGER/Line shapefiles for your region of interest.
[State codes](https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code) and
[county codes](https://en.wikipedia.org/wiki/List_of_United_States_INCITS_codes_by_county)
are available online. These codes are required for all mapping functions.

You may specify multiple county codes, but you may only specify one state code,
and all county codes must be within the same state. This feature is intended
to accommodate North Carolina district health departments (see information from the
[UNC School of Government](https://humanservices.sog.unc.edu/visualization-all/))
but may be useful in other states.

As of version 0.1, clusteR is hard-coded to retrieve 2020 TIGER/Line shapefiles.
Support for other years is planned in later versions.

## geoids

If you have already selected U.S. Census blocks to serve as your clusters, you
will need a file to link cluster identifiers to U.S. Census GEOIDs. If you have
not selected them yet, you can do so using `make_clusters` or a similar
function.

### make_clusters

`make_clusters` will generate a file for you that matches cluster identifiers
to GEOIDs. If you have already run `setup` previously, `make_clusters` will
obtain your state and county FIPS/INCITS codes from your configuration, if
available. Otherwise, you will need to specify them, or provide the names of
your state and county/counties of interest. Providing names will display
a message from [tidycensus](https://walker-data.com/tidycensus/) with the
relevant FIPS/INCITS codes so you don't need to look them up.

### Custom clusters

clusteR is [highly extensible][Extensions], and you can create custom functions
or scripts to manage cluster selection for you. Your function or script must
save a delimited text file with two columns: "geoid" (specifying the 15-digit
Census block ID) and "cluster" (matching the cluster identifiers in your
cohort file).

Most commonly, you will simply have your clusters in advance of using clusteR.
In this case, save a delimited text file (probably a CSV) with columns as
specified above. Your cluster identifiers can be anything you choose, but they
must match the "Cluster" column in your cohort file.

## setup_cohort

For clusteR to perform management tasks, it needs to maintain a master file of
cohort members, their information, and their participation status. clusteR
maintains a raw text file in the Cohort subdirectory with this information and
automatically updates some of the information as survey responses come in.

### Required fields

clusteR requires certain fields in the cohort file. These are:

- **ID**: A unique identifier, which can be in any format.
- **Cluster**: An identifier of the cluster a participant is in.
- **Name**: The participant's name, which defaults to "Current Resident".
- **Mailing**: The participant's mailing street address.
- **Physical**: When the participant's mailing address is not their residence,
the participant's actual street address of residence.
- **City**: The city of the mailing address.
- **State**: The state of the mailing address.
- **ZIP**: The ZIP code of the mailing address.
- **Phone**: The participant's phone number.
- **Email**: The participant's email address.
- **Consent**: The participant's consent status ("Yes", "No", "Unknown",
"Do not contact", or NA), defaulting to NA.
- **Status**: The participant's completion status with enrollment information,
which is automatically determined.

Most of these fields can be formatted however you like, but Consent is picky.
You must use only the following options:

- **Yes**: The participant has participated and consented to be surveyed.
- **No**: The participant has participated or refused and did not consent to be
surveyed.
- **Unknown**: The participant has participated, but did not return a consent.
- **Do not contact**: The participant wishes to be removed from the cohort; this
is optional and "No" can be used instead.
- **NA**: The system default, which should be used when a prospective
participant has not yet been reached.

`update_cohort` will throw an error if anything other than these options is
used. For an Alchemer survey (but, importantly, not a CSV file), you can enter
a key in `setup_get_alc` to convert your responses for you.

### Input

During initial setup, you will need to specify a cohort input file, which must
be a:

- **.csv text file**
- **.xls or .xlsx Excel file**
- **.rds R data file**

No other format is supported. There are two ways to provide this input file.

#### Pre-generate and provide (easy) {-}

The easy way is to generate the file in advance and provide its file path to
`setup` as a string. The file should be pre-filled with as much participant
information as you have available.

#### Generate during setup (harder) {-}

The more complicated way is to write and run a function as part of setup that
will generate your cohort input file for you. This function can work however you
like, but must be available when you run `setup` and must return only the file
path, as a string, to the properly-formatted input file. If you choose to
generate this file during setup, it is recommended to write your `setup` call in
an R script that you source rather than running `setup` interactively in the
console.

### Cohort file storage

The cohort data file will be stored in the automatically-generated Cohort
subdirectory and will be named `short_name` as provided in `setup`. *It is not
recommended to edit this file by hand.* However, should you need to edit it by
hand, it is a CSV file that can be edited in a text editor or Excel.

Because this file is plain-text, it should be stored in a secure location.
clusteR does not have built-in protection for confidential information.

### Editor

During setup, clusteR will produce an Editor.csv file in the Cohort subdirectory
so that you can manually make changes to existing cohort member records and add
new records. It will include all columns in your custom cohort file, but will
show no data from the file.

## setup_get

A survey generates survey data, and in order to manage the survey, clusteR needs
to connect to this data. clusteR natively supports two data sources, but other
packages can add support for additional data sources, and end users can write
functions to support a custom data source. The two default sources are:

- **.csv files**, connected with [`setup_get_csv`][Comma-delimited (.csv) data source].
- **Alchemer surveys**, connected with [`setup_get_alc`][Alchemer data source].

Additional native sources will be documented here in future releases.

### Comma-delimited (.csv) data source

The simplest data source in existence is the humble .csv file: a text file with
values separated by commas and observations separated by newlines. These files
are easy to create, modify, and view.

clusteR accepts comma-delimited files as data sources via
[`setup_get_csv`][Comma-delimited (.csv) data source] and `get_csv`). You will need:

- A permanent file path (given as a string) denoting the location of the input
.csv file.
- Any arguments you wish to pass to `read_csv` to read your file correctly.
(`col_types` is recommended!)

To set up a comma-delimited data source, you will need to run `setup` and
include this argument:

`setup_get = setup_get_csv(file, ...)`

Once setup is completed, clusteR saves this information. You will not need to do
this again.

### Alchemer data source

[Alchemer](https://www.alchemer.com/) is a survey tool used widely by local
public health agencies in the United States. Alchemer surveys are handled
natively by clusteR, not least because the author uses Alchemer at his local
health department.

clusteR accepts Alchemer survey data using Alchemer's [SurveyResponse API call](https://apihelp.alchemer.com/help/surveyresponse-sub-object-v5) via
[`setup_get_alc`][Alchemer data source] and `get_alc`. You will need:

- An Alchemer account, with your survey already created.
- The Survey ID, which is a number that can be found in the URL when you are
building a survey right after /id/.
- An API token passed as a string.
- An API token secret (password) passed as a string.
- A codebook (as a *tab*-delimited file) to convert Alchemer questions to
dataframe columns.

To set up an Alchemer survey as your data source, you will need to run `setup`
and include this argument:

`setup_get = setup_get_alc(survey_id, api_token, api_token_secret, codebook)`

Once setup is completed, clusteR saves this information. You will not need to do
this again.

### Custom data sources

In general, because each data source and its format are unique, clusteR is very
flexible with custom data sources. However, there are a few requirements
(replace x with a shorthand for your data source):

1. Your package or custom script must include a `setup_get_x` function that can
be called in `setup`.
2. Your `setup_get_x` function must return a list of named objects, which will
be included in config.rds and loaded by clusteR as `.cluster$cfg$setup_get`.
3. If your get function is found in a custom script (or if your get function
requires additional, non-standard setup), you must include the file path to the
script as a string named `run_before` in this list.
4. Your package or custom script must include a `get_x` function that can be
called by `get_data` and retrieve all its arguments (such as URLs, file paths,
and API keys) from `.cluster$cfg$setup_get`.
5. Your `get_x` function must be named (as a string) at
`.cluster$cfg$setup_get$get`.
6. Your `get_x` function must return a plain dataframe, tibble, or similar with
at least the required cohort fields properly named
(see `vignette('setup_cohort')`). `get_data` will handle the rest. Note that
this dataframe does not have to be particularly *clean*, just meet these basic
requirements.

While there is nothing stopping you from implementing an alternative data
structure to the system default, it is not recommended. `get_data` will save and
archive data in an appropriate format that is compatible across data source
types and with all native clusteR functions.

### Why not Excel?

You may be wondering why the author hasn't created functions to get data from a
Microsoft Excel source file, even though clusteR supports Excel for other tasks
(like importing a cohort file). The reasons are:

1. You can easily save your Excel file as a CSV.
2. If you *can't* easily save your Excel file as a CSV, it is probably because
you have formatted your Excel file in a way that R (and therefore clusteR) will
not handle well.

To prevent very confusing and difficult to troubleshoot issues that would
inevitably result from using clusteR with messy Excel files, clusteR requires
you to either make a CSV, which will make formatting problems more obvious to
you and force you to fix them sooner, or learn to write and implement your own
`setup_get_excel` and `get_excel` functions, which is considerably more
time-consuming.

## Additional variables

As mentioned previously, clusteR is [highly extensible][Extensions], and your
extensions may need persistent variables. While `setup` will not perform any
validation on these additional variables, it will save them to config.

If you have created (or required) custom variables, you can refer to them using
their names in the list `.cluster$cfg`, which is loaded whenever clusteR is
attached. For example, you can access the project name using
`.cluster$cfg$name`.

<!--chapter:end:02-setup.Rmd-->

---
output: html_document
---

# Cohort

The focus of clusteR is the cohort file: a data file where information about
your survey participants is stored. This section focuses on updating,
maintaining, and exporting cohort information. For creating a cohort file, see
[`setup_cohort`][setup_cohort].

## update_cohort

`update_cohort` (and its partner, `update_confirm`) is the workhorse of
clusteR. Using the data source specified in [`setup_get`][setup_get] and the
[Editor], `update_cohort` reviews new information and applies changes to the
cohort file. It also archives the cohort file so you can recover past versions.

The function creates a report displaying changes to be made to the cohort file.
The first section displays changes from your survey data source, and the second
displays changes from Editor. These sections are broken down into subsections
by cohort file field, with each subsection containing a table of changes to be
made (if any). Each table displays the ID, the existing value (.coh), and the
new value (.src/.man). It also stores versions of the cohort file, Editor,
and data source in memory, so `update_cohort` and `update_confirm` must be
run in the same R session.

When you have reviewed the Update Errors report and it displays only accurate
information, run `update_confirm` to write the updates to the cohort file.
Changes can be made in your data source or in Editor.

### Editor

Editor is an automatically-generated CSV file located in the Cohort folder.
It will be read by `update_cohort`, and changes you make in Editor will override
any existing or future updates from your data source. It is recommended to make
changes to the cohort file with Editor rather than in the data source itself to
preserve the integrity of your survey response data. You may also add entries
to the Cohort file using Editor.

clusteR will not overwrite cohort information from your data source with blank
cells in Editor. It does not overwrite entire rows, and it never overwrites
answers to survey questions that are not stored in your cohort file.

### view_progress

You can generate and view a simple report on survey completion, including data
and maps by cluster, by running `view_progress`. The color scale on the
completion maps is customizable via `breaks` and `colors`, which are passed to
`ggplot2::scale_fill_stepsn`.

### view_map

You may need to view the U.S. Census blocks used as clusters for your survey.
`view_map` displays these blocks on a simple map with county lines. It is
customizable, including a title, subtitle, fill, and background colors. The map
on [`view_progress`][view_progress] is more comprehensive.

## Maintaining the cohort

While `update_cohort` (and `update_confirm`) handle most of the cohort
maintenance automatically, you may need to perform additional, more manual
maintenance tasks. These functions assist with manual cohort monitoring and
maintenance.

### restore_cohort

Each time the cohort file is updated, an archived version is saved in
Cohort/Archive. You can restore an archived version using `restore_cohort`. When
a specific archive is not named, the most recent is displayed and can be saved
by typing "save" in the console. You can also name a file in Cohort/Archive to
restore an older version. Unless you type "save" in the console, the displayed
archive version is not stored as the cohort file.

### view_cohort

You can view the cohort at any time with `view_cohort`. It also invisibly
returns the cohort file, so you can use the assignment operator `<-` to
save it to memory. If you would like to do this silently, use
`view_cohort(FALSE)`.

### search_cohort

You can search (more accurately, filter) the cohort with `search_cohort`, which
displays the results in a viewer and invisibly returns the filtered results.
`search_cohort` accepts normal `dplyr::filter` syntax.

## Reaching the cohort

clusteR includes functions to output data about cohort participants. This can
often be done in PDF or CSV format for convenience.

[`make_email`][make_email], [`make_mailing`][make_mailing], and
[`make_phone`][make_phone] allow you to output cohort information filtered by
individual characteristics. They can be manually filtered by any column in the
cohort file or `.status` can be used to include only those who have completed
the survey (status "Completed - enrolled", "Completed - re-enroll", or 
"Completed - unenroll"), whose responses are pending (status "Enrolled", 
"Re-enroll", or "Not enrolled"), or who are enrolled in the cohort (status
"Enrolled" or "Completed - enrolled").

[`make_groups`][make_groups], with [`make_walklist`][make_walklist], outputs
cohort information filtered by cluster characteristics. The `filt` argument
allows users to filter clusters by a summary table of cluster characteristics.

- `n`, the total number of cohort members in each cluster
- `completed`, `completed_pct`: The number or percent (out of 100) of
participants with any Completed status
- `pending`, `pending_pct`: The number or percent (out of 100) of participants
with an Enrolled, Re-enroll, or Not enrolled status
- `enrolled`, `enrolled_pct`: The number or percent (out of 100) of participants
with a Completed - enrolled or Enrolled status

### make_email

`make_email` outputs each participant's ID, name, and email address. See
[Reaching the cohort] for filtering options.

### make_mailing

`make_mailing` outputs each participant's ID, name, mailing address, city,
state, and ZIP code. See [Reaching the cohort] for filtering options.

### make_phone

`make_phone` outputs each participant's ID, name, and phone number. See
[Reaching the cohort] for filtering options.

### make_groups

`make_groups` filters clusters by a summary table of cluster characteristics
(using the `filt` argument), groups clusters, and creates Assignments.csv in the
Contacts folder. It also outputs a plot of these automatic groups. `make_groups`
should be followed by [`make_walklist`][make_walklist]

By default, `make_groups` uses the internal function `mult_kmeans`, which uses
a kmeans algorithm to attempt to group clusters into approximately even groups
by their proximity. You can specify the number of groups to create with `k`, the
number of times to run the kmeans algorithm with `runs`, and the number of
iterations (times centers are calculated and groups are assigned) with
`iter.max`.

clusteR is [highly extensible][Extensions]. You can create and use other
grouping functions using the `fn` argument and the dynamic dots (`...`) to
specify any necessary arguments beyond the defaults. `make_groups` requires the
function to output a dataframe with the following columns:

- `cluster`, matching the cohort file cluster identifier
- `geoid`, matching the Census block GEOID
- `lat` and `long`, the latitude and longtitude of a point within the cluster
- `ur`, showing whether the block is urban or rural
- `geometry`, an *sf* geometry field to draw the block shape

### make_walklist

`make_walklist`, unlike [`make_mailing`][make_mailing], exports addresses in a
formatted PDF intended to be used in door-to-door outreach. You can specify the
cohort file columns to be included in the output by changing `cols`.

The default walk list template is very simple. `custom_walklist` creates a
template in the Scripts folder that you can modify and use by specifying its
path as `template`. If there are additional objects needed for your template,
they can be provided to `make_walklist` as named arguments.

## export_cohort

Cohort information is stored as a plain text file. The data can be exported to
a CSV file with `export_cohort`.

By default, the exported file will be named after `name` specified during
`setup`. You can alter it by passing a string to `.name`.

By default, the status of the cohort file is exported as-is. If `.status` is set
to TRUE, participant status will be updated to remove information about survey
completion and participants with an unenroll or do not contact flag will be
removed.

Setting `.removed` to TRUE will create removed.csv, which lists all participants
removed by setting `.status` to TRUE.

All other arguments passed to `export_cohort` are passed on to `dplyr::mutate`.
To remove columns from the output, you can set them to NULL.

<!--chapter:end:03-cohort.Rmd-->

---
output: html_document
---

# Data

The focus of your technical work outside clusteR will be the setup and
implementation of your data collection tool. clusteR can't help with that
(see [What clusteR can do]), but it can connect to your data automatically.

This section focuses on obtaining, cleaning, and exporting data. For
establishing a data connection, see [setup_get]. For analyzing data, see
[Analysis].

## get_data

`get_data` downloads and archives survey responses from your data source.
clusteR natively supports [Alchemer](https:://www.alchemer.com) and CSV data
sources. `get_data` itself archives data and both invisibly returns the file
path to that archive and saves it to the config as `.cluster$cfg$last_data`.

`get_csv`, a native function, obtains data from a CSV according to
[`setup`][Setup] and returns it. If you need to import data to R for manual
work and you are using a CSV file, run `df <- get_csv()`.

`get_alc`, a native function, obtains data from Alchemer according to
`setup`, cleans up and standardizes the JSON, and returns it. If you
need to import data to R for manual work and you are using Alchemer, run
`df <- get_alc`.

clusteR is [highly extensible][Extensions], including providing a framework for
[custom data sources]. To support a custom data source, `get_data` needs to
find a `setup_get` list in config with any necessary (named) variables to
run your `get_x` function. `setup_get` should be created in [`setup`][Setup]
using a `setup_get_x` function. `get_x`, like `get_csv` and `get_alc`, should
return a tidy dataframe with formatted (though not necessarily cleaned) data,
which `get_data` will handle.

## Cleaning

clusteR provides a simple, friendly way to clean survey data: `clean_data`.
By default, `clean_data` will use the most recent data file archived by
[`get_data`][get_data], but you can specify a different archive file using
`.x`.

You can programmatically weight data using `.wt`. clusteR natively supports
inverse probability weighting according to a generalized linear model with
[`clean_weight`][clean_weight]. `.wt` is very robust, so if a simpler solution
is desired (e.g. calculating weights using simple mutates), create a weight
variable using regular cleaning mutates instead.

Arguments passed to `clean_data` are passed on to `dplyr::mutate`
to modify existing columns and create new ones. Columns are set by your `get_x`
function: `get_csv` will use the first row of your input CSV as names, while
`get_alc` will use a codebook, for instance.

`clean_data` archives its output according to the name of the input file, which
by default is the time the data was downloaded. Running `clean_data` on the
same raw data archive twice will overwrite the previously cleaned version of
the same archive.

## Weighting

Weighting is usually operationalized as part of cleaning (`.wt`) and is
performed before cleaning mutates.

### Simple weighting schemes

If you are calculating simple weights based on columns in survey responses or
the cohort file, you should create your weight variable within the dynamic dots
(`clean_data(...)`). If you are depending on columns that you have not yet
created, create the weight column later in your `clean_data` call.

For example, if using
[CDC's CASPER](https://www.cdc.gov/casper/media/pdfs/CASPER-toolkit-3_508.pdf),
you will generally be able to calculate weights based on cluster-level
information you can keep in the cohort file or refer to in your `clean_data`
call.

### clean_weight

The author's weighting approach is based on each individual participant's
estimated, adjusted probability of completing the survey if selected to
participate. This approach relies on data in the cohort file, data in survey
responses, data obtained from external sources (such as the U.S. Census), and
calculated fields. It also requires a generalized linear model based on these
variables.

`clean_weight` returns a customized function that takes the survey responses
and cohort file, performs temporary mutates, runs a logistic regression to
estimate adjusted probabilities of participation for all cohort members, and
outputs a probability (`prob`) and inverse probability weight (`ipw`) for
each respondent. [`clean_data`][Cleaning] calls this function and joins the
weight variables before performing its own (persistent) mutates and archiving
cleaned data.

`formula` is passed to
[`stats::glm`](https://search.r-project.org/R/refmans/stats/html/glm.html).
`glm` is set to use a logit link and the binomial family. All other `glm`
options are set to the default.

`.import` allows you to provide a file path to a cluster-level data file that
is joined to the survey data for weighting according to the cluster identifier.
The file must contain a column titled `Cluster` with the cluster identifiers as
found in the cohort file. The author, for example, uses `.import` to include
Census demographic data for each cluster.

`.filt` allows you to filter data before weighting to limit which cohort members
are included in weighting. Filters are applied before weighting mutates or
modeling.

Arguments passed to `...` are passed on to `dplyr::mutate`.
These are weighting mutates, so they are accessible to `stats::glm` for modeling
but are not included in `clean_data` output.

### Custom weighting schemes

clusteR is [highly extensible][Extensions], including providing a framework to
create complex, custom weighting schemes and apply those weights to cleaned
data. If you develop a custom weighting function, you must `source` it (or
otherwise attach it) before passing it to `.wt` in [`clean_data`][Cleaning].
Your custom weighting function must return a function with exactly one argument
(`x`), the dataframe of raw data, that itself returns a dataframe with at least
two columns: `ID`, a column of participant IDs, and at least one numeric column
of weights to be included with `clean_data` output. `clean_data` will join
weights according to `ID`.

## export_data

You may wish to export your (raw or cleaned) survey responses to a file. You
can use `export_data` to create a CSV from archived survey data.

Arguments passed to `...` are passed on to `dplyr::select`. Unlike
`dplyr::select`, leaving `...` blank will include all columns, not no columns.
Otherwise, however, default `dplyr::select` behavior applies; if, for instance,
you wish to rename `Q1` to `Q01`, running:

`export_data("Q01" = Q1)`

will export only `Q01`. Instead, you may need to run:

`export_data(everything(), "Q01" = Q1)`

`.name` allows you to specify the export file name. The file will be saved in
the Survey Data folder.

`.dat` allows you to specify a data archive to export. By default, `export_data`
uses the most recently cleaned data (preferred) or the most recently downloaded
data (backup).

`.deid` deidentifies the data export, removing `Address` and all columns in the
cohort file other than `ID`. If you need finer control of column inclusion,
pass these requirements to `...` rather than enabling `.deid`.

<!--chapter:end:04-data.Rmd-->

---
output: html_document
---

# Analysis

clusteR provides tools to perform data analysis. Once data has been cleaned,
you are ready to analyze. Data management tasks, like [cleaning] and
[weighting], are covered in an earlier section.

## view_topline

This is the primary function to calculate and view survey results. It
automatically combines columns, calculates the frequency of each answer, and
applies weights to survey responses, then generates a report of the results.

`view_topline` only requires the `codebook` argument, which should be a string
representing a file path to a key for the survey questions. The file must be
tab-delimited and include a `QN` column containing the question numbers
corresponding to columns in the cleaned data and a `type` column specifying the
question type. The type may be:

- `mc`: Multiple-choice, select one answer per question
- `ms`: Multiple-select, select as many answers as apply per question
- `no`: Numeric, provide a numerical answer to a question
- `ft`: Free-text, provide a free-text answer to a question (which is usually
categorized and coded like a multiple-select question during cleaning)

Without specifying other arguments, `view_topline` generates a simple PDF report
based on the most recent cleaned data without weighting. This behavior can be
altered with the following:

- `data` can be set to a different file path to use different data
- `make` can be changed to "csv" to produce a tidy spreadsheet
- `template` can be changed to a path to a .Rmd file to customize the report
produced; a starter template is available with `custom_topline`
- `wt` can be set to apply survey weights, which must be a numeric variable in
the survey data
- `exclude` can be set to a character vector of questions to exclude from
weighting, such as demographic variables; this does not exclude questions from
the report, which can be accomplished by excluding them from the codebook file

## view_breakdown

This function breaks down survey results by a stratification variable, typically
a demographic variable. It automatically combines columns, calculates the
frequency of each answer, and applies weights to survey responses, then
generates a report of the results.

The only additional argument in `view_breakdown` compared to
[`view_topline`][view_topline] is `strata`, which allows you to specify a the
name of a stratification variable as a string. Results tables in the report will
have an additional column on the left representing the demographic variable.
This variable must already exist in the survey data.

A starter custom template is available with `custom_breakdown`.

<!--chapter:end:05-analysis.Rmd-->

---
output: html_document
---

# Extensions

clusteR is built for epidemiologists in local, regional, or state health
departments to manage a cluster-sampled survey. These surveys may have varied
study designs, outreach strategies, and analytic schemes. With that variability
in mind, clusteR is modular, and will work with custom functions and scripts to
support unique needs.

The author's crystal ball can't predict all uses of clusteR. In places where
there is no modular support, you can write new functions to access and modify
the files directly as needed.

The areas where clusteR supports extensions are:

- The selection of [custom clusters]
- [Additional variables] during setup that are added to `.cluster$cfg` when
clusteR is loaded
- Custom cluster grouping methods for use with [`make_groups`][make_groups]
- Custom data sources to access with [`get_data`][get_data]
- Functions to implement [custom weighting schemes]
- Custom templates for [`make_walklist`][make_walklist],
[`view_topline`][view_topline], and [`view_breakdown`][view_breakdown]

If you develop an interesting extension for clusteR, please let us know so we
can list and describe it here.

<!--chapter:end:06-extensions.Rmd-->

