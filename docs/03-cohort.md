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
