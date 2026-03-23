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
otherwise attach it) before passing it to `.wt` in [`clean_data`][clean_data].
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
