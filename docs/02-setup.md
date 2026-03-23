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
