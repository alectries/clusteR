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
