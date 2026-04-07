# clusteR

clusteR is a package that helps local epidemiologists to manage cluster-sampled
cohort surveys similar to
[CDC's CASPER](file:///C:/LocalData/clusteR/clusteR/docs/%22https://www.cdc.gov/casper/php/overview/index.html).

Please take a moment to review
[clusteR's documentation](https://alectries.github.io/clusteR).

## Installation

Run the following commands in your R console to install clusteR:

```
install.packages("pak")
pak::pak("alectries/clusteR")
```

You can update clusteR by running `pak::pak()` and selecting
the option for clusteR.

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
