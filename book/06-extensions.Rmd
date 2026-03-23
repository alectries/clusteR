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
