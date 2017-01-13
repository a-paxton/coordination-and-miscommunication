# Dynamics of Coordination in Miscommunication: Data analysis for Paxton, Roche, & Tanenhaus (under review)

This repo contains the code for the analyses presented in our manuscript, "Two to tangle: Conceptualizing miscommunication as a dyadic process" (Paxton, Roche, & Tanenhaus, under review).

## Overview

The repo contains several analysis files, an R markdown, and a markdown file.

* `coordination-and-miscommunication.Rmd`: R markdown with all data preparation, analysis, and visualization presented in our manuscript. Note that it includes some visualizations and tables that are mentioned but (for brevity) not included in the manuscript itself.
* `coordination-and-miscommunication.Rmd`: A markdown file generated by the R markdown of the same name. We recommend that you open this version to view in your browser.
* `./supplementary-code/libraries-and-functions-CM.r`: Loads in necessary libraries and creates new functions for our analyses.
* `./supplementary-code/continuous-rqa-parameters-CM.r` Identifies the appropriate parameters for continuous cross-recurrence quantification analysis (CRQA).
* `./supplementary-code/recurrence-settings-CM.r`: Sets appropriate parameters for categorical cross-recurrence quantification analysis (CRQA).


## Notes on running and viewing

For best viewing in a browser, we recommend selecting the `coordination-and-miscommunication.md`, rather than the similarly named `.Rmd` file. (Analyses should be run using the `.Rmd` file of the same name.)

For those unfamiliar with R markdown, we recommend taking a look at [RStudio's introduction to R markdown](http://rmarkdown.rstudio.com/) before attempting to run the `.Rmd` file. (Be sure to download [RStudio](https://www.rstudio.com/) first, if you do not already have it installed on your machine.)

