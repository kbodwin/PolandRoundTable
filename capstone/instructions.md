# Shiny Application for Visualization

To use the Shiny app to visualize networks over time, you must first install our custom R package, PolandRoundTable. You will need a recent version of R (ideally, 4.0+).

## Installation

First, run the following code to install the remotes package.

install.packages("remotes")

Use it to install one of the following custom PolandRoundTable packages:

### Original Shiny App 

remotes::install_github("kbodwin/PolandRoundTable")

### Capstone Shiny App 

remotes::install_github("kbodwin/PolandRoundTable", ref = "capstone")

### Beta Version

remotes::install_github("jcolt45/PolandRoundTable")

## Dependencies

The installation process may prompt you to install some dependencies, or other packages that are needed for the PolandRoundTable package to function. Even if you already have these packages installed, it is probably a good idea to update them to the most recent version by re-installing.

If you are not comfortable navigating these installs, simply run the following to insure that you have all necessary software:

prt_dep <- c("igraph",
    "dplyr",
    "magrittr",
    "purrr",
    "lubridate",
    "tidyr",
    "shiny",
    "tidygraph",
    "plotly",
    "shinyWidgets",
    "devtools",
    "shinythemes",
    "ggraph",
    "ggiraph",
    "ggstar")

install.packages(prt_dep)

## Producing the App

Once you have finished all installations, you may use the app at any time. Simply open an R console and run:

library(PolandRoundTable)

### Individuals-as-Nodes

For the Individuals-as-Nodes App, exucute the following:

run_network_app()

### Organizations-as-Nodes

For the Organizations-as-Nodes App (only in the capstone or beta version), exucute the following:

run_network_app_flipped()