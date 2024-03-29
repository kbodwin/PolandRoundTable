% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_network.R
\name{get_edgelist_members}
\alias{get_edgelist_members}
\title{Make an edgelist of members with organizational overlap in given date range}
\usage{
get_edgelist_members(
  affils_by_date,
  on_cols,
  start,
  get_edge_names = TRUE,
  end = NULL,
  weight_col = NULL
)
}
\arguments{
\item{affils_by_date}{Data frame of affiliations with start and end dates}

\item{start}{A start of date range, in YYYY-MM-DD string format.}

\item{get_edge_names}{Boolean; should we label the edges by name?}

\item{end}{An end of date range, in YYYY-MM-DD string format.}
}
\value{
A tibble with pairs of members who were in the same institution or participated in the same event in the date range.
}
\description{
Make an edgelist of members with organizational overlap in given date range
}
