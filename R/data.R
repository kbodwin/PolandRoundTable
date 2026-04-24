#' @importFrom tibble tibble
NULL

#' Additional information about members of the Polish Round Table opposition.
#'
#' @format A data frame with 6 variables:
#' \describe{
#' \item{\code{Member.ID}}{ID number used for network matching}
#' \item{\code{Full.Name}}{Name of individual}
#' \item{\code{Age}}{Age at time of Round Table.}
#' \item{\code{Gender}}{Identified gender at time of Round Table}
#' \item{\code{RT.Affiliation}}{Currently only "Opposition"}
#' \item{\code{Profession}}{Category of profession at time of Round Table}
#' }
#'
"member_meta_info"

#' Additional information about organizations (institutions and/or events).
#'
#' @format A data frame with 7 variables:
#' \describe{
#' \item{\code{Org.ID}}{ID code used for network matching}
#' \item{\code{Org.Name}}{Long name for the institute or event.}
#' \item{\code{Org.Group.1}}{Short identifier for the institute or event; for example, "WRPOLY" for Wroclaw Polytechnic University}
#' \item{\code{Org.Group.2}}{Short identifier for a subcategory of the institute or event; for example, "FMIN" for "Faculty of Mining"}
#' \item{\code{Type}}{Category of organization.}
#' \item{\code{City}}{Primary city location of institute or event.}
#' #' \item{\code{Vovoidship}}{Primary regional location of institute or event.}
#' }
#'
"organization_meta_info"

#' Affiliation dates
#'
#' Table with start and end dates for each member's affiliation to various organizations.
#'
#' @format A data frame with 4 variables:
#' \describe{
#' \item{\code{Member.ID}}{The ID number of the individual.}
#' \item{\code{Org.ID}}{The ID number of the organization}
#' \item{\code{Start.Date}}{The date the individual first became involved with the organization.}
#' \item{\code{End.Date}}{The date the individual left the organization; either by de-affiliating, or because the event or institude ended.}
#' }
"affiliation_dates"

#' All pre-calculated metrics
#'
#' Table all metrics calculated for each individual for each month
#'
#' @format A data frame with N variables:
#' \describe{
#' \item{\code{Member.ID}}{The ID number of the individual.}
#' \item{\code{Start.Date}}{The date the individual first became involved with the organization.}
#' \item{\code{End.Date}}{The date the individual left the organization; either by de-affiliating, or because the event or institude ended.}
#' }
"all_metrics_by_month"
