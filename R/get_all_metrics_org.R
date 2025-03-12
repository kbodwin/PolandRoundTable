#------- By Organization ------#

#' Find the degree, betweenness, cross-group degree, and normalized versions of
#' those measures for a set of Organizations over a time period
#'
#' @param affils_by_date The data frame containing edge connections.  Format must match
#' built-in dataset `affiliation_dates`
#' @param weight_by Which option to compute edge weights
#' @param min_cons min connections to keep an edge
#' @param orgs A character vector of org IDs to limit computations to
#' @param start A start of date range, in DD-MM-YYYY string format.  Defaults to beginning of dates in data.
#' @param end An end of date range, in DD-MM-YYYY string format. Defaults to end of dates in data.
#' @param timesteps A string ("days", "months", "years") for the level of aggregation
#' of the affils_by_date before computing metrics
#'
#' @return A tibble of individual Member IDs and their metrics
#'
#' @import dplyr
#' @import lubridate
#' @export
get_all_metrics_orgs <- function(affils_by_date,
                            weight_by,
                            totals,
                            min_cons = 1,
                            orgs = NULL,
                            start = NULL,
                            end = NULL,
                            timesteps = "months") {
  if(is.null(orgs)) {
    orgs <- unique(affils_by_date$Org.ID)
  }

  if (is.null(start)) {
    start <- min(affils_by_date$Start.Date, na.rm = TRUE)
  }

  if (is.null(end)) {
    end <- max(affils_by_date$End.Date, na.rm = TRUE)
  }

  start <- lubridate::ymd(start)
  end <- lubridate::ymd(end)
  range <- lubridate::interval(start, end)

  if (timesteps == "months") {

    n_steps <- range %/% months(1) - 1
    starts <- start + months(0:n_steps)
    ends <- starts + months(1)

  } else if (timesteps == "days") {

    n_steps <- range %/% days(1) - 1
    starts <- start + days(0:n_steps)
    ends <- starts + days(1)

  } else if (timesteps == "years") {

    n_steps <- range %/% years(1) - 1
    starts <- start + years(0:n_steps)
    ends <- starts + years(1)
  }

  res <- purrr::map2_dfr(starts, ends,
                         ~get_one_metrics_step_org(affils_by_date,
                                               weight_by,
                                               totals,
                                               min_cons,
                                               orgs,
                                               start = .x,
                                               end = .y))

  return(res)

}

#' Find the betweenness measures for one individual in a time period
#'
#' @param affils_by_date The data frame containing edge connections.  Format must match
#' built-in datasets `affiliation_dates`
#' @param weight_by Which columns to use to compute edges
#' @param min_cons Weight column
#' @param orgs A character vector of org IDs.
#' @param start A start of date range, in DD-MM-YYYY string format.  Defaults to beginning of dates in data.
#' @param end An end of date range, in DD-MM-YYYY string format. Defaults to end of dates in data.
#' @param timesteps A string ("days", "months", "years") for the level of aggregation
#' of the affils_by_date before computing betweenness.
#'
#' @return A tibble with the metrics of all invididuals in one time window.
#'
#' @import dplyr
#' @import lubridate
#' @export
get_one_metrics_step_org <- function(affils_by_date,
                                 weight_by,
                                 totals,
                                 min_cons = 1,
                                 orgs = NULL,
                                 start = NULL,
                                 end = NULL) {
  if(is.null(orgs)) {
    orgs <- unique(affils_by_date$Org.ID)
  }

  if (is.null(start)) {
    start <- min(affils_by_date$Start.Date, na.rm = TRUE)
  }

  if (is.null(end)) {
    end <- max(affils_by_date$Start.Date, na.rm = TRUE)
  }


  start <- lubridate::ymd(start)
  end <- lubridate::ymd(end)

  edgelist <- get_edgelist_orgs(affils_by_date,
                                   weight_by,
                                   totals,
                                   start = start,
                                   end = end,
                                   min_cons)

  if (is.null(edgelist)) {
    res <- rep(NA, length(orgs))
    names(res) <- orgs
    return(res)
  }

  graph <- igraph::graph_from_data_frame(edgelist, directed = FALSE)

  is_valid <- orgs %in% as_ids(V(graph))

  res <- tibble(
    Org.ID = orgs,
    Centrality = NA,
    Degree = NA,
    Cross.Degree = NA
  )

  res$Centrality[is_valid] <- igraph::betweenness(graph, orgs[is_valid])
  res$Degree[is_valid] <- igraph::degree(graph, orgs[is_valid])

  # purrr::map_dbl(members,
  #                       ~betweenness_checked(graph, edgelist, .x))

  # res_deg <- purrr::map_dbl(members,
  #                           ~degree_checked(graph, edgelist, .x))

  ## Cross-cons only
  # edges_cross <- edgelist %>%
  #   left_join(member_meta_info %>% rename_all(~paste0(.x,"_from")),
  #             by = c("from" = "Member.ID_from")) %>%
  #   left_join(member_meta_info %>% rename_all(~paste0(.x,"_to")),
  #             by = c("to" = "Member.ID_to")) %>%
  #   filter(RT.Affiliation_from != RT.Affiliation_to)

  #cross_graph <- igraph::graph_from_data_frame(edges_cross, directed = FALSE)
  #
  # res_deg_cross <- purrr::map_dbl(members,
  #                           ~degree_checked(cross_graph, edges_cross, .x))
  #
  # res <- bind_cols(members, res_bet, res_deg, res_deg_cross)
  # names(res) <- c("Member.ID", "Centrality", "Degree", "Cross.Degree")

  res$Centrality.Normalized = as.vector(scale(res$Centrality))
  res$Degree.Normalized = as.vector(scale(res$Degree))
  #res$Cross.Degree.Normalized = as.vector(scale(res$Cross.Degree))
  res$Centrality.Rank = order(res$Centrality, decreasing = TRUE)
  res$Degree.Rank = order(res$Degree, decreasing = TRUE)
  #res$Cross.Degree.Rank = order(res$Cross.Degree, decreasing = TRUE)


  cat(as.character(start))

  res$Start.Date = start
  res$End.Date = end

  return(res)

}




#------- Helpers -------#

#' Calculates betweenness but first checks if vertex is valid
#'
#' @param graph An igraph object
#' @param edgelist A tibble of edges
#' @param vertex The vertex to calculate betweenness of
#'
#' @return A double
#'
betweenness_checked_orgs <- function(graph, edgelist, vertex) {

  if (vertex %in% c(edgelist$from, edgelist$to)) {

    igraph::betweenness(graph, vertex)

  } else {

    NA

  }

}


#------- Helpers -------#

#' Calculates betweenness but first checks if vertex is valid
#'
#' @param graph An igraph object
#' @param edgelist A tibble of edges
#' @param vertex The vertex to calculate betweenness of
#'
#' @return A double
#'
degree_checked_orgs <- function(graph, edgelist, vertex) {

  if (vertex %in% c(edgelist$from, edgelist$to)) {

    igraph::degree(graph, vertex)

  } else {

    NA

  }

}
