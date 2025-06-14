#' Helper Function for get_edgelist_orgs to get total member connections by affiliations
#'
#' @param affils_by_date Data frame of affiliations with start and end dates
#' @param affil_by which affiliation to get stats for
#' @param start A start of date range, in YYYY-MM-DD string format.
#' @param end An end of date range, in YYYY-MM-DD string format.
#' @return
#' @import dplyr readr
#' @export
get_cons_by_afil_orgs <- function(affils_by_date,
                             affil_by,
                             start,
                             end = NULL) {
  affil_mat <- affils_by_date %>%
    filter(Start.Date <= end &
             End.Date >= start) %>%
    filter(RT.Affiliation == affil_by) %>%
    select(Member.ID, Org.ID) %>%
    distinct() %>%
    mutate(
      match = 1
    ) %>%
    tidyr::pivot_wider(names_from = Org.ID,
                       values_from = match,
                       values_fill = 0) %>%
    select(-Member.ID) %>%
    as.matrix() %>%
    crossprod()


  orgs <- rownames(affil_mat)


  if (length(orgs) == 0) {
    return(tibble())
  }

  affil_count <- affil_mat %>%
    as_tibble() %>%
    mutate(
      from = orgs
    ) %>%
    tidyr::pivot_longer(-from,
                        names_to = "to",
                        values_to = paste0(affil_by, "_Cons"))

  return(affil_count)
}


#' @param affils_by_date Data frame of affiliations with start and end dates
#' @param affil_by which affiliation to get stats for
#' @param start A start of date range, in YYYY-MM-DD string format.
#' @param end An end of date range, in YYYY-MM-DD string format.
#' @return
#' @import dplyr readr
#' @export
get_cons_by_afil_umb <- function(affils_by_date,
                                  affil_by,
                                  start,
                                  end = NULL) {
  affil_mat <- affils_by_date %>%
    filter(Start.Date <= end &
             End.Date >= start) %>%
    filter(RT.Affiliation == affil_by) %>%
    select(Member.ID, Umbrella) %>%
    distinct() %>%
    mutate(
      match = 1
    ) %>%
    tidyr::pivot_wider(names_from = Umbrella,
                       values_from = match,
                       values_fill = 0) %>%
    select(-Member.ID) %>%
    as.matrix() %>%
    crossprod()


  orgs <- rownames(affil_mat)


  if (length(orgs) == 0) {
    return(tibble())
  }

  affil_count <- affil_mat %>%
    as_tibble() %>%
    mutate(
      from = orgs
    ) %>%
    tidyr::pivot_longer(-from,
                        names_to = "to",
                        values_to = paste0(affil_by, "_Cons"))

  return(affil_count)
}



#' Make an edgelist of organizations with member overlap in given date range
#' This function should be updated to take a dataset piped in, and to calculate
#' edgeweight in the newer way.
#'
#' @param affils_by_date Data frame of affiliations with start and end dates
#' @param weight_by options for computing edgeweights
#' @param totals data with total member stats for each org
#' @param start A start of date range, in YYYY-MM-DD string format.
#' @param end An end of date range, in YYYY-MM-DD string format.
#' @param min_cons minimum connections needed to keep an edge
#' @param custom T/F for if we are using Custom Mass Org option
#' @return A tibble with pairs of organizations and their number of shared members in that range.
#' @import dplyr readr
#' @export
get_edgelist_orgs <- function(affils_by_date,
                              weight_by,
                              totals,
                              start,
                              end = NULL,
                              min_cons = 1,
                              custom = FALSE,
                              mass_weights = "No") {

  if (is.null(end)) {
    end <- start
  }

  start <- lubridate::ymd(start)
  end <- lubridate::ymd(end)

  affil_mat <- affils_by_date %>%
    filter(Start.Date <= end &
             End.Date >= start) %>%
    select(Member.ID, Org.ID) %>%
    distinct() %>%
    mutate(
      match = 1
    ) %>%
    tidyr::pivot_wider(names_from = Org.ID,
                values_from = match,
                values_fill = 0) %>%
    select(-Member.ID) %>%
    as.matrix() %>%
    crossprod()

   orgs <- rownames(affil_mat)

   if (length(orgs) == 0) {
     return(NULL)
   }


   edgelist <- affil_mat %>%
     as_tibble() %>%
     mutate(
       from = orgs
     ) %>%
     tidyr::pivot_longer(-from,
                  names_to = "to",
                  values_to = "num_members")

   if (custom == TRUE){
     mass_orgs = affils_by_date %>%
       filter(Start.Date <= end &
                End.Date >= start) %>%
       filter(Category == "Mass Organization") %>%
       pull(Umbrella) %>%
       unique()

     mass_orgs_ids = affils_by_date %>%
       filter(Start.Date <= end &
                End.Date >= start) %>%
       filter(is.na(Subgroup) & Category == "Mass Organization") %>%
       select(Umbrella, Org.ID) %>%
       distinct()

     subgroups <- affils_by_date %>%
       filter(Start.Date <= end &
                End.Date >= start) %>%
       filter(Umbrella %in% mass_orgs) %>%
       filter(!is.na(Subgroup)) %>%
       group_by(Org.ID, Umbrella) %>%
       summarise(num_members = n()) %>%
       ungroup()

     subgroup_cons <- subgroups %>%
       rename(from = Org.ID) %>%
       left_join(subgroups, by = "Umbrella") %>%
       rename(to = Org.ID) %>%
       rename(num_members = num_members.x) %>%
       select(to, from, num_members)

     from_mass <- mass_orgs_ids %>%
       rename(from = Org.ID) %>%
       left_join(subgroups, by = "Umbrella") %>%
       rename(to = Org.ID) %>%
       select(from, to, num_members)

     to_mass <- mass_orgs_ids %>%
       rename(to = Org.ID) %>%
       left_join(subgroups, by = "Umbrella") %>%
       rename(from = Org.ID) %>%
       select(from, to, num_members)

     custom_cons <- rbind(rbind(from_mass, to_mass), subgroup_cons)
   }



   gov_count <- get_cons_by_afil_orgs(affils_by_date, "Government", start, end)
   if (nrow(gov_count) != 0){
     edgelist <- left_join(edgelist, gov_count, by = c("to", "from"))
     if (custom == TRUE){
       custom_cons <- left_join(custom_cons, gov_count, by = c("to", "from"))
     }
   } else {
     edgelist <- edgelist %>%
       mutate(Government_Cons = 0)
     if (custom == TRUE){
       custom_cons <- custom_cons %>%
         mutate(Government_Cons = 0)
     }
   }
   opp_count <- get_cons_by_afil_orgs(affils_by_date, "Opposition", start, end)
   if (nrow(opp_count) != 0){
     edgelist <- left_join(edgelist, opp_count, by = c("to", "from"))
     if (custom == TRUE){
       custom_cons <- left_join(custom_cons, opp_count, by = c("to", "from"))
     }
   }else {
     edgelist <- edgelist %>%
       mutate(Opposition_Cons = 0)
     if (custom == TRUE){
       custom_cons <- custom_cons %>%
         mutate(Opposition_Cons = 0)
     }
   }
   church_count <- get_cons_by_afil_orgs(affils_by_date, "Church", start, end)
   if (nrow(church_count) != 0){
     edgelist <- left_join(edgelist, church_count, by = c("to", "from"))
     if (custom == TRUE){
       custom_cons <- left_join(custom_cons, church_count, by = c("to", "from"))
     }
   }else {
     edgelist <- edgelist %>%
       mutate(Church_Cons = 0)
     if (custom == TRUE){
       custom_cons <- custom_cons %>%
         mutate(Church_Cons = 0)
     }
   }
   expert_count <- get_cons_by_afil_orgs(affils_by_date, "Expert", start, end)
   if (nrow(expert_count) != 0){
     edgelist <- left_join(edgelist, expert_count, by = c("to", "from"))
     if (custom == TRUE){
      custom_cons <- left_join(custom_cons, expert_count, by = c("to", "from"))
     }
   }else {
     edgelist <- edgelist %>%
       mutate(Expert_Cons = 0)
     if (custom == TRUE){
       custom_cons <- custom_cons %>%
         mutate(Expert_Cons = 0)
     }
   }

   edgelist[is.na(edgelist)] <- 0

   edgelist <- edgelist %>%
     filter(num_members >= min_cons) %>%
     #filter(to != from) %>%
     mutate(weight = 1) %>%
     left_join(totals, by = c("from" = "Org.ID"))

   if (custom == TRUE){
     custom_cons[is.na(custom_cons)] <- 0

     custom_cons <- custom_cons %>%
       filter(num_members >= min_cons) %>%
       #filter(to != from) %>%
       mutate(weight = 1) %>%
       left_join(totals, by = c("from" = "Org.ID"))

     if (mass_weights == "no"){
       edgelist <- rbind(edgelist, custom_cons)
     }
   }


   if (weight_by == "Total"){
     edgelist <- edgelist %>%
       mutate(weight = num_members)
   } else if (weight_by == "PropMems"){
     edgelist <- edgelist %>%
       mutate(weight = case_when(
         num_members == 0 | Total == 0 ~ 1,
         TRUE ~ num_members / Total
       ))
   } else if (weight_by == "Ratio"){
     edgelist <- edgelist %>%
       mutate(weight = case_when(
         Government + Opposition == 0 ~ 0.01,
         TRUE ~ 0.01 + (1 - (abs(Government/(Government + Opposition) - 0.5) * 2))))
   }

   if (custom == TRUE & mass_weights == "yes"){
     edgelist <- rbind(edgelist, custom_cons)
   }

   return(edgelist)
}


#' Make an edgelist of organizations with member overlap in given date range
#' This function should be updated to take a dataset piped in, and to calculate
#' edgeweight in the newer way.
#'
#' @param affils_by_date Data frame of affiliations with start and end dates
#' @param weight_by options for computing edgeweights
#' @param totals data with total member stats for each org
#' @param start A start of date range, in YYYY-MM-DD string format.
#' @param end An end of date range, in YYYY-MM-DD string format.
#' @param min_cons minimum connections needed to keep an edge
#' @return A tibble with pairs of organizations and their number of shared members in that range.
#' @import dplyr readr
#' @export
get_edgelist_umb <- function(affils_by_date,
                              weight_by,
                              totals,
                              start,
                              end = NULL,
                              min_cons = 1) {

  if (is.null(end)) {
    end <- start
  }

  start <- lubridate::ymd(start)
  end <- lubridate::ymd(end)

  affil_mat <- affils_by_date %>%
    filter(Start.Date <= end &
             End.Date >= start) %>%
    select(Member.ID, Umbrella) %>%
    distinct() %>%
    mutate(
      match = 1
    ) %>%
    tidyr::pivot_wider(names_from = Umbrella,
                       values_from = match,
                       values_fill = 0) %>%
    select(-Member.ID) %>%
    as.matrix() %>%
    crossprod()

  orgs <- rownames(affil_mat)

  if (length(orgs) == 0) {
    return(NULL)
  }


  edgelist <- affil_mat %>%
    as_tibble() %>%
    mutate(
      from = orgs
    ) %>%
    tidyr::pivot_longer(-from,
                        names_to = "to",
                        values_to = "num_members")



  gov_count <- get_cons_by_afil_umb(affils_by_date, "Government", start, end)
  if (nrow(gov_count) != 0){
    edgelist <- left_join(edgelist, gov_count, by = c("to", "from"))
  } else {
    edgelist <- edgelist %>%
      mutate(Government_Cons = 0)
  }
  opp_count <- get_cons_by_afil_umb(affils_by_date, "Opposition", start, end)
  if (nrow(opp_count) != 0){
    edgelist <- left_join(edgelist, opp_count, by = c("to", "from"))
  }else {
    edgelist <- edgelist %>%
      mutate(Opposition_Cons = 0)
  }
  church_count <- get_cons_by_afil_umb(affils_by_date, "Church", start, end)
  if (nrow(church_count) != 0){
    edgelist <- left_join(edgelist, church_count, by = c("to", "from"))
  }else {
    edgelist <- edgelist %>%
      mutate(Church_Cons = 0)
  }
  expert_count <- get_cons_by_afil_umb(affils_by_date, "Expert", start, end)
  if (nrow(expert_count) != 0){
    edgelist <- left_join(edgelist, expert_count, by = c("to", "from"))
  }else {
    edgelist <- edgelist %>%
      mutate(Expert_Cons = 0)
  }

  edgelist[is.na(edgelist)] <- 0

  edgelist <- edgelist %>%
    filter(num_members >= min_cons) %>%
    #filter(to != from) %>%
    mutate(weight = 1) %>%
    left_join(totals, by = c("from" = "Umbrella"))


  if (weight_by == "Total"){
    edgelist <- edgelist %>%
      mutate(weight = num_members)
  } else if (weight_by == "PropMems"){
    edgelist <- edgelist %>%
      mutate(weight = case_when(
        num_members == 0 | Total == 0 ~ 1,
        TRUE ~ num_members / Total
      ))
  } else if (weight_by == "Ratio"){
    # edgelist <- edgelist %>%
    #   mutate(weight = 1 + abs(Government + Opposition))
    edgelist <- edgelist %>%
      mutate(weight = case_when(
        Government + Opposition == 0 ~ 0.01,
        TRUE ~ 0.01 + (1 - (abs(Government/(Government + Opposition) - 0.5) * 2))))
  }

  return(edgelist)
}



# ------------------INDIVIDUALS-------------------------------

#' Make an edgelist of members with organizational overlap in given date range
#'
#' @param affils_by_date Data frame of affiliations with start and end dates
#' @param start A start of date range, in YYYY-MM-DD string format.
#' @param end An end of date range, in YYYY-MM-DD string format.
#' @param get_edge_names Boolean; should we label the edges by name?
#' @return A tibble with pairs of members who were in the same institution or participated in the same event in the date range.
#' @import dplyr
#' @export
get_edgelist_members <- function(affils_by_date,
                                 on_cols,
                                 start,
                                 get_edge_names = TRUE,
                                 end = NULL,
                                 weight_col = NULL) {

  ## No end date supplied, we snapshot at start date

  if (is.null(end)) {
    end <- start
  }

  start <- lubridate::ymd(start)
  end <- lubridate::ymd(end)

  #### Filter by date range

  affils_by_date <- affils_by_date %>%
    filter(Start.Date <= end
           & End.Date >= start)

  ## No weight col supplied, we make one to weight everything as 1

  if (is.null(weight_col)) {

    affils_by_date <- affils_by_date %>%
      mutate(
        weight = 1
      )

    weight_col = "weight"

  }


  tmp <-  affils_by_date %>%
    select(Member.ID, on_cols[1], weight_col) %>%
    drop_na(on_cols[1])


    bad <- is.na(tmp[[on_cols[1]]]) | tmp[[on_cols[1]]] == ""
    tmp[bad, weight_col] <- 0

    affil_mat <- tmp %>%
      distinct() %>%
      tidyr::pivot_wider(names_from = Member.ID,
                values_from = weight_col,
                values_fill = 0) %>%
    select(-on_cols[1]) %>%
    as.matrix() %>%
    crossprod()

  for(i in 2:length(on_cols)) {

     tmp <- affils_by_date  %>%
      select(Member.ID, on_cols[1:i], weight_col) %>%
      drop_na(on_cols[1])

      bad <- bad | is.na(tmp[[on_cols[i]]]) | tmp[[on_cols[i]]] == ""
      tmp[bad, weight_col] <- 0

      affil_mat_2 <- tmp %>%
        distinct() %>%
        tidyr::pivot_wider(names_from = Member.ID,
                         values_from = weight_col,
                         values_fill = 0) %>%
      select(-on_cols[1:i]) %>%
      as.matrix() %>%
      crossprod()


    affil_mat <- affil_mat + affil_mat_2


  }

  mems <- rownames(affil_mat)

  if (length(mems) == 0) {
    return(NULL)
  }

  edgelist <- affil_mat %>%
    as_tibble() %>%
    mutate(
      from = mems
    ) %>%
    tidyr::pivot_longer(-from,
                 names_to = "to",
                 values_to = "weight") %>%
    filter(parse_number(from) < parse_number(to)) %>%
    filter(weight > 0)


  # # drop duplicates
  # edgelist <- edgelist %>%
  #   mutate(
  #     c1 = map2_chr(from, to, ~c(.x, .y) %>% min()),
  #     c2 = map2_chr(to, from, ~c(.x, .y) %>% max())
  #   ) %>%
  #   distinct(c1, c2, .keep_all = TRUE)

  if (get_edge_names) {

  edgelist <- edgelist %>%
    mutate(
      #edge_members = "OOPS"
      edge_orgs = map2_chr(to, from, ~find_edge_members(affils_by_date, "Member.ID", "Umbrella", "Umbrella.Name", .x, .y))
    )
  }

  return(edgelist)

}
