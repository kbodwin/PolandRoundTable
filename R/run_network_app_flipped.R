#' @import shiny shinyWidgets shinythemes
#' @import igraph tidygraph ggraph graphlayouts
#' @import plotly
#' @import devtools
#' @import dplyr tidyr
#' @import ggiraph
#' @export
run_network_app_flipped <- function() {

  member_meta_info <- member_meta_info %>%
    mutate(
      RT.Affiliation = forcats::fct_relevel(RT.Affiliation,
                                            "Opposition",
                                            "Government",
                                            "Church",
                                            "Expert",
                                            "Observer")
    )

  type_cat_cols <- ggcolors(6)[-4]
  names(type_cat_cols) <- c("Opposition",
                            "Government",
                            "Church",
                            "Expert",
                            "Observer")


  node_choices <- c(member_meta_info$Member.ID)
  names(node_choices) <- c(member_meta_info$Full.Name)

  # mem_name_choices <- member_meta_info %>%
  #   get_opts_list(Full.Name)

  mem_rt_choices <- member_meta_info %>%
    get_opts_list(RT.Affiliation)

  mem_job_choices <- member_meta_info %>%
    get_opts_list(Profession.Sector)

  org_choices <- organization_meta_info$Org.ID
  names(org_choices) <- organization_meta_info$Organization.Name

  org_name_choices <- organization_meta_info %>%
    get_opts_list(Organization.Name)

  org_umbrella_name_choices <- organization_meta_info %>%
    get_opts_list(Umbrella.Name)

  org_cat_choices <- organization_meta_info %>%
    get_opts_list(Category)

  org_type_choices <- organization_meta_info %>%
    get_opts_list(Type)

  #CHANGES
  node_var <- "Org.ID"
  node_labels <- "Organization.Name"
  edge_var <- "Member.ID"
  edge_labels <- "Full.Name"

  full_data <- affiliation_dates %>%
    select(Org.ID, Member.ID, Start.Date, End.Date) %>%
    dplyr::left_join(member_meta_info) %>%
    dplyr::left_join(organization_meta_info)

  edge_name_choices <- member_meta_info %>%
    mutate(
      Name = paste0(Last.Name, ", ", First.Middle.Name)
    ) %>%
    get_opts_list(Member.ID, labels = "Name")

  node_name_choices <- organization_meta_info %>%
    get_opts_list(Org.ID, labels = "Organization.Name")


  shinyApp(
    ui = tagList(
      navbarPage(
        "Polish Round Table Network",
        theme = "journal",
        tabPanel("Setup",
                 sidebarPanel(

                   ## Go button
                   actionButton("setup", "Done with Setup", class = "btn-primary"),

                   h3("Choose which Organizations to include in the network."),

                   h4("These choices are combined; for example, if you select
                        a category and a specific org, that specific org will be
                        included whether or not it is in the category."),

                   pickerInput('node_include_cat',
                               'Categories of organization to include:',
                               choices = org_cat_choices,
                               options = list(`actions-box` = TRUE),
                               multiple = TRUE,
                               selected = org_cat_choices
                   ),

                   pickerInput('node_include_type',
                               'Types of organizations to include:',
                               choices = org_type_choices,
                               options = list(`actions-box` = TRUE,
                                              liveSearch = TRUE),
                               multiple = TRUE
                   ),

                   pickerInput('node_include_specific',
                               'Specific organization to include:',
                               choices = org_name_choices,
                               options = list(`actions-box` = TRUE,
                                              liveSearch = TRUE),
                               multiple = TRUE
                   ),

                   h3("Choose which individuals will be used to create edge connections."),

                   h4("These choices are combined; for example, if you select
                        a profession and a specific person, that specific person will be
                        included whether or not they have that profession."),

                   pickerInput('edge_include_rt',
                               'Round Table affiliations to include:',
                               choices = mem_rt_choices,
                               options = list(`actions-box` = TRUE),
                               multiple = TRUE,
                               selected = mem_rt_choices
                   ),


                   pickerInput('edge_include_job',
                               'Professions to include:',
                               choices = mem_job_choices,
                               options = list(`actions-box` = TRUE),
                               multiple = TRUE
                   ),

                   pickerInput('edge_include_specific',
                               'Specific individuals to include:',
                               choices = edge_name_choices,
                               options = list(`actions-box` = TRUE,
                                              liveSearch = TRUE),
                               multiple = TRUE
                   ),

                   h3("Choose how edges will be computed and weighted."),

                   # How to compute edge weights
                   selectInput('edge_type',
                               'Weight options:',
                               choices = c("Total Connections" = "tot_con",
                                           "Ratio" = "ratio",
                                           "Mix" = "mix"
                               )
                   ),

                   # Make events "linger" for more than a month
                   sliderInput('event_length',
                               'How many months after are Events be considered to last?',
                               value = 1,
                               min = 1, max = 120,
                               sep = ""),

                   # Treat all connections as persistent
                   selectInput('lifelong',
                               'Should connections last forever after initial affiliation?',
                               choices = c("No" = "none",
                                           "Events only" = "events",
                                           "Yes, all connections" = "all"
                               ),
                               selected = "No")#,

                   # selectInput('cross',
                   #             'Use cross-RT-group connections only?',
                   #             choices = c("No" = FALSE, "Yes" = TRUE),
                   #             selected = "No"
                   # )
                 ),
                 mainPanel(
                   dataTableOutput("dataset")
                 )
        ),
        tabPanel("Explore Social Network",
                 sidebarPanel(
                   ## Go button
                   actionButton("make_network", "Draw Network", class = "btn-primary"),
                   h3("Choose a Date"),
                   div(style="display: inline-block;vertical-align:top; width: 150px;",
                       selectInput('month1',
                                   'Month',
                                   choices = 1:12,
                                   selected = 1
                       )),
                   div(style="display: inline-block;vertical-align:top; width: 150px;",
                       selectInput('day1',
                                   'Day',
                                   choices = 1:31,
                                   selected = 1
                       )),
                   sliderInput('year1',
                               'Year',
                               value = 1979,
                               min = 1945, max = 1989,
                               sep = ""),
                   # h3("End of Date Range"),
                   # selectInput('month2',
                   #             'Month',
                   #             choices = 1:12
                   # ),
                   # selectInput('day2',
                   #             'Day',
                   #             choices = 1:31
                   # ),
                   # sliderInput('year2',
                   #             'Year',
                   #             value = 1979,
                   #             min = 1945, max = 1989,
                   #             sep = ""),
                   h3("Node Creation Options"),

                   radioButtons('umb_orgs',
                                'Node Creation Options:',
                                choices = c("Umbrella" ="umb",
                                            "Separate Subgroups" = "sub",
                                            "Sub w/ Artificial Mass Org Connections" = "c")
                   ),


                   sliderInput('min_size',
                               "Minimum Number of Members to keep a Node:",
                               value = 1,
                               min = 1, max = 15),

                   sliderInput('min_cons',
                               "Minimum Number of Connections to keep a Node:",
                               value = 0,
                               min = 0, max = 15),

                   h3("Edge Creation Options"),


                   sliderInput('min_edges',
                               "Minimum Connections for an Edge:",
                               value = 1,
                               min = 1, max = 15),


                   radioButtons('weight_by',
                                'Weight edges by:',
                                choices = c("None" ="None",
                                            "Total Connections" = "Total",
                                            "Proportion of Members" = "PropMems",
                                            "Ratio Of Gov/Opp" = "Ratio"),
                                selected = "PropMems"
                   ),

                   radioButtons('mass_weights',
                                'Force Weights of Artficial Connections to 1?',
                                choices = c("Yes" ="yes",
                                            "No" = "no")
                   ),


                   h3("Change Node Appearance"),

                   # Highlight a node by color
                   #uiOutput("node_color_specific"),

                   # Highlight a node by shape
                   #uiOutput("node_shape_specific")

                   # Color groups
                   radioButtons('node_color_by_group',
                                'Different colors for groups of:',
                                choices = c(
                                  "None" = "None",
                                  "Category" = "Category",
                                  "Affiliation" = "Affil"),
                                selected = "Affil"
                   ),

                   pickerInput('node_shape_specific',
                               'Highlight individual organizations:',
                               choices = org_umbrella_name_choices,
                               options = list(`actions-box` = TRUE),
                               multiple = TRUE
                   ),

                   # pickerInput('node_shape_org',
                   #             'Highlight members of an organization:',
                   #             choices = org_umbrella_name_choices,
                   #             options = list(`actions-box` = TRUE),
                   #             multiple = TRUE
                   # ),

                   # Shape groups
                   # radioButtons('node_shape_by_group',
                   #              'Different shapes for groups of:',
                   #              choices = c(
                   #                "None" = "None",
                   #                "Round Table Affiliation" = "RT.Affiliation",
                   #                "Profession.Sector" = "Profession.Sector",
                   #                "Gender" = "Gender")
                   # ),

                   # # Resize nodes by
                   # radioButtons('node_size',
                   #              'Resize nodes by:',
                   #              choices = c(
                   #                "None" = "None",
                   #                "Overall betweenness" = "betweenness",
                   #                "Overall degree" = "degree",
                   #                "Cross-group degree" = "cross_degree")
                   # ),


                   textInput('node_color',
                             "Node color (if not by groups)",
                             value = "cyan4"),

                   sliderInput('node_size',
                               "Base node size",
                               value = 10,
                               min = 0, max = 20),


                   h3("Change Edge Appearance"),


                   # Selecting edge transparency
                   sliderInput('edge_width',
                               'Edge Width',
                               value = 0.3,
                               min = 0, max = 1),


                   # Selecting edge transparency
                   sliderInput('edge_transparency',
                               'Edge Transparency',
                               value = 0.3,
                               min = 0, max = 1),

                   # # Color groups
                   # radioButtons('edge_color_cross',
                   #              'Different colors for cross-connections?',
                   #              choices = c(
                   #                "No",
                   #                "Yes")
                   # ),

                   # Weight
                   radioButtons('edge_size_weight',
                                'Resize by weight?',
                                choices = c(
                                  "No",
                                  "Yes")
                   ),


                   h3("Change graph layout algorithm."),

                   # Removing a node
                   radioButtons('network_layout',
                                'Algorithm:',
                                choices = c("fr", "nicely", "kk"),
                                selected = "fr"
                   ),
                 ),
                 mainPanel(
                   girafeOutput('my_network', width = "700px", height = "700px"),
                   #tableOutput('test')
                   dataTableOutput("selected_node_info")
                 )
        ),
        tabPanel("Explore trends over time",
                 sidebarPanel(

                   h3("Choose metrics"),

                   radioButtons('compute',
                                'Compute:',
                                choices = c(
                                  "Static" = "static",
                                  "On Fly" = "compute"
                                )
                   ),

                   h4("CENTRALITY (or 'betweenness') is a measure of how important
                    the individual is to the connectedness of the network; in
                    essence, it measures how many other pairs of nodes are connected
                    through this individual."),
                   br(),

                   h4("DEGREE refers to the total number of connections that an
                    individual has in the network"),
                   br(),

                   # h4("CROSS-GROUP DEGREE refers to the total number of connections
                   #     that an individual has to those in different  Round Table
                   #     affiliations."),
                   # br(),

                   h4("NORMALIZED (or 'relative') measures take the chosen metric
                   and subtract the overall average across all individuals in the
                   network, then divide by the standard deviation.  This gives
                   a measure of the importance of the individual relative to the
                   overall network structure."),
                   br(),

                   # Choose metric
                   radioButtons('metric',
                                'Metric:',
                                choices = c(
                                  "Centrality" = "Centrality",
                                  "Normalized Centrality" = "Centrality.Normalized",
                                  "Degree" = "Degree",
                                  "Normalized Degree" = "Degree.Normalized"#,
                                  #"Cross-Group Degree" = "Cross.Degree",
                                  #"Normalized Cross-Group Degree" = "Normalized.Cross.Degree",
                                )
                   ),

                   # h4("Depending on the number of individuals and the range of time
                   # chosen, computing these metrics to create the plot may be very
                   # slow.
                   #
                   # The centrality and degree over time for all individuals in the
                   # full network (i.e. all individuals and organizations included)
                   # has been pre-computed, to allow for faster visualization."),
                   #
                   # radioButtons('custom',
                   #              'Would you like to use the pre-computed metrics instead?',
                   #              choices = c(
                   #                "Yes, use default full network." = "default",
                   #                "No, keep my choices from the SETUP tab." = "custom"),
                   #              selected = "default"
                   # ),


                   h3("Start of Date Range"),
                   div(style="display: inline-block;vertical-align:top; width: 150px;",
                       selectInput('month_start_2',
                                   'Month',
                                   choices = 1:12,
                                   selected = 1
                       )),
                   div(style="display: inline-block;vertical-align:top; width: 150px;",
                       selectInput('day_start_2',
                                   'Day',
                                   choices = 1:31,
                                   selected = 1
                       )),
                   sliderInput('year_start_2',
                               'Year',
                               value = 1945,
                               min = 1945, max = 1989,
                               sep = ""),
                   h3("End of Date Range"),
                   selectInput('month_end_2',
                               'Month',
                               choices = 1:12
                   ),
                   selectInput('day_end_2',
                               'Day',
                               choices = 1:31
                   ),
                   sliderInput('year_end_2',
                               'Year',
                               value = 1989,
                               min = 1945, max = 1989,
                               sep = ""),

                   h3("Choose which organizations to include"),

                   pickerInput('org_lines',
                               'Choose orgs:',
                               choices = node_name_choices,
                               options = list(`actions-box` = TRUE),
                               multiple = TRUE
                   ),

                   h3("Color by categories?"),

                   # Color groups
                   radioButtons('color_lines_by_group',
                                'Color by:',
                                choices = c(
                                  "None" = "None",
                                  "Category" = "Category",
                                  "Affiliation" = "Affil")
                   ),

                   h3("Group by categories?"),


                   h3("Line Appearance"),

                   sliderInput('line_size',
                               "Line thickness",
                               value = 1,
                               min = 0, max = 3),

                   ## Go button
                   actionButton("make_line_plot", "Draw line plot", class = "btn-primary"),



                 ),
                 mainPanel(
                   plotOutput('my_line_plot', width = "700px", height = "700px"),
                   dataTableOutput('metric_df')
                 )
        ) #tabset
      ) #tabpanel
    ), #ui
    server = function(input, output, session) {

      #### Setup Options ####

      ## node_include_cat -> Category
      ## node_include_type -> Type
      ## node_include_specific -> Name
      ## edge_include_rt -> RT.Affiliation
      ## edge_include_job -> Profession.Sector
      ## edge_include_specific -> Member.ID

      dat <- eventReactive(input$setup, {

        full_data %>%
          filter(Category %in% input$node_include_cat |
                   Type %in% input$node_include_type |
                   Organization.Name %in% input$node_include_specific) %>%
          filter(RT.Affiliation %in% input$edge_include_rt |
                   Profession.Sector %in% input$edge_include_job |
                   Member.ID %in% input$edge_include_specific) %>%
          adj_affil_list(input$lifelong,
                         input$event_length)
      })

      output$dataset <- renderDataTable(dat())


      #### Get Selected Dates ####
      first_date <- reactive(get_date(input$year1, input$month1, input$day1))

      last_date <- reactive(get_date(input$year1, input$month1, input$day1))

      #### After Setup and Before Network: ####
      ## Narrow down data by date
      ## Get options for drop-downs

      dat_limited <- reactive({
        dat() %>%
          filter(Start.Date <= last_date(),
                 End.Date >= first_date())
      })
#FL
      # nodes_list <- reactive({
      #   dat_limited() %>%
      #     distinct(Member.ID, Full.Name, Last.Name, First.Middle.Name) %>%
      #     mutate(
      #       Name = paste0(Last.Name, ", ", First.Middle.Name)
      #     ) %>%
      #     arrange(Name)
      # })

      nodes_list <- reactive({
        if (input$umb_orgs == "umb"){
          el <- dat_limited() %>%
            distinct(Umbrella) %>%
            arrange(Umbrella)
        } else {
          el <- dat_limited() %>%
            distinct(Org.ID) %>%
            arrange(Org.ID)
        }
        el
      })
#FL
      # node_name_choices <- reactive({
      #   setNames(nodes_list()$Member.ID,
      #            nodes_list()$Name)
      # })

      org_umbrella_name_choices <- reactive({
        setNames(nodes_list()$Org.ID,
                 nodes_list()$Organization)
      })

      # selected_highlight <- NULL
      #
      # observeEvent(input$draw_network, {
      #   selected_highlight <- isolate(input$node_color_specific)
      # })

      # output$node_shape_specific <- renderUI({
      #   pickerInput('node_shape_specific',
      #               'Highlight individual(s):',
      #               choices = node_name_choices(),
      #               options = list(`actions-box` = TRUE),
      #               multiple = TRUE,
      #               selected = selected_highlight
      #   )
      # })

      # output$node_color_specific <- renderUI({
      #   pickerInput('node_color_specific',
      #               'Highlight with color:',
      #               choices = node_name_choices(),
      #               options = list(`actions-box` = TRUE),
      #               multiple = TRUE
      #   )
      # })

      # in server.R create reactiveVal
      current_selection <- reactiveVal(NULL)

      # org_members <- reactive({
      #   dat_limited() %>%
      #     filter(Umbrella.Name %in% input$node_shape_org) %>%
      #     pull(Member.ID) %>%
      #     intersect(nodes_list()$Member.ID)
      # })
#fl
      # node_highlighted <- reactive({
      #   c(input$node_shape_specific,
      #     org_members(),
      #     input$my_network_selected)
      # })
      #
      # # now store your current selection in the reactive value
      # observeEvent(node_highlighted(), {
      #   current_selection(node_highlighted())
      # })
      #
      # #now if you are updating your menu
      # observeEvent(node_name_choices(), {
      #   updatePickerInput(session, inputId = "node_shape_specific",
      #                     choices = node_name_choices(),
      #                     selected = current_selection())
      # })

      node_highlighted <- reactive({
        c(input$node_shape_specific,
          #org_members(),
          input$my_network_selected)
      })

      # now store your current selection in the reactive value
      observeEvent(node_highlighted(), {
        current_selection(node_highlighted())
      })

      #now if you are updating your menu
      observeEvent(org_umbrella_name_choices(), {
        updatePickerInput(session, inputId = "node_shape_specific",
                          choices = org_umbrella_name_choices(),
                          selected = current_selection())
      })

      org_totals <- reactive({
        el <- dat_limited() %>%
          distinct(Org.ID, Member.ID, RT.Affiliation) %>%
          group_by(Org.ID, RT.Affiliation) %>%
          summarize(count = n()) %>%
          pivot_wider(names_from = "RT.Affiliation", values_from = "count", values_fill = 0) %>%
          group_by(Org.ID) %>%
          summarize(across(.cols = c("Opposition", "Church", "Government", "Expert"), sum)) %>%
          mutate(Total = Opposition + Church + Government + Expert)
        el[is.na(el)] <- 0
        el %>%
          mutate(Affil = case_when(
            Opposition > Government ~ "Opposition",
            Government > Opposition ~ "Government",
            Opposition == 0 & Government == 0 ~ "None",
            Opposition == Government ~ "Neutral"
          ))
      }) %>%
        bindEvent(input$make_network)

      umb_totals <- reactive({
        el <- dat_limited() %>%
          distinct(Umbrella, Member.ID, RT.Affiliation) %>%
          group_by(Umbrella, RT.Affiliation) %>%
          summarize(count = n()) %>%
          pivot_wider(names_from = "RT.Affiliation", values_from = "count", values_fill = 0) %>%
          group_by(Umbrella) %>%
          summarize(across(.cols = c("Opposition", "Church", "Government", "Expert"), sum)) %>%
          mutate(Total = Opposition + Church + Government + Expert)
        el[is.na(el)] <- 0
        el %>%
          mutate(Affil = case_when(
            Opposition > Government ~ "Opposition",
            Government > Opposition ~ "Government",
            Opposition == 0 & Government == 0 ~ "None",
            Opposition == Government ~ "Neutral"
          ))
      }) %>%
        bindEvent(input$make_network)

      # org_affil <- reactive({
      #   org_totals() %>%
      #     mutate(Affil = case_when(
      #       Opposition > Government ~ "Opposition",
      #       Government > Opposition ~ "Government",
      #       Opposition == 0 & Government == 0 ~ "None",
      #       Opposition == Government ~ "Neutral"
      #     ))
      # }) %>%
      #   bindEvent(input$make_network)


      my_edgelist <- reactive({

        #### Make Graph ####
        ## reactive: first_date
        ## reactive: last_date
        if (input$umb_orgs == "umb"){
          el <- dat_limited() %>%
            get_edgelist_umb(input$weight_by,
                              umb_totals(),
                              start = first_date(),
                              end = last_date(),
                              min_cons = input$min_edges)

        } else if (input$umb_orgs == "sub") {
          el <- dat_limited() %>%
            get_edgelist_orgs(input$weight_by,
                             org_totals(),
                             start = first_date(),
                             end = last_date(),
                             min_cons = input$min_edges)
        } else { #custom
          mass_orgs = dat_limited() %>%
            filter(Category == "Mass Organization") %>%
            pull(Umbrella) %>%
            unique()

          general <- dat_limited() %>%
            filter(Umbrella %in% mass_orgs) %>%
            group_by(Member.ID, Umbrella) %>%
            mutate(
              has_sub = any(!is.na(Subgroup)),
              member_type = case_when(
                is.na(Subgroup) & !has_sub ~ "General Member",
                TRUE ~ "In Sub"
              )
            ) %>%
            distinct(Member.ID, Umbrella, member_type)

          dat2 <- dat_limited() %>%
            left_join(general, by = c("Member.ID", "Umbrella")) %>%
            mutate(Subgroup = ifelse(member_type == "General Member", "General Member", Subgroup)) %>%
            filter(!(is.na(Subgroup) & Category == "Mass Organization")) %>%
            select(-member_type)

          el <- dat2 %>%
            get_edgelist_orgs(input$weight_by,
                              org_totals(),
                              start = first_date(),
                              end = last_date(),
                              min_cons = input$min_edges,
                              custom = TRUE,
                              mass_weights = input$mass_weights)
        }

        if (input$min_size > 1){
          if (input$umb_orgs == "umb"){
            drop_size <- umb_totals() %>%
              filter(Total >= input$min_size) %>%
              pull(Umbrella)
          } else {
            drop_size <- org_totals() %>%
              filter(Total >= input$min_size) %>%
              pull(Org.ID)
          }

          el <- el %>%
            filter(from %in% drop_size) %>%
            filter(to %in% drop_size)
        }

        if (input$min_cons > 0){
          drop_cons <- el %>%
            filter(from != to) %>%
            group_by(from) %>%
            summarise(count = n()) %>%
            filter(count >= input$min_cons) %>%
            pull(from)

          el <- el %>%
            filter(from %in% drop_cons) %>%
            filter(to %in% drop_cons)
        }

        if (input$weight_by != "Ratio"){
          el <- el %>%
            mutate(
              weight = log(weight + 1, base = max(weight))/10
            ) # scale edge weights to have better visuals
        }
        el
      }) %>%
        bindEvent(input$make_network)


      nodes_list_ext <- reactive({
        if (input$umb_orgs == "umb"){
          el <- nodes_list() %>%
            left_join(umb_totals(), by = "Umbrella")
        } else {
          el <- nodes_list() %>%
            left_join(org_totals(), by = "Org.ID")
        }
        el
      })%>%
        bindEvent(input$make_network)

      umbrella_meta_info <- reactive({
        organization_meta_info %>%
          distinct(Umbrella, Umbrella.Name)
      })



      #### Calculate layout ####
      prev_layout <- NULL

      my_node_layout <- reactive({
        set.seed(1989)
        if (input$umb_orgs == "umb"){
          el <- get_layout_df(my_edgelist() %>%
                                select(from, to, weight),
                              node_meta = nodes_list_ext(),
                              node_var = node_var,
                              weight_col = "weight",
                              prev_layout = prev_layout,
                              algorithm = input$network_layout) %>%
            left_join(umbrella_meta_info(), by = c("name" = "Umbrella")) %>%
            left_join(umb_totals(), by = c("name" = "Umbrella")) %>%
            mutate(node_title = paste0(Umbrella.Name,
                                       "\nTotal: ", Total,
                                       "\nID: ", name,
                                       "\nO: ", Opposition, ", G: ", Government,
                                       "\nE: ", Expert, ", C: ", Church)) %>%
            mutate(
              None = "1",  # so that if "None" is selected, things don't change
            )
        } else {
          el <- get_layout_df(my_edgelist() %>%
                          select(from, to, weight),
                        node_meta = nodes_list_ext(),
                        node_var = node_var,
                        weight_col = "weight",
                        prev_layout = prev_layout,
                        algorithm = input$network_layout) %>%
            left_join(organization_meta_info, by = c("name" = "Org.ID")) %>%
            left_join(org_totals(), by = c("name" = "Org.ID")) %>%
            mutate(node_title = paste0(Organization.Name,
                                       "\nTotal: ", Total,
                                       "\nID: ", name,
                                       "\nO: ", Opposition, ", G: ", Government,
                                       "\nE: ", Expert, ", C: ", Church)) %>%
            mutate(
              None = "1",  # so that if "None" is selected, things don't change
            )
        }
        el
      })


      #### Set upnode appearance ####


      ## node_color_by_group: "None" or column name
      ## node_shape_by_group: "None" or column name
      ## node_color_specific: a Member.ID (matches "name")
      ## node_shape_specific: a Member.ID

      node_colors <- reactive({

        if (!is.null(input$node_color_specific)) {

          n <- length(unique(input$node_color_specific))
          these_cols <- ggcolors(n)
          cols <- rep("black", nrow(my_node_layout()))
          vals <- rep("Node", nrow(my_node_layout()))

          for (i in 1:n) {
            org <- input$node_color_specific[i]
            here <- my_node_layout()$name == org
            cols[here] <- these_cols[i]
            vals[here] <- my_node_layout()$Organization.Name[here]
          }
        } else {

          vals <- my_node_layout()[[input$node_color_by_group]]

          vals_num <- vals %>%
            factor() %>%
            as.integer()

          if (input$node_color_by_group == "Affil") {
            cols <- type_cat_cols[vals]
          } else if (input$node_color_by_group == "None") {

            cols <- rep(input$node_color, length(vals))
            names(cols) <- vals

          } else {
            cols <- ggcolors(max(vals_num))[vals_num]
            names(cols) <- vals
          }

        }

        cols

      })

      # node_shapes <- reactive({
      #
      #   shapes = rep(19, nrow(my_node_layout()))
      #
      #   if (!is.null(input$node_shape_specific)) {
      #     shapes[my_node_layout()$name == input$node_shape_specific] = 17
      #   }
      #
      #   shapes
      #
      # })


      # node_sizes <- reactive({
      #
      #   sizes <- rep(input$node_size, nrow(my_node_layout()))
      #   all_highlighted <- c(input$node_shape_specific, input$node_color_specific)
      #
      #   if (!is.null(all_highlighted)) {
      #     sizes[my_node_layout()$name %in% all_highlighted] = 3*input$node_size
      #   }
      #
      #   sizes
      #
      # })



      #### Set up edge locations and info ####

      my_edgelist_locs <- reactive({

        el <- my_edgelist() %>%
          left_join(my_node_layout() %>% rename_all(~paste0(.x,"_from")),
                    by = c("from" = "name_from")) %>%
          left_join(my_node_layout() %>% rename_all(~paste0(.x,"_to")),
                    by = c("to" = "name_to")) %>%
          mutate(edge_title = paste0("Between: ", from, " ", to,
                                     "\nTotal: ", num_members,
                                     "\nO: ", Opposition_Cons, ", G: ", Government_Cons,
                                     "\nE: ", Expert_Cons, ", C: ", Church_Cons))
        el
      })



      #### Set up edge location and appearance ####
      ## edge_color_cross: T/F
      ## edge_size_weight: T/F

      edge_colors <- reactive({

        colors <- rep("black", nrow(my_edgelist_locs()))

        # if (input$edge_color_cross == "Yes") {
        #
        #   colors[(my_edgelist_locs()$`RT.Affiliation_from` != my_edgelist_locs()$`RT.Affiliation_to`)] = "purple"
        #
        # }

        colors

      })

      edge_weights <- reactive({
        if (input$edge_size_weight == "No") {

          1*input$edge_width

        } else {

          my_edgelist_locs()$weight*input$edge_width

        }

      })

      #output$test <- renderTable(my_edgelist_details())


      #### Plot it ####

      my_title <- reactive({
        format(first_date(), "%b %d, %Y")
      }) %>%
        bindEvent(input$make_network)

      output$my_network <- renderGirafe({

        p <- my_node_layout() %>%
          ggplot() +
          geom_segment(data = my_edgelist_locs(),
                       aes(x = x_from, y = y_from,
                           xend = x_to, yend = y_to),
                       alpha = input$edge_transparency,
                       color = edge_colors(),
                       linewidth = edge_weights()) +
          geom_segment_interactive(data = my_edgelist_locs(),
                                   aes(x = x_from, y = y_from,
                                       xend = x_to, yend = y_to,
                                       tooltip = edge_title),
                                   alpha = 0,
                                   color = "black",
                                   linewidth = edge_weights()*10) +
          geom_point_interactive(aes(x = x, y = y,
                                     tooltip = node_title,
                                     color = names(node_colors()),
                                     data_id = name),
                                 #color = node_colors(),
                                 #shape = node_shapes(),
                                 size = input$node_size/10
          ) +
          ggstar::geom_star(data = my_node_layout() %>%
                              filter(name %in% node_highlighted()),
                            aes(x = x, y = y),
                            fill = "lightyellow",
                            size = input$node_size/5) +
          # geom_label(data = my_node_layout() %>%
          #              filter(name %in% input$my_network_selected),
          #            aes(x = x, y = y, label = Full.Name),
          #            fill = "lightyellow",
          #            nudge_x = 1,
          #            nudge_y = 1,
          #            size = input$node_size) +
          theme_void() +
          theme(aspect.ratio=1,
                legend.position="bottom",
                legend.box.background = element_rect(colour = "black"),
                legend.margin=margin(c(1,5,5,5))) +
          ggtitle(my_title()) +
          # geom_blank(aes(color = "Edge Colors")) +
          scale_color_manual(name = "",
                             values = node_colors()) #+
        # scale_color_manual(name = "Node Colors",
        #                    values = node_colors()) +
        # scale_shape_manual(name = "Highlighted",
        #                    values = node_shapes())


        if (input$node_color_by_group == "None") {
          p <- p + theme(legend.position = "none")
        }

        girafe(ggobj = p) %>%
          girafe_options(
            opts_zoom(min = .5, max = 5),
            opts_tooltip(use_fill = TRUE,
                         use_stroke = TRUE),
            opts_selection(type = "multiple")
          )
      })

      output$selected_node_info <- renderDataTable({

        dat_limited() %>%
          filter(Org.ID %in% node_highlighted()) %>%
          arrange(Organization.Name) %>%
          select(Organization.Name, Full.Name, RT.Affiliation, Start.Date, End.Date)

      })

      ########### Panel 3: Line Plots ############

      #### Get Selected Dates ####
      first_date_2 <- reactive(get_date(input$year_start_2, input$month_start_2, input$day_start_2))

      last_date_2 <- reactive(get_date(input$year_end_2, input$month_end_2, input$day_end_2))

      all_metrics_df <- reactive({

        dat <- get_all_metrics_orgs(affiliation_dates,
                                    input$weight_by,
                                    org_totals(),
                                    min_cons = input$min_edges)

        dat
      }) %>%
        bindEvent(input$setup)

      metric_df <- reactive({
        if (input$compute == "compute"){
          dat <- all_metrics_df() %>%
            filter(Org.ID %in% input$org_lines,
                   Start.Date <= last_date_2(),
                   End.Date >= first_date_2()) %>%
            left_join(organization_meta_info, by = "Org.ID")%>%
            left_join(org_totals(), by = "Org.ID")
        } else {
          dat <- all_metrics_by_month_orgs %>%
            filter(Org.ID %in% input$org_lines,
                   Start.Date <= last_date_2(),
                   End.Date >= first_date_2()) %>%
            left_join(organization_meta_info, by = "Org.ID")%>%
            left_join(org_totals(), by = "Org.ID")
        }

        dat$Selected.Metric = dat[[input$metric]]

        dat

      }) %>%
        bindEvent(input$make_line_plot)


      output$metric_df <- renderDataTable({
        metric_df() %>%
          dplyr::filter(!is.na(Selected.Metric)) %>%
          select(Org.ID, Organization.Name, input$metric, Start.Date, End.Date)
      }) %>%
        bindEvent(input$make_line_plot)


      output$my_line_plot <- renderPlot({

        p <- metric_df() %>%
          plot_metric(metric_col = Selected.Metric,
                      group_col = Org.ID) +
          geom_line(linewidth = input$line_size)

        if (input$color_lines_by_group != "None") {
          p <- p + aes_string(color = input$color_lines_by_group,
                              linetype = Org.ID)
        }



        pretty_names <- c(
          "Centrality" = "Betweenness centrality",
          "Centrality.Normalized" = "Betweenness centrality (normalized each month)",
          "Degree" = "Total degree",
          "Degree.Normalized" = "Degree (normalized each month)",
          "Cross.Degree" = "Cross-group degree (based on RT affiliation)",
          "Normalized.Cross.Degree" = "Cross-group degree (normalized each month)"
        )

        len_range <- difftime(last_date_2(), first_date_2())/30
        if (len_range < 36) {
          my_breaks <- "1 month"
          my_date_label <- "%B %Y"
        } else if (len_range < 120) {
          my_breaks <- "6 months"
          my_date_label <- "%B %Y"
        } else {
          my_breaks <- "1 year"
          my_date_label <- "%Y"
        }

        p +
          theme_minimal() +
          labs(
            title = glue::glue("{pretty_names[input$metric]} over time"),
            x = "",
            y = ""
          ) +
          scale_x_date(date_breaks = my_breaks,
                       date_labels = my_date_label) +
          theme(
            axis.text.x = element_text(angle = 45, vjust = 1.2, hjust=1)
          )


      }) %>% bindEvent(input$make_line_plot)


    } #server
  ) #shinyapp

} #function

