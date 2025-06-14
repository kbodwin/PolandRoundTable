#' @import shiny shinyWidgets shinythemes
#' @import igraph tidygraph ggraph graphlayouts
#' @import plotly
#' @import devtools
#' @import dplyr tidyr
#' @import ggiraph
#' @export
run_network_app <- function() {

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

  node_var <- "Member.ID"
  node_labels <- "Full.Name"
  edge_var <- "Org.ID"
  edge_labels <- "Organization.Name"

  full_data <- affiliation_dates %>%
    select(Member.ID, Org.ID, Start.Date, End.Date) %>%
    dplyr::left_join(member_meta_info) %>%
    dplyr::left_join(organization_meta_info)

  node_name_choices <- member_meta_info %>%
    mutate(
      Name = paste0(Last.Name, ", ", First.Middle.Name)
    ) %>%
    get_opts_list(Member.ID, labels = "Name")

  shinyApp(
    ui = tagList(
      navbarPage(
        "Polish Round Table Network",
        theme = "journal",
        tabPanel("Setup",
                 sidebarPanel(

                   ## Go button
                   actionButton("setup", "Done with Setup", class = "btn-primary"),

                   h3("Choose which individuals will be shown in the network."),

                   h4("These choices are combined; for example, if you select
                        a profession and a specific person, that specific person will be
                        included whether or not they have that profession."),

                   pickerInput('node_include_rt',
                               'Round Table affiliations to include:',
                               choices = mem_rt_choices,
                               options = list(`actions-box` = TRUE),
                               multiple = TRUE,
                               selected = mem_rt_choices
                   ),


                   pickerInput('node_include_job',
                               'Professions to include:',
                               choices = mem_job_choices,
                               options = list(`actions-box` = TRUE),
                               multiple = TRUE
                   ),

                   pickerInput('node_include_specific',
                               'Specific individuals to include:',
                               choices = node_name_choices,
                               options = list(`actions-box` = TRUE,
                                              liveSearch = TRUE),
                               multiple = TRUE
                   ),

                   h3("Choose which organizations will be used to create edge connections."),

                   h4("These choices are combined; for example, if you select
                        a category and a specific org, that specific org will be
                        included whether or not it is in the category."),

                   pickerInput('edge_include_cat',
                               'Categories of organization to include:',
                               choices = org_cat_choices,
                               options = list(`actions-box` = TRUE),
                               multiple = TRUE,
                               selected = org_cat_choices
                   ),

                   pickerInput('edge_include_type',
                               'Types of organizations to include:',
                               choices = org_type_choices,
                               options = list(`actions-box` = TRUE,
                                              liveSearch = TRUE),
                               multiple = TRUE
                   ),

                   pickerInput('edge_include_specific',
                               'Specific organization to include:',
                               choices = org_name_choices,
                               options = list(`actions-box` = TRUE,
                                              liveSearch = TRUE),
                               multiple = TRUE
                   ),

                   h3("Choose how edges will be computed and weighted."),

                   # How to compute edge weights
                   selectInput('edge_type',
                               'One edge per organization, or bonus points for umbrella groups?',
                               choices = c("Subgroup + Umbrella group" = "group_labs",
                                           "Organization ID" = "org_id"
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
                                  "Round Table Affiliation" = "RT.Affiliation",
                                  "Profession" = "Profession.Sector",
                                  "Gender" = "Gender")
                   ),

                   pickerInput('node_shape_specific',
                               'Highlight individuals:',
                               choices = node_name_choices,
                               options = list(`actions-box` = TRUE),
                               multiple = TRUE
                   ),

                   pickerInput('node_shape_org',
                               'Highlight members of an organization:',
                               choices = org_umbrella_name_choices,
                               options = list(`actions-box` = TRUE),
                               multiple = TRUE
                   ),

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

                   # Color groups
                   radioButtons('edge_color_cross',
                                'Different colors for cross-connections?',
                                choices = c(
                                  "No",
                                  "Yes")
                   ),

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

                   h4("BETWEENNESS (or centrality) is a measure of how important
                    the individual is to the connectedness of the network; in
                    essence, it measures how many other pairs of nodes are connected
                    through this individual."),
                   br(),

                   h4("DEGREE refers to the total number of connections that an
                    individual has in the network"),
                   br(),
                   
                   h4("CROSS BETWEENNEESS is a custom measure which is similar to betweenness
                      but instead is calculated using only Government-Opposition pairs."),
                   br(),
                   
                   h4("CLUSTER-NORMALIZED CROSS BETWEENNEESS normalizes cross betweenness
                      using the sizes of Louvain clusters being bridged; it emphasizes uniqueness
                      and scales down arbitrary score increases due to an organization's size."),
                   br(),

                   # h4("CROSS-GROUP DEGREE refers to the total number of connections
                   #     that an individual has to those in different  Round Table
                   #     affiliations."),
                   # br(),

                   h4("STANDARDIZED (or 'relative') measures take the chosen metric
                   and subtract the overall average across all individuals in the
                   network, then divide by the standard deviation.  This gives
                   a measure of the importance of the individual relative to the
                   overall network structure."),
                   br(),

                   # Choose metric
                   radioButtons('metric',
                                'Metric:',
                                choices = c(
                                  "Betweenness" = "Centrality",
                                  "Standardized Betweenness" = "Centrality.Normalized",
                                  "Degree" = "Degree",
                                  "Standardized Degree" = "Degree.Normalized",
                                  "Cross Betweenness" = "CrossBetweenness",
                                  "Cluster-Normalized Cross Betweenness" = "Cross.Betweenness.Norm"#,
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
                  div(style="display: inline-block;vertical-align:top; width: 150px;",
                      selectInput('month_end_2',
                                  'Month',
                                  choices = 1:12)
                  ),
                  div(style="display: inline-block;vertical-align:top; width: 150px;",
                      selectInput('day_end_2',
                                  'Day',
                                  choices = 1:31)
                  ),
                  sliderInput('year_end_2',
                              'Year',
                              value = 1989,
                              min = 1945, max = 1989,
                              sep = ""),

                   h3("Choose which individuals to include"),

                  # Search box with Select button
                  div(
                    style = "display: flex; gap: 10px; align-items: flex-start;",
                    div(
                      style = "flex-grow: 1;",
                      textInput("member_search", "Search for an individual by name:")
                    ),
                    actionButton("confirm_search",
                                 "Select",
                                 class = "btn-secondary",
                                 style = "height: 38px; margin-top: 25px;")
                  ),
                  
                  # Dropdown with Clear button
                  div(
                    style = "display: flex; gap: 10px; align-items: flex-start;",
                    div(
                      style = "flex-grow: 1; min-width: 0;",
                      pickerInput(
                        'person_lines',
                        'Selected individuals:',
                        choices = node_name_choices,
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )
                    ),
                    actionButton(
                      "clear_person_lines",
                      "Clear",
                      class = "btn-secondary",
                      style = "height: 38px; white-space: nowrap; margin-top: 25px;"
                    )
                  ),

                   h3("Color by categories?"),

                   # Color groups
                   radioButtons('color_lines_by_group',
                                'Color by:',
                                choices = c(
                                  "None" = "None",
                                  "Round Table Affiliation" = "RT.Affiliation",
                                  "Profession" = "Profession.Sector",
                                  "Gender" = "Gender")
                   ),

                   h3("Group by categories?"),

                   # Color groups
                   radioButtons('group_lines',
                                'One line for each:',
                                choices = c(
                                  "None" = "None",
                                  "Round Table Affiliation" = "RT.Affiliation",
                                  "Profession" = "Profession.Sector",
                                  "Gender" = "Gender")
                   ),

                   h3("Line Appearance"),

                   sliderInput('line_size',
                               "Line thickness",
                               value = 1,
                               min = 0, max = 3),

                   ## Go button
                   actionButton("make_line_plot", "Draw line plot", class = "btn-primary"),



                 ),
                 
                 # Plots, tables, and download buttons -- will be hidden if there isn't anything to display yet
                 mainPanel(
                   plotOutput('my_line_plot', width = "700px", height = "700px"),
                   conditionalPanel(
                     condition = "input.make_line_plot > 0",
                     downloadButton("download_plot", "Download Plot (PNG)")
                   ),
                   
                   # Affiliation table and download
                   conditionalPanel(
                     condition = "output.affiliation_df_ready",
                     h3("Institutional Affiliations"),
                     dataTableOutput("affiliation_table"),
                     downloadButton("download_affiliation_table", "Download Affiliations CSV")
                   ),
                   
                   # Metric table and download
                   conditionalPanel(
                     condition = "output.metric_df_ready",
                     div(
                       h3("Metric Values Over Time"),
                       checkboxInput("aggregate_metrics", "Aggregate over time", value = FALSE),
                       checkboxInput("show_all_metrics", "Show all metrics", value = FALSE)
                     ),
                     dataTableOutput("metric_df"),
                     downloadButton("download_metric_df", "Download Metric CSV")
                   )
                 )
                 
        ) #tabset
      ), #tabpanel
    
    # Pressing enter key will select entered member in search box
    tags$script(HTML("
      $(document).on('keypress', function(e) {
        if (e.which == 13 && $('#member_search').is(':focus')) {
          setTimeout(function() {
            $('#confirm_search').click();
          }, 150); // delay in ms allows input to register
        }
      });
    "))

    ), #ui
    server = function(input, output, session) {

      #### Setup Options ####

      ## edge_include_cat -> Category
      ## edge_include_type -> Type
      ## edge_include_specific -> Name
      ## node_include_rt -> RT.Affiliation
      ## node_include_job -> Profession.Sector
      ## node_include_specific -> Member.ID

      dat <- eventReactive(input$setup, {

        full_data %>%
          filter(Category %in% input$edge_include_cat |
                   Type %in% input$edge_include_type |
                   Organization.Name %in% input$edge_include_specific) %>%
          filter(RT.Affiliation %in% input$node_include_rt |
                   Profession.Sector %in% input$node_include_job |
                   Member.ID %in% input$node_include_specific) %>%
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

      nodes_list <- reactive({
        dat_limited() %>%
          distinct(Member.ID, Full.Name, Last.Name, First.Middle.Name) %>%
          mutate(
            Name = paste0(Last.Name, ", ", First.Middle.Name)
          ) %>%
          arrange(Name)
      })

      node_name_choices <- reactive({
        setNames(nodes_list()$Member.ID,
                 nodes_list()$Name)
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

      org_members <- reactive({
        dat_limited() %>%
          filter(Umbrella.Name %in% input$node_shape_org) %>%
          pull(Member.ID) %>%
          intersect(nodes_list()$Member.ID)
      })

      node_highlighted <- reactive({
        c(input$node_shape_specific,
          org_members(),
          input$my_network_selected)
      })

      # now store your current selection in the reactive value
      observeEvent(node_highlighted(), {
        current_selection(node_highlighted())
      })

      #now if you are updating your menu
      observeEvent(node_name_choices(), {
        updatePickerInput(session, inputId = "node_shape_specific",
                          choices = node_name_choices(),
                          selected = current_selection())
      })




      my_edgelist <- reactive({

        #### Make Graph ####
        ## reactive: first_date
        ## reactive: last_date
        ## input: edge_type = group_labs or org_id

        if (input$edge_type == "group_labs") {

          el <- dat_limited() %>%
            arrange(Umbrella, Subgroup) %>%
            get_edgelist_members(on_cols = c("Umbrella", "Subgroup"),
                                 start = first_date(),
                                 end = last_date())

        } else if (input$edge_type == "org_id") {

          el <- dat_limited() %>%
            arrange(Org.ID) %>%
            get_edgelist_members(on_cols = list("Org.ID"),
                                 start = first_date(),
                                 end = last_date())

        }

        el %>%
          mutate(
            weight = log(weight + 1, base = max(weight))/10
          ) # scale edge weights to have better visuals

      }) %>%
        bindEvent(input$make_network)

      #### Calculate layout ####
      prev_layout <- NULL

      my_node_layout <- reactive({
        set.seed(1989)
        get_layout_df(my_edgelist() %>%
                        select(from, to, weight),
                      node_meta = nodes_list(),
                      node_var = node_var,
                      weight_col = "weight",
                      prev_layout = prev_layout,
                      algorithm = input$network_layout) %>%
          left_join(member_meta_info, by = c("name" = "Member.ID")) %>%
          mutate(
            None = "1",  # so that if "None" is selected, things don't change
          )
      })

      observeEvent(my_node_layout(), {
        prev_layout <- isolate(my_node_layout())
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
            mem <- input$node_color_specific[i]
            here <- my_node_layout()$name == mem
            cols[here] <- these_cols[i]
            vals[here] <- my_node_layout()$Full.Name[here]
          }
        } else {

          vals <- my_node_layout()[[input$node_color_by_group]]

          vals_num <- vals %>%
            factor() %>%
            as.integer()

          if (input$node_color_by_group == "RT.Affiliation") {
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

        my_edgelist() %>%
          left_join(my_node_layout() %>% rename_all(~paste0(.x,"_from")),
                    by = c("from" = "name_from")) %>%
          left_join(my_node_layout() %>% rename_all(~paste0(.x,"_to")),
                    by = c("to" = "name_to"))

      })



      #### Set up edge location and appearance ####
      ## edge_color_cross: T/F
      ## edge_size_weight: T/F

      edge_colors <- reactive({

        colors <- rep("black", nrow(my_edgelist_locs()))

        if (input$edge_color_cross == "Yes") {

          colors[(my_edgelist_locs()$`RT.Affiliation_from` != my_edgelist_locs()$`RT.Affiliation_to`)] = "purple"

        }

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
                                       tooltip = edge_orgs),
                                   alpha = 0,
                                   color = "black",
                                   linewidth = edge_weights()*10) +
          geom_point_interactive(aes(x = x, y = y,
                                     tooltip = Full.Name,
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
          filter(Member.ID %in% node_highlighted()) %>%
          arrange(Last.Name) %>%
          select(Full.Name, RT.Affiliation, Organization.Name, Start.Date, End.Date)

      })

      ########### Panel 3: Line Plots ############
      
      
      # To enable fuzzy matching when searching polish names
      remove_accents <- function(x) {
        iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
      }

      # To be used for saving plots and tables
      get_filename <- function() {
        # Only take the first n names to avoid file name length issues
        name_num <- 10
        names_vec <- metric_df() %>%
          filter(!is.na(Selected.Metric)) %>%
          pull(Full.Name) %>%
          unique() %>%
          na.omit() %>%
          head(name_num)
        names_ascii <- iconv(names_vec, from = "UTF-8", to = "ASCII//TRANSLIT")
        names_safe <- names_ascii %>%
          gsub("[^A-Za-z0-9]+", "_", .) %>%
          paste(collapse = "_")
        if (identical(names_safe, "")) names_safe <- "data"
        paste0(names_safe, "_", input$metric, "_", first_date_2(), "_", last_date_2())
      }

      observeEvent(input$confirm_search, {
        req(input$member_search)
        
        # Split search input into cleaned lowercase terms
        search_terms <- strsplit(input$member_search, ",")[[1]] |>
          trimws() |>
          tolower() |>
          remove_accents()
        
        matched_ids <- member_meta_info %>%
          mutate(
            FullName = tolower(remove_accents(paste(First.Middle.Name, Last.Name)))
          ) %>%
          filter(
            Reduce(`|`, lapply(search_terms, function(term) grepl(term, FullName)))
          ) %>%
          pull(Member.ID)
        
        if (length(matched_ids) > 0) {
          updatePickerInput(
            session = session,
            inputId = "person_lines",
            selected = unique(c(input$person_lines, matched_ids))
          )
        }
      })


      ## Clear selected members -- tied to clear button
      observeEvent(input$clear_person_lines, {
        updatePickerInput(session, "person_lines", selected = character(0))
      })


      #### Get Selected Dates ####
      first_date_2 <- reactive(get_date(input$year_start_2, input$month_start_2, input$day_start_2))

      last_date_2 <- reactive(get_date(input$year_end_2, input$month_end_2, input$day_end_2))

      # nodes_list_2 <- reactive({
      #   dat_limited_2() %>%
      #     distinct(Member.ID, Full.Name, Last.Name, First.Middle.Name) %>%
      #     mutate(
      #       Name = paste0(Last.Name, ", ", First.Middle.Name)
      #     ) %>%
      #     arrange(Name)
      # })
      #
      # node_name_choices_2 <- reactive({
      #   setNames(nodes_list_2()$Member.ID,
      #            nodes_list_2()$Name)
      # })
      #
      # # in server.R create reactiveVal
      # current_selection_2 <- reactiveVal(NULL)
      #
      # # now store your current selection in the reactive value
      # observeEvent(input$person_lines, {
      #   current_selection_2(input$person_lines)
      # })
      #
      # #now if you are updating your menu
      # observeEvent(node_name_choices_2(), {
      #   updatePickerInput(session, inputId = "person_lines",
      #                     choices = node_name_choices_2(),
      #                     selected = current_selection_2())
      # })
      #

      all_metrics_df <- reactive({
        if (input$edge_type == "group_labs") {

          dat <- get_all_metrics(affiliation_dates,
                                 on_cols = c("Umbrella", "Subgroup"))
        }  else if (input$edge_type == "org_id") {
          dat <- get_all_metrics(affiliation_dates,
                                 on_cols = list("Org.ID"))
        }
        dat
      }) %>%
        bindEvent(input$setup)


      metric_df <- reactive({
        dat <- all_metrics_by_month %>%
          filter(Member.ID %in% input$person_lines,
                 Start.Date <= last_date_2(),
                 End.Date >= first_date_2()) %>%
          left_join(member_meta_info)

        

        dat$Selected.Metric = dat[[input$metric]]

        dat


        #### maybe one day: compute on the fly
        # } else if (input$custom == "custom") {
        #
        #   if (input$edge_type == "group_labs") {
        #
        #       dat() %>%
        #         arrange(Umbrella, Subgroup) %>%
        #         get_betweenness_members(on_cols = list("Umbrella",
        #                                             c("Umbrella", "Subgroup")),
        #                                 members = input$person_lines,
        #                                 start = first_date_2(),
        #                                 end = last_date_2()) %>%
        #       left_join(member_meta_info)
        #
        #     } else if (input$edge_type == "org_id") {
        #
        #       dat() %>%
        #         arrange(Org.ID) %>%
        #         get_betweenness_members(on_cols = list("Org.ID"),
        #                              start = first_date_2(),
        #                              members = input$person_lines,
        #                              end = last_date_2()) %>%
        #         left_join(member_meta_info)
        #
        #     }
        #}

      # }) %>% bindEvent(input$make_line_plot)
      })
      
      affiliation_df <- reactive({
        affiliation_dates %>%
          filter(
            Member.ID %in% input$person_lines,
            Start.Date <= last_date_2(),
            End.Date >= first_date_2()
          ) %>%
          select(
            Full.Name,
            RT.Affiliation,
            Organization.Name,
            Start.Date,
            End.Date
          )
      })
      
      output$affiliation_table <- renderDataTable({
        affiliation_df()
      }) %>%
        bindEvent(input$make_line_plot)
      
      # Helper function for aggregating selected metric df
      compute_avg_metric_df <- function(df, metric_label) {
        df %>%
          mutate(Year = format(as.Date(Start.Date), "%Y")) %>%
          group_by(Full.Name, RT.Affiliation, Year) %>%
          summarise(avg = mean(Selected.Metric, na.rm = TRUE), .groups = "drop") %>%
          group_by(Full.Name, RT.Affiliation) %>%
          summarise("{metric_label}" := mean(avg, na.rm = TRUE), .groups = "drop") %>%
          arrange(desc(.data[[metric_label]]))
      }
  
      # To be used when saving the metric_df
      current_metric_table <- reactiveVal(NULL)
      
      output$metric_df <- renderDataTable({
        df <- metric_df()
        result <- NULL
        if (input$aggregate_metrics) {
          if (input$show_all_metrics) {
            metric_cols <- c("Centrality", "Degree", "Centrality.Normalized", 
                             "Degree.Normalized", "CrossBetweenness", "Cross.Betweenness.Norm")
            
            all_results <- lapply(metric_cols, function(metric) {
              tmp_df <- df %>%
                filter(!is.na(.data[[metric]])) %>%
                mutate(Selected.Metric = .data[[metric]])
              
              metric_label <- paste0("Overall_Avg_", metric)
              compute_avg_metric_df(tmp_df, metric_label)
            })
            
            result <- reduce(all_results, full_join, by = c("Full.Name", "RT.Affiliation"))
          } else {
            metric_label <- paste0("Overall_Avg_", input$metric)
            result <- df %>%
              filter(!is.na(.data[[input$metric]])) %>%
              mutate(Selected.Metric = .data[[input$metric]]) %>%
              compute_avg_metric_df(metric_label)
          }
        } else {
          if (input$show_all_metrics) {
            result <- df %>%
              filter(!is.na(.data[[input$metric]])) %>%
              select(Full.Name, RT.Affiliation,
                     Centrality, Degree, Centrality.Normalized,
                     Degree.Normalized, CrossBetweenness, Cross.Betweenness.Norm,
                     Start.Date, End.Date
              )
          } else {
            result <- df %>%
              filter(!is.na(.data[[input$metric]])) %>%
              select(Full.Name, RT.Affiliation, input$metric, Start.Date, End.Date)
          }
        }
        current_metric_table(result)
        result
      })

      # output$my_line_plot <- renderPlot({
      generate_plot <- function() {
        if (input$group_lines != "None") {
          p <- metric_df() %>%
            plot_metric(metric_col = Selected.Metric,
                        group_col = !!sym(input$group_lines)) +
            geom_line(linewidth = input$line_size)
        } else {
          p <- metric_df() %>%
            plot_metric(metric_col = Selected.Metric,
                        group_col = Full.Name) +
            geom_line(linewidth = input$line_size)

          if (input$color_lines_by_group != "None") {
            p <- p + aes_string(color = input$color_lines_by_group,
                                linetype = Full.Name)
          }
        }

        pretty_names <- c(
          "Centrality" = "Betweenness centrality",
          "Centrality.Normalized" = "Betweenness centrality (normalized each month)",
          "Degree" = "Total degree",
          "Degree.Normalized" = "Degree (normalized each month)",
          "Cross.Degree" = "Cross-group degree (based on RT affiliation)",
          "Normalized.Cross.Degree" = "Cross-group degree (normalized each month)",
          "CrossBetweenness" = "Cross betweenness (across factions)",
          "Cross.Betweenness.Norm" = "Cluster-normalized cross betweenness (using Louvain clusters)"
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
      }
      
      output$affiliation_table <- renderDataTable({
        affiliation_df()
      })

      current_plot <- reactiveVal(NULL)
      plot_filename_prefix <- reactiveVal("plot")
      
      output$my_line_plot <- renderPlot({
        plot_obj <- generate_plot()

        # Save selected options to be used as a filename if plot is saved
        prefix <- get_filename()
        plot_filename_prefix(prefix)

        # save the current plot so that it may be tied to download plot button
        current_plot(plot_obj)
        plot_obj
      }) %>% bindEvent(input$make_line_plot)

      # Controls visibility of the plot + download button
      output$plot_ready <- reactive({
        nrow(metric_df() %>% dplyr::filter(!is.na(Selected.Metric))) > 0
      })
      outputOptions(output, "plot_ready", suspendWhenHidden = FALSE)

      
      ### Download handlers
      # Plot download
      output$download_plot <- downloadHandler(
        filename = function() {
        paste0(plot_filename_prefix(), ".png")
        },
        content = function(file) {
          ggsave(
            filename = file,
            plot = current_plot(),
            width = 10,
            height = 6,
            dpi = 300,
            bg = "white"
          )
        }
      )
      
      # CSV download: metric_df
      output$download_metric_df <- downloadHandler(
        filename = function() {
          suffix <- if (input$aggregate_metrics) {
            if (input$show_all_metrics) "_aggregated_all" else "_aggregated"
          } else {
            ""
          }
          paste0(get_filename(), suffix, ".csv")
        },
        content = function(file) {
          write.csv(current_metric_table(), file, row.names = FALSE)
        }
      )
      
      # CSV download: affiliation_df
      output$download_affiliation_table <- downloadHandler(
        filename = function() {
          paste0(get_filename(), "_affiliations.csv")
        },
        content = function(file) {
          write.csv(affiliation_df(), file, row.names = FALSE)
        }
      )

      # Show/hide download buttons and tables only when data is available
      output$affiliation_df_ready <- reactive({
        nrow(affiliation_df()) > 0
      })
      outputOptions(output, "affiliation_df_ready", suspendWhenHidden = FALSE)

      output$metric_df_ready <- reactive({
        nrow(metric_df() %>% dplyr::filter(!is.na(Selected.Metric))) > 0
      })
      outputOptions(output, "metric_df_ready", suspendWhenHidden = FALSE)

    } #server
  ) #shinyapp

} #function
