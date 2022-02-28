annotationsPage <- div(id = "annotations_div",
                       conditionalPanel(
                         condition = "input.availableNetworks == 0",
                         bsAlert("tabUploadMainAlert"),
                         uiOutput("uiLoadGraphOptionsOutput_annotations_tab")
                       ),
                       conditionalPanel(
                         condition = "input.availableAnnotations == 0",
                         bsAlert("tabUploadMainAlert"),
                         uiOutput("uiLoadGraphOptionsOutput_annotations_annotations_tab")
                       ),
                       conditionalPanel(
                         condition = "input.availableExpressions == 0",
                         bsAlert("tabUploadMainAlert3"),
                         uiOutput("uiStoredGraphsOutputSelectUploadExpressions")
                       ),
                       br(),
                       tabsetPanel(
                         tabPanel(
                           "Convex Hulls",
                           br(),
                           tags$div(class = "strategies",
                                    selectInput(
                                      "layouts",
                                      "General layout:",
                                      choices = layouts_ui,
                                      selected = selected_layouts,
                                      multiple = FALSE
                                    ),
                                    selectInput(
                                      "local_layout",
                                      "Local layout:",
                                      choices = layouts_ui,
                                      selected = selected_layouts,
                                      multiple = FALSE
                                    ),
                                    br(),
                                    prettyRadioButtons(
                                      inputId = "convex_layout_strategy",
                                      label = "Layout modification strategy:",
                                      choices = c("Simple layout", "Virtual node per group", "Group gravity", "Supernodes per group"),
                                      thick = TRUE,
                                      shape = "curve",
                                      animation = "pulse",
                                      status = "info"
                                    ),
                                    sliderInput(
                                      inputId = "repeling_force",
                                      label = "Adjust force strength:",
                                      min = 1,
                                      max = 20,
                                      value = 10
                                    )
                           ), # strategies
                           br(),
                           prettyCheckbox(
                             inputId = "show_labels",outline = T,fill = T,bigger = T,
                             label = "Show Labels",
                             thick = TRUE,
                             shape = "curve",
                             animation = "pulse",
                             status = "info",
                             inline = F,
                             value = T
                           ),
                           prettyCheckbox(
                             inputId = "expressions",outline = T,fill = T,bigger = T,
                             label = "Show node - coloring",
                             thick = TRUE,
                             shape = "curve",
                             animation = "pulse",
                             status = "info",
                             inline = F,
                             value = F
                           ),
                           prettyCheckbox(
                             inputId = "some_labels",outline = T,fill = T,bigger = T,
                             label = "Show only labels of selected annotation groups",
                             thick = TRUE,
                             shape = "curve",
                             animation = "pulse",
                             status = "info",
                             inline = F,
                             value = F
                           ),
                           hr(),
                           div(id="interactive_convex_hulls_loader",
                               shinycssloaders::withSpinner(
                                 uiOutput("interactive_convex_hulls")
                               )
                           ),
                           eval(ui_dataTable_panel("chooseGroups")),
                           tags$style(
                             HTML(
                               ".js-irs-3 .irs-bar {border-top-color: #2C8160; border-bottom-color: #2C8160;} .js-irs-3 .irs-bar-edge {border-color: #2C8160;}
                                .js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #2C8160;}"
                             )
                           ),
                           
                           tags$style(
                             HTML(
                               ".js-irs-4 .irs-bar {border-top-color: #2C8160;border-bottom-color: #2C8160;} .js-irs-4 .irs-bar-edge {border-color: #2C8160;}
                                .js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #2C8160;}"
                             )
                           ),
                           
                           tags$style(
                             HTML(
                               ".js-irs-5 .irs-bar {border-top-color: #2C8160; border-bottom-color: #2C8160;} .js-irs-5 .irs-bar-edge {border-color: #2C8160;}
                                .js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: #2C8160;}"
                             )
                           ),
                           
                           div(
                             style = "display:inline-block",
                             sliderInput(
                               inputId = "scaling_coordinates_convex",
                               label = "Scale the coordinates:",
                               min = 1,
                               max = 10,
                               value = 1
                             )
                           ),
                           div(
                             style = "display:inline-block",
                             sliderInput(
                               "scaling_nodes_convex",
                               "Adjust node size:",
                               min = 0.2,
                               max = 5,
                               value = 2,
                               step = 0.2
                             )
                           ),
                           div(
                             style = "display:inline-block",
                             sliderInput(
                               "scaling_labels_convex",
                               "Adjust label size:",
                               min = 0,
                               max = 30,
                               value = 10,
                               step = 2
                             )
                           ),
                           br(),
                           br(),
                           downloadBttn("HTML_convex", "Download HTML file", 
                                        style = "bordered", 
                                        color = "success",
                                        size = "sm",
                                        no_outline = T),
                           br(),
                           br()
                         ),
                         
                         tabPanel(
                           "Pie - Chart Nodes",
                           fluidRow(
                             tags$div(class = "strategies",
                                      selectInput(
                                        "layouts2",
                                        "General layout:",
                                        choices = layouts_ui,
                                        selected = selected_layouts,
                                        multiple = FALSE
                                      ),
                                      selectInput(
                                        "local_layout2",
                                        "Local layout:",
                                        choices = layouts_ui,
                                        selected = selected_layouts,
                                        multiple = FALSE
                                      ),
                                      br(),
                                      prettyRadioButtons(
                                        inputId = "piechart_layout_strategy",
                                        label = "Layout modification strategy:",
                                        choices = c("Simple layout", "Virtual node per group", "Group gravity", "Supernodes per group"),
                                        thick = TRUE,
                                        shape = "curve",
                                        animation = "pulse",
                                        status = "info"
                                      ),
                                      sliderInput(
                                        inputId = "repeling_force2",
                                        label = "Adjust force strength:",
                                        min = 1,
                                        max = 20,
                                        value = 10
                                      )
                             ), # strategies pies
                             hr(),
                             prettyCheckbox(
                               inputId = "show_labels_pies",outline = T,fill = T,bigger = T,
                               label = "Show Labels",
                               thick = TRUE,
                               shape = "curve",
                               animation = "pulse",
                               status = "info",
                               inline = F,
                               value = T
                             ),
                             prettyCheckbox(
                               inputId = "expressions_pies",outline = T,fill = T,bigger = T,
                               label = "Show node - coloring",
                               thick = TRUE,
                               shape = "curve",
                               animation = "pulse",
                               status = "info",
                               inline = F,
                               value = F
                             ),
                             prettyCheckbox(
                               inputId = "some_labels_pies",outline = T,fill = T,bigger = T,
                               label = "Show only labels of selected annotation groups",
                               thick = TRUE,
                               shape = "curve",
                               animation = "pulse",
                               status = "info",
                               inline = F,
                               value = F
                             ),
                             div(id="tabVizPie_charts_loader",
                                 shinycssloaders::withSpinner(
                                   uiOutput('tabVizPie_charts')
                                 )
                             ),
                             eval(ui_dataTable_panel("chooseGroups2")),
                           ),
                           tags$style(
                             HTML(
                               ".js-irs-0 .irs-bar {border-top-color: #2C8160; border-bottom-color: #2C8160;} .js-irs-0 .irs-bar-edge {border-color: #2C8160;}
                                .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #2C8160;}"
                             )
                           ),
                           
                           tags$style(
                             HTML(
                               ".js-irs-1 .irs-bar {border-top-color: #2C8160;border-bottom-color: #2C8160;} .js-irs-1 .irs-bar-edge {border-color: #2C8160;}
                                .js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #2C8160;}"
                             )
                           ),
                           
                           tags$style(
                             HTML(
                               ".js-irs-2 .irs-bar {border-top-color: #2C8160; border-bottom-color: #2C8160;} .js-irs-2 .irs-bar-edge {border-color: #2C8160;}
                                .js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #2C8160;}"
                             )
                           ),
                           
                           div(
                             style = "display:inline-block",
                             sliderInput(
                               "scaling_coordinates_pies",
                               "Scale the coordinates",
                               min = 1,
                               max = 10,
                               value = 1
                             )
                           ),
                           div(
                             style = "display:inline-block",
                             sliderInput(
                               "scaling_nodes_pies",
                               "Adjust node size:",
                               min = 10,
                               max = 30,
                               value = 15
                             )
                           ),
                           div(
                             style = "display:inline-block",
                             sliderInput(
                               "scaling_labels_pies",
                               "Adjust label size:",
                               min = 0,
                               max = 30,
                               value = 10,
                               step = 2
                             )
                           ),
                           
                           
                           hr(),
                           class = 'box-panel-padding',
                           class = 'box-panel',
                           
                           br(),
                           br(),
                           downloadBttn("HTML_pies", "Download HTML file",
                                        style = "bordered", 
                                        color = "success",
                                        size = "sm",
                                        no_outline = T),
                           br(),
                           br()
                           
                         ),
                         
                         tabPanel(
                           "Convex Hulls - 3D",
                           helpText("Select the layout you want in the analysis."),
                           selectInput(
                             "layouts_3D",
                             "3D layout:",
                             choices = layouts_3D,
                             selected = selected_layouts_3d,
                             multiple = FALSE
                           ),
                           prettyCheckbox(
                             inputId = "Dark",outline = T,fill = T,bigger = T,
                             label = "Dark mode",
                             thick = TRUE,
                             shape = "curve",
                             animation = "pulse",
                             status = "info",
                             inline = F,
                             value = F
                           ),
                           prettyCheckbox(
                             inputId = "show_labels_3D",outline = T,fill = T,bigger = T,
                             label = "Show labels on network",
                             thick = TRUE,
                             shape = "curve",
                             animation = "pulse",
                             status = "info",
                             inline = F,
                             value = F
                           ),
                           prettyCheckbox(
                             inputId = "show_some_labels_3D",outline = T,fill = T,bigger = T,
                             label = "Show only labels of selected annotation groups",
                             thick = TRUE,
                             shape = "curve",
                             animation = "pulse",
                             status = "info",
                             inline = F,
                             value = F
                           ),
                           prettyCheckbox(
                             inputId = "expressions_3D",outline = T,fill = T,bigger = T,
                             label = "Show node - coloring",
                             thick = TRUE,
                             shape = "curve",
                             animation = "pulse",
                             status = "info",
                             inline = F,
                             value = F
                           ),
                           div(id="convex_hull_3D_loader",
                               shinycssloaders::withSpinner(
                                 uiOutput("convex_hull_3D")
                               )
                           ),
                           eval(ui_dataTable_panel("chooseGroups_3D")),
                           
                           tags$style(
                             HTML(
                               ".js-irs-0 .irs-bar {border-top-color: #2C8160; border-bottom-color: #2C8160;} .js-irs-0 .irs-bar-edge {border-color: #2C8160;}
                                .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #2C8160;}"
                             )
                           ),
                           
                           tags$style(
                             HTML(
                               ".js-irs-1 .irs-bar {border-top-color: #2C8160;border-bottom-color: #2C8160;} .js-irs-1 .irs-bar-edge {border-color: #2C8160;}
                                .js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #2C8160;}"
                             )
                           ),
                           
                           tags$style(
                             HTML(
                               ".js-irs-2 .irs-bar {border-top-color: #2C8160; border-bottom-color: #2C8160;} .js-irs-2 .irs-bar-edge {border-color: #2C8160;}
                                .js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #2C8160;}"
                             )
                           ),
                           div(
                             style = "display:inline-block",
                             sliderInput(
                               inputId = "scaling_coordinates_convex_3D_X",
                               label = "Scale x coordinates:",
                               min = 1,
                               max = 10,
                               value = 1
                             )
                           ),
                           div(
                             style = "display:inline-block",
                             sliderInput(
                               inputId = "scaling_coordinates_convex_3D_Y",
                               label = "Scale y coordinates:",
                               min = 1,
                               max = 10,
                               value = 1
                             )
                           ),
                           div(
                             style = "display:inline-block",
                             sliderInput(
                               inputId = "scaling_coordinates_convex_3D_Z",
                               label = "Scale z coordinates:",
                               min = 1,
                               max = 10,
                               value = 1
                             )
                           ),
                           
                           br(),
                           br(),
                           downloadBttn("HTML_convex_3D", "Download HTML file",
                                        style = "bordered", 
                                        color = "success",
                                        size = "sm",
                                        no_outline = T),
                           br(),
                           br()
                         ),
                         
                         # tags$a("Large window",target="_blank",href="output_convex_11188.html")
                         tabPanel("Venn Diagrams",
                                  br(),
                                  div(style = "display:inline-block",uiOutput("vennDiagram1")),
                                  div(style = "display:inline-block",uiOutput("vennDiagram2")),
                                  br(),
                                  plotOutput("vennDiagrams",width = 600, height = 600),
                                  div(style = "display:inline-block",eval(ui_dataTable_panel("venn_table1", FALSE))),
                                  div(style = "display:inline-block",eval(ui_dataTable_panel("venn_table2", FALSE))),
                                  div(style = "display:inline-block",eval(ui_dataTable_panel("venn_table_summ", FALSE)))
                                  
                         )
                         
                       ) #tabsetPanel
                       
)
