topologyPage <- div(id = "topology_div",
                    sidebarLayout(
                      sidebarPanel(
                        bsAlert("tabTopologySideAlert"),
                        conditionalPanel(condition = "input.statisticsMethodsMainTabsetPanel == 'tableView'",
                                         uiOutput("uiStoredGraphsOutputSelectTopolopgy")),
                        conditionalPanel(
                          condition = "input.statisticsMethodsMainTabsetPanel == 'plotView'",
                          uiOutput("uiStoredGraphsOutputMultipleSelectTopolopgy")
                        ),
                      ),
                      conditionalPanel(condition = "input.availableNetworks == 0",
                                       mainPanel(
                                         bsAlert("tabTopologyMainAlert"),
                                         tabsetPanel(
                                           id = "statisticsMethodsMainTabsetPanel",
                                           tabPanel(
                                             "Summaries",
                                             value = "tableView",
                                             icon = icon("table"),
                                             div(
                                               div(
                                                 checkboxGroupInput(
                                                   "statistics2",
                                                   "The statistics:",
                                                   choices = statistics,
                                                   selected = selected_statistics
                                                 ),
                                                 eval(ui_dataTable_panel("statres", FALSE)),
                                                 class = 'box-panel-padding',
                                                 class = 'box-panel'
                                               ),
                                               div(
                                                 span(
                                                   actionButton("btnStatSelectAll", "Select all", style = "float: right;"),
                                                   class = "input-group-btn"
                                                 ),
                                                 span(actionButton("btnStatSelectNone", "Clear"), class = "input-group-btn"),
                                                 class = "input-group"
                                               )
                                             )
                                           ),
                                           tabPanel(
                                             "Comparative Plots",
                                             value = "plotView",
                                             icon = icon("chart-bar"),
                                             div(
                                               div(
                                                 radioButtons(
                                                   "statistics",
                                                   "The statistics:",
                                                   choices = statistics,
                                                   selected = selected_statistics
                                                 ),
                                                 uiOutput("statisticsMethodsPlotRender"),
                                                 class = 'box-panel-padding'
                                               ),
                                               class = 'box-panel'
                                             ),
                                             tags$style(
                                               HTML(
                                                 ".js-irs-6 .irs-bar {border-top-color: #2C8160; border-bottom-color: #2C8160;} .js-irs-6 .irs-bar-edge {border-color: #2C8160;}
                                .js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: #2C8160;}"
                                               )
                                             ),
                                             
                                             sliderInput(
                                               "statisticsPlotPercentMagnify",
                                               "Adjust plot height (% taller)",
                                               min = 0,
                                               max = 100,
                                               value = 0
                                             ),
                                             span(
                                               actionButton("btnRefreshPalette", "Refresh palette", icon = icon("sync"))
                                             )
                                           )
                                           
                                         )
                                       ))
                    )      
)
