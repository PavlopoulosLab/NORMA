networkPage <- div(id = "network_div",
  conditionalPanel(
    condition = "input.availableNetworks == 0",
    bsAlert("tabUploadMainAlert"),
    uiOutput("uiLoadGraphOptionsOutput_just_network")
  ),
  br(), br(),
  tabsetPanel(
    tabPanel(
      "Interactive Network",
      visNetworkOutput('tabVizIgraphSimple', width = 1000, height = 550),
      class = 'box-panel-padding',
      
      class = 'box-panel',
    ),
    
    tabPanel(
      "Automated Community Detection",
      br(),
      selectInput(
        "automated_annotations",
        "Community Detection Algorithms:",
        choices = automated_annotations_ui,
        selected = selected_automated_annotations,
        multiple = FALSE
      ),
      downloadButton("downloadData", "Export as annotation file", icon("download")),
      br(),
      br(),
      prettyCheckbox(
        inputId = "show_labels_algorithms_tab",outline = T,fill = T,bigger = T,
        label = "Show Labels",
        thick = T,
        shape = "curve",
        animation = "pulse",
        status = "info",
        inline = F,
        value = F
      ),
      plotOutput("modularity_plot", width = 1000, height = 600),
      eval(ui_dataTable_panel('Modularity_table')),
      br()
    )
  ) #tabsetPanel
)
