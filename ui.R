library(shiny)


ui_options <- c("ui_table_line_height" = "80%")

ui_css <- paste0(
  '
    .box-panel {
    border-radius: 0 0 5px 5px;
    border-width: 1px;
    border-color: #d7d7d7;
    border-style: none solid solid solid;
    }
    .box-panel-padding {
    padding: 20px 20px 20px 20px;
    }
    .tabBox-panel {
    padding: 20px 20px 20px 20px;
    border-width: 1px;
    border-color: #d7d7d7;
    border-radius: 0px 0px 8px 8px;
    border-style: none solid solid solid;
    background: #ffffff;
    }

    .centerBlock {
    float: none;
    margin: 0 auto;
    }

    ',
  ".dataTables_wrapper td {
    line-height: ",
  ui_options['ui_table_line_height'],
  ";
    }"
)



ui_dataTable_panel <- function(datasetName, pagination = TRUE) {
  return(parse(
    text = paste0(
      "div(div(DT::dataTableOutput('",
      datasetName,
      "'), class='box-panel-padding'), class='box-panel')"
    )
  ))
}


fixedPage(
  theme = shinytheme("sandstone"),
  # shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
  tags$head(tags$style(HTML(css_generated)),
            tags$style(
              HTML("hr {border-top: 1px solid #000000;}")
            )),
  useShinyjs(),
  tags$head(tags$script(src = "cyjs.js")),
  tags$head(tags$script(src = "intro.js")),
  tags$head(tags$script(src = "introbutton.js")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "intro.css")),
  tags$img(src = b64_1),
  navbarPage(
    "NORMA: The NetwORk Makeup Artist",
    header = tags$head(tags$style(type = "text/css", ui_css)),
    tabPanel(
      "Welcome",
      h1("Welcome to NORMA, the NetwORk Makeup Artist"),
      strong("a tool for visualization of annotation groups."),
      br(),
      br(),
      helpText(
        "NORMA is a handy tool for interactive network annotation, visualization and topological analysis,
              able to handle multiple networks and annotations simultaneously. Annotations and/or node groups can
              be precomputed or automatically calculated and users can combine several networks and groupings they
              are interested in. Annotated networks can be visualized using both pie-chart nodes and shaded convex
              hulls and can be shown using several layouts while users can isolate the groups of interest interactively.
              In addition, NORMA is suitable for direct comparison of topological features between one or more networks.
              In order for NORMA to run, two simple steps are required:"
      ),
      tags$ul(
        tags$li("1.Please load your network file(s), name it and hit the add button"),
        tags$li(
          "2.Please load your annotation file(s), name it and hit the add button"
        )
      ),
      tags$ul(
        "Input file instructions as well as downloadable examples can be found in the Help/About page."
      )
      
    ),# tabPanel 'Welcome'
    
    tabPanel(
      "Upload",
      icon = icon("upload"),
      sidebarLayout(
        sidebarPanel(
          bsAlert("tabUploadSideAlert"),
          bsAlert("tabUpload_up_to_10000_rows"),
          actionButton("introButton", "Guide Tutorial"),
          br(),
          br(),
          helpText("Please follow the steps below:"),
          helpText(tags$ul(
            tags$li("Load your network file in tab delimited format"),
            tags$li("Give it a name"),
            tags$li("Hit the ADD button")
          )),
          selectInput(
            "uiLoadGraphOptionsInput",
            "1: Choose Network(s)",
            c(
              "File upload" = "oF",
              "Example STRING BCAR3 Network" = "oR_String_interactions",
              "Example Drosophila TAU Network" = "oR_Drosophila"
            )
          ),
          uiOutput("uiLoadGraphOptionsOutput"),
          # checkboxInput("weighted",label = "Weight", value =F),
          div(
            span(
              actionButton(
                "btnAddNetwork",
                "ADD",
                icon = icon("plus"),
                style = "color: #fff; background-color: #7F461B; border-color: #7F461B"
              ),
              class = "input-group-btn"
            ),
            tags$input(
              id = "networkName",
              type = "text",
              class = "form-control",
              placeholder = "Type network name ..",
              value = "Network name"
            ),
            class = "input-group"
          ),
          uiOutput("uiStoredGraphsOutputRadio"),
          hr(),
          
          ######2nd button-annotation
          helpText("Please follow the steps below:"),
          helpText(tags$ul(
            tags$li("Load your annotation file in tab delimited format"),
            tags$li("Give it a name"),
            tags$li("Hit the ADD button")
          )),
          selectInput(
            "uiLoadGraphOptionsInput_annotations",
            "2: Choose Annotation(s)",
            c(
              "File upload" = "oF",
              "Example BCAR3 STRING GO KEGG" = "oR_String_Annotation_KEGG",
              "Example BCAR3 STRING GO MF" = "oR_String_Annotation_MF",
              "Example BCAR3 STRING GO BP" = "oR_String_Annotation_BP",
              "Example TAU Drosophila KEGG" = "oR_Drosophila_KEGG",
              "Example TAU Drosophila Louvain" = "oR_Drosophila_Luvain"
            )
          ),
          uiOutput("uiLoadGraphOptionsOutput_annotations"),
          div(
            span(
              actionButton(
                "btnAddNetwork2",
                "ADD",
                icon = icon("plus"),
                style = "color: #fff; background-color: #8D4A43; border-color: #8D4A43"
              ),
              class = "input-group-btn"
            ),
            tags$input(
              id = "annotationName",
              type = "text",
              class = "form-control",
              placeholder = "Type annotation name ..",
              value = "Annotation name"
            ),
            class = "input-group"
          ),
          uiOutput("uiStoredGraphsOutputRadio_annotations"),
          hr(),
          
          
          ######3rd button-expression
          helpText("Please follow the steps below:"),
          helpText(tags$ul(
            tags$li("Load your node - coloring file in tab delimited format"),
            tags$li("Give it a name"),
            tags$li("Hit the ADD button")
          )),
          selectInput(
            "uiLoadExpressions",
            "3: Choose node - coloring file(s)",
            c("File upload" = "oF",
              # "Expression_file_STRING" = "oR_Expression_file_STRING",
              "Example TAU Drosophila node - coloring file" = "oR_Expression_file_Drosophila"
              )
          ),
          uiOutput("uiLoadExpressions"),
          div(
            span(
              actionButton(
                "btnAddExpression",
                "ADD",
                icon = icon("plus"),
                style = "color: #fff; background-color: #B89778; border-color: #B89778"
              ),
              class = "input-group-btn"
            ),
            tags$input(
              id = "expressionName",
              type = "text",
              class = "form-control",
              placeholder = "Type expression name ..",
              value = "Node - coloring file name"
            ),
            class = "input-group"
          ),
          uiOutput("uiStoredGraphsOutputRadioEx"),
          hr()
        ),
        #sidebarPanel
        
        ##############################################################
        
        mainPanel(
          conditionalPanel(
            condition = "input.availableNetworks == 0",
            bsAlert("tabUploadMainAlert"),
            uiOutput("uiStoredGraphsOutputSelectUpload")
          ),
          
          conditionalPanel(
            condition = "input.availableAnnotations == 0",
            bsAlert("tabUploadMainAlert2"),
            uiOutput("uiStoredGraphsOutputSelectUpload2")
          ),
          
          
          
          tabsetPanel(
            tabPanel(
              "Table View of Network(s)",
              icon = icon("table"),
              eval(ui_dataTable_panel('datasettab1'))
            ),
            tabPanel(
              "Table View of Annotation(s)",
              icon = icon("table"),
              eval(ui_dataTable_panel('datasettab2'))
            )
          ) #tabsetPanel
        ) # mainPanel
      ) # sidebarLayout
    ),
    # tabPanel 'Upload File(s)'
    
    tabPanel(
      "Network",
      conditionalPanel(
        condition = "input.availableNetworks == 0",
        bsAlert("tabUploadMainAlert"),
        uiOutput("uiLoadGraphOptionsOutput_just_network")
      ),
      br(),
      br(),
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
      )#tabsetpanel
    ),
    tabPanel(
      "Annotations",
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
          # Layouts #
          helpText("Select the layout for the analysis:"),
          
          selectInput(
            "layouts",
            "The layouts:",
            choices = layouts_ui,
            selected = selected_layouts,
            multiple = FALSE
          ),
          prettyCheckbox(
            inputId = "layouts_with_virtual_nodes",outline = T,fill = T,bigger = T,
            label = "Allow modification of the selected layout taking into account the groups",
            thick = TRUE,
            shape = "curve",
            animation = "pulse",
            status = "info",
            inline = F,
            value = F
          ),
          hr(),
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
          uiOutput("interactive_convex_hulls"),
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
            helpText("Select the layout you want in the analysis."),
            selectInput(
              "layouts2",
              "The layouts:",
              choices = layouts_ui,
              selected = selected_layouts,
              multiple = FALSE
            ),
            prettyCheckbox(
              inputId = "layouts_with_virtual_nodes_pies",outline = T,fill = T,bigger = T,
              label = "Allow modification of the selected layout taking into account the groups",
              thick = TRUE,
              shape = "curve",
              animation = "pulse",
              status = "info",
              inline = F,
              value = F
            ),
            hr(),
            ###############
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
            uiOutput('tabVizPie_charts'),
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
            "The 3D layouts:",
            choices = layouts_3D,
            selected = selected_layouts,
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
          uiOutput("convex_hull_3D"),
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
      
    ),
    
    tabPanel(
      "Topology",
      icon = icon("globe", lib = "glyphicon"),
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
                               icon = icon("bar-chart"),
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
                                 actionButton("btnRefreshPalette", "Refresh palette", icon = icon("refresh"))
                               )
                             )
                             
                           )
                         ))
      )
    ),
    
    
    tabPanel(
      "Help/About",
      icon = icon("question"),
      
      tabsetPanel(
        tabPanel(
          "Input File",
          br(),
          
          strong("NORMA mainly accepts 3 different files as input:."),
          br(),
          br(),
          strong("The network file:"),
          helpText(
            "It is an obligatory, 2-column (unweighted) or 3-column (weighted), tab-delimited file, containing all network connections of an undirected network. This file must contain headers, namely: 'Source' and 'Target' (and 'Weight' optionally). Notably, self-loops and multiple-edges are eliminated automatically."
          ),
          strong("The annotation file:"),
          helpText(
            "
 It is an obligatory, 2-column, tab-delimited file which contains information about the defined groups. The first column contains the group names whereas the second column contains the node names in a group separated by a comma (,) and without spaces. No headers are allowed."
          ),
          strong("The expression file:"),
          helpText(
            "
The expression file: It is an optional, 2-column, tab-delimited file which contains information about node coloring (e.g. gene expressions). The first column contains the node names and the second column the node colors (e.g. red, green, yellow, blue, orange, #00ff00, #ff0000, #ffff00). Nodes without color assignment will be colored gray. No headers are allowed in this file."
          ),
          helpText("
Examples are shown below:"),
          pre(
            "
Network File:               Annotation File:                        Node-coloring file            Warnings!

Source  Target  Weight      Group-2 BCL2L1,MDM4,MDM2,CHEK2          CDKN1A  blue                - Network file: Must have headers: Source - Target
CDKN1A  TP53    5           Group-5 TP53,EP300                      TP53    blue                - Annotation file: no headers, no spaces only commas
TP53	MDM2    1           Group-1 CDKN2A,ATM,TP53BP2,MDM2         MDM4    #00ff00              (e.g. BCL2L1,MDM4,MDM2)
MDM4	TP53    3           Group-4 CHEK2,CREBBP,MDM2               BCL2L1  red
BCL2L1	TP53    4           Group-3 TP53,BCL2L1                     CHEK2   red                 - Node-coloring file: no headers
CHEK2   ATM     2           Group-6 MDM4,MDM2                       ATM     red                   Colors can be either color names (e.g. blue, red) 
TP53    EP300   1                                                   TP53BP2 red                   or hex codes (#00ff00, #ff0000, #ffff00)
ATM	TP53    4                                                   CDKN2A  blue
TP53    CREBBP  1                                                   EP300   #ffff00
MDM4    MDM2    1                                                   CREBBP  red
CHEK2	TP53    2                                                   MDM2    blue
TP53BP2	TP53    8
CDKN2A	TP53    3
CDKN2A	MDM2    3
ATM	MDM2    1
EP300	CREBBP  2
  .       .     .
  .       .     .
  .       .     .
"
          ),
          
          strong("Usage:"),
          helpText(
            "Users can upload as many network and annotation files as they like. Every time a network or an annotation file is uploaded, a name can be given first.
Once a network or an annotation file has been named and uploaded, it will appear as an option in any of the NORMA's dropdown selection lists. Users can remove indifferent annotations or networks at any time.
"
          ),
          br(),
          br(),
          strong("Troubleshooting:"),
          helpText("Nodes referenced in the annotation file must comply with nodes referenced in the network file. If a node in the annotation does not appear in the network file, NORMA won't be able to process the files and produce a proper visualization. To address this problem, we have implemented an R script which accepts a network file and an annotation file as inputs and generates a corrected annotation output file. In this output file, node names in annotations which do not appear in the network file are discarded. Notably, node names in both files must not contain any commas (,)."),
         
          br(),
          downloadLink('R_script', "Download R script here"),
          br(),
          helpText(tags$ul(
            tags$li("Install R / Rstudio"),
            tags$li("Open the file with Rstudio"),
            tags$li("Run the file"),
            tags$li("The output will be written in a new file named 'annotations_cleaned.txt'. ")
          )),
          br(),
          br()
          
        ),
        #Tabpanel Input File
        
        tabPanel(
          "EXAMPLES",
          br(),
          strong("STRING example 1 (TP53 interactors):"),
          br(),
          downloadLink('string_net_tp53', "STRING Network file"),
          br(),
          downloadLink('string_annot', "STRING Annotation file"),
          br(),
          downloadLink('string_expr', "STRING Expression file"),
          br(),
          hr(),
          br(),
          strong("STRING example 2 (BCAR3 interactors):"),
          br(),
          downloadLink('string_net_bcar3', "STRING Network file"),
          br(),
          downloadLink('string_bp', "GO Annotation - Biological Process"),
          br(),
          downloadLink('string_mf', "GO Annotation - Molecular Function"),
          br(),
          downloadLink('string_kegg', "KEGG pathways"),
          br(),
          hr(),
          br(),
          strong("Drosophila (Tau) Network (PMID:31488613, PMCID:PMC6794924, DOI:10.1523/JNEUROSCI.0391-19.2019):"),
          br(),
          downloadLink('dros_net', "Drosophila Network file"),
          br(),
          downloadLink('dros_annot', "Drosophila Kegg pathways"),
          br(),
          downloadLink('dros_louvain', "Drosophila Louvain automated annotation file"),
          br(),
          downloadLink('dros_express', "Drosophila Expression file"),
          br(),
          hr(),
          br(),
          strong("Human Gene Co-expression Network (PMID:19081792, PMCID:PMC2597745, DOI:10.1371/journal.pone.0003911):"),
          helpText("http://bioinfow.dep.usal.es/coexpression/"),
          br(),
          downloadLink('co_express', "Gene Co-expression Network"),
          br(),
          downloadLink('co_express_bp', "GO Annotation - Biological Process"),
          br(),
          downloadLink('co_express_mf', "GO Annotation - Molecular Function"),
          br(),
          downloadLink('co_express_cc', "GO Annotation - Cellular Components"),
          br(),
          downloadLink('co_express_kegg', "KEGG pathways"),
          br(),
          downloadLink('co_express_mcode', "MCODE Node coloring"),
          br(),
        hr(),
        br(),
        strong("COVID-19:"),
        helpText("Intact Database"),
        br(),
        downloadLink('covid_19_net', "COVID-19 Network"),
        br(),
        downloadLink('covid_19_interpro', "HomoSapiens Protein Domains - INTERPRO"),
        br(),
        downloadLink('covid_19_bp', "HomoSapiens GO Annotation - Biological Process"),
        br(),
        downloadLink('covid_19_mf', "HomoSapiens GO Annotation - Molecular Function"),
        br(),
        downloadLink('covid_19_cc', "HomoSapiens Protein Domains GO Annotation - Cellular Components"),
        br(),
        downloadLink('covid_19_kegg', "HomoSapiens Protein Domains KEGG pathways"),
        br(),
        downloadLink('covid_19_smart', "HomoSapiens Protein Domains SMART"),
        br(),
        hr(),
        br(),
        strong("Gallus gallus:"),
        helpText("BioGrid Database"),
        br(),
        downloadLink('Gallus_gallus_net', "Gallus gallus Network"),
        br(),
        downloadLink('Gallus_gallus_kegg', "BioGrid Gallus gallus KEGG pathways"),
        br(),
        br()
      ),
        
        tabPanel(
          "The Upload Tab",
          br(),
          helpText(
            "Once one or more network and annotation files have been named and uploaded, they will appear as options in the dropdown selection lists. Users can select a network or an annotation file at a time and see its content as an interactive table. Notably, one can search by suffix in the table, using the Search field. "
          ),
          tags$img(src = b64_2),
          tags$img(src = b64_3),
          tags$img(src = b64_4),
          br(),
          br()
          
        ),
        #Tabpanel Upload
        
        tabPanel(
          "The Network Tab",
          br(),
          strong(
            "This Tab consists of two sub-tabs dedicated to network analysis and visualization. These are: (i) the Interactive Network and the (ii) the Automated Community Detection."
          ),
          br(),
          br(),
          strong("Interactive Network:"),
          helpText(
            "This Tab offers a dynamic network visualization in its simplest form. Nodes are connected with undirected edges and their coordinates are calculated using a force-directed layout. The network is fully interactive as zooming, dragging and panning are allowed either by using the mouse or the navigation buttons. In addition, nodes can be selected and dragged anywhere on the plane, whereas the first neighbors of any node can be highlighted upon selection. Finally, the network view is automatically updated when a different network is selected."
          ),
          br(),
          tags$img(src = b64_5),
          br(),
          br(),
          strong("Automated Community Detection:"),
          helpText(
            "This Tab is used for the automatic calculation of communities whereas the exported file(s) can be used as input annotation file(s). Users can assign nodes to communities (not necessarily uniquely), using various options. These are:"
          ),
          helpText(tags$ul(
            tags$li(
              "Fast-Greedy: This function tries to find densely connected subgraphs (also called communities) via directly optimizing a modularity score."
            ),
            tags$li(
              "Louvain: This function implements a multi-level modularity optimization algorithm for finding community structures and is based on the modularity measure and a hierarchical approach."
            ),
            tags$li(
              "Label-Propagation: This is a fast, nearly linear time algorithm for detecting community structures in a network by labeling the vertices with unique labels and then updating the labels by majority voting in the neighborhood of the vertex."
            ),
            tags$li(
              "Walktrap: This function tries to find densely connected subgraphs in a graph via random walks. The idea is that short random walks tend to stay in the same community."
            ),
            tags$li(
              "Betweenness: Many networks consist of modules which are densely connected between themselves but sparsely connected to other modules. Clustering is made by ‘breaking’ the bridges which connect densely connected regions."
            ),
          )),
          br(),
          tags$img(src = b64_6),
          br(),
          br(),
          helpText(
            "Once a community detection method has been selected, users can see the results as interactive and searchable tables or as static plots for an at-a-glance view. In order for users to take advantage of NORMA’s advanced interactive visualization capabilities, the automatically generated annotations must be first exported and then imported as annotation input files."
          )
          
        ),
        #Tabpanel Network
        
        
        tabPanel(
          "The Annotations Tab",
          br(),
          strong(
            "This Tab is NORMA’s strongest feature and is used to visualize annotated networks in an easy and user-friendly way. Annotated, are the networks with (pre-)defined clusters, communities, subgraphs, marked regions or neighborhoods.

Through the Annotation Tab, users can select between any of the uploaded networks or annotation files and visualize them in combination. Network and Annotation selections can be done by the offered dropdown selection lists.

The Annotation Tab consists of three sub-tabs. These are the: (i) Convex Hull, (ii) Pie-chart nodes and (iii) Venn diagrams.
"
          ),
          br(),
          br(),
          strong("Convex Hulls:"),
          helpText(
            "In this tab, the selected network is initially visualized after applying any of the offered layout algorithms and shaded convex hulls are then used to highlight communities in a Venn-diagram-like view. A node might belong to more than one group. In this case, NORMA tries to bring closer together the overlapping regions which share common nodes while simultaneously it tries to keep the distinct groups apart. Groups are highlighted using visually distinct colors, whereas transparency is used to efficiently highlight the overlapping regions."
          ),
          br(),
          tags$img(src = b64_7),
          br(),
          br(),
          strong("Pie-chart nodes:"),
          helpText(
            "In this Tab, the selected network is initially visualized after applying any of the offered layout algorithms and nodes are then visualized as pie-charts, divided into slices to illustrate the groups a node belongs to. If a node for example belongs to four groups, then the pie chart will consist of four equal slices colored with distinct colors. Nodes which do not belong to any group are marked gray.
"
          ),
          br(),
          tags$img(src = b64_8),
          
          br(),
          br(), 
          
          strong("Convex Hulls 3D:"),
          helpText(
            "Like in 2D Convex Hulls, in this tab, the selected network is initially visualized after applying any of the offered 3D layout algorithms and 3D shaded convex hulls are then used to highlight communities in a 3D Venn-diagram-like view. The visualization is fully interactive and a dark mode visualization is also supported.
"
          ),
          br(),
          tags$img(src = b64_3D_convex),
          
          br(),
          br(),
          strong("Node coloring:"),
          helpText(
            "Often, one might want to assign certain colors to nodes in order to encode certain information. In a gene expression network for example, one might want to highlight the up- and down-regulated genes. Once an expression file has been loaded (see Input file section), nodes in the Convex Hull will be filled with the color of interest whereas nodes in the Pie-Chart tab will appear with a colored border. As node coloring is an optional feature, one can enable or disable this functionality at any time (selection box).
"
          ),
          br(),
          tags$img(src = b64_9),
          br(),
          br(),
          strong("Layouts:"),
          helpText(
            "Several layouts are offered for network visualization in both Convex Hull and Pie Chart sub-tabs. These are:
"
          ),
          helpText(
            tags$ul(
              tags$li(
                "Fruchterman-Reingold: It places nodes on the plane using the force-directed layout algorithm developed by Fruchterman and Reingold."
              ),
              tags$li(
                "Random: This function places the vertices of the graph on a 2D plane uniformly using random coordinates."
              ),
              tags$li(
                "Circle: It places vertices on a circle, ordered by their vertex ids."
              ),
              tags$li(
                "Kamada-Kawai: This layout places the vertices on a 2D plane by simulating a physical model of springs."
              ),
              tags$li(
                "Reingold-Tilford: This is a tree-like layout and is suitable for trees or graphs without many cycles."
              ),
              tags$li("LGL: A force directed layout suitable for larger graphs."),
              tags$li(
                "Graphopt: A force-directed layout algorithm, which scales relatively well to large graphs."
              ),
              tags$li(
                "Gem: It places vertices on the plane using the GEM force-directed layout algorithm."
              ),
              tags$li(
                "Star: It places vertices of a graph on the plane, according to the simulated annealing algorithm by Davidson and Harel."
              ),
              tags$li("Grid: This layout places vertices on a rectangular 2D grid.")
            ),
            helpText("NOTE: NORMA provides the option to slightly modify the selected layout in order to make groups as distinct as possible, thus avoiding unecessary overlaps which may occur due to the original layout.")
          ),
          br(),
          strong("Interactivity and Visualization:"),
          helpText(
            "NORMA gives a variety of options for the creation of optimal custom views. Network zoom in/out and panning functionalities are offered while users can interactively drag any node and place it anywhere on the plane. In addition to the visualized networks, groups are shown in an interactive table whose rows are colored accordingly. By selecting one or more groups, one can adjust the convex hulls as well as the pie-chart nodes accordingly. Colored groups (rows) in the table correspond to colored groups in the offered views and vice versa. In addition, users have the option to show and hide the labels or only keep the labels of the selected groups of interest while labels below a certain zoom level are hidden for clarity. Finally, sliders to adjust node and label sizes as well as a slider to scale the network size are offered."
          ),
          br(),
          strong("Venn Diagrams:"),
          helpText("Users are allowed to choose any pair of nodes and visualize the common groups they belong to as a Venn diagram. As it is not in the scope of NORMA to provide more complex Venn diagrams, users are encouraged to visit other online applications dedicated to this purpose. "),
          tags$img(src = b64_12)

        ),
        #Tabpanel Annotations
        
        tabPanel(
          "The Topology Tab",
          br(),
          strong(
            "This Tab is used for automated topological analysis and direct comparison of topological features between two or more networks.
"
          ),
          helpText("The topological features are: "),
          helpText(
            tags$ul(
              tags$li(
                "Number of Edges: Shows the number of edges in the network. NORMA accepts networks with less than 10000 edges.
"
              ),
              tags$li(
                "Number of Nodes: Shows the number of nodes in the network.
"
              ),
              tags$li(
                "Density: The density of a graph is the ratio of the number of edges and the number of possible edges.
"
              ),
              tags$li(
                "Average path length: The average number of steps needed to go from one node to another.
"
              ),
              tags$li(
                "Clustering Coefficient: A metric which shows if the network has the tendency to form clusters. Values between 0 and 1.
"
              ),
              tags$li(
                "Modularity: This function calculates how modular is a given division of a graph into subgraphs.
"
              ),
              tags$li(
                "Average Eccentricity: The distance from a particular vertex to all other vertices in the graph is taken and among those distances, the eccentricity is the highest of distances."
              ),
              tags$li(
                "Average number of Neighbors: It is the total number of neighbors per node divided by the number of nodes.
"
              ),
              tags$li(
                "Centralization betweenness: It is an indicator of a node's centrality in a network. It is equal to the number of shortest paths from all vertices to all others that pass through that node. Betweenness centrality quantifies the number of times a node acts as a bridge along the shortest path between two other nodes.
"
              ),
              tags$li(
                "Centralization degree: It is defined as the number of links incident upon a node.
"
              )
            )
          ),
          br(),
          
          strong(
            "The Topology Tab is divided into two sub-tabs. These are: (i) the Summaries and (ii) the Comparative Plots.
"
          ),
          br(),
          strong("Summaries:"),
          helpText(
            "This Tab shows the aforementioned topological measures in a numerical form as a table view. Users can select one or more features of interest through the offered checkboxes and show them accordingly. Notably, this can be done for one network at a time upon selection (dropdown selection list).
"
          ),
          br(),
          tags$img(src = b64_10),
          
          br(),
          strong("Comparative Plots:"),
          helpText(
            "This Tab can be used to directly compare the topological features of two or more networks simultaneously. In contrast to the previous Tab, users are allowed to select one topological feature at a time (radio buttons) but as many networks as they like (check boxes). Once two or more networks and one topological feature have been selected, direct comparisons can be made by the generated bar charts. A slider to adjust the chart height is offered.
"
          ),
          tags$img(src = b64_11),
          br()
          
        ) #Tabpanel Topology
        
        
      ),
      #tabsetPanel
      
      
    ),
    # tabPanel 'Help'
    
    
    tabPanel(
      "People",
      icon = icon("users"),
      strong("The Team:"),
      tags$ul(
        tags$li("Mikaela Koutrouli - BSRC 'Alexander Fleming'"),
        tags$li("Evangelos Karatzas - BSRC 'Alexander Fleming'"),
        tags$li("Katerina Papanikolopoulou - BSRC 'Alexander Fleming'"),
        tags$li("Yorgos Sofianatos - BSRC 'Alexander Fleming'"),
        tags$li("Georgios A. Pavlopoulos - BSRC 'Alexander Fleming'")
      ),
      br(),
      strong("Code:"),
      helpText("Available at: https://github.com/PavlopoulosLab/NORMA"),
      br(),
      strong("Publications:"),
      helpText(
        "NORMA has been submitted for publication.
            Information will be provided once the paper is online"
      )
      
    )
  )
)
