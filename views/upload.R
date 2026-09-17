uploadPage <- div(id = "upload_div",
                  sidebarLayout(
                    
                    # sidebarPanel ####
                    sidebarPanel(
                      actionButton("introButton", "Guide Tutorial"),
                      br(),
                      hr(),
                      tags$div(class="network_div",
                               tags$h3('1. Networks'),
                               selectInput(
                                 "uiLoadGraphOptionsInput",
                                 "Choose Network(s)",
                                 c(
                                   "File upload" = "oF",
                                   "Example STRING BCAR3 Network" = "oR_String_interactions",
                                   "Example Drosophila TAU Network" = "oR_Drosophila"
                                 )
                               )%>%
                                 shinyInput_label_embed(
                                   shiny_iconlink() %>%
                                     bs_embed_popover(
                                       title = paste0("Please follow the steps below:
- Load your network file in tab delimited format
- Give it a name
- Hit the ADD button")
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
                               uiOutput("uiStoredGraphsOutputRadio")
                      ),
                      hr(),
                      tags$div(class="annotations_div",
                               tags$h3('2. Annotations'),
                        ######2nd button-annotation
                        selectInput(
                          "uiLoadGraphOptionsInput_annotations",
                          "Choose Annotation(s)",
                          c(
                            "File upload" = "oF",
                            "Example BCAR3 STRING GO KEGG" = "oR_String_Annotation_KEGG",
                            "Example BCAR3 STRING GO MF" = "oR_String_Annotation_MF",
                            "Example BCAR3 STRING GO BP" = "oR_String_Annotation_BP",
                            "Example TAU Drosophila KEGG" = "oR_Drosophila_KEGG",
                            "Example TAU Drosophila Louvain" = "oR_Drosophila_Luvain"
                          )
                        )%>%
                          shinyInput_label_embed(
                            shiny_iconlink() %>%
                              bs_embed_popover(
                                title = paste0("Please follow the steps below:
- Load your annotation file in tab delimited format
- Give it a name
- Hit the ADD button")
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
                        uiOutput("uiStoredGraphsOutputRadio_annotations")
                      ),
                      hr(),
                      tags$div(class="expressions_div",
                        ######3rd button-expression
                        tags$h3('3. Node-coloring'),
                        selectInput(
                          "uiLoadExpressionsInput",
                          "Choose node - coloring file(s)",
                          c("File upload" = "oF",
                            # "Expression_file_STRING" = "oR_Expression_file_STRING",
                            "Example TAU Drosophila node - coloring file" = "oR_Expression_file_Drosophila"
                          )
                        )%>%
                          shinyInput_label_embed(
                            shiny_iconlink() %>%
                              bs_embed_popover(
                                title = paste0("Please follow the steps below:
- Load your node - coloring file in tab delimited format
- Give it a name
- Hit the ADD button")
                              )
                          ),
                        uiOutput("uiLoadExpressionsOutput"),
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
                        uiOutput("uiStoredGraphsOutputRadioEx")
                      )
                    ), #sidebarPanel
                    
                    # mainPanel ####
                    mainPanel(
                      conditionalPanel(
                        condition = "input.availableNetworks == 0",
                        uiOutput("uiStoredGraphsOutputSelectUpload")
                      ),
                      
                      conditionalPanel(
                        condition = "input.availableAnnotations == 0",
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
)
