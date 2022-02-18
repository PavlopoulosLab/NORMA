# libs and files ####
source("./views/welcome.R", local=TRUE)
source("./views/upload.R", local=TRUE)
source("./views/network.R", local=TRUE)
source("./views/annotations.R", local=TRUE)
source("./views/topology.R", local=TRUE)
source("./views/help.R", local=TRUE)
source("./views/about.R", local=TRUE)

# CSS ####
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

# UI fixedPage ####
fixedPage(
  theme = shinytheme("sandstone"),
  # shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
  tags$head(tags$style( HTML(css_generated) ),
            tags$style( HTML("hr {border-top: 1px solid #000000;}") )),
  useShinyjs(),
  useShinyalert(),
  # tags$head(tags$script(src = "cyjs.js")),
  tags$head(tags$script(src = "intro.js")),
  tags$head(tags$script(src = "introbutton.js")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "intro.css")),
  tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
  tags$img(src = b64_1),
  navbarPage(
    "NORMA: The NetwORk Makeup Artist",
    header = tags$head(tags$style(type = "text/css", ui_css)),
    tabPanel( "Welcome", welcomePage ),
    tabPanel( "Upload", icon = icon("upload"), uploadPage ),
    tabPanel( "Network", networkPage ),
    tabPanel( "Annotations", annotationsPage),
    tabPanel( "Topology", icon = icon("globe", lib = "glyphicon"), topologyPage ),
    tabPanel( "Help", icon = icon("question"), helpPage ),
    tabPanel( "About", icon = icon("users"), aboutPage )
  )
)
