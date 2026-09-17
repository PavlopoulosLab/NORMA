# libs and files ####
source("./views/welcome.R", local=TRUE)
source("./views/upload.R", local=TRUE)
source("./views/network.R", local=TRUE)
source("./views/annotations.R", local=TRUE)
source("./views/topology.R", local=TRUE)
source("./views/help.R", local=TRUE)
source("./views/about.R", local=TRUE)

# UI fixedPage ####
fixedPage(
  theme = shinytheme("sandstone"),
  useShinyjs(),
  useShinyalert(),
  tags$head(tags$script(src = "intro.js")),
  tags$head(tags$script(src = "introbutton.js")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "norma.css")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "intro.css")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "annot_table.css")),
  tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
  tags$img(src = b64_1),
  navbarPage("NORMA: The NetwORk Makeup Artist",
    tabPanel( "Welcome", welcomePage ),
    tabPanel( "Upload", icon = icon("upload"), uploadPage ),
    tabPanel( "Network", networkPage ),
    tabPanel( "Annotations", annotationsPage),
    tabPanel( "Topology", icon = icon("globe", lib = "glyphicon"), topologyPage ),
    tabPanel( "Help", icon = icon("question"), helpPage ),
    tabPanel( "About", icon = icon("users"), aboutPage )
  )
)
