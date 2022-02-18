welcomePage <- div(id = "welcome_div",
                   h1("Welcome to NORMA, the NetwORk Makeup Artist"),
                   strong("a tool that enables the visualization of annotation groups."),
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
                     tags$li("1. Please load your network file(s), name it and hit the add button"),
                     tags$li(
                       "2. Please load your annotation file(s), name it and hit the add button"
                     )
                   ),
                   tags$ul(
                     "Input file instructions as well as downloadable examples can be found in the HELP/EXAMPLES page."
                   )
)
