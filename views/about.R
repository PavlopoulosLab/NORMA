aboutPage <- div(id = "about_div",
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
                 br(),
                 tags$a(href="https://pubmed.ncbi.nlm.nih.gov/34171457/", "NORMA: The Network Makeup Artist - A Web Tool for Network Annotation Visualization."),
                 helpText(" Genomics Proteomics Bioinformatics. 2021 Jun 22:S1672-0229(21)00130-3. doi: 10.1016/j.gpb.2021.02.005. Epub ahead of print. PMID: 34171457.")
)
