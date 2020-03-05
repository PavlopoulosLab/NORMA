README
NORMA-The network makeup artist: a web tool for network annotation visualization

What is this repository for?
NORMA is a web tool for interactive network annotation visualization and topological analysis, able to handle multiple networks and annotations simultaneously. Precalculated annotations (e.g. Gene Ontology/Pathway enrichment or clustering results) can be uploaded and visualized in a network either as colored pie-chart nodes or as color-filled convex hulls in a Venn-diagram-like style. In the case where no annotation exists, algorithms for automated community detection are offered. Users can adjust the network views using standard layout algorithms or allow NORMA to slightly modify them for visually better group separation. Once a network view is set, users can interactively select and highlight any group of interest in order to generate publication-ready figures. Briefly, with NORMA, users can encode three types of information simultaneously. These are: i) the network, ii) the communities or annotations and iii) node categories or expression values. Finally, NORMA offers basic topological analysis and direct topological comparison across any of the selected networks.

How do I get set up?
Install R
Install Rstudio
Install libraries in the global.R file
Open ui.R or server.R and Run project
Publication
Has been submitted for publication
Who do I talk to?
Please contact koutrouli@fleming.gr or pavlopoulos@fleming.gr