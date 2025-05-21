#### NORMA-The network makeup artist: a web tool for network annotation visualization

*What is this repository for?*

NORMA is a web tool for interactive network annotation visualization and topological analysis, able to handle multiple networks and annotations simultaneously.
Precalculated annotations (e.g. Gene Ontology/Pathway enrichment or clustering results) can be uploaded and visualized in a network either as colored pie-chart nodes or as color-filled convex hulls in a Venn-diagram-like style.
In the case where no annotation exists, algorithms for automated community detection are offered.
Users can adjust the network views using standard layout algorithms or allow NORMA to slightly modify them for visually better group separation.
Once a network view is set, users can interactively select and highlight any group of interest in order to generate publication-ready figures.
Briefly, with NORMA, users can encode three types of information simultaneously.
These are: i) the network, ii) the communities or annotations and iii) node categories or expression values.
Finally, NORMA offers basic topological analysis and direct topological comparison across any of the selected networks.

*How do I get set it up?*

* Install R
* Install Rstudio
* Install libraries in the global.R file
* Open ui.R or server.R and Run project

*How does it work?*

Try the tool online at https://pavlopoulos-lab-services.org/shiny/app/norma or clone this repository and run it locally. 

*What is the input file format?*

Examples and instructions regarding the input files can be found in NORMA's Help pages (Tab Input File).

*Are there any datasets in order to try NORMA out?*

You can download various examples from NORMA's Help pages (Tab Examples).

*Who do I talk to?*

Please contact pavlopoulos@fleming.gr

**Publication**

NORMA-The network makeup artist: a web tool for network annotation visualization 

Koutrouli M., Karatzas E, Papanikolopoulou K, Pavlopoulos GA.

Genomics Proteomics and Bioinformatics, 2021 Jun 22:S1672-0229(21)00130-3. doi: 10.1016/j.gpb.2021.02.005. 

PMID:34171457 https://pubmed.ncbi.nlm.nih.gov/34171457/
