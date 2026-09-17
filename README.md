<!-- Badges -->

[![R-CMD-check](https://github.com/yourusername/NORMA/workflows/R-CMD-check/badge.svg)](https://github.com/yourusername/NORMA/actions)
[![Shiny App](https://img.shields.io/badge/Shiny-online-brightgreen)](https://pavlopoulos-lab-services.org/shiny/app/norma)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

# NORMA: The Network Makeup Artist

> A web tool for interactive network annotation visualization and topological analysis.

---

## 📖 Table of Contents

1. [Overview](#overview)
2. [Key Features](#key-features)
3. [Installation](#installation)
4. [Usage](#usage)
5. [Input Formats](#input-formats)
6. [Examples & Demo Data](#examples--demo-data)
7. [Contact & Support](#contact--support)
8. [Publications](#publications)
9. [License](#license)

---

## 📝 Overview

NORMA (Network Makeup Artist) is a Shiny-based web application that lets you:

* **Visualize** multiple networks and their annotations (e.g., GO terms, pathway enrichments)
* **Overlay** annotations as pie-chart nodes or convex-hull “venn” shapes
* **Detect** communities automatically when no annotations are provided
* **Refine** layouts for optimal group separation with common graph algorithms
* **Highlight** and export publication-quality figures interactively
* **Compare** network topology metrics across different datasets

---

## 🚀 Key Features

* **Multi-network support**: Load and visualize several networks side by side.
* **Annotation overlays**:

  * **Pie-chart nodes** for multi-category annotations
  * **Convex-hulls** for set-overlap style visualization
* **Community detection**: Fast algorithms (e.g., Louvain, Infomap) for de novo grouping.
* **Flexible layouts**: Fruchterman–Reingold, Kamada–Kawai, circular, grid, and more.
* **Interactive figure export**: Customize colors, sizes, labels; download as PNG or PDF.
* **Topological analysis**: Compute degree, clustering coefficient, betweenness, etc., and compare across networks.

---

## 🛠 Installation

1. **Install R** (≥4.0)
2. **Install RStudio** (optional, but recommended)
3. **Clone this repo**

   ```bash
   git clone https://github.com/yourusername/NORMA.git
   cd NORMA
   ```
4. **Install required packages**

   ```r
   # From your R console:
   install.packages(c(
     "shiny", "igraph", "visNetwork", "plotly",
     "data.table", "DT", "RColorBrewer"
   ))
   ```
5. **Run the app**

   ```r
   # In RStudio or R console:
   shiny::runApp("path/to/NORMA")
   ```

---

## 💻 Usage

* **Online demo**: [Launch NORMA in your browser](https://pavlopoulos-lab-services.org/shiny/app/norma)
* **Local**: Once the app is running, use the sidebar to upload:

  * **Network file** (edge list or adjacency)
  * **Annotation file** (tab-delimited with node → category)
* Toggle between “Pie-chart nodes” and “Convex-hulls” in **Visualization**.
* Explore **Layout**, **Annotation**, and **Analysis** tabs for customization.

---

## 📂 Input Formats

* **Network**:

  * Edge list: `source<TAB>target`
  * Adjacency matrix: CSV or TSV
* **Annotations**:

  * Plain table:

    ```text
    node_id<TAB>category1,category2,...
    ```
* See the **Help → Input File** tab in the app for detailed examples.

---

## 📊 Examples & Demo Data

Download sample networks and annotation sets from the app’s **Help → Examples** tab or directly from our website. These include:

* Human protein–protein interaction subnetworks
* GO term enrichment outputs for test datasets
* Synthetic networks demonstrating overlapping modules

---

## 📬 Contact & Support

For questions, issues, or feature requests, please email:

> **George A. Pavlopoulos**
> [pavlopoulos@fleming.gr](mailto:pavlopoulos@fleming.gr)

Or open an issue on GitHub:
[https://github.com/yourusername/NORMA/issues](https://github.com/yourusername/NORMA/issues)

---

## 📚 Publications

* **NORMA: The Network Makeup Artist**
  Koutrouli M., Karatzas E., Papanikolopoulou K., Pavlopoulos G.A.
  *Genomics, Proteomics & Bioinformatics*. 2022 Jun;20(3):578⎼586. Epub 2021 Jun 24.
  doi: [10.1016/j.gpb.2021.02.005](https://doi.org/10.1016/j.gpb.2021.02.005)
  PMID: [34171457](https://pubmed.ncbi.nlm.nih.gov/34171457/)

* **The network makeup artist (NORMA-2.0): Distinguishing annotated groups in a network using innovative layout strategies**
  Karatzas E., Koutrouli M., Baltoumas F.A., Papanikolopoulou K., Bouyioukos C., Pavlopoulos G.A.
  *Bioinformatics Advances*. 2022 May 13;2(1)\:vbac036.
  doi: [10.1093/bioadv/vbac036](https://doi.org/10.1093/bioadv/vbac036)
  PMID: [36699373](https://pubmed.ncbi.nlm.nih.gov/36699373/)

---

## 📄 License

This project is licensed under the **MIT License** – see the [LICENSE](LICENSE) file for details.
