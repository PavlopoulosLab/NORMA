# Help pages - Download Example files ####
output$dros_net <- downloadHandler(
  filename = function() {
    paste('Drosophila (TAU) Network file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(dros_net, file, row.names = F, sep = "\t", quote = F)
  }
)
output$dros_annot <- downloadHandler(
  filename = function() {
    paste('Drosophila KEGG pathways', '.txt', sep = '')
  },
  content = function(file) {
    write.table(dros_annot, file,row.names = F, col.names = F, sep = "\t", quote = F)
  }
)
output$dros_louvain <- downloadHandler(
  filename = function() {
    paste('Drosophila Louvain automated annotation', '.txt', sep = '')
  },
  content = function(file) {
    write.table(dros_louvain, file,row.names = F, col.names = F, sep = "\t", quote = F)
  }
)
output$dros_express <- downloadHandler(
  filename = function() {
    paste('Drosophila Expressions file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(dros_express, file,row.names = F, col.names = F, sep = "\t", quote = F)
  }
)

#-------------------------------------------#

output$string_net_tp53 <- downloadHandler(
  filename = function() {
    paste('STRING TP53 Network file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(string_net_tp53, file, row.names = F, sep = "\t", quote = F)
  }
)
output$string_annot <- downloadHandler(
  filename = function() {
    paste('STRING TP53 Annotation file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(string_annot, file, row.names = F,col.names = F, sep = "\t", quote = F)
  }
)
output$string_expr <- downloadHandler(
  filename = function() {
    paste('STRING TP53 Expression file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(string_expr, file, row.names = F,col.names = F, sep = "\t", quote = F)
  }
)

#-------------------------------------------#

output$string_net_bcar3 <- downloadHandler(
  filename = function() {
    paste('STRING BCAR3 Network file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(string_net_bcar3, file, row.names = F, sep = "\t", quote = F)
  }
)
output$string_bp <- downloadHandler(
  filename = function() {
    paste('STRING BCAR3 GO Biological Processes file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(string_bp, file, row.names = F,col.names = F, sep = "\t", quote = F)
  }
)
output$string_mf <- downloadHandler(
  filename = function() {
    paste('STRING BCAR3 GO Molecular functions file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(string_mf, file, row.names = F,col.names = F, sep = "\t", quote = F)
  }
) 
output$string_kegg <- downloadHandler(
  filename = function() {
    paste('STRING BCAR3 GO KEGG file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(string_kegg, file, row.names = F,col.names = F, sep = "\t", quote = F)
  }
)

#-------------------------------------------#

output$co_express <- downloadHandler(
  filename = function() {
    paste('Human Gene Co-expression Network file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(co_express, file, row.names = F, sep = "\t", quote = F)
  }
)
output$co_express_bp <- downloadHandler(
  filename = function() {
    paste('Human Gene Co-expression GO Biological Processes file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(co_express_bp, file,row.names = F,col.names = F, sep = "\t", quote = F)
  }
)
output$co_express_mf <- downloadHandler(
  filename = function() {
    paste('Human Gene Co-expression GO Molecura functions file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(co_express_mf, file,row.names = F,col.names = F, sep = "\t", quote = F)
  }
) 
output$co_express_cc <- downloadHandler(
  filename = function() {
    paste('Human Gene Co-expression Cellular Componets file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(co_express_cc, file,row.names = F,col.names = F, sep = "\t", quote = F)
  }
)
output$co_express_kegg <- downloadHandler(
  filename = function() {
    paste('Human Gene Co-expression KEGG pathways file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(co_express_kegg, file,row.names = F,col.names = F, sep = "\t", quote = F)
  }
) 
output$co_express_mcode <- downloadHandler(
  filename = function() {
    paste('Human Gene Co-expression MCODE node coloring file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(co_express_mcode, file,row.names = F,col.names = F, sep = "\t", quote = F)
  }
)
#-----------------------------------------------------------------------------#

output$covid_19_net <- downloadHandler(
  filename = function() {
    paste('COVID-19 Network file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(covid_19_net, file, row.names = F, quote = F, sep = "\t")
  }
)
output$covid_19_interpro <- downloadHandler(
  filename = function() {
    paste('HomoSapiens Protein Domains - INTERPRO file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(covid_19_interpro, file, row.names = F, col.names= F, quote = F, sep = "\t")
  }
)
output$covid_19_bp <- downloadHandler(
  filename = function() {
    paste('HomoSapiens GO Annotation - Biological Process file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(covid_19_bp, file, row.names = F, col.names= F, quote = F, sep = "\t")
  }
)
output$covid_19_mf <- downloadHandler(
  filename = function() {
    paste('HomoSapiens GO Annotation - Molecular Function file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(covid_19_mf, file, row.names = F, col.names= F, quote = F, sep = "\t")
  }
)
output$covid_19_cc <- downloadHandler(
  filename = function() {
    paste('HomoSapiens Protein Domains GO Annotation - Cellular Components file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(covid_19_cc, file, row.names = F, col.names= F, quote = F, sep = "\t")
  }
)
output$covid_19_kegg <- downloadHandler(
  filename = function() {
    paste('HomoSapiens Protein Domains KEGG pathways file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(covid_19_kegg, file, row.names = F, col.names= F, quote = F, sep = "\t")
  }
)
output$covid_19_smart <- downloadHandler(
  filename = function() {
    paste('HomoSapiens Protein Domains SMART file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(covid_19_smart, file, row.names = F, col.names= F, quote = F, sep = "\t")
  }
)

#-----------------------------------------------------------------------------#

output$Gallus_gallus_net <- downloadHandler(
  filename = function() {
    paste('Gallus gallus Network file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(Gallus_gallus_net, file,row.names = F, quote = F, sep = "\t")
  }
)
output$Gallus_gallus_kegg <- downloadHandler(
  filename = function() {
    paste('BioGrid Chicken Gallus KEGG pathways file', '.txt', sep = '')
  },
  content = function(file) {
    write.table(Gallus_gallus_kegg, file,row.names = F, col.names = F, quote = F, sep = "\t")
  }
)

output$R_script <- downloadHandler(
  filename = function() {
    paste('Annotation_cleaner', '.R', sep = '')
  },
  content = function(file) {
    write.table(R_script, file, row.names = F,col.names = F, sep = "\t", quote = F)
  }
)
