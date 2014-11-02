##############################################################################################
# graph : object of class 'graph' representing pathway
#         (or subset of pathway) to be plotted.
# de    : log2 fold change, entrez GeneID named vector
#         of differentially expressed genes/proteins.
# organism: 3-letter KEGG code of organism 
#           (e.g., "mmu" for mouse).
# cols  : vector of colours indicating heatmap gradient.
# all   : entrez GeneID character vector of background
#         genes/proteins.
##############################################################################################

pathwayHeatmap <- function(graph,layout="fdp",de,cols,subGList=list(),nodeAttrs=list(),attrs=list()){
  require(graph);
  
  deKID <- names(de);
  allKID <- all;
  
  # Retrieves the log2FC of nodes in the graph from 'de'.
  logfc <- de[match(nodes(graph),deKID)];
  
  # Annotates logfcs with node names.
  names(logfc) <- nodes(graph); 
  
  # NA values in logfc are nodes that were not in 'de'; set
  # values to 0.
  logfc[is.na(logfc)] <- 0;
  
  # Translate logfcs so that ~0 values are equal to half the
  # length of the cols vector. These values represent small
  # fold change between the two classes are mapped to the
  # colour corresponding to small FC (i.e., middle colour).
  incol <- round(logfc + trunc(length(cols)/2));
  incol[incol > length(cols)] <- length(cols);
  incol[incol < 0] <- 1;
  
  # Identify if nodes in the graph were detected in the
  # background. *May or may not keep this.*
  #undetected <- !nodes(graph) %in% allKID;
  
  logcol <- cols[incol];
  logcol[logfc==0] <- "darkgrey";
  
  names(logcol) <- names(logfc);
  
  nAttrs <- nodeAttrs;
  nAttrs$fillcolor <- logcol;
  
  if(!length(globalAttrs)){
    attrs=list(node=list(shape="circle",
                         fontsize=20,
                         fixedsize=T),
               edge=list(arrowsize="0.6"));
  }
  if(length(subGList)){
    plot(graph,
         layout,
         subGList=subGList,
         nodeAttrs = nAttrs,
         attrs = attrs);
  }else{
    plot(graph,
         layout,
         nodeAttrs = nAttrs,
         attrs = attrs);
  }
}