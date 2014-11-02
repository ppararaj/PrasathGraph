upstreamSearcher <- function(graph,node){
  require(graph);
  
  inPath <- unlist(inEdges(node,graph));
  alreadyChecked <- node;
  
  while(length(inPath) > 0){
    addToInPath <- vector();
    for(i in 1:length(inPath)){
      currNode <- inPath[i];
      
      if(currNode %in% alreadyChecked){
        next;
      }
      moreConnections <- unlist(inEdges(currNode,graph));
      moreConnections <- moreConnections[!(moreConnections %in% alreadyChecked)];
      if(length(moreConnections)){
        addToInPath <- c(addToInPath,moreConnections);
      }
      alreadyChecked <- c(alreadyChecked,currNode);
    }
    inPath <- addToInPath[!(addToInPath %in% alreadyChecked)];
  }
  alreadyChecked <- unname(alreadyChecked);
  return(alreadyChecked);
}