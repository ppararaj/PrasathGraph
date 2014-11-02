downstreamSearcher <- function(graph,node){
  require(graph);
  
  outPath <- unlist(edges(graph)[node]);
  alreadyChecked <- node;
  
  while(length(outPath) > 0){
    addToOutPath <- vector();
    for(i in 1:length(outPath)){
      currNode <- outPath[i];
      
      if(currNode %in% alreadyChecked){
        next;
      }
      
      moreConnections <- unlist(edges(graph)[currNode]);
      moreConnections <- moreConnections[!(moreConnections %in% alreadyChecked)];
      if(length(moreConnections)){
        addToOutPath <- c(addToOutPath,moreConnections);
      }
      alreadyChecked <- c(alreadyChecked,currNode);
    }
    outPath <- addToOutPath[!(addToOutPath %in% alreadyChecked)];
  }
  alreadyChecked <- unname(alreadyChecked);
  return(alreadyChecked);
}