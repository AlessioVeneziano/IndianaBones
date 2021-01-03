#' Read topological skeleton
#' 
#' Read topological skeleton file exported from the Amira/Avizo software in ASCII format.
#' 
#' @param path path of the topological skeleton file (exported from Amira/Avizo in ASCII format)
#' @param removeLoops logical: if TRUE, loop skeleton branches (same initial and ending node) are removed
#' @param removeConn logical: if TRUE, removes the nodes (and linked branches) connected to more than \code{maxConn} branches
#' @param maxConn maximum number of branches connected to each node; ignored if \code{removeConn = FALSE} 
#' 
#' @return A list including: node coordinates (VertexCoordinates), branch coordinates (EdgePointsCoordinates), node-to-node connectivity (EdgeConnectivity),
#' number of 3D points constituting each branch (NumEdgePoints) and thickness at each branch point (Thickness).
#'
#' @author Alessio Veneziano
#'
#' @export

readAmiraSkeleton<-function(path,removeLoops=T,removeConn=F,maxConn=10){
  sk<-readLines(path)
  
  info<-grep("define",sk)
  info<-gsub("define ","",sk[info])
  info<-strsplit(info," ")
  info<-matrix(unlist(info),3,2,T)
  
  at<-grep("@",sk)
  at<-at[-(1:(length(at)/2))]
  dims<-as.numeric(info[,2])[c(1,2,2,3,3)]
  
  skel<-list(info)
  for(i in 1:length(dims)){
    tmp<-sk[(at[i]+1):(at[i]+dims[i])]
    tmp<-as.numeric(unlist(strsplit(tmp," ")))
    skel[[i+1]]<-t(matrix(tmp,ncol=dims[i],byrow=F))
  }
  names(skel)<-c("info","VertexCoordinates","EdgeConnectivity",
                 "NumEdgePoints","EdgePointsCoordinates",
                 "Thickness")
  
  if(removeLoops){
    nLoops<-sum(apply(skel$EdgeConnectivity,1,diff)==0)
    
    keepEC<-which(apply(skel$EdgeConnectivity,1,diff)!=0)
    keepEPC<-rep(1:length(skel$NumEdgePoints),skel$NumEdgePoints)
    keepEPC<-which(keepEPC%in%keepEC)
    
    skel$EdgeConnectivity<-skel$EdgeConnectivity[keepEC,]
    skel$NumEdgePoints<-skel$NumEdgePoints[keepEC,]
    skel$EdgePointsCoordinates<-skel$EdgePointsCoordinates[keepEPC,]
    skel$Thickness<-skel$Thickness[keepEPC,]
    
    skel$info[,2]<-as.character(c(nrow(skel$VertexCoordinates),
                                  nrow(skel$EdgeConnectivity),
                                  nrow(skel$EdgePointsCoordinates)))
    
    message(paste("Loops removed:",nLoops))
  }
  
  if(removeConn){
    nodeC<-table(c(skel$EdgeConnectivity)+1)
    
    delVC<-which(nodeC>maxConn)
    delEC<-apply(skel$EdgeConnectivity+1,2,function(x,b) which(x%in%b),b=delVC)
    delEC<-unique(unlist(delEC))
    delEPC<-rep(1:length(skel$NumEdgePoints),skel$NumEdgePoints)
    delEPC<-which(delEPC%in%delEC)
    
    skel$VertexCoordinates<-skel$VertexCoordinates[-delVC,]
    skel$EdgeConnectivity<-skel$EdgeConnectivity[-delEC,]
    skel$NumEdgePoints<-skel$NumEdgePoints[-delEC]
    skel$EdgePointsCoordinates<-skel$EdgePointsCoordinates[-delEPC,]
    skel$Thickness<-skel$Thickness[-delEPC]
    
    skel$info[,2]<-as.character(c(nrow(skel$VertexCoordinates),
                                  nrow(skel$EdgeConnectivity),
                                  nrow(skel$EdgePointsCoordinates)))
    
    message(paste("Nodes with more than",paste(maxConn,"connections removed")))
  }
  
  return(skel)
}

