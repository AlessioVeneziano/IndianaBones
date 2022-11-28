#' Skeleton Length
#'
#' Calculates the branch length of the topological skeleton and the average branch length per node.
#'
#' @param skel topological skeleton, output of \code{readAmiraSkeleton}
#'
#' @return
#' \item{edgeL}{a vector of branch lengths, of length equal to the number of branches in the topological skeleton.}
#' \item{nodeL}{a vector of average branch length per node, of length equal to the number of nodes in the topological skeleton.}
#'
#' @author Alessio Veneziano, Fabio Alfieri
#'
#' @references
#' Veneziano A, Cazenave M, Alfieri F, Panetta D, Marchi D. 2021. Novel strategies for the characterization of cancellous bone morphology: Virtual isolation and analysis. American Journal of Physical Anthropology.
#'
#' @examples
#' #Calculate branch length of a topological skeleton
#' data(exampleSkeleton)
#' len<-skelLength(exampleSkeleton)
#'
#' @export

skelLength<-function(skel){
  skVC<-skel[[2]]
  skEC<-skel[[3]]
  skNEP<-skel[[4]]
  skEPC<-skel[[5]]

  edgeP<-skEPC
  edgeN<-skNEP
  edgeL<-c(skNEP*NA)
  for(i in 1:nrow(skEC)){
    edge<-skEC[i,]+1
    p<-edgeP[1:edgeN[1],]
    Diag<-cbind(1:(edgeN[1]-1),2:edgeN[1])
    edgeL[i]<-sum(as.matrix(dist(p))[Diag])
    edgeP<-edgeP[-c(1:edgeN[1]),]
    edgeN<-edgeN[-1]
  }
  meanL<-aggregate(c(edgeL,edgeL),list(c(skEC[,1],skEC[,2])),mean)$x

  return(list(edgeL=edgeL,nodeL=meanL))
}
