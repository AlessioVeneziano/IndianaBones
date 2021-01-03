#' Skeleton connectivity
#'
#' Calculates the connectivity of the topological skeleton as the number of branches connected to each node.
#'
#' @param skel topological skeleton, output of \code{readAmiraSkeleton}
#' 
#' @return A vector of the number of branches connected to each node. The vector has length equal to the number of nodes in the topological skeleton.
#' 
#' @author Alessio Veneziano
#'
#' @examples
#' #Calculate skeleton connectivity
#' data(exampleSkeleton)
#' conn<-skelConnectivity(exampleSkeleton)
#'
#' @export

skelConnectivity<-function(skel){
  skVC<-skel[[2]]
  skEC<-skel[[3]]
  nodeC<-as.vector(table(c(skEC)+1))
  
  return(nodeC)
}
