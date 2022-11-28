#' Skeleton tortuosity
#'
#' Calculates branch tortuosity of the topological skeleton.
#'
#' @param skel topological skeleton, output of \code{readAmiraSkeleton}
#' @param cores number of cores for parallel computation (default=2)
#'
#' @details Tortuosity of a segment is the ratio between its length and the linear distance between its starting and ending points. In the topological
#' skeleton, tortuosity is calculated as the ratio between the the branch length and the linear distance between the nodes it connects. Tortuosity
#' ranges from 1 to infinity.
#'
#' @return A vector of branch tortuosity, of length equal to the number of branches in the topological skeleton.
#'
#' @author Alessio Veneziano, Fabio Alfieri
#'
#' @references
#' Veneziano A, Cazenave M, Alfieri F, Panetta D, Marchi D. 2021. Novel strategies for the characterization of cancellous bone morphology: Virtual isolation and analysis. American Journal of Physical Anthropology.
#' Roque WL, & Alberich-Bayarri A. 2015. Tortuosity influence on the trabecular bone elasticity and mechanical competence. In Jorge RN, Tavares JM (Eds) Developments in Medical Image Processing and Computational Vision, pp 173-191. Springer, Cham
#'
#' @examples
#' #Calculate branch tortuosity of the topological skeleton
#' data(exampleSkeleton)
#' tort<-skelTortuosity(exampleSkeleton)
#'
#' @export

skelTortuosity<-function(skel,cores = 2){
  require(foreach)
  require(doParallel)
  registerDoParallel(cores=cores)
  func<-function(aa,bb,cc,dd){
    edge<-bb+1
    direct<-aa[edge,]
    indirect<-cc[dd[1]:dd[2],]
    Diag<-cbind(dd[1]:(dd[2]-1),(dd[1]+1):dd[2])
    Diag<-Diag-min(Diag)+1
    d1<-c(dist(direct))
    d2<-sum(as.matrix(dist(indirect))[Diag])
    return(d2/d1)
  }

  skVC<-skel[[2]]
  skEC<-skel[[3]]
  skNEP<-skel[[4]]
  skEPC<-skel[[5]]

  skNEP<-c(0,cumsum(skNEP))
  skNEP<-cbind(skNEP[-length(skNEP)]+1,skNEP[-1])

  edgeTO<-foreach(j=1:nrow(skEC)) %dopar% func(skVC,skEC[j,],skEPC,skNEP[j,])
  edgeTO<-unlist(edgeTO)

  return(edgeTO)
}

