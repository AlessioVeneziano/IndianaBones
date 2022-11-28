#' Skeleton thickness
#'
#' Extracts branch thickness from the topological skeleton. The thickness is recorded at each point of the branch.
#'
#' @param skel topological skeleton, output of \code{readAmiraSkeleton}
#'
#' @return A vector of branch thickness calculated at each coordinate making up the branch.
#'
#' @author Alessio Veneziano, Fabio Alfieri
#'
#' @references
#' Veneziano A, Cazenave M, Alfieri F, Panetta D, Marchi D. 2021. Novel strategies for the characterization of cancellous bone morphology: Virtual isolation and analysis. American Journal of Physical Anthropology.
#'
#' @examples
#' #Extract thickness information from the topological skeleton
#' data(exampleSkeleton)
#' dens<-skelThickness(exampleSkeleton)
#'
#' @export

skelThickness<-function(skel){
  skEPC<-skel[[5]]
  skTH<-skel[[6]]

  edgeTH<-skTH

  return(edgeTH)
}
