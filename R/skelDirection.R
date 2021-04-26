#' Skeleton angle and direction
#'
#' Calculates the direction of the branches of the topological skeleton. The vector sum of all directions is used to calculate
#' an overall angle of the skeleton branches with respect to a reference axis.
#'
#' @param skel topological skeleton, output of \code{readAmiraSkeleton}
#' @param Axis the axis to use as reference for the calculation of the branch angle.
#'
#' @details The 3D angle is calculated in radians between a reference axis and the unitary resultant of all branch directions
#' obtained by vector sum in 3D. \code{Axis} can be provided by the user as a matrix with starting and ending coordinates of
#' the axis. Alternatively, if \code{Axis} is set to 1, 2 or 3, the reference axis is calculated as the principal component
#' axes of the skeleton node coordinates using \code{getAxis}.
#'
#' @return
#' \item{angles}{a vector of branch angles, of length equal to the number of branches.}
#' \item{directions}{a matrix of branch vector directions.}
#' \item{refAxis}{a matrix of the coordinates of the reference axis.}
#' \item{resDir}{a vector of the main branch direction resulting from vector sum.}
#' \item{resAng}{the main branch angle calculated in radians.}
#'
#' @author Alessio Veneziano
#'
#' @references
#' Veneziano A, Cazenave M, Alfieri F, Panetta D, Marchi D. 2021. Novel strategies for the characterization of cancellous bone morphology: Virtual isolation and analysis. American Journal of Physical Anthropology.
#'
#' @export

skelDirection<-function(skel,ax = 1){
  skVC<-skel[[2]]
  skEC<-skel[[3]]+1

  if(length(ax)==1){ax<-getAxis(skVC,ax)
  } else if (is.matrix(ax)){ax<-ax
  } else (stop("Axis must be specified"))
  ref<-c(abs(diff(ax)))
  D<-apply(skEC,1,function(b,x) diff(x[b,]),x=skVC)
  D<-t(D)

  rD<-apply(D,2,sum)
  rDN<-rD/norm(rD,"2")
  aDN<-vectorAngle(rDN,ref)

  ang<-apply(D,1,vectorAngle,b=ref)

  return(list(angles=ang,directions=D,refAxis=ax,resDir=rDN,resAng=aDN))
}

