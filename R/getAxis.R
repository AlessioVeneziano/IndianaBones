#' Axes of point cloud
#'
#' Calculates principal component axes of 3D point cloud.
#'
#' @param p a 3D point cloud in a KxM matrix
#' @param ax principal component axis to calculate (default = 1)
#'
#' @details \code{findAxis} uses the function \code{princomp} to calculate the major axes of a 2D or 3D point cloud.
#' The axis specifies if the first (ax = 1), second (ax = 2) or third (ax = 3) principal component axis is calculated.
#'
#' @return A matrix of dimension 2xM with initial and ending points of the axis.
#'
#' @author Alessio Veneziano
#'
#' @examples
#' #Calculate and plot principal axes of 3D point cloud
#' x<-rnorm(1000,0,0.1)
#' y<-rnorm(1000,0,0.2)
#' z<-x+rnorm(1000,0,0.3)
#'   p<-cbind(x,y,z)
#' a1<-getAxis(p,1)
#' a2<-getAxis(p,2)
#' a3<-getAxis(p,3)
#'   require(rgl)
#'     points3d(p)
#'     lines3d(a1,col="red")
#'     lines3d(a2,col="blue")
#'     lines3d(a3,col="green")
#'
#' @export

getAxis<-function(p,ax = 1){
  if(ncol(p)>3 || is.vector(p)){stop("p must be a 2D (ncol = 2) or 3D (ncol = 3) matrix")}
  if(ncol(p)==2 & ax==3){stop("Principal Component axis 3 is not available for a 2D matrix")}

  cc<-c(mean(p[,1]),mean(p[,2]),mean(p[,3]))
  pca<-prcomp(p)
  ref<-cbind(c(cc[1]-pca$rotation[1,ax],cc[1]+pca$rotation[1,ax]),
             c(cc[2]-pca$rotation[2,ax],cc[2]+pca$rotation[2,ax]),
             c(cc[3]-pca$rotation[3,ax],cc[3]+pca$rotation[3,ax]))

  return(ref)
}

