#' Spheric Volume
#'
#' Generates a Spheric volume of user's specified size.
#'
#' @param nvoxel number of voxels along the diameter of the sphere.
#' @param offset number of voxels of the border around the cubic volume
#'
#' @details \code{makeSphere} is used by the function \code{makeVOI} to extract VOIs from a 3D volume.
#'
#' @return
#' \item{coords}{a matrix of voxel coordinates of the sphere.}
#' \item{volume}{a binarised 3D array of the volume of sphere.}
#'
#' @author Alessio Veneziano
#'
#' @examples
#' #Create a volume of a 21 voxels diameter sphere with 2 voxels of offset
#' sph<-makeSphere(21,2)
#'
#' #Plot the 3D coords of the sphere
#' require(rgl)
#' points3d(sph$coords,col="grey",size=2)
#'
#' #Plot a slice of the volume (the sphere is in white pixels)
#' image(sph$volume[,,10],col=grey(0:32/32))
#'
#' @export

makeSphere<-function(nvoxel,offset=2){
  coords<-expand.grid(1:nvoxel,1:nvoxel,1:nvoxel)
  coords<-as.matrix(coords)
  bc<-round(apply(coords,2,mean))

  eucl<-function(a,b){sqrt(sum((a-b)^2))}
  r<-eucl(bc,c(bc[1:2],nvoxel))

  d<-apply(coords,1,eucl,b=bc)
  coords<-coords[d<=r,]

  vol<-array(0,rep(nvoxel+offset*2,3))
  coords<-coords+offset
  vol[coords]<-1

  return(list(coords=coords,volume=vol))
}
