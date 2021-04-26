#' Cubic Volume
#'
#' Generates a Cubic volume of user's specified size.
#'
#' @param nvoxel number of voxels along the side of the cube.
#' @param offset number of voxels of the border around the cubic volume
#'
#' @details \code{makeCube} is used by the function \code{makeVOI} to extract VOIs from a 3D volume.
#'
#' @return
#' \item{coords}{a matrix of voxel coordinates of the cube.}
#' \item{volume}{a binarised 3D array of the volume of the cube.}
#'
#' @author Alessio Veneziano
#'
#' @examples
#' #Create a volume of a 21x21x21 cube with 2 voxels of offset
#' cube<-makeCube(21,2)
#'
#' #Plot the 3D coords of the cube
#' require(rgl)
#' points3d(cube$coords,col="grey",size=2)
#'
#' #Plot a slice of the volume (the cube is in white pixels)
#' image(cube$volume[,,10],col=grey(0:32/32))
#'
#' @export

makeCube<-function(nvoxel,offset=2){
  coords<-expand.grid(1:nvoxel,1:nvoxel,1:nvoxel)
  coords<-as.matrix(coords)

  vol<-array(0,rep(nvoxel+offset*2,3))
  coords<-coords+offset
  vol[coords]<-1

  return(list(coords=coords,volume=vol))
}
