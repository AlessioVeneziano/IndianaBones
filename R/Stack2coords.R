#' Binarised stack to coordinates
#'
#' Convert a binarised image stack into a 3D point cloud.
#'
#' @param Stack an image stack object
#'
#' @details \code{Stack2coords} convert a binarised image stack into a 3D point cloud, assuming the bone material has pixel value 1.
#'
#' @return A matrix of dimension Nx3 (N = number of bone voxels).
#'
#' @author Alessio Veneziano
#'
#' @export

Stack2coords<-function(Stack){
  if(!isBinarized(Stack)){stop("image is not binarized")}
  if(!is01(Stack)){Stack<-as01(Stack)}
  dims<-dim(Stack)
  if(length(dims)>3){Stack<-drop(Stack[,,1,])}

  p<-which(Stack==1,T)

  return(p)
}

