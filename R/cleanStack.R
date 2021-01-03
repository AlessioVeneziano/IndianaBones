#' Clean Image Stack
#' 
#' Delete single pixels or isolated pixel islands from an image stack. The pixels are checked in 3D (voxels).
#' 
#' @param Stack a binary image stack object
#' @param nvoxel maximum number of 3D pixels (voxels) considered as isolated islands
#' 
#' @details \code{cleanStack} uses the 'connected component' algorithm (using the function \code{components} in the package 'mmand') to find isolated pixel islands in the 3D volume of the image stack. The islands are then deleted from the stack.
#' The function works on binary images, assuming that background pixels are of value '0'. \code{cleanStack} checks the neighborhood of 3D pixels (voxels) to find clusters that are disconnected from other pixels. The 26 neighbors of each 3D pixels
#' are checked using a 'box' kernel defined within the function. The argument \code{nvoxel} specifies the maximum number of 3D pixels that are considered an island: islands consisting of a number of 3D pixels lower or equal to \code{nvoxel} are
#' deleted. When \code{nvoxel} has value '0' (default) all but the largest pixel island are removed.
#' 
#' @return An array of the same dimension as \code{Stack} with 3D pixel islands removed.
#'
#' @author Alessio Veneziano
#'
#' @examples
#' #Clean image stack keeping only the largest connected region
#' data(exampleStack)
#' compStack<-cleanStack(exampleStack,nvoxel=0)
#' image(exampleStack[,,30])
#' image(compStack[,,30])
#'
#' @export

cleanStack<-function(Stack,nvoxel = 0){
  require(mmand)
  
  if(!isBinarized(Stack)){stop("image is not binarized")}
  
  kern<-array(1,c(3,3,3))
  comp<-components(Stack,kern)
  tab<-table(comp)
  
  if(nvoxel==0){
    tab<-sort(tab,T)
    largest<-as.numeric(names(tab)[1])
    Stack[which(comp!=largest,arr.ind=T)]=0
  } else {Stack[comp%in%which(tab<=nvoxel)]<-0}
  
  return(Stack)
}

