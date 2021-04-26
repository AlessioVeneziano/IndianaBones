#' Convert pixel values to 0 and 1
#'
#' Convert the pixel values of a binarised image stack to zeros and ones.
#'
#' @param Stack an image stack object
#'
#' @return An array of the same dimensions as \code{Stack} with pixel values changed to 0 and 1.
#'
#' @author Alessio Veneziano
#'
#' @export

as01<-function(Stack){
  if(!isBinarized(Stack)){stop("image is not binarized")}

  v<-unique(c(Stack))
  if(any(v==0)){
    Stack[which(Stack!=0,T)]=1
  } else {
    Stack[which(Stack==v[1],T)]<-0
    Stack[which(Stack==v[2],T)]<-1
  }

  return(Stack)
}
