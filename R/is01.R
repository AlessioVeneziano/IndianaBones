#' Check 01 pixels in image stack
#'
#' Check if the pixel values of a binarised image stack are zeros and ones.
#'
#' @param Stack an image stack object
#'
#' @return \code{TRUE} or \code{FALSE} depending on whether \code{Stack} has 01 pixels.
#'
#' @author Alessio Veneziano
#'
#' @export

is01<-function(Stack){
  if(!isBinarized(Stack)){stop("image is not binarized")}

  v<-unique(c(Stack))
  all(v%in%c(0,1))
}
