#' Check binarized image stack
#'
#' Checks if an image stack is binarized (contains only two types of pixels, black and white).
#'
#' @param Stack an image stack object
#'
#' @return \code{TRUE} or \code{FALSE} depending on whether \code{Stack} is binarized or not.
#'
#' @author Alessio Veneziano
#'
#' @export

isBinarized<-function(Stack){
  val<-unique(c(Stack))
  ifelse(length(val)==2,T,F)
}
