#' Scale image stack
#'
#' Scales the pixel values of an image stack between a new maximum and minimum values.
#'
#' @param Stack an image stack object
#' @param newMin the new minimum pixel value (default=0)
#' @param newMax the new maximum pixel value (default=1)
#'
#' @return An array of the same dimensions as \code{Stack} with scaled pixel values.
#'
#' @author Alessio Veneziano
#'
#' @export

scaleStack<-function(Stack,newMin=0,newMax=1){
  (Stack-min(Stack))/(max(Stack)-min(Stack))*
    (newMax-newMin)+newMin
}
