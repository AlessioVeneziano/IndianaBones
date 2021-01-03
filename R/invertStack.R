#' Invert image stack
#' 
#' Inverts all the pixel values in an image stack, creating a negative of the images.
#' 
#' @param Stack an image stack object
#' 
#' @return An array of the same dimensions as \code{Stack} with inverted pixel values.
#' 
#' @author Alessio Veneziano
#' 
#' @examples
#' #Invert pixel values of binary volume and plot 2D image
#' data(exampleStack)
#' invStack<-invertStack(exampleStack)
#' image(exampleStack[,,30])
#' image(invStack[,,30])
#' 
#' @export

invertStack<-function(Stack){
  im<-max(Stack)-Stack
  
  return(im)
}
