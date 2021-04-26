#' Write TIFF image stack
#'
#' Writes a 3D image stack array to a TIFF image stack. It uses the function \code{writeTIFF} in the package 'tiff' and runs in parallel using the packages 'foreach' and 'doParallel'.
#'
#' @param Stack an image stack object
#' @param path path where the TIFF images will be saved
#' @leadname leading part of the image name
#' @bits number of bits
#' @param cores number of cores for parallel computation (default=2)
#'
#' @author Alessio Veneziano
#'
#' @export

writeTiffStack<-function(Stack,path,leadname="stack_",bits=8,cores = 2){
  require(tiff)
  require(doParallel)
  require(foreach)

  if(!bits%in%c(8,16,32)){stop("bits has to be 8, 16 or 32")}

  dims<-dim(Stack)
  n<-dims[length(dims)]

  nam<-sprintf("%06d",1:n)
  nam<-paste(leadname,nam,sep="")
  nam<-paste(nam,".tif",sep="")
  nam<-paste(path,nam,sep="/")

  perm<-1:length(dims)
  perm[1:2]<-perm[2:1]
  Stack<-aperm(Stack,perm)

  registerDoParallel(cores=cores)
  if(length(dims)>3){
    suppressWarnings(
      im<-foreach(i=1:n) %dopar% tiff::writeTIFF(Stack[,,,i],nam[i],bits))
  } else {
    suppressWarnings(
      im<-foreach(i=1:n) %dopar% tiff::writeTIFF(Stack[,,i],nam[i],bits))
  }
}

