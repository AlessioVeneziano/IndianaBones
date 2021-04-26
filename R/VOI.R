#' Extract VOI from 3D image Stack
#'
#' This function extracts a Volume Of Interest (VOI) of the desired size and shape from a 3D image stack.
#'
#' @param Stack an image stack object
#' @param shape the desired shape of the VOI. Options are 'sphere' (default) or 'cube'
#' @param shape maximum number of 3D pixels (voxels) considered as isolated islands
#' @param size for a spheric VOI, the number of voxels along the diameter of the sphere; for a cubic VOI, number of voxels along the side of the cube.
#' @param pos center position of the VOI.
#'
#' @details \code{VOI} is a simple function to extract a VOI from a 3D image stack. It needs careful positioning.
#'
#' @return An array of dimensions size x size x size containing the VOI subset from the image stack.
#'
#' @author Alessio Veneziano
#'
#' @examples
#' #Extract a spherical VOI at the barycenter of a Stack
#' data(exampleStack)
#' pos<-apply(which(Stack!=0,T),2,mean)
#' voi<-VOI(exampleStack,"sphere",50,pos)
#' image(voi[,,20])
#'
#' @export

VOI<-function(Stack,shape=c("sphere","cube"),size,pos){
  dims<-dim(Stack)
  if(length(dims)>3){Stack<-drop(Stack[,,1,])}
  if(any(dims<size)){stop("VOI size is larger than the Stack")}

  if(missing(size)){stop("'size' must be specified")}
  if(missing(pos)){stop("'pos' must be specified")}

  if(length(shape)==2 | all(shape=="sphere")){fun<-makeSphere
  } else if (all(shape=="cube")){
    fun<-makeCube} else {stop("shape must be either 'sphere' or 'cube'")}

  voi<-fun(size,0)
  voic<-voi$coords
  voiv<-voi$volume
  vc<-apply(voic,2,mean)
  voic<-t(t(voic)-(vc-pos))
  voic<-round(voic)

  voiv[voiv!=1]<-NA
  voiv[!is.na(voiv)]<-Stack[voic]

  return(voiv)
}

