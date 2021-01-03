#' Bone segmentation
#'
#' Performs the digital separation of cancellous and compact bone in a micro-CT image stack.
#'
#' @param Stack binary micro-CT image stack of bone specimen (in 3D array form)
#' @param strel KxK structuring element (with odd K) for dilation/erosion operators
#' @param iterMask number of dilation/erosion iterations for generating the bone mask
#' @param iterFill number of dilation/erosion iterations for filling the inside of the bone (cancellous and empty spaces)
#' @param iterCompact number of dilation/erosion iterations for refining the compact bone
#' @param cleanVoid maximum number of 3D pixels (voxels) considered as isolated islands. This argument is used to clean the image stack during the processing (see Details).
#'
#' @details \code{splitBone} uses an iterative process to separate cancellous and compact bone in a micro-CT image stack. The protocol consists of five sequential operations alternating
#' dilation/erosion (using \code{dilate} and \code{erode} from the package 'EBImage') to subtractions between images. Dilations and erosions are applied along the Z direciton of the
#' image stack (along the third dimension of \code{Stack}). The structuring element (\code{strel}) is the binarized kernel (of odd dimensions) used for 'probing' the shapes in the
#' input image. The arguments \code{iterMask}, \code{iterFill} and \code{iterCompact} specify the number of dilation/erosion iterations to be performed in each step of the protocol.
#' The function \code{splitBone} implements the following workflow:
#' Step 1: the white pixels of the binary image stack undergo dilations (iterMask) that fill the empty spaces (voids) within the bone; the same amount of erosions (iterMask) shrinks
#' the bone back to its original size and external contours. The result is a mask of the whole bone volume.
#' Step 2: the mask is substracted from the binary image, forming a new stack with only empty spaces preserved.
#' Step 3: multiple dilations (iterFill) of the voids fill the spaces occupied by the cancellous bone and erosions (iterFill) restore its contours. The result is a stack preserving
#' the internal region of the bone (cancellous + voids).
#' Step 4: the internal region is then subtracted from the mask, thus isolating the compact bone. The compact bone is refined using dilation/erosion (iterCompact).
#' Step 5: the cancellous bone is finally obtained by subtracting the voids and the compact bone from the mask.
#' The argument \code{cleanVoid} defines the type of cleaning applied to the image stack at Step 3. Cleaning may be useful to remove small pixel leftovers of incomplete
#' dilation/erosion. The function \code{cleanStack} is used to identify and delete isolated regions of 3D pixels (voxels). When \code{cleanVoid} has value '0', all but
#' the largest pixel island are removed; when \code{cleanVoid} is \code{NULL}, no cleaning is performed; when \code{cleanVoid > 0}, islands consisting of a number of
#' 3D pixels lower or equal to \code{cleanVoid} are deleted.
#'
#' @return
#' \item{mask}{an array of the bone mask of the same dimension as \code{Stack}.}
#' \item{void}{an array of the empty spaces within the bone (voids) of the same dimension as \code{Stack}.}
#' \item{fill}{an array of the internal region of the bone (cancellous + voids) of the same dimension as \code{Stack}.}
#' \item{compact}{an array of the compact bone of the same dimension as \code{Stack}.}
#' \item{trab}{an array of the cancellous bone of the same dimension as \code{Stack}.}
#'
#' @author Alessio Veneziano
#'
#' @references
#' Urbach ER, Wilkinson MH. 2007. Efficient 2-D grayscale morphological transformations with arbitrary flat structuring elements. IEEE Transactions on image processing, 17(1), 1-8.
#'
#' @examples
#' #Segment cancellous and compact bone from 3D image stack
#' data(exampleStack)
#' require(EBImage)
#' strel<-makeBrush(3,"disc")
#' seg<-splitBone(Stack,strel,3,2,3,0)
#'   image(exampleStack[,,30],col=grey(0:32/32))
#'   image(seg$trab[,,30],col=grey(0:32/32))
#'   image(seg$comp[,,30],col=grey(0:32/32))
#'
#' @export
  
splitBone<-function(Stack,strel,iterMask=3,iterFill=3,iterCompact=3,cleanVoid=NULL){
  require(EBImage)
  require(mmand)

  if(!isBinarized(Stack)){stop("image is not binarized")}
  im<-Stack

  message("now performing: MASKING")
  mask<-im
  if(length(iterMask)==0){
    mask<-EBImage::fillHull(mask)
  } else {
    for(j in 1:iterMask){mask<-EBImage::dilate(mask,strel)}
    for(j in 1:iterMask){mask<-EBImage::erode(mask,strel)}
    mask<-EBImage::fillHull(mask)
  }

  void<-im*0
  void[im==0]<-1
  void[mask==0]<-0

  if(!is.null(cleanVoid)){
    void<-cleanStack(void,cleanVoid)
  }

  message("now performing: FILLING")
  fill<-void
  for(j in 1:iterFill){fill<-EBImage::dilate(fill,strel)}
  for(j in 1:iterFill){fill<-EBImage::erode(fill,strel)}
  fill<-fillHull(fill)

  message("now performing: EXTRACTION OF COMPACT BONE")
  compact<-mask
  compact[fill==1]<-0
  for(j in 1:iterCompact){compact<-EBImage::dilate(compact,strel)}
  for(j in 1:iterCompact){compact<-EBImage::erode(compact,strel)}

  message("now performing: EXTRACTION OF CANCELLOUS BONE")
  trab<-im
  trab[compact==1]<-0

  return(list(mask=mask,void=void,fill=fill,compact=compact,trab=trab))
}
