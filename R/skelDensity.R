#' Skeleton density
#'
#' Calculates 3D density of nodes and branches of the topological skeleton based on a Kernel density estimate,
#' using the \code{kde3d} function in the package 'misc3d'. Density is calculated as point per cube centimeter.
#'
#' @param skel topological skeleton, output of \code{readAmiraSkeleton}
#' @param method defines if the density is calculated based on the skeleton nodes ('node') or branches ('edge')
#' @param Scale logical: if TRUE, the noes and branch coordinates are scaled on their overall variance
#' @param nxyz a vector of length 3 indicating the dimensions of the 3D grid for the estimation of the kernel density (default = 50x50x50)
#' 
#' @return
#' \item{Density}{a vector of densities estimated at each node (if \code{method = 'node'}; unit: node per cube cm) or branch point (if \code{method = 'edge'}; unit: branch point per cube cm).}
#' \item{map}{an array of dimension \code{nxyz} with the estimated kernel density.}
#' 
#' @author Alessio Veneziano
#'
#' @references
#' Venables, WN and Ripley, BD. 2002. Modern Applied Statistics with S. Fourth edition. Springer.
#'
#' @examples
#' #Calculate and visualize skeleton node density
#' data(exampleSkeleton)
#' dens<-skelDensity(exampleSkeleton,method="node")
#'   image(dens$map[,,25])
#'
#' @export
  
skelDensity<-function(skel,method=c("node","edge"),Scale=T,nxyz=c(50,50,50)){
  require(misc3d)
  require(Morpho)
  
  skVC<-skel[[2]]
  skEPC<-skel[[5]]
  
  if(length(method==2)){skx<-skVC
  } else {
    if(method=="node"){skx<-skVC
    } else {skx<-skEPC}
  }
  
  if(Scale){skx<-matrix(scale(c(skx)),nrow(skx),ncol(skx),byrow=F)}
  
  dd<-kde3d(skx[,1],skx[,2],skx[,3],n=nxyz)
  
  k<-expand.grid(dd$x,dd$y,dd$z)
  kpos<-expand.grid(1:nxyz[1],1:nxyz[2],1:nxyz[3])
  clo<-mcNNindex(as.matrix(k),skx,k=1)
  clo<-as.matrix(kpos[clo,])
  kdens<-dd$d[clo]
  
  return(list(Density=kdens,map=dd$d))
}

