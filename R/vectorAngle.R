#' Calculate angle between two vectors
#'
#' Calculates angle between vectors.
#'
#' @param a first vector
#' @param b second vector
#'
#' @return Angle between vectors in radians.
#'
#' @author Alessio Veneziano, Fabio Alfieri
#'
#' @export

vectorAngle<-function(a,b){
  acos(a%*%b/(sqrt(a%*%a)*sqrt(b%*%b)))
}
