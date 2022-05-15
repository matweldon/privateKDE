#' Resample points to preserve privacy
#'
#' This function takes a set of x,y coordinates and resamples them
#' according to a nearest-neighbour kernel, so that points in sparser
#' parts of the distribution have a larger bandwidth, and their
#' kernels overlap with high probability on the kernels of at least
#' K other points
#'
#' @param coords a two-column matrix of coordinates
#' @param id a vector of integers representing group membership (default NULL)
#' @param K the number of nearest neighbours each kernel must overlap with
#'        default 5.
#' @param size the number of points to sample. Default \code{nrow(x)}
#'
#' @return a two-column matrix of resampled points
#'
#' @export
private_nn_resample = function(coords,id=NULL,K=5,size=nrow(x)){

  bandwidths = numeric(NA,nrow(coords))

  ### Calculate pairwise distances
  dists = pw.dists(coords)

  ### Same id indicator matrix for masking
  same_id = sapply(id,function(i){i==id})

  ### Get K'th nearest neighbour in same id
  nn = sapply(1:nrow(dists),function(x){
    y <- dists[x,same_id[x,]]
    return(y[order(y)][K])
  })

  ### Resample with replacement
  resamp <- sample(1:nrow(coords),replace=T,size=size)

  nn_new <- nn[resamp]

  coords_new <- coords[resamp,]

  ### -------------------------------------------------------------
  ### --        Sample from uniform disc
  ### -------------------------------------------------------------
  ### x <- r*cos(angle)
  ### y <- r*sin(angle)


  ### Sample using polar coords with radius=dist(K)+eps
  ### Use adjusted sample R*sqrt(runif(1))
  r <- sapply(nn_new,function(x){ x*sqrt(runif(1)) })
  angle <- runif(size,0,2*pi)

  ### Transform to cartesian and add to resampled dataframe
  xjitter <- r*cos(angle)
  yjitter <- r*sin(angle)

  res = matrix(NA,size,2)
  ### Add sampled noise to location
  res[,1] = coords_new[,1] + xjitter
  res[,2] = coords_new[,2] + yjitter

  return(res)
}












# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


