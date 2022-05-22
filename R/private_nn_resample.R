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
#' @param resample the points with replacement? Default TRUE
#'
#' @return a two-column matrix of resampled points with attributes
#'         "id", "K" and "bandwidth"
#'
#' @export
private_nn_resample = function(coords,id=NULL,K=5,size=nrow(x)
                               ,resample=TRUE){

    ### Calculate pairwise distances
  dists = pw.dists(coords)

  ### Same id indicator matrix for masking
  same_id = sapply(id,function(i){i==id})

  ### Get distance to K'th nearest neighbour in same id
  bandwidths = sapply(1:nrow(dists),function(x){
    y <- dists[x,same_id[x,]]
    return(y[order(y)][K])
  })

  if(resample){
    ### Resample with replacement
    resamp = sample(1:nrow(coords),replace=T,size=size)

    bandwidths = bandwidths[resamp]
    id = id[resamp]
    coords = coords[resamp,]
  }


  ### -------------------------------------------------------------
  ### --        Sample from uniform disc
  ### -------------------------------------------------------------
  ### x <- r*cos(angle)
  ### y <- r*sin(angle)


  ### Sample using polar coords with radius=dist(K)+eps
  ### Use adjusted sample R*sqrt(runif(1))
  r <- sapply(bandwidths,function(x){ x*sqrt(runif(1)) })
  angle <- runif(size,0,2*pi)

  ### Transform to cartesian and add to resampled dataframe
  xjitter <- r*cos(angle)
  yjitter <- r*sin(angle)

  res = matrix(NA,size,2)
  ### Add sampled noise to location
  res[,1] = coords[,1] + xjitter
  res[,2] = coords[,2] + yjitter

  attr(res,"bandwidth") = bandwidths
  attr(res,"K") = K
  attr(res,"id") = id
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


