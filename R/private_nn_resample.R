#' Resample points to preserve privacy
#'
#' This function takes a set of x,y coordinates and resamples them
#' according to a nearest-neighbour kernel, so that points in sparser
#' parts of the distribution have a larger bandwidth, and their
#' kernels overlap with high probability on the kernels of at least
#' K other points
#'
#' @param x a two-column matrix of coordinates
#' @param id a vector of integers representing group membership (default NULL)
#' @param K the number of nearest neighbours each kernel must overlap with
#'        default 5.
#' @param size the number of points to sample. Default \code{nrow(x)}
#'
#' @return a two-column matrix of resampled points
#'
#' @export
private_nn_resample = function(x,id=NULL,K=5,size=nrow(x)){

  bandwidths = numeric(NA,nrow(x))


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


