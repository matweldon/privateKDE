
<!-- README.md is generated from README.Rmd. Please edit that file -->

# privateKDE

<!-- badges: start -->
<!-- badges: end -->

The goal of privateKDE is to …

## Installation

You can install the development version of privateKDE from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("matweldon/privateKDE")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
#library(privateKDE)
library(ggplot2)
library(MASS)

## Simulate a distribution of points including sparse and sharp elements. The sharp element is
## a uniform triangle, and the sparse element is a uniform array.
Nsparse = 15
Ndense = 100
coords1 = data.frame(x = runif(Nsparse,0,8)
                    ,y = runif(Nsparse,0,8))
coords2 = data.frame(x = runif(2*Ndense,3,6)
                    ,y = runif(2*Ndense,3,6))
coords2 = coords2[coords2$x<coords2$y,]
coords3 = data.frame(x = 10, y = 10)
coords = rbind(coords1,coords2,coords3)

ggplot(coords,aes(x=x,y=y)) + geom_point()
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
# version 2 (with ggplot2)
nn = 50
getLevel <- function(x,y,prob) { 
    kk <- MASS::kde2d(x,y)
    dx <- diff(kk$x[1:2])
    dy <- diff(kk$y[1:2])
    sz <- sort(kk$z)
    c1 <- cumsum(sz) * dx * dy
    approx(c1, sz, xout = 1 - prob)$y
}

# 90 and 50% contours
L99 <- getLevel(coords$x, coords$y, 0.99)
L90 <- getLevel(coords$x, coords$y, 0.90)

kk <- MASS::kde2d(coords$x, coords$y,n=nn,lims=c(0,12,0,12))
dimnames(kk$z) <- list(kk$x, kk$y)
dc <- data.frame(x=rep(kk$x,nn),y=rep(kk$y,each=nn),z=as.numeric(kk$z))

p <- ggplot() + geom_point(data=coords,aes(x=x,y=y)) +
geom_contour(data=dc,aes(x=x,y=y,z=z), breaks=L99, colour="red") +
geom_contour(data=dc,aes(x=x,y=y,z=z), breaks=L90, colour="yellow") +
ggtitle("95 (red) and 90 (yellow) contours of data") +
  coord_cartesian(xlim=c(0,12),ylim=c(0,12))

p
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
