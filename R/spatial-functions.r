# loading the required packages
require(rgdal);require(sp);require(maptools)
require(ggplot2);require(RColorBrewer)
require(ggmap)

en2ll <- function(x,y=NULL,inc=F){
  if(is.null(y)){
    if(ncol(x)!=2){stop("y is missing and x is not a matrix")}
    y <- x[,2]
    x <- x[,1]
  }
  df <- data.frame(x,y)
  coordinates(df) <- ~x+y
  
  proj4string(df) <- CRS("+init=epsg:27700")
  
  df2 <- spTransform(df,CRS("+init=epsg:4326"))
  
  df3 <- as.data.frame(df2)
  names(df3) <- c("lon","lat")
  
  if(inc==T){return(cbind(df3,df))}
  else{return(df3)}
}

# Pairwise distance matrix
pw.dists <- function(x,y=NULL,nn="n"){
  ## nn="n" returns matrix of pairwise distances
  ## nn="m" returns the indicator matrix for nearest neighbours
  ## nn="v" returns a vector of nearest neighbour indices
  ## (to code: option "r" for ranked distances)
  if(is.null(y)){
    if(dim(x)[2]==2){
      coords <- x
      y <- x[,2]
      x <- x[,1]
    }else{
      stop("x doesn't have two columns and y not supplied")
    }
    }
  else{coords <- cbind(x,y)}
  res <- apply(coords,1
               ,function(c){sqrt( (c[1] - x)^2 + (c[2]-y)^2  )})
  if(ncol(res)!=nrow(res)){stop("pw.dists problem")}
  if(nn!="n"){
    diag(res) <- NA
    idx <- apply(res,1,which.min)
    if(nn=="m"){
      m <- matrix(0,nrow(res),ncol(res))
      m[cbind(1:nrow(res),idx)] <- 1
      res <- m
    }else{res<-idx}
  }
  return(res)
}

# bipartite distance matrix
bi.dists <- function(x,y=NULL,x2,y2=NULL,nn="n"){
  ## nn="n" returns rectangular matrix of distances
  ## nn="m" returns the indicator matrix for nearest neighbours
  ## for elements of first set only
  ## nn="v" returns a length(x) vector of nearest 
  ## neighbour indices for elements of first set only
  ## (to code: option "r" for ranked distances)
  if(is.null(y)){
    if(dim(x)[2]==2){
      coords <- x
      y <- x[,2]
      x <- x[,1]
    }else{
      stop("x doesn't have two columns and y not supplied")
    }
  }
  if(is.null(y2)){
    if(dim(x2)[2]==2){
      coords2 <- x2
      y2 <- x2[,2]
      x2 <- x2[,1]
    }else{
      stop("x2 doesn't have two columns and y2 not supplied")
    }
  }
  else{coords2 <- cbind(x2,y2)}
  
  res <- apply(coords,1
               ,function(c){ifelse(c[3]==g2,
                 sqrt( (c[1] - x2)^2 + (c[2]-y2)^2 )
               )})
  res <- t(res)
  if(nn!="n"){
    idx <- apply(res,1,which.min)
    if(nn=="m"){
      m <- matrix(0,nrow(res),ncol(res))
      m[cbind(1:nrow(res),idx)] <- 1
      res <- m
    }else{res<-idx}
  }
  return(res)
}

bi_dists_grouped <- function(x,x2,nn="n"){
  ## x and x2 both have three columns. The last column is the groups
  ## nn="n" returns rectangular matrix of distances
  ## nn="m" returns the indicator matrix for nearest neighbours
  ## for elements of first set only
  ## nn="v" returns a length(x) vector of nearest 
  ## neighbour indices for elements of first set only
  ## (to code: option "r" for ranked distances)
  ## Need to re-add all the control-flow stuff
  
  res <- apply(x,1
               ,function(c){ifelse(c[3]==x2[,3],
                                sqrt( (c[1] - x2[,1])^2 + (c[2]-x2[,2])^2)
                                ,NA
               )})
  res <- t(res)
  if(nn!="n"){
    idx <- apply(res,1,function(x){
      z<-which.min(x);
      if(length(z)==1){return(z)}else{return(NA)}
    })
    if(nn=="m"){
      m <- matrix(0,nrow(res),ncol(res))
      m[cbind(1:nrow(res),idx)] <- 1
      res <- m
    }else{res<-idx}
  }
  return(res)
}

# x <- c(357576,395230,460044)
# y <- c(459031,504035,452396)
# 
# print(en2ll(x,y))
# #           x        y
# # 1 -2.649077 54.02552
# # 2 -2.075038 54.43171
# # 3 -1.086280 53.96417
# # Correct!