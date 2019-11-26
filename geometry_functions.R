#Euclidean distance between two points

euc.dist <- function(point1, point2) {
  
  sqrt ( sum ( ( point1 - point2 ) ^ 2 ) )
  
}


#vector of Euclidean distances between consecutive points in a matrix
distances_old <- function(x_points, y_points) {
  mt <- matrix(c(x_points, y_points), ncol = 2, byrow = FALSE)
  len <- nrow(mt) - 1
  dist<-vector(mode="numeric", length=len )
  for (i in 1 : len) dist[i] <- euc.dist (mt[i,],mt[i+1,])
  return(dist)

}

#vector of Euclidean distances between consecutive points in a matrix
distances <- function(x_points, y_points) {
  mt <- matrix(c(x_points, y_points), ncol = 2, byrow = FALSE)
  sqrt(rowSums(diff(mt)^2))
}

#directional persistence
DP <- function(x_points, y_points, max_t){
 
  DP_series <- vector(mode = "numeric", length = max_t)
  for (i in 1:max_t){
    output <- vector(mode = "numeric", length = i)
    for (it in 1:i){
      
     
      x_p <- x_points[seq(it, length(x_points), i)]
      y_p <- y_points[seq(it, length(y_points), i)]
        output[it] <- mean(cos(angles(x_p, y_p)))
    
    }
    DP_series[i] <- mean((output))
  }
  return(DP_series)
}


#vector of Euclidean distances between one point and set of other points
PointToSetDistances <- function( point, mt ){
  
  len <- nrow(mt)
  
  dist<-vector(mode="numeric", length=len )
  
  for (i in 1 : len) dist[i] <- euc.dist (point,mt[i,])
  
  dist
  
  
  
}



# turning angles (-pi<angle<pi) between consecutive points in a matrix
angles <- function(x_points, y_points){
  mt <- matrix(c(x_points, y_points), ncol = 2, byrow = FALSE)
  mDevOr<- diff(mt) 
  angles <- sapply (diff(atan2(mDevOr[,2], mDevOr[,1])),
                    function (x) {
                      if (x > pi) x <- x - ( 2 * pi ) 
                      else if (x<= -pi) x <- x + ( 2 * pi )
                      else x <- x
                    }
  )
  return(angles)
}


# turning angles (-pi<angle<pi) between consecutive points in a matrix
angles_old <- function(x_points, y_points){
  
  mt <- matrix(c(x_points, y_points), ncol = 2, byrow = FALSE)
  mDevOr<- mt[-1,] - mt[-nrow(mt),] 
  a <- atan2 (mDevOr[-1,2], mDevOr[-1,1]) - atan2 (mDevOr[-nrow(mDevOr),2], mDevOr[-nrow(mDevOr),1])
  angles <- sapply (a,
                    function (x) {
                      if (x > pi) x <- x - ( 2 * pi ) 
                      else if (x<= -pi) x <- x + ( 2 * pi )
                      else x <- x
                    }
  )
  return(angles)
}


# beeline distance between first and last point in a set
displacement <- function (mt) {
  
  euc.dist( mt[1,], mt [nrow(mt), ] )
  
  
}

#efficiency of matrix of points (net displacement / actual distance traveled)
efficiency <- function (mt) {
  
  displacement(mt) / sum ( distances ( mt ) )
  
}

density_y <- function(x){
  tmp <- density(x, n  = 180, from = 0, to = 179) 
  return(tmp$y)
  }

