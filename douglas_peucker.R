#'Ramer-Douglas-Peucker algorithm for curve fitting with a PolyLine
#' 
#' @param points a 2D matrix with the coordinates of 2D points
#' @param epsilon an number between 0 and 1. Recomended 0.01.
#' @return A matrix with the points of segments of a Poly Line.
#' @export
#' @seealso \code{\link[HistDAWass]{data2hist}} function
#' @export
DouglasPeucker=function(points,epsilon){
  dmax=0
  index=0
  end=nrow(points)
  ResultList=numeric(0)
  if (end<3) return (ResultList=rbind(ResultList,points))
  for (i in 2:(end-1)){
    d=ShortestDistance(points[i,], line=rbind(points[1,],points[end,]))
    if (d>dmax){
      index=i
      dmax=d
    }
  }
  #if dmax is greater than epsilon recursively apply
  if (dmax>epsilon){
    # print(dmax)
    recResults1=DouglasPeucker(points[1:index,],epsilon)
    recResults2=DouglasPeucker(points[index:end,],epsilon)
    ResultList=rbind(ResultList,recResults1,recResults2)
    
  }
  else
  {
    ResultList=rbind(ResultList,points[1,],points[end,])
  }
  ResultList=as.matrix(ResultList[!duplicated(ResultList),])
  colnames(ResultList)=c("x","p")
  return(ResultList)
}
#' Shortes distance from a point o a 2d segment
#' 
#' @param p coordinates of a point
#' @param line a 2x2 matrix with the coordinates of two points defining a line
#' @return A numeric value, the Euclidean distance of point \code{p} to the \code{line}.
#' @export
#' @seealso \code{\link[HistDAWass]{data2hist}} function and \code{\link[HistDAWass]{DouglasPeucker}} function
#' @export
ShortestDistance=function(p, line){
  x1=line[1,1]
  y1=line[1,2]
  x2=line[2,1]
  y2=line[2,2]
  x0=p[1]
  y0=p[2]
  d=abs((y2-y1)*x0-(x2-x1)*y0+x2*y1-y2*x1)/sqrt((y2-y1)^2+(x2-x1)^2)
  return(as.numeric(d))
}



douglas_peucker_simplification <- function(x, y, epsilon_max) {
  
  points_matrix <- matrix(c(x,y), ncol=2 )
  sums <- vector(mode = "numeric", length = epsilon_max)
  sums[1]<-sum(distances(points_matrix[,1], points_matrix[,2]))
  
  i <- 1
  length_simplified <- nrow(points_matrix)
  
  for (i in 1:(epsilon_max - 1)) {
    simplified_track <- DouglasPeucker(points_matrix, i*1)
    i <- i+1
    length_simplified <- nrow(simplified_track)
    if (length_simplified>2) sums[i]<-sum(distances(simplified_track[,1], simplified_track[,2]))
    else {
      sums[i:epsilon_max] <- sum(distances(simplified_track[,1], simplified_track[,2]))
      break
    }
  } 
  return(sums/sums[1])
}



