douglas_peucker_simplification <- function(x, y, epsilon_max) {
  
  points_matrix <- matrix(c(x,y), ncol=2 )
  sums <- vector(mode = "numeric", length = epsilon_max)
  sums[1]<-sum(distances(points_matrix[,1], points_matrix[,2]))
  
  i <- 1
  length_simplified <- nrow(points_matrix)
  
  for (i in 1:(epsilon_max - 1)) {
    simplified_track <- RDPCPP(points_matrix, i*1)
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



