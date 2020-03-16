#install.packages("volesti")
#install.packages("ggplot2")
library(volesti)
library(ggplot2)

# Create the n-dimensional and m facets unit cube
vol<-function(n,m){
polytope <- GenRandHpoly(n,m)
vol=volume(polytope)
return(list(polytope=polytope,volume=vol))
}
# example
vol(10,50)

# Sample points from the cube using BW random walks and walk lengths
label <- function(x) {
  c('Ball walk')[x+1]
}
for (step in c(100, 200, 400, 800)) {
  ball_walk <- sample_points(vol(10,50)[[1]], 100, 'uniform', 'BW', walk_step = step)
  
  # Project points onto the plane and bind points together for plotting
  walks <- data.frame(cbind(ball_walk[,1:2],0))
  walks <- data.frame(transform(walks, X3 = label(X3)))
  
  # Plot the projections
  p <- ggplot(walks, aes(X1, X2, colour=X3)) + geom_point() +
    scale_colour_manual("Random walks", values = c("red")) +
    ggtitle("Walk length:", step)
  print(p)
}

