#install.packages("volesti")
#install.packages("ggplot2")
#install.packages("cubature")
library(volesti)
library(ggplot2)
library(cubature)

# Number of dimensions of the space
num_dim = 4
step_size = 0.5
num_points = 10000

# Vector containing the corresponding coefficients of the evaluate function
matrix_vector = vector(mode = "numeric", length = 2*num_dim+1)

distance <- function(p1, p2)
{
  dis = 0
  for(i in 1:num_dim)
  {
    dis = dis + (p1[i] - p2[i])^2
  }
  dis = sqrt(dis)
  return(dis)
}

generator<- function()
{
  matrix_vector[1] <- 10*runif(1)+10
  for(i in 1:num_dim)
  {
  matrix_vector[i+1] <- 50*runif(1) - 50
  }
  for(i in 1:num_dim)
  {
    matrix_vector[i+num_dim+1] <<- 10*runif(1) + 5
  }
  
  matrix_vector[2] <- 100
  matrix_vector=matrix_vector
  Mean=c(-matrix_vector[2]/(2*matrix_vector[num_dim+2]), -matrix_vector[num_dim+1]/(2*matrix_vector[2*num_dim+1]))
  SD=c(1/sqrt(2*matrix_vector[num_dim+2]), 1/sqrt(2*matrix_vector[num_dim+4]))
  return(list(matrix_vector,Mean,SD))
}

# Evaluates the value at a particular point
evaluate <- function(point)
{
  point_matrix_vector <- vector(mode = "numeric", length = 2*num_dim+1)
  dis = 0
  point_matrix_vector[1] = 1
  for(i in 1:num_dim)
  {
    point_matrix_vector[i+1] = point[i]
  }
  for(i in 1:num_dim)
  {
    point_matrix_vector[i+1+num_dim] = point[i]^2
  }
  for(i in 1:2*num_dim+1)
  {
    dis = dis + point_matrix_vector[i]*matrix_vector[i]
  }
  return(dis)
}

# Marsaglia Method used followed by MCMC Algorithm
random_point <- function(start_point,prob)
{
  next_step = vector(mode="numeric", length = num_dim)
  next_point = vector(mode="numeric", length = num_dim)
  origin = rep(0, num_dim)
  next_step = rnorm(num_dim)
  scaling = step_size / distance(origin, next_step)
  for(i in 1: num_dim)
  {
    next_step[i] = scaling*next_step[i]
    next_point[i] = start_point[i] + next_step[i]
  }
  transition = exp(-evaluate(next_point)+evaluate(start_point))
  if (transition > prob)
    return(next_point)
  else
    return(start_point)
}

ball_walk <-function(start_point,num_points,prob)
{
  random_walk_points = vector("list", num_points)
  random_walk_points[[1]] = start_point
  for(i in 2:num_points)
  {
    next_point = random_point(start_point,prob)
    random_walk_points[[i]] = next_point
    start_point = next_point
  }
  return(random_walk_points)
}



# Generating a random vector as Starting Point (as of now)
vec = vector("numeric", num_dim)
for(i in 1:num_dim)
{
  vec[i] = runif(1)
  vec[i] = 0
}
generator()
cat("Start = ", vec[1], vec[3], "\n")
random_walk_points = ball_walk(vec,num_points,prob=runif(1))
displacement = distance(random_walk_points[[1]], random_walk_points[[num_points]])
cat("End = ", random_walk_points[[num_points]][1], random_walk_points[[num_points]][num_dim], "\n")
x = vector("numeric", num_points)
for(i in 1:num_points)
{
  x[i] = random_walk_points[[i]][1]
}
y = vector("numeric", num_points)
for(i in 1:num_points)
{
  y[i] = random_walk_points[[i]][num_dim]
}

# Plotting them in a graph
ggplot(as.data.frame(cbind(x,y)),aes(x, y)) +
  geom_point(size=2, shape=23)+labs(title=paste("Random BW\nStep Size = ", step_size," Number of Dimensions = ", num_dim, "Number of Points = ", num_points))



