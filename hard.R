solution <- function(grad,init_point,initial_step,max_steps) {
  # First step: standard gradient descent
  gradient_last = grad(init_point)
  point_last = init_point
  point = init_point- initial_step*gradient_last
  gradient = grad(point)
  
  s_last = point - point_last
  y_last = gradient - gradient_last
  
  for(i in 1:max_steps) {
    if(all(gradient==0|is.na(gradient)==T)) {
      break
    }
    
    # Barzilai-Borwen step
    bb_step = (s_last %*% s_last) / (y_last %*% s_last)
    
    point_last = point
    gradient_last = gradient
    
    point = point - c(bb_step)*gradient
    gradient = grad(point)
    
    s_last = point - point_last
    y_last = gradient - gradient_last
  }
  
  point
}

# Minimize an example function
oracle <- function(x) {
  c(2*(x[1]*exp(x[2]) - 2*x[2]*exp(-x[1]))*(exp(x[2]) + 2*x[2]*exp(x[1])),
    2*(x[1]*exp(x[2]) -2*x[2]*exp(-x[1]))*(x[1]*x[2]*exp(x[2]) - 2*exp(-x[1])))
}
f <- function(x) {
  (x[1]*exp(x[2]) - 2*x[2]*exp(-x[1]))^2
}

minimum <- solution(oracle,c(pi, pi),0.00001,10000)
print(minimum)
print(f(minimum))
minimum <- solution(oracle,c(pi, pi),0.001,10000)
print(minimum)
print(f(minimum))
