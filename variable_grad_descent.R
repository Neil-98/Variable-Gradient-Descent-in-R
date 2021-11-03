library(ggplot2)

get_cost <- function(W, Y, X) {
  cost <- norm(x = (X %*% matrix(t(W)) - Y) ^ 2, type = "2")
  return(cost)
}

get_gradient <- function(W, Y, X) {
  gradient <- t(X) %*% X %*% W - t(X) %*% Y
  return(gradient)
}

# Descent step
stepwise_descent <- function(W, Y, X, learning_rate) {
  gradient <- get_gradient(W, Y, X)
  subtrahend <-  learning_rate * gradient
  W <- W - subtrahend
  return(W)
}

# Looping stepwise_descent, reducing learning_rate in the ratio of decrease in cost (learning_rate = learning_rate * cost/prev_cost)
gradient_descent <- function(W, Y, X, learning_rate, no_of_steps) { # cost_grad_record: A detailed history of costs, weights, and gradients at each step of gradient descent
  
  ### Code required just for record-keeping, not for gradient descent!
  cost_grad_table <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(cost_grad_table) <- c("Step_No", "Cost", "Weights", "Gradient", "Learning_Rate")
  ###
  
  
  prev_cost <- get_cost(W, Y, X)
  sprintf("Initial Cost = ", prev_cost)
  i <- 1
  for (i in 1: no_of_steps) {
    
    cost <- prev_cost
    
    ### Code required just for record-keeping, not for gradient descent!
    gradient <- get_gradient(W, Y, X)
    cost_grad_record <- cbind(i, cost, W, gradient, learning_rate)
    colnames(cost_grad_record) <- c("Step_No", "Cost", "Weights", "Gradient", "Learning_Rate")
    cost_grad_table <- rbind(cost_grad_table, cost_grad_record)
    ###
    
    W <- stepwise_descent(W, Y, X, learning_rate)
    
    cost <- get_cost(W, Y, X)
    
    if (prev_cost != 0) {
      learning_rate <- learning_rate * (cost/ prev_cost) # Update learning_rate
      sprintf("Learning rate = ", learning_rate)
    }
    
    prev_cost <- cost
  }
  
  ### Code required just for record-keeping, not for gradient descent!
  final_cost <- get_cost(W, Y, X)
  final_gradient <- get_gradient(W, Y, X)
  final_cost_grad_record <- cbind(i + 1, final_cost, W, final_gradient, learning_rate)
  colnames(final_cost_grad_record) <- c("Step_No", "Cost", "Weights", "Gradient", "Learning_Rate")
  cost_grad_table <- rbind(cost_grad_table, final_cost_grad_record)
  ### 
  
  weights_and_record <- list(W, cost_grad_table)
  names(weights_and_record) <- c("Final_Weights", "Record")
  
  return(weights_and_record)
}

# DESIGN:
# Input (3 x 3 matrix) 
#   x                     -> output (3 x 1 vector)
# weights (3 x 1 vector)

input <- matrix(rnorm(3 * 3, mean = 0, sd = 1), 3, 3)
output <- c(1, 0, 0)
weights <- matrix(rnorm(3, mean = 0, sd = 1), 3, 1)

weights_and_record <- gradient_descent(weights, output, input, learning_rate =  0.01, no_of_steps = 500)
final_weights <- weights_and_record$Final_Weights
print(weights_and_record)

