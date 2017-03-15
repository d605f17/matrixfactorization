Q <- matrix(runif(1000, 0, 1), nrow = 10, ncol = 2)
P <- matrix(runif(1000, 0, 1), nrow = 10, ncol = 2)
lambda <- 0.02
gamma <- 0.05

library(readr)
ratingsmatrix <- read_delim("C:/Users/RasmusKrusaa/Desktop/p6/matrixfactorization/testmatrix.CSV", 
                         ";", escape_double = FALSE, col_names = FALSE, 
                         trim_ws = TRUE)
ratings <- which(ratingsmatrix != "NA", arr.ind = T)

regSquaredError <- 1000
for(step in 1:100){
  prevRegSquaredError <- regSquaredError
  regSquaredError <- 0
  
  # Calculate Regularized Squared Error
  for(row in 1:nrow(ratings)){
    qi <- as.matrix(Q[ratings[row, 2], ])
    pu <- as.matrix(P[ratings[row, 1], ])
    rui <- ratingsmatrix[ratings[row, 1], ratings[row, 2]]
    
    regSquaredError <- 
      as.numeric(rui - t(qi) %*% pu)^2 +
      lambda * (norm(qi, type = "f")^2 + norm(pu, type = "f")^2)
  }
  
  # Updating P and Q
  for(row in 1:nrow(ratings)){
    tmpQ <- Q[ratings[row, 2], ] 
    tmpP <- P[ratings[row, 1], ]
    
    error <- 
      as.numeric(ratingsmatrix[ratings[row, 1], ratings[row, 2]]) -
      t(Q[ratings[row, 2], ]) %*% P[ratings[row, 1], ]
    
    Q[ratings[row, 2], ] <- tmpQ + gamma *
      (error * tmpP - lambda * tmpQ)
    P[ratings[row, 1], ] <- tmpP + gamma *
      (error * tmpQ - lambda * tmpP)
  }
  
  print(step)
  print(regSquaredError)
  
  # if difference in error is small stop!
  if(prevRegSquaredError - regSquaredError < 0.01)
    break
}

Rhat <- P %*% t(Q)

