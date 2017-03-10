Q <- matrix(runif(1000, 0, 1), nrow = 10, ncol = 2)
P <- matrix(runif(1000, 0, 1), nrow = 10, ncol = 2)
lambda <- 0.02
gamma <- 0.05

library(readr)
ratingsmatrix <- read_delim("C:/Users/RasmusKrusaa/Desktop/p6/matrixfactorization/testmatrix.CSV", 
                         ";", escape_double = FALSE, col_names = FALSE, 
                         trim_ws = TRUE)
ratings <- which(ratingsmatrix != "NA", arr.ind = T)

for(step in 1:5000){
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
}

Rhat <- P %*% t(Q)

