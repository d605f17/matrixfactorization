library(readr)
source("SimUsers.R");

Q <- matrix(runif(1000, 0, 1), nrow = 1682, ncol = 20)
P <- matrix(runif(1000, 0, 1), nrow = 943, ncol = 20)
lambda <- 0.02
gamma <- 0.05

traindata <- read_delim(paste(getwd(), "/ml-100k/u1.base", sep = ""), 
                            "\t", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)
colnames(traindata) <- c("userId", "movieId", "rating", "timestamp");

ratingsmatrix <- matrix(nrow = 943, ncol = 1682);

for(row in 1:nrow(traindata)){
  ratingsmatrix[as.numeric(traindata[row, 1]), as.numeric(traindata[row, 2])] <- as.numeric(traindata[row, 3])
}

ratings <- which(ratingsmatrix != "NA", arr.ind = T)

regSquaredError <- 1000000000
for(step in 1:5000){
  prevRegSquaredError <- regSquaredError
  regSquaredError <- 0
  
  # Calculate Regularized Squared Error
  for(row in 1:nrow(ratings)){
    qi <- as.matrix(Q[ratings[row, 2], ])
    pu <- as.matrix(P[ratings[row, 1], ])
    rui <- ratingsmatrix[ratings[row, 1], ratings[row, 2]]
    
    #Check for NA entries in similarUsers
    regSquaredError <- regSquaredError +
      as.numeric(rui - t(qi) %*% pu)^2 +
      lambda * (norm(qi, type = "f")^2 + norm(pu, type = "f")^2);
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
  print(prevRegSquaredError)
  print(regSquaredError)
  
  # if difference in error is small stop!
  if(prevRegSquaredError - regSquaredError < 0.1) {
    print("forskel lille")
    break
  }
  if(regSquaredError < 1) {
    print("lille fejl")
    break
  }
}

Rhat <- P %*% t(Q)

