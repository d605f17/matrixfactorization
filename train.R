train <- function(filename, lambda, lambdaU, lambdaI, gamma, n){
  Q <- matrix(runif(numberOfItems * K, 0, 0.1), nrow = numberOfItems, ncol = K)
  P <- matrix(runif(numberOfUsers * K, 0, 0.1), nrow = numberOfUsers, ncol = K)
  library(readr)
  # Reading trainData -----
  print("Reading training data")
  trainData <- read_delim(paste(getwd(), "/ml-100k/", filename, ".base", sep = ""),
                          "\t", escape_double = FALSE, trim_ws = TRUE, 
                          col_names = c("userId", "movieId", "rating", "timestamp"),
                          col_types = cols(
                            userId = col_integer(),
                            movieId = col_integer(),
                            rating = col_integer(),
                            timestamp = col_integer()
                          )
  );
  trainData <<- as.matrix(trainData)
  # -----

  # Creating RatingsMatrix for performance reasons -----
  print("Creating ratings matrix...")
  ratingsMatrix <- matrix(nrow = numberOfUsers, ncol = numberOfItems)
  for(row in 1:nrow(trainData)){
    ratingsMatrix[as.numeric(trainData[row, 1]), as.numeric(trainData[row, 2])] <- as.numeric(trainData[row, 3])
  }
  ratingsMatrix <<- ratingsMatrix
  # -----
  
  # Writing or reading simUsers and simItems matrices ----
  simUsersMatrix <<- computeSimUsersMatrix(filename)
  simItemsMatrix <<- computeSimItemsMatrix(filename)
  # ----
  
  regSquaredError <- 0

  logToFile(paste(filename, "Logfile.txt", sep = ""),
            "time, iteration, regSquaredError")
  
  print("Training started")
  for(step in 1:5000){
    prevRegSquaredError <- regSquaredError
    regSquaredError <- 0
    
    print(Sys.time())
    for(row in 1:nrow(trainData)){
      userId <- as.numeric(trainData[row, 1])
      itemId <- as.numeric(trainData[row, 2])
      rating <- as.numeric(trainData[row, 3])
      
      q_j <- t(as.matrix(Q[itemId, ]))
      p_i <- t(as.matrix(P[userId, ]))
      r_ij <- rating
      
      # Check for NA entries in similarUsers
      
      # Compute SimItems and SimUsers
      simItems <- t(as.matrix(simItemsVector(itemId, n, Q)))
      simUsers <- t(as.matrix(simUsersVector(userId, n, P)))
      
      error <- as.numeric(r_ij - p_i %*% t(q_j))
      # Calculating Regularized Squared Error -----
      
      # Original regSquaredError
      # regSquaredError <- as.numeric(regSquaredError + 
      #                                 error^2 + 
      #                                 lambda * (norm(q_j, type = "f")^2 + norm(p_i, type = "f")^2)
      # )
      
      # Updating P and Q riginal mf -----
      # Q[itemId, ] <- q_j + gamma * (error * p_i - lambda * q_j)
      # P[userId, ] <- p_i + gamma * (error * q_j - lambda * p_i)
      # -----
      
      # regSquaredError from Joshi et al.
      regSquaredError <-  as.numeric(regSquaredError +
                                       error^2 +
                                       lambda * (norm(q_j, type = "f")^2 + norm(p_i, type = "f")^2) +
                                       lambdaU * norm(p_i - simUsers, type = "f")^2 +
                                       lambdaI * norm(q_j - simItems, type = "f")^2)
      # -----
      
      # Updating P and Q joshi et al. -----
      Q[itemId, ] <- q_j + gamma * (error * p_i - lambda * q_j - lambdaI * (q_j - simItems))
      P[userId, ] <- p_i + gamma * (error * q_j - lambda * p_i - lambdaU * (p_i - simUsers))
      # -----
    }
    
    # if difference in error is small stop!
    if(abs(prevRegSquaredError - regSquaredError) < 1) {
      break
    }
    if(regSquaredError < 1) {
      break
    }
    
    print(paste(step, "iterations out of 5000 completed"))
    print(paste("error:", prevRegSquaredError - regSquaredError))
    logToFile(paste(filename, "Logfile.txt", sep = ""),
              paste(Sys.time(), step, regSquaredError, sep = ",")
    )
  }
  
  return(P %*% t(Q))
}

logToFile <- function(filename, string) {
  write(string, file = filename, append = T)
}
