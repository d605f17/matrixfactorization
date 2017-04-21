computeSimUsersMatrix <- function(filename){
  if(file.exists(paste(getwd(), "/", filename, "SimUsers.csv", sep = ""))){
    print("SimUsers file found")
    userSimilarityMatrix <- read_csv(paste(getwd(), "/", paste(tolower(filename), "SimUsers.csv", sep = ""), sep = ""), 
                               col_names = FALSE, cols(.default = col_integer()))
    return(as.matrix(userSimilarityMatrix))
  } else {
    print("SimUsers file not found. This will take some time")
    userSimilarityMatrix <- matrix(nrow = numberOfUsers, ncol = numberOfUsers)
    
    for(user1 in 1:numberOfUsers){
      print(Sys.time())
      for(user2 in 1:numberOfUsers){
        if(user1 == user2)
          userSimilarityMatrix[user1, user2] <- -1
        else if (user2 < user1)
          userSimilarityMatrix[user1, user2] <- userSimilarityMatrix[user2, user1]
        else
          userSimilarityMatrix[user1, user2] <- similarityUser(user1, user2)
      }
      
       print(paste(user1, "out of", numberOfUsers, "completed"))
    }
    
    for(row in 1:numberOfUsers){
      userSimilarityMatrix[row, ] <- order(userSimilarityMatrix[row, ], decreasing = TRUE)
    }
    write.table(userSimilarityMatrix, file = paste(filename, "SimUsers.csv", sep = ""), sep = ",", row.names = F, col.names = F)
    return(userSimilarityMatrix)
  }  
  
}

similarityUser <- function(userI, userJ){
  #Consider a minimum of co-rated items
  
  avgUserI <- avgUserRating(userI);
  avgUserJ <- avgUserRating(userJ);
  
  topPart <- 0;
  bottomPart1 <- 0;
  bottomPart2 <- 0;
  
  for(item in 1:numberOfItems){
    ratingUserI <- as.numeric(ratingsMatrix[userI, item])
    ratingUserJ <- as.numeric(ratingsMatrix[userJ, item])
    
    if(!is.na(ratingUserI) && !is.na(ratingUserJ)){
      topPart <- topPart + (ratingUserI - avgUserI)*(ratingUserJ - avgUserJ);
      bottomPart1 <- bottomPart1 + (ratingUserI - avgUserI)^2;
      bottomPart2 <- bottomPart2 + (ratingUserJ - avgUserJ)^2;
    }
  }
  
  return(topPart / (sqrt(bottomPart1) * sqrt(bottomPart2)))
}

avgUserRating <- function(user){
  ratedItems <- 0;
  accumulatedRatings <- 0;
  
  for(item in 1:numberOfItems){
    rating <- as.numeric(ratingsMatrix[user, item])
    if(!is.na(rating)){
      ratedItems <- ratedItems + 1;
      accumulatedRatings <- accumulatedRatings + rating;
    }
  }
  
  return(accumulatedRatings / ratedItems);
}

simUsersVector <- function(userId, n, P) {
  result <- 0
  users <- head(simUsersMatrix[userId, ], n)
  
  for(u in users){
    result <- result + P[u, ]
  }
  
  return(1/n * result)
}
