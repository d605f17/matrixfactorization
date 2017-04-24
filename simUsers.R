computeSimUsersMatrix <- function(filename){
  if(file.exists(paste(getwd(), "/", filename, "SimUsers.csv", sep = ""))){
    print("SimUsers file found")
    userSimilarityMatrix <- read_csv(paste(getwd(), "/", paste(tolower(filename), "SimUsers.csv", sep = ""), sep = ""), 
                               col_names = FALSE, cols(.default = col_double()))
    return(as.matrix(userSimilarityMatrix))
  } else {
    print("SimUsers file not found. This will take some time")
    userSimilarityMatrix <- matrix(nrow = numberOfUsers, ncol = numberOfUsers)
    
    for(user1 in 1:numberOfUsers){
      for(user2 in 1:numberOfUsers){
        if(user1 == user2)
          userSimilarityMatrix[user1, user2] <- -1
        else if (user2 < user1)
          userSimilarityMatrix[user1, user2] <- userSimilarityMatrix[user2, user1]
        else
          userSimilarityMatrix[user1, user2] <- pearsonUsers(user1, user2)
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

pearsonUsers <- function(user1, user2){
  coRatedItems <- which(!is.na(ratingsMatrix[user1, ]) & !is.na(ratingsMatrix[user2, ]))
  
  if(length(coRatedItems) < 5)
    return(0)
  
  return(cor(ratingsMatrix[user1, ], 
             ratingsMatrix[user2, ], 
             use = "pairwise.complete.obs", 
             method = "pearson"))
}

avgUserRating <- function(user){
  ratedItems <- which(!is.na(ratingsMatrix[user, ]));
  accumulatedRatings <- 0;
  
  for(item in ratedItems){
    rating <- as.numeric(ratingsMatrix[user, item])
    accumulatedRatings <- accumulatedRatings + rating;
  }
  
  return(accumulatedRatings / length(ratedItems));
}

simUsersVector <- function(userId, n, P) {
  result <- 0
  users <- head(simUsersMatrix[userId, ], n)
  
  for(u in users){
    result <- result + P[u, ]
  }
  
  return(1/n * result)
}
