nSimUsers <- function(user, n){
  similarityMatrix <- matrix(0, nrow = nrow(ratingsmatrix), ncol = 1)
  
  for(userIndex in 1:nrow(ratingsmatrix)){
    sim <- similarity(user, userIndex);
    similarityMatrix[userIndex, ] <- c(sim);
  }
  
  return(order(similarityMatrix, decreasing = TRUE)[1:n]);
}

similarity <- function(userI, userJ){
  #Consider a minimum of co-rated items
  
  avgUserI <- avgRating(userI);
  avgUserJ <- avgRating(userJ);
  
  topPart <- 0;
  bottomPart1 <- 0;
  bottomPart2 <- 0;
  
  for(item in 1:ncol(ratingsmatrix)){
    ratingUserI <- as.numeric(ratingsmatrix[userI, item]);
    ratingUserJ <- as.numeric(ratingsmatrix[userJ, item]);
    
    if(!is.na(ratingUserI) && !is.na(ratingUserJ)){
      topPart <- topPart + (ratingUserI - avgUserI)*(ratingUserJ - avgUserJ);
      bottomPart1 <- bottomPart1 + (ratingUserI - avgUserI)^2;
      bottomPart2 <- bottomPart2 + (ratingUserJ - avgUserJ)^2;
    }
  }
  
  return(topPart / (sqrt(bottomPart1) * sqrt(bottomPart2)))
}

avgRating <- function(user){
  ratedItems <- 0;
  accumulatedRatings <- 0;
  
  for(col in 1:ncol(ratingsmatrix)){
    rating <- as.numeric(ratingsmatrix[user, col]);
    if(!is.na(rating)){
      ratedItems <- ratedItems + 1;
      accumulatedRatings <- accumulatedRatings + rating;
    }
  }
  
  return(accumulatedRatings / ratedItems);
}