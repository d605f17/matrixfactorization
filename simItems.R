computeSimItemsMatrix <- function(filename){
  if(file.exists(paste(tolower(filename), "SimItems.csv", sep = ""))){
    print("SimItems file found")
    simItemsMatrix <- read_csv(paste(getwd(), "/", paste(tolower(filename), "SimItems.csv", sep = ""), sep = ""), 
                               col_names = FALSE, cols(.default = col_double()))
    return(as.matrix(simItemsMatrix))
  } else {
    print("SimItems file not found. This will take some time")
    itemSimilarityMatrix <- matrix(nrow = numberOfItems, ncol = numberOfItems)
    
    for(item1 in 1:numberOfItems){
      print(Sys.time())
      for(item2 in 1:numberOfItems){
        if(item1 == item2)
          itemSimilarityMatrix[item1, item2] <- -1
        else if (item2 < item1)
          itemSimilarityMatrix[item1, item2] <- itemSimilarityMatrix[item2, item1]
        else
          itemSimilarityMatrix[item1, item2] <- similarityItem(item1, item2)
      }
      
      print(paste(item1, "out of", numberOfItems, "completed"))
    }
    
    for(row in 1:numberOfItems){
      itemSimilarityMatrix[row, ] <- order(itemSimilarityMatrix[row, ], decreasing = TRUE)
    }
    
    write.table(itemSimilarityMatrix, file = paste(filename, "SimItems.csv", sep = ""), sep = ",", row.names = F, col.names = F)
    return(itemSimilarityMatrix)
  }
}

similarityItem <- function(itemI, itemJ){
  #Consider a minimum of co-rated items
  
  avgItemI <- avgItemRating(itemI);
  avgItemJ <- avgItemRating(itemJ);
  
  coUsers <- which(!is.na(ratingsMatrix[, itemI]) & !is.na(ratingsMatrix[, itemJ]))
  if(length(coUsers) < 5)
    return(0)
  
  topPart <- 0;
  bottomPart1 <- 0;
  bottomPart2 <- 0;
  
  for(user in coUsers){
    ratingItemI <- as.numeric(ratingsMatrix[user, itemI])
    ratingItemJ <- as.numeric(ratingsMatrix[user, itemJ])
    
    topPart <- topPart + (ratingItemI - avgItemI)*(ratingItemJ - avgItemJ);
    bottomPart1 <- bottomPart1 + (ratingItemI - avgItemI)^2;
    bottomPart2 <- bottomPart2 + (ratingItemJ - avgItemJ)^2;
}
  
  return(topPart / (sqrt(bottomPart1) * sqrt(bottomPart2)))
}

avgItemRating <- function(item){
  userRatings <- which(!is.na(ratingsMatrix[, item]))
  accumulatedRatings <- 0
  
  for(user in userRatings){
    rating <- as.numeric(ratingsMatrix[user, item])
    accumulatedRatings <- accumulatedRatings + rating
  }
  
  return(accumulatedRatings / length(userRatings))
}

simItemsVector <- function(itemId, n, Q) {
  result <- 0
  items <- head(simItemsMatrix[itemId, ], n)
  
  for(i in items){
    result <- result + Q[i, ]
  }
  
  return(1/n * result)
}
