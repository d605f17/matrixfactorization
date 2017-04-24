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
          itemSimilarityMatrix[item1, item2] <- pearsonItems(item1, item2)
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

pearsonItems <- function(item1, item2){
  coUsers <- which(!is.na(ratingsMatrix[, item1]) & !is.na(ratingsMatrix[, item2]))
  if(length(coUsers) < 5)
    return(0)
  
  return(cor(ratingsMatrix[, item1],
             ratingsMatrix[, item2],
             use = "pairwise.complete.obs",
             method = "pearson"))
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
