makeTestData <- function(filename) {
  testData <- read_delim(paste(getwd(), "/ml-100k/", filename, ".test", sep = ""), 
                         "\t", escape_double = FALSE, trim_ws = TRUE,
                         col_names = c("userId", "movieId", "rating", "timestamp"),
                         col_types = cols(
                           userId = col_integer(),
                           movieId = col_integer(),
                           rating = col_integer(),
                           timestamp = col_integer()
                         )
  )
  
  return(as.matrix(testData))
}

MAE <- function(predictions, testData) {
  numberOfUsers <- nrow(testData)
  
  error <- 0
  for(u in 1:numberOfUsers){
    prediction <- predictions[as.numeric(testData[row, 1]), as.numeric(testData[row, 2])]
    error <- error + abs(as.numeric(testData[row, 3]) - prediction)
  }
  
  return(1/numberOfUsers * error)
}

RMSE <- function(predictions, testData) {
  numberOfUsers <- nrow(testData)
  
  error <- 0
  for(u in 1:numberOfUsers){
    prediction <- predictions[as.numeric(testData[row, 1]), as.numeric(testData[row, 2])]
    error <- error + (as.numeric(testData[row, 3]) - prediction)^2
  }
}

precision <- function(predictions, testData, threshhold){
  numberOfUsers <- nrow(predictions)
  
  accumulatedPrecision <- 0
  for(u in 1:numberOfUsers){
    print(u)
    
    known <- testData[which(testData[, 1] == u), 2]
    knownPositives <- testData[which(testData[, 1] == u & 
                                       testData[, 3] >= threshhold), 2]
    positives <- intersect(known, 
                           which(predictions[u, ] >= threshhold))
    truePositives <- intersect(positives, knownPositives)
    
    tmpResult <- length(truePositives) / length(positives)
    
    if(length(positives) != 0){
      accumulatedPrecision <- accumulatedPrecision + tmpResult
    }                                  
  }
  
  return(1/numberOfUsers * accumulatedPrecision)
}

recall <- function(predictions, testData, threshhold){
  numberOfUsers <- nrow(predictions)
  
  accumulatedRecall <- 0
  for(u in 1:numberOfUsers){
    print(u)
    
    known <- testData[which(testData[, 1] == u), 2]
    knownPositives <- testData[which(testData[, 1] == u & 
                                       testData[, 3] >= threshhold), 2]
    positives <- intersect(known, 
                           which(predictions[u, ] >= threshhold))
    truePositives <- intersect(positives, knownPositives)
    
    if(length(knownPositives) != 0){
      accumulatedRecall <- accumulatedRecall + 
        length(truePositives) / length(knownPositives)
    }
  }
  
  return(1/numberOfUsers * accumulatedRecall)
}