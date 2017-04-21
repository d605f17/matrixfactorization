test <- function(filename, Rhat) {
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
  
  squaredError <- 0
  absError <- 0
  
  for(row in 1:nrow(testData)){
    squaredError <- squaredError + (as.numeric(testData[row, 3]) - Rhat[as.numeric(testData[row, 1]), as.numeric(testData[row, 2])])^2
    absError <- absError + abs(as.numeric(testData[row, 3]) - Rhat[as.numeric(testData[row, 1]), as.numeric(testData[row, 2])])
  }
  
  RMSE <- sqrt(1/nrow(testData) * squaredError)
  MAE <- 1/nrow(testData) * absError
  
  print(paste("---", filename, "---"))
  print(paste("RMSE=", RMSE))
  print(paste("MAE=", MAE))
}
