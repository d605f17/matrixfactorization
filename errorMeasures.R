testData <- read_delim("C:/Users/RasmusKrusaa/Desktop/p6/matrixfactorization/ml-100k/u1.test", 
                       +     "\t", escape_double = FALSE, col_names = FALSE, 
                       +     trim_ws = TRUE)

squaredError <- 0
absError <- 0

for(row in 1:nrow(testData)){
  squaredError <- squaredError + (as.numeric(testData[row, 3]) - Rhat[as.numeric(testData[row, 1]), as.numeric(testData[row, 2])])^2
  absError <- absError + abs(as.numeric(testData[row, 3]) - Rhat[as.numeric(testData[row, 1]), as.numeric(testData[row, 2])])
}

RMSE_train <- sqrt(1/nrow(testData) * squaredError)
MAE <- 1/nrow(testData) * absError
