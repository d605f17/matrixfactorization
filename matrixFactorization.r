library(readr);

source("train.R")
source("test.R")
source("simUsers.R")
source("simItems.R")

numberOfUsers <- 943
numberOfItems <- 1682
K <- 20
lambda <- 0.05
lambdaU <- 0.05
lambdaI <- 0.05
gamma <- 0.005

files <- c("ua", "ub")
for(file in files){
  predictions <- train(file,
                       matrix(runif(numberOfItems * K, 0, 1), nrow = numberOfItems, ncol = K),
                       matrix(runif(numberOfUsers * K, 0, 1), nrow = numberOfUsers, ncol = K), 
                       lambda, lambdaU, lambdaI, gamma)
  test(file, predictions)
}





