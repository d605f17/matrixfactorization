library(readr);

source("train.R")
source("test.R")
source("simUsers.R")
source("simItems.R")

numberOfUsers <- 943
numberOfItems <- 1682
K <- 20
lambda <- 0.05
gamma <- 0.005

files <- c("ua", "ub")
for(file in files){
  for(n in c(10, 20, 30, 40, 50))
  predictions <- train(file, lambda, lambdaU = 0.1, lambdaI = 0.1, gamma, n)
  test(file, predictions)
  predictions <- train(file, lambda, lambdaU = 0.05, lambdaI = 0.05, gamma, n)
  test(file, predictions)
  predictions <- train(file, lambda, lambdaU = 0.01, lambdaI = 0.01, gamma, n)
  test(file, predictions)
  predictions <- train(file, lambda, lambdaU = 0.005, lambdaI = 0.005, gamma, n)
  test(file, predictions)
  predictions <- train(file, lambda, lambdaU = 0.001, lambdaI = 0.001, gamma, n)
  test(file, predictions)
  predictions <- train(file, lambda, lambdaU = 0.0005, lambdaI = 0.0005, gamma, n)
  test(file, predictions)
  predictions <- train(file, lambda, lambdaU = 0.0001, lambdaI = 0.0001, gamma, n)
  test(file, predictions)
}


