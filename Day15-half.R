library(tidyverse)
vec <- c(2,0,6,12,1,3)
vec <- c(0, 3,6)
i <- length(vec) + 1
for (i in length(vec)+1: 30000000){
  if (!duplicated(vec)[i-1]){
    vec[i] <- 0
  }else{
    vec[i] <- (i-1) - nth(which(vec == vec[i-1]), -2)
  }
  i <- i + 1
}
vec[2020]
