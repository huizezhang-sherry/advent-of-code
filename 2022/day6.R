library(tidyverse)
raw <- readLines(here::here("data-2022/day6.txt")) %>% strsplit("") %>% .[[1]]

find_marker <- function(n){
  i <- n
  while(i <= length(str)){
    reduced <- reduce(str[(i - n + 1): i], union)

    if (length(reduced) < n){
      i <- i + 1
    } else{
      break
    }

  }

  return(i)
}

find_marker(4)
find_marker(14)

