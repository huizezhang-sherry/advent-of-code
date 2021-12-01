library(tidyverse)
dt <- readLines("data/day9-input.txt") %>% as.numeric()
length <- 25

# part 1
for (i in 1: length(dt)){
  j <- i + length
  cand <- expand.grid(dt[i:j],dt[i:j]) %>% filter(Var1 != Var2) %>% mutate(add = Var1 + Var2) %>% pull(add) %>% unique()
  if(!dt[j+ 1] %in% cand) {
    out <- list(out = dt[j + 1], j = j)
    break
  }
  i <- i + 1
}
out$out

# part 2
i <- 1
l <- 1
for (i in 1: 1000){
  for (l in 1: length(dt)){
    if (i + l < length(dt)) {
      j <- i + l
      if (sum(dt[i: j]) == out$out){
        res <- list(i = i, j = j)
        break
      }
    }
    l <- l + 1
  }
  i <-  i + 1
}

sum(range(dt[res$i: res$j]))
