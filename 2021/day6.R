library(tidyverse)

raw <- readLines(here::here("data-2021/day6-input.txt")) %>%
  strsplit(",") %>% .[[1]] %>% as.numeric()

# Part 1
vec <- raw

i <- 1
while (i <= 30){

  vec <- vec - 1

  zero_idx <- which(vec == -1)
  zeros_n <- sum(vec == -1)
  vec[zero_idx] <- 6

  vec <- append(vec, rep(8, zeros_n))
  print(vec)
  i <- i + 1
}

length(vec)

# Part 2
vec <-   raw
tmp <- table(vec) %>% as_tibble() %>%
  mutate(vec = as.numeric(vec)) %>%
  add_row(vec = c(0, 5:8), n = rep(0, 5))

total <- length(vec)
for (i in 1:256){
  tmp <- tmp %>%
    mutate(vec = vec-1) %>%
    count(vec, wt = n)

  inc <- tmp %>% filter(vec == -1) %>% pull(n)
  total <- total + inc

  tmp <- tmp %>%
    add_row(vec = 8, n = inc) %>%
    mutate(vec = ifelse(vec == -1, vec + 7, vec))
}

print(total)

