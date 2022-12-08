library(tidyverse)
raw <- readLines(here::here("data-2022/day8.txt"))
nrow <- ncol <- 99
dt <- raw %>%
  strsplit("") %>% unlist() %>% as.numeric() %>% matrix(nrow = nrow, ncol = ncol, byrow = TRUE)

res <- c()

find_vis <- function(i,j){
  if (all(dt[i, j] > dt[1:(i - 1), j])) res <- c(res, "top")
  if (all(dt[i, j] > dt[(i + 1):ncol, j])) res <- c(res, "bottom")
  if (all(dt[i, j] > dt[i, 1:(j - 1)])) res <- c(res, "left")
  if (all(dt[i, j] >   dt[i, (j + 1):nrow])) res <- c(res, "right")
  return(res)

}

inside <- crossing(x = 2: (nrow-1), y = 2:(ncol-1)) %>%
  rowwise() %>%
  mutate(pos = list(find_vis(x, y))) %>%
  filter(length(pos) > 0) %>%
  nrow()

inside + ncol * 2 + (nrow -2) * 2

# Part 2
find_ntrees <- function(i,j){

  top <- last(which(dt[i,j] <=  dt[1:(i - 1), j]))
  bottom <- first(which(dt[i,j] <= dt[(i + 1):ncol, j]))
  left <- last(which(dt[i,j] <=  dt[i, 1:(j - 1)]))
  right <- first(which(dt[i,j] <=  dt[i, (j + 1):nrow]))

  less <- list(top, bottom, left, right) %>% map_dbl(~ifelse(is.na(.x), 0, .x))
  pos <- c("top", "bottom", "left", "right")
  res <- map2_dbl(less, pos, ~find_n(less = .x, pos = .y, i = i, j = j))
  prod(res)

}

find_n <- function(less, pos, i, j){
  if (pos == "top") {if (less == 0) {res <- i - 1} else{res <- i - less}}
  if (pos == "bottom"){if (less == 0) {res <- nrow - i} else{res <- less}}
  if (pos == "right"){if (less == 0) {res <- ncol - j} else{res <- less}}
  if (pos == "left"){if (less == 0) {res <- j - 1} else{res <- j - less}}
  return(res)
}


my <- crossing(x = 2: (nrow-1), y = 2:(ncol-1)) %>%
  rowwise() %>%
  mutate(pos = find_ntrees(i = x, j = y)) %>%
  ungroup()

max(my$pos)


