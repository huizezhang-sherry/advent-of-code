library(tidyverse)

# set up
raw <- readLines(here::here("data-2021/day5-input.txt")) %>%
  as_tibble() %>%
  separate("value", into = c("x", "y"), sep = " -> ") %>%
  separate("x", into =c("x1", "y1"), sep = ",") %>%
  separate("y", into =c("x2", "y2"), sep = ",") %>%
  mutate(across(x1:y2, as.numeric))

find_coords <- function(input){
  tmp <- input %>%
    rowwise() %>%
    mutate(
      x_dir = ifelse(x1 < x2, 1, -1),
      y_dir = ifelse(y1 < y2, 1, -1),
      x = list(seq(x1, x2, x_dir)),
      y = list(seq(y1, y2, y_dir))) %>%
    select(x, y) %>%
    unnest() %>%
    mutate(
      x = x + 1,
      y = y + 1) %>%
    as.matrix()

  return(tmp)
}

check_counts <- function(coords){
  size <-  1000
  check <- matrix(rep(0, size * size), nrow = size, ncol = size)
  i <- 1
  for(i in 1: nrow(coords)) {
    check[coords[i,1], coords[i,2]] <- check[coords[i,1], coords[i,2]] + 1
    i <- i + 1
  }
  return(check)
}


# Part 1
input <- raw %>% filter(x1 == x2 | y1 == y2)
coords <- find_coords(input)
check <- check_counts(coords)
sum(check >= 2)

# part 2
coords <- find_coords(raw)
check <- check_counts(coords)
sum(check >= 2)

