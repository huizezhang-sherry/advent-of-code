library(tidyverse)
library(progressr)
# Part 1
raw <- readLines(here::here("data-2021/day9-input.txt")) %>%
  str_split("") %>%
  unlist() %>%
  as.numeric() %>%
  as_tibble() %>%
  mutate(row = rep(1:100, each = 100),
         col = rep(1:100, 100))

find_nearby <- function(row_idx, col_idx){
  raw %>%
    filter(between(row, row_idx - 1, row_idx + 1),
           between(col, col_idx - 1, col_idx + 1)) %>%
    filter(row == row_idx | col == col_idx) %>%
    filter(!(row == row_idx & col == col_idx))
}

out <- raw %>%
  rowwise() %>%
  mutate(neighbour = list(find_nearby(row, col) %>% pull(value))) %>%
  mutate(test = all(neighbour > value))

vec <- out %>% filter(test) %>% pull(value)
sum(vec + 1)


# Part 2
raw <- readLines(here::here("data-2021/day9-input.txt")) %>%
  str_split("") %>%
  unlist() %>%
  as.numeric() %>%
  as_tibble() %>%
  mutate(row = rep(1:100, each = 100),
         col = rep(1:100, 100))

res <- raw %>%
  mutate(value = ifelse(value != 9, 0, value)) %>%
  pivot_wider(names_from = col, values_from = value) %>%
  select(-row) %>%
  as.matrix(byrow = TRUE)
rownames(res) <- 1:(dim(res)[1])

i <- 1; j <- 1; k <- 1
checked <- tibble(row = numeric() , col = numeric(), group = numeric())
local_check <- tibble(row = numeric() , col = numeric())
master <- raw %>% filter(value != 9) %>% select(row, col)
total_row <-  raw %>% filter(value != 9) %>% nrow()
current_row <- total_row - nrow(master)

while (nrow(master) > 0){

  i <- as.numeric(master[1,1])
  j <- as.numeric(master[1,2])
  new_group <- TRUE

  while(new_group | nrow(local_check) != 0) {

    if (!new_group){
      i <- local_check[1, 1]
      j <- local_check[1, 2]
      local_check <- local_check[-1,]
    } else{
      new_group <- FALSE
    }

      checked <- checked %>% add_row(row = i, col = j, group = k)
      row <- seq(i - 1, i + 1, 1)
      col <- seq(j - 1, j + 1, 1)

      row <- row[row <= dim(res)[1]]
      col <- col[col <= dim(res)[2]]

      local <- res[row, col]
      new_check <- which(!(local == 9), arr.ind = TRUE) %>%
        as_tibble() %>%
        mutate(col = as.numeric(colnames(local)[col]),
               row = as.numeric(rownames(local)[row])) %>%
        filter(row == i | col == j) %>%
        setdiff(checked %>% select(row, col)) %>%
        as.matrix(byrow = TRUE)

      local_check <- rbind(local_check, new_check) %>% distinct()

    }

  k <- k + 1
  print(glue::glue("new group: {k}"))
  master <- setdiff(master, checked %>% select(row, col))
}

checked %>%
  count(group) %>%
  slice_max(n, n = 3) %>% pull(n) %>% prod()
