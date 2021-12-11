library(tidyverse)

# Part 1
raw <- readLines(here::here("data-2021/day11-input.txt")) %>%
  str_split("") %>%
  unlist() %>%
  as.numeric() %>%
  as_tibble() %>%
  mutate(row = rep(1:10, each = 10),
         col = rep(1:10, 10),
         flash = FALSE)

# differ from the find_nearby in day9 by allowing the diagonal
find_nearby2 <- function(dt, row_idx, col_idx){
  dt %>%
    filter(between(row, row_idx - 1, row_idx + 1),
           between(col, col_idx - 1, col_idx + 1)) %>%
    filter(!(row == row_idx & col == col_idx))
}

flash <- function(data){
  data %>%
    mutate(
      flash = ifelse(value > 9 | value == 0, TRUE, FALSE),
      value = ifelse(flash, 0, value),
      neighbour2 = list(find_nearby2(data, row, col) %>% pull(value)),
      value = ifelse(!flash, value + sum(neighbour2==0) - sum(neighbour==0), value),
      neighbour = list(neighbour2)
      )
}

i <- 1; res <- raw; n_flash <- 0
for (i in 1: 100){

  print(glue::glue("i = {i}"))
  res <- res %>% mutate(value = value  + 1, flash = FALSE, neighbour2 = NULL)

  res <- res; res2 <- res; j <- 1
  while(j ==1 | !identical(res, res2)){
    print(glue::glue("  j = {j}"))
    res <- res2
    if (j == 1){
      res <- res %>%
        rowwise() %>%
        mutate(neighbour = list(find_nearby2(raw, row, col) %>% pull(value)))
    }
    res <- res %>% flash()
    res2 <- res %>% flash()

    j <- j + 1
  }

  n_flash <- n_flash + res %>% filter(flash) %>% nrow()

  i <- i + 1

}

n_flash

# Part 2
i <- 1; res <- raw; n_flash <- 0
while (n_flash != nrow(res)){

  print(glue::glue("i = {i}"))
  res <- res %>% mutate(value = value  + 1, flash = FALSE, neighbour2 = NULL)

  res <- res; res2 <- res; j <- 1
  while(j ==1 | !identical(res, res2)){
    print(glue::glue("  j = {j}"))
    res <- res2
    if (j == 1){
      res <- res %>%
        rowwise() %>%
        mutate(neighbour = list(find_nearby2(raw, row, col) %>% pull(value)))
    }
    res <- res %>% flash()
    res2 <- res %>% flash()

    j <- j + 1
  }

  n_flash <- res %>% filter(flash) %>% nrow()
  print(n_flash)

  i <- i + 1


}

i - 1
