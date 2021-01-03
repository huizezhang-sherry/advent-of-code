library(tidyverse)

lower <- function(pos){
  start <- pos$start
  end <- (pos$start + pos$end - 1)/2
  return(list(start = start, end = end))
}

upper <- function(pos){
  start <- (pos$start + pos$end + 1)/2
  end <- pos$end
  return(list(start = start, end = end))
}

calc_id <- function(string){

  length <- str_length(string)
  i <- 1
  pos_x <- list(start = 0, end = 127)
  pos_y <- list(start = 0, end = 7)

  for (i in 1: length){
    letter <- str_sub(string, i, i)
    if (letter %in% c("B", "F")){
      pos_x <- switch(letter,
                      "B" = upper(pos_x),
                      "F" = lower(pos_x))
    } else if (letter %in% c("R", "L")){
      pos_y <- switch(letter,
                      "R" = upper(pos_y),
                      "L" = lower(pos_y))
    }
    i <- i + 1
  }

  x <- unique(pos_x) %>% unlist()
  y <- unique(pos_y) %>% unlist()
  id <- x * 8 + y

  return(list(x = x, y = y, id = id))
}

dt <- readLines("data/day5-input.txt")
max(map_dbl(dt, ~calc_id(.x)$id))

x <- map_dbl(dt, ~calc_id(.x)$x)
y <- map_dbl(dt, ~calc_id(.x)$y)
coord <- tibble(x = x, y = y) %>% arrange(x, y)

my_x <- coord %>%
  mutate(check = c(rep(6, 3), rep(7: 111, each = 8))) %>%
  filter(x != check) %>% head(1) %>% pull(check)
my_y <- setdiff(0:7, coord %>% filter(x == my_x) %>% pull(y))
my_x * 8 + my_y
