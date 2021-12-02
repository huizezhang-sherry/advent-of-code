library(tidyverse)
raw <- read_delim(here::here("data-2021/day2-input.txt"),
                  delim = " ",
                  col_names = c("direction", "n"))
# Part 1
tmp <- raw %>%
  mutate(n = ifelse(direction == "up", -n, n))

hor <- tmp %>% filter(direction == "forward") %>% pull(n) %>% sum()
vert <- tmp %>% filter(direction != "forward") %>% pull(n) %>% sum()
hor * vert

# Part 2
hor <- 0
vert <- 0
aim <- 0
for (i in 1:nrow(tmp)){
  if (tmp$direction[i] == "forward"){
    hor <- hor + tmp$n[i]
    vert <- vert + aim * tmp$n[i]
  } else if (tmp$direction[i] != "forward"){
    aim <- aim + tmp$n[i]
  }
  print(aim)
  #print(vert)
  i <- i + 1
}
hor * vert
