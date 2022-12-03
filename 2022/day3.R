library(tidyverse)
raw <- readLines(here::here("data-2022/day3.txt"))
rule <- tibble(sym = c(letters, LETTERS), value = 1:52)

# Part 1
find_com <- function(str){
  mid <- nchar(str)/2
  a <- strsplit(str_sub(str, end = mid), split = "")[[1]]
  b <- strsplit(str_sub(str, start = mid + 1), split = "")[[1]]
  intersect(a, b)
}

raw %>%
  map(find_com) %>%
  unlist() %>%
  as_tibble() %>%
  left_join(rule, by = c("value" = "sym")) %>%
  pull(value) %>%
  sum()


# Part 2
raw <- readLines(here::here("data-2022/day3.txt"))
tibble(raw = raw) %>%
  mutate(id = rep(1:100, each = 3)) %>%
  nest_by(id) %>%
  rowwise() %>%
  mutate(common = strsplit(data$raw, split = "") %>% reduce(intersect)) %>%
  left_join(rule, by = c("common" = "sym")) %>%
  pull(value) %>%
  sum()
