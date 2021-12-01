library(tidyverse)
raw <- as_tibble(readLines(here::here("data-2021/day1-input.txt"))) %>%
  mutate(value = as.numeric(value))

# Part 1
raw %>%
  mutate(inc = value - lag(value, default = NA) > 0) %>%
  count(inc)

# Part 2
out <- slider::slide_dbl(raw$value, sum, .after = 2) %>%
  diff()
sum(out > 0)
