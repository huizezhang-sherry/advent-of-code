#r -> s -> p -> r
# 1 -> 3 -> 2 -> 1
library(tidyverse)
raw <- as_tibble(readLines(here::here("data-2022/day2.txt")))

# Part 1
rule <- raw %>% unique() %>%
  separate(col = value, into = c("from", "to"), sep = " ") %>%
  mutate(score = case_when(to == "X" ~ 1,
                           to == "Y" ~ 2,
                           TRUE ~ 3),
         score = score + c(3, 6, 0, 0, 3, 6, 0, 6, 3))

raw %>%
  separate(col = value, into = c("from", "to"), sep = " ") %>%
  left_join(rule) %>%
  pull(score) %>%
  sum()


# Part 2
rule <- raw %>% unique() %>%
  separate(col = value, into = c("from", "res"), sep = " ") %>%
  mutate(score = case_when(res == "X" ~ 0,
                           res == "Y" ~ 3,
                           TRUE ~ 6),
         score = score + c(1, 1, 3, 2, 2, 2, 1, 3, 3))

raw %>%
  separate(col = value, into = c("from", "res"), sep = " ") %>%
  left_join(rule) %>%
  pull(score) %>%
  sum()
