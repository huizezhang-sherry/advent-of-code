library(tidyverse)
raw <- readLines(here::here("data-2022/day4.txt")) %>% as_tibble()

# Part 1
clean <- raw %>%
  separate(value, into = c("from", "to"), sep = ",") %>%
  separate(from, into = c("from_from", "from_to"), sep = "-") %>%
  separate(to, into = c("to_from", "to_to"), sep = "-") %>%
  mutate(across(everything(), as.numeric))

clean %>%
  mutate(contain1 = (from_from <= to_from) & (from_to >= to_to),
         contain2 = (to_from <= from_from) & (to_to >= from_to),
         contain = contain1 | contain2) %>%
  filter(contain) %>%
  pull(contain) %>%
  sum()


# Part 2
no_overlap <- clean %>%
  mutate(no_overlap = (from_from > to_to) | from_to < to_from) %>%
  filter(no_overlap) %>%
  pull(no_overlap) %>%
  sum()
nrow(clean) - no_overlap
