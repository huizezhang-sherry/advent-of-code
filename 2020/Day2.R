library(tidyverse)
dt_raw <- as_tibble(readLines("data/day2-input.txt"))

# Part 1
dt <- dt_raw %>%
  separate(value, into = c("bound", "letter", "password"), sep = " ") %>%
  separate(bound, into = c("lower", "upper"), sep = "-") %>%
  mutate( lower = as.numeric(lower),
          upper = as.numeric(upper),
          letter = str_remove(letter, ":"))

dt %>%
  mutate(occ = str_count(password, letter),
         pass = occ >= lower & occ <= upper) %>%
  count(pass)



# Part 2
dt %>%
  mutate(first = str_sub(password, lower, lower),
         second = str_sub(password, upper, upper),
         pass = xor(first == letter, second == letter)) %>%
  count(pass)
