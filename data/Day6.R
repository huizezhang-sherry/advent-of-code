library(tidyverse)
dt <- readLines("data/day6-input.txt")

str_split(paste0(dt, collapse = " "), "  ")[[1]] %>%
  str_split("") %>%
  map_dbl(~unique(.x) %>% str_remove_all(" ") %>% paste(collapse = "") %>% nchar) %>% sum()

ans <- str_split(paste0(dt, collapse = " "), "  ")[[1]] %>%
  str_split("")

max_col <- max(str_count(ans %>% map(~paste(.x, collapse = "")), " ")) + 1

ans %>% map_chr(~paste(.x, collapse = "")) %>% as_tibble() %>%
  separate(col = value, sep = " ", into = paste0("col", 1: max_col)) %>%
  mutate(id = row_number()) %>%
  pivot_longer(names_to = "col",values_to = "val", col = -id) %>%
  filter(!is.na(val)) %>%
  mutate(letter = str_split(val, "")) %>%
  unnest(letter) %>%
  group_by(id) %>%
  add_count(letter) %>%
  mutate(n_person = n_distinct(col)) %>%
  filter(n == n_person) %>%
  distinct(letter)

