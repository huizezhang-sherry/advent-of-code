library(tidyverse)

# Part 1
raw <- readLines(here::here("data-2021/day10-input.txt")) %>%
  as_tibble() %>%
  mutate(id = row_number())

cancel_pair <- function(dt){
  dt %>%
    mutate(
    value = str_replace_all(value, "\\(\\)", ""),
    value = str_replace_all(value, "\\[\\]", ""),
    value = str_replace_all(value, "\\{\\}", ""),
    value = str_replace_all(value, "\\<\\>", "")
  )
}

# first cancel out all the good pairs
leftover <- tibble(); leftover2 <- raw
while(!identical(leftover, leftover2)){
  leftover <- leftover2
  leftover <- leftover %>% cancel_pair
  leftover2 <- leftover %>% cancel_pair
}

# if the string doesn't have any of the four closing symbol then it is incomplete,
# otherwise it is corrupted
out <-  leftover %>%
  mutate(value = str_remove_all(value, "\\(|\\[|\\{|\\<"))
corrupted <- out %>% filter(value != "")
# incomplete_id for part 2
incomplete_id <- out %>% filter(value == "") %>% pull(id)

# the first closing symbol in the string is the problematic one, since otherwise it
# should have already be cancelled out
corrupted %>%
  mutate(p = str_sub(value, 1, 1)) %>%
  mutate(penalty = case_when(
    str_detect(p, "\\)") ~ 3,
    str_detect(p, "\\]") ~ 57,
    str_detect(p, "\\}") ~ 1197,
    str_detect(p, "\\>") ~ 25137
  )) %>%
  pull(penalty) %>%
  sum(na.rm = TRUE)

# Part 2
leftover %>%
  filter(id %in% incomplete_id) %>%
  mutate(
    value = str_replace_all(value, "\\(", "1"),
    value = str_replace_all(value, "\\[", "2"),
    value = str_replace_all(value, "\\{", "3"),
    value = str_replace_all(value, "\\<", "4")
  ) %>%
  rowwise() %>%
  mutate(value = list(rev(str_split(value, "")[[1]]))) %>%
  # use purrr::reduce to calc the score
  mutate(score = reduce(as.numeric(value), function(a, b)  a * 5 + b)) %>%
  pull(score) %>%
  median()


