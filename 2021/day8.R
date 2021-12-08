library(tidyverse)

# Part 1
raw <- as_tibble(readLines(here::here("data-2021/day8-input.txt"))) %>%
  separate("value",into = c("train", "test"), sep = " \\| ", extra = "merge")

raw %>%
  pull(test) %>%
  str_split(" ") %>%
  map_dbl(~str_length(.x) %in% c(2, 3, 4, 7) %>% sum) %>%
  sum()

reorder_code <- function(vec){
   map_chr(vec, ~str_split(.x, "") %>% map_chr(~sort(.x) %>% paste(collapse = "")))
}

# Part 2
clean <- raw %>%
  mutate(id = row_number(),
         across(c(train, test), ~str_split(.x, " "))) %>%
  rowwise() %>%
  mutate(across(c(train, test), ~list(reorder_code(.x)))) %>%
  ungroup()


find_rule <- function(row){
  matched <- as_tibble(row) %>%
    mutate(num = case_when(
      str_length(value) == 2 ~ 1,
      str_length(value) == 3 ~ 7,
      str_length(value) == 4 ~ 4,
      str_length(value) == 7 ~ 8,
      str_length(value) == 5 ~ 235,
      str_length(value) == 6 ~ 690,
    ))

  # 235 all have three horizontal lines, this can used to differentiate 0 from 6 & 9
  three_horizontal <- Reduce(intersect, matched %>% filter(num == 235) %>% pull(value) %>% str_split(""))
  # to differentiate 6 & 9, see if the two right vertical lines both present
  # 3 can be differentiate from 2 and 5 in the same fashion
  right_vertical <- matched %>% filter(num == 1) %>% pull(value) %>% str_split("") %>% .[[1]]
  # to differentiate 2 & 5, count the match with 4, 5 has 3 matches while 2 only have 2 matches
  match_four <- matched %>% filter(num == 4) %>% pull(value) %>% str_split("") %>% .[[1]]

  out <- matched %>%
    mutate(value = reorder_code(value)) %>%
    rowwise() %>%
    mutate(check_horizontal = all(map_lgl(three_horizontal, ~str_detect(value, .x))),
           check_right = all(map_lgl(right_vertical, ~str_detect(value, .x))),
           match_four = sum(map_dbl(match_four, ~str_detect(value, .x)))) %>%
    mutate(num = case_when(
      num == 690 & (!check_horizontal) ~ 0,
      num == 690 & check_right ~ 9,
      num == 690 & !check_right ~ 6,
      num == 235 & check_right ~ 3,
      num == 235 & match_four == 3 ~5,
      num == 235 & match_four == 2 ~2,
      TRUE ~ num
    )) %>%
    select(value, num)

  out
}

rules <- clean %>%
  mutate(rule = map(train, find_rule)) %>%
  select(id, rule) %>%
  unnest(rule)

out3 <- clean %>%
  unnest_longer(test) %>%
  left_join(rules, by = c("id", "test" = "value")) %>%
  select(-test) %>%
  nest(num) %>%
  mutate(data = map_dbl(data, ~paste(pull(.x), collapse = "") %>% as.numeric())) %>%
  pull(data) %>%
  sum()

out3
