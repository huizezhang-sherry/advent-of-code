library(tidyverse)
dt <- readLines("data/day4-input.txt")

# Part 1
# my attempt
tibble(dt) %>%
  mutate(new_line = dt %>% str_detect("^[:space:]*$"),
         group = cur_group_rows())

# from Emil
passport <- str_split(paste(dt, collapse = " "),"  ")[[1]]
matches <- c("byr","iyr","eyr","hgt","hcl","ecl","pid")
str_extract_all(passport, paste0(matches, collapse = "|")) %>%
  map_lgl(~all(matches %in% .x)) %>%
  sum()

# Part 2
max <- max(str_count(passport, ":"))
valid <- passport %>%
  as_tibble() %>%
  mutate(id = row_number()) %>%
  separate(sep = " ", into = c(paste0("col", 1:max)), col = value, extra = "merge") %>%
  pivot_longer(names_to = "col", values_to = "pair", cols = col1:col8) %>%
  separate(pair, sep = ":", into = c("key", "name")) %>%
  select(-col) %>%
  filter(!is.na(key)) %>%
  pivot_wider(names_from = key, values_from = name) %>%
  mutate(byr_check = if_else(between(byr, 1920, 20002), TRUE, FALSE),
         iyr_check = if_else(between(iyr, 2010, 2020), TRUE, FALSE),
         eyr_check = if_else(between(eyr, 2020, 2030), TRUE, FALSE),
         hgt_check = case_when(str_detect(hgt, "cm") & between(parse_number(hgt), 150, 193) ~ "TRUE",
                                str_detect(hgt, "in")& between(parse_number(hgt), 59, 76) ~ "TRUE",
                                FALSE ~ "FALSE"),
         hgt_check = as.logical(hgt_check),
         hcl_check  = map_lgl(str_split(hcl, ""), ~.x %in% c(0:9, letters[1:6], "#") %>% all()),
         hcl_check2 = str_detect(hcl, "#") & str_length(hcl) == 7,
         ecl_check = ecl %in% c("amb","blu","brn", "gry", "grn", "hzl", "oth"),
         pid_check = str_length(pid) == 9 & str_sub(pid, end = 1) ==0)


cols <- colnames(valid)[10:17]

valid %>% transmute(valid = byr_check + iyr_check + eyr_check + hgt_check +
                      hgt_check + hcl_check + hcl_check + ecl_check + pid_check)  %>%
  count(valid)



