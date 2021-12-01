dt <- readLines("data/day20-input.txt")

# part 1
puzzle <- as_tibble(dt) %>%
  mutate(pattern = value,
         value = parse_number(value),
         pattern = ifelse(!is.na(value), NA, pattern)) %>%
  fill(value) %>%
  filter(!is.na(pattern), str_length(pattern) != 0) %>%
  group_by(value)


puzzle %>% nest() %>%
  mutate(right = map_chr(data, ~str_sub(.x)))

left_right <- puzzle %>% mutate(first = str_sub(pattern, end = 1),
                  last = str_sub(pattern, start = -1),
                  id = row_number(),
                 ) %>%
  nest() %>%
  transmute(left1 = map_chr(data, ~str_c(.x$first, collapse = "")),
         right1= map_chr(data, ~str_c(.x$last, collapse = "")))

top_bot <- puzzle %>% mutate( id = row_number(),
                   top1 = ifelse(id== 1,  pattern, NA),
                   bottom1 = ifelse(id== max(id),  pattern, NA)) %>%
  fill(top1) %>% filter(!is.na(bottom1)) %>% select(-id, -pattern)

clean <- left_right %>% mutate(left2 = stringi::stri_reverse(left1),
                               right2 = stringi::stri_reverse(right1)) %>%
  left_join(top_bot %>% mutate(top2 = stringi::stri_reverse(top1),
                               bottom2 = stringi::stri_reverse(bottom1)), by = "value") %>%
  pivot_longer(cols = -c(value), names_to = "pos", values_to = "string") %>% ungroup()


dup_string <- clean %>% mutate(dup = duplicated(string)) %>% filter(dup) %>% pull(string)

match <- clean %>% mutate(match = ifelse(string %in% dup_string, TRUE, FALSE))

match %>% pivot_wider(names_from = "pos", values_from = match) %>%
  group_by(value) %>%
  nest() %>%
  transmute(left = map_lgl(data, ~any(.x$left1, .x$left2)),
         right = map_lgl(data, ~any(.x$right1, .x$right2)),
         top = map_lgl(data, ~any(.x$top1, .x$top2)),
         bottom = map_lgl(data, ~any(.x$bottom2, .x$bottom2))) %>%
  pivot_longer(-value, names_to = "pos", values_to = "match") %>%
  summarise(n = sum(match, na.rm = TRUE)) %>%
  filter(n == 2) %>% pull(value) %>% prod


