library(tidyverse)
dt_raw <- readLines("data/day11-example.txt")

# part 1
dt <- dt_raw %>%
  as_tibble() %>%
  mutate(row_orig = row_number(),
         value = str_split(value, "")) %>%
  unnest(value) %>%
  group_by(row_orig) %>%
  mutate(col_orig = row_number()) %>%
  ungroup() %>%
  mutate(value = ifelse(value == "L", 1, NA))

new_dt <- tibble(value = numeric())
counter <- 1
nrow <- length(dt_raw)
ncol <- unique(str_length(dt_raw))

while(!identical(new_dt$value, dt$value)){

  if (counter != 1) dt <- new_dt

  res <- dt %>% mutate(row_up = row_orig - 1,
                       row_down = row_orig + 1,
                       col_left = col_orig - 1,
                       col_right = col_orig + 1,
                       index = row_number()) %>%
    pivot_longer(names_to = "id_name", values_to = "id", cols = row_orig: col_right) %>%
    separate(id_name, into = c("pos", "dir"), sep = "_") %>%
    pivot_wider(names_from = pos, values_from = id) %>%
    group_by(index, value) %>%
    nest() %>%
    transmute(permu = map(data, ~expand_grid(.x$row, .x$col))) %>%
    unnest(permu) %>%
    rename(row_permu = `.x$row`,
           col_permu = `.x$col`) %>%
    filter(between(row_permu, 1, nrow), between(col_permu, 1, ncol)) %>%
    ungroup(value) %>%
    filter(!is.na(row_permu), !is.na(col_permu)) %>%
    mutate(orig_id = row_number()) %>%
    filter(orig_id != 1) %>%
    select(-orig_id)

  eight_count <- res %>%
    left_join(dt, by = c("row_permu" = "row_orig", "col_permu" = "col_orig")) %>%
    summarise(count = sum(value.y, na.rm = TRUE), .groups = "keep")

  new_dt <- dt %>%
    mutate(index = row_number()) %>%
    left_join(eight_count, by = "index") %>%
    mutate(value = ifelse(is.na(value), value,
                          ifelse(count >=4, 0, value)),
           value = ifelse(is.na(value), value,
                          ifelse(count == 0, 1, value))) %>%
    select(-index, -count)

  counter <- counter + 1
  print(counter)
}

sum(dt$value, na.rm = TRUE)

