library(tidyverse)

# part 1
raw <- readLines(here::here("data-2021/day7-input.txt")) %>%
  strsplit(",") %>% .[[1]] %>% as.numeric()
center <- median(raw)
sum(abs(raw - center))

# part 2
center <- floor(mean(raw))
out <- as_tibble(abs(raw - center)) %>%
  rowwise() %>%
  mutate(val = list(seq(0,value, 1)),
         sum = sum(val)) %>%
  pull(sum) %>%
  sum()
