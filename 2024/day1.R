library(tidyverse)
# Part 1
raw <- readLines(here::here("data-2024/day1-input.txt")) |> as_tibble() |>
  separate(value, c("V1", "V2"), sep = 5) |>
  mutate(V1 = as.numeric(V1), V2 = as.numeric(V2))

sum(abs(sort(raw$V1) - sort(raw$V2)))

# Part 2
res <- map_dfr(raw$V1, ~tibble(x = .x, y = sum(raw$V2 %in% .x)))
sum(res$x * res$y)

