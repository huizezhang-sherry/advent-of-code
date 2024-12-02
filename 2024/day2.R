library(tidyverse)
raw <- readLines(here::here("data-2024/day2-input.txt"))

# Part 1
create_vec <- function(str){
  str |> str_split(" ") |> unlist() |> as.numeric()
}

check_safe <- function(str){
  vec <-  create_vec(str) |> diff()
  (all(vec > 0) | all(vec < 0)) & all(between(abs(vec), 1, 3))
}

res <- map_dbl(raw, check_safe)
sum(res)

# Part 2
res2 <- map_dbl(raw[res == 0], ~{
  vec <-  create_vec(.x)
  res1 <- map_dbl(1:length(vec), ~{
    vec2 <- diff(vec[-.x])
    (all(vec2 > 0) | all(vec2 < 0)) & all(between(abs(vec2), 1, 3))
  })
  if (sum(res1) >= 1) return(1) else(0)
}) |> sum()
res2 + sum(res)
