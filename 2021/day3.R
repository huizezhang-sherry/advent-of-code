library(tidyverse)
raw <- as_tibble(readLines(here::here("data-2021/day3-input.txt"))) %>%
  separate(col = value, into = as.character(seq(1:13)), sep = "") %>%
  pivot_longer(cols = -`1`, names_to = "col", values_to = "val")


res <- raw %>%
  group_by(col) %>%
  count(val) %>%
  pivot_wider(names_from = val, values_from = n) %>%
  mutate(gamma = ifelse(`0` > `1`, 0, 1),
         epsilon = ifelse(`0` > `1`, 1, 0),
         col = as.numeric(col)) %>%
  arrange(col)

g <- res %>%
  pull(gamma) %>%
  paste(collapse = "") %>%
  strtoi(base = 2)

e <- res %>%
  pull(epsilon) %>%
  paste(collapse = "") %>%
  strtoi(base = 2)
g * e




# Part 2
find_bin <- function(input = c("oxygen", "co2")){

  if (input == "oxygen"){
    larger <- 1; smaller <- 0
  } else{
    larger <- 0; smaller <- 1
  }

  raw <- as_tibble(readLines(here::here("data-2021/day3-input.txt"))) %>%
      separate(col = value, into = as.character(seq(1:13)), sep = "") %>%
      mutate(`1` = row_number())

  i <- 2
  while(nrow(raw) >= 2){
    col <- colnames(raw)[i]
    target <- table(raw[[col]])

    if (length(target) == 1){
      raw <- raw
    } else if (target[1] == target[2]){
      raw <- raw %>% filter(!!sym(col) == larger)
    } else if (names(which.max(target)) == 1){
      raw <- raw %>% filter(!!sym(col) == larger)
    } else{
      raw <- raw %>% filter(!!sym(col) == smaller)
    }

    i <- i + 1
  }

  print(raw)

  as_tibble(readLines(here::here("data-2021/day3-input.txt"))) %>%
    filter(row_number() == raw$`1`) %>%
    strtoi(base = 2)
}

oxy <- find_bin(input= "oxygen")
co2 <- find_bin(input= "co2")
oxy * co2

