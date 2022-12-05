library(tidyverse)
# toy examples
dt <- list(
  c("Z", "N"),
  c("M", "C", "D"),
  c("P")
)

rules <- tibble(n = c(1,3,2,1), from = c(2,1,2,1), to = c(1,3,1,2))

# Part 1
raw <- readLines(here::here("data-2022/day5.txt"))
rules <- as_tibble(raw[11:length(raw)]) %>%
  separate(col = "value", sep = " from | to ", into = c("n", "from", "to")) %>%
  mutate(across(everything(), parse_number))

dt <- list(
  c("S", "T", "H", "F", "W", "R"),
  c("S", "G", "D", "Q", "W"),
  c("B", "T", "W"),
  c("D", "R", "W", "T", "N", "Q", "Z", "J"),
  c("F", "B", "H", "G", "L", "V", "T", "Z"),
  c("L", "P", "T", "C", "V", "B", "S", "G"),
  c("Z", "B", "R", "T", "W", "G", "P"),
  c("N", "G", "M", "T", "C", "J", "R"),
  c("L", "G", "B", "W")
  )

i <- 1
while (i <= nrow(rules)){
  to <- rules$to[i]
  from <- rules$from[i]
  rep <- rules$n[i]
  n <- 1

  repeat{
    to_last <- length(dt[[from]])
    dt[[to]] <- c(dt[[to]], dt[[from]][to_last])
    from_last <- length(dt[[from]])-1
    dt[[from]] <- dt[[from]][0:from_last]
    n <- n + 1
    if (n > rep){
      break
    }
  }

  i <- i + 1
}

map_chr(dt, ~.x[length(.x)]) %>% paste0(collapse = "")

# Part 2
i <- 1
while (i <= nrow(rules)){
  to <- rules$to[i]
  from <- rules$from[i]
  rep <- rules$n[i]

  to_start <- length(dt[[from]]) - rep + 1
  to_last <- length(dt[[from]])
  dt[[to]] <- c(dt[[to]], dt[[from]][to_start:to_last])
  from_last <- length(dt[[from]])-rep
  dt[[from]] <- dt[[from]][0:from_last]

  i <- i + 1
}
map_chr(dt, ~.x[length(.x)]) %>% paste0(collapse = "")
