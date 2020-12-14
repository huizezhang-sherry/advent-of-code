library(tidyverse)
dt <- readLines("data/day10-input.txt") %>% as.numeric()

# part 1
vec <- sort(dt)
sorting <- tibble(orig  = c(0, vec)) %>%
  add_row(orig = max(vec) + 3) %>%
  mutate(lag = lag(orig, default = 0),
         diff = orig - lag)
count <- sorting %>% count(diff)

count$n[2] * count$n[3]

# part 2

mid <- sorting %>%
  mutate(lead = lead(orig, default = 0),
         diff2 = lead - orig,
         mid = ifelse(diff ==1 & diff2 == 1, TRUE, FALSE))  %>% pull(mid)

i <- 1
counter <- 0
acc <- c()
for (i in 1: length(mid)){
  if (mid[i] == TRUE) {
    counter <- counter + 1
  } else{
    if (counter != 0) acc <- append(acc, counter)
    counter <- 0
  }
  i <- i  + 1
}

acc_aft <- c()
for (i in 1: length(acc)){
  if (acc[i] == 3) acc_aft[i] <-  7
  if (acc[i] == 2) acc_aft[i] <-  4
  if (acc[i] == 1) acc_aft[i] <-  2
}

prod(acc_aft)
