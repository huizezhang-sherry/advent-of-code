# part 1
library(tidyverse)
dt_raw <- readLines("data/day7-input.txt")

comma <- max(str_count(dt_raw, ","))
dt <- as_tibble(dt_raw) %>%
  separate(sep = " contain ", into = c("from", "to"), col = value) %>%
  separate(sep = ", ", into = paste0("t0", 1: (comma+ 1)), col = to) %>%
  pivot_longer(names_to = "to", values_to = "value", cols = -from) %>%
  filter(!is.na(value)) %>%
  separate(sep = " ", into = c("count", "value"), col = value, extra = "merge") %>%
  mutate(from = str_replace(from, " bags|bag", ""),
         value = str_replace(value, " bags(.)| bag(.)| bag", ""),
         count = as.numeric(count))

container <- c("shiny gold")
res_old <- c("shiny gold")
res_new <- list()
while (length(res_old) != 0){
  for (i in  1: length(res_old)){
    res_new[[i]] <- dt %>% mutate(shiny_gold = str_detect(value, res_old[i])) %>% filter(shiny_gold) %>% pull(from)
    i <- i + 1

  }
  container <- union(container, unique(unlist(res_new)))
  res_old <- unique(unlist(res_new))
  res_new <- list()
  i <- 1
}

length(container) - 1

# Part 2
container <- list()
bag <- c("shiny gold")

i <- 1
while (!all(bag == "other")){
  container[i] <- list(dt %>% filter(from %in% bag))
  bag <- container[[i]] %>% pull(value) %>% unique()
  i <- i + 1
}

i <- length(container)
tally <- container[[i]] %>% group_by(from) %>%  tally(count)

for (i in length(container): 2){

  tally <- container[[i-1]] %>% left_join(tally, by = c("value" = "from")) %>%
    mutate(count = count + count * n) %>% group_by(from) %>% tally(count)

  i <- i-1
}

tally %>% pull(n)
