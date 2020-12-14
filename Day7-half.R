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
         value = str_replace(value, " bags(.)| bag(.)", ""),
         count = as.numeric(count))

container <- c("shiny gold")
res_old <- c("shiny gold")
res_new <- list()
while (length(res_old) != 0){
  for (i in  1: length(res_old)){
    res_new[[i]] <- dt %>% mutate(shiny_gold = str_detect(value, res_old[i])) %>% filter(shiny_gold) %>% pull(from)
    i <- i + 1

  }
  print(res_new)
  container <- union(container, unique(unlist(res_new)))
  cat("container: ", container, "\n")
  res_old <- unique(unlist(res_new))
  res_new <- list()
  i <- 1
}

length(container) - 1

# Part 2
# container <- tibble()
# res_old <- c("shiny gold")
# res_new <- list()
# while (length(res_old) != 0){
#   for (i in  1: length(res_old)){
#     res_new[[i]] <- dt %>% mutate(shiny_gold = str_detect(from, res_old[i])) %>% filter(shiny_gold)
#     i <- i + 1
#
#   }
#   res_new_comb <- map_dfr(res_new, bind_rows)
#   res_old <- res_new_comb %>% pull(value) %>% unique()
#   container <- container %>% bind_rows(res_new_comb)
#   res_new <- list()
#   i <- 1
# }
#
#
#
# ans <- container %>% filter(is.na(count)) %>% distinct() %>% mutate(n = 1) %>% select(from, n)
# last <- ans %>% pull(from)
# while (length(last) > 1){
#   ans <- container %>% filter(value %in% last) %>%
#     full_join(ans, by = c("value" = "from")) %>%
#     mutate(from = ifelse(is.na(from), value, from),
#            count = ifelse(is.na(count), 1, count),
#            n = count * n) %>%
#     count(from, wt = n)
#   last <- ans %>% pull(from) %>% unique()
#   }
# ans$n
