library(tidyverse)

raw <- readLines(here::here("data-2021/day12-input.txt")) %>%
  as_tibble() %>%
  separate(value, into = c("node1", "node2"), sep = "-") %>%
  mutate(id = row_number())

find_connected <- function(node){

  all <- raw %>% pivot_longer(-id, names_to = "node", values_to = "value")
  connected_id <- all %>% filter(value == {{node}}) %>% pull(id)
  all %>% filter(id %in% connected_id, value != {{node}}) %>% pull(value)
}

update <- function(x){
  last_one <- last(x)
  if (last_one != "end"){
    found <- c(x[toupper(x) != x]) # this include start
    to_append <- find_connected(last_one)
    to_append <- to_append[!to_append %in% found]
    map(to_append, ~c(x, .x)) %>% map(~unlist(.x))
  } else{
    list(x)
  }

}


res <- list()
res[[1]] <- "start"
while(!all(map_lgl(res, ~last(.x) == "end"))){
  res <- res %>% map(update) %>% flatten()
  print(length(res))
}

length(res)

# Part 2
# too long doesn't work
update2 <- function(x){
  last_one <- last(x)
  if (last_one != "end"){
    lower <- x[toupper(x) != x]
    to_append <- find_connected(last_one)
    to_append <- to_append[to_append != "start"]
    map(to_append, ~c(x, .x)) %>% map(~unlist(.x) %>% cancell())
  } else{
    list(x)
  }
}

cancell <- function(x){
  if (table(x)[["start"]] > 1) NULL

  lower <- x[toupper(x) != x & x != "start"]
  tab <- table(lower)
  prob <- which(tab >= 2)

  if (length(prob) != 0){
    if (length(prob) > 1 | tab[prob] > 2){
      NULL
    } else{
      x
    }
  } else{
    x
  }

}

res <- list()
res[[1]] <- "start"
while(!all(map_lgl(res, ~last(.x) == "end"))){
  res <- res %>% map(update2) %>% flatten() %>% compact()
  print(length(res))
}

length(res)
