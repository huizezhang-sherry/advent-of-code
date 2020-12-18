library(tidyverse)
dt <- readLines("data/day13-input.txt")

# part 1
target <- dt[1] %>% as.numeric()
bus <- str_remove_all(dt[2], ",x") %>% str_split(pattern = ",") %>% .[[1]] %>% as.numeric()
wait <- map_dbl(as.list(bus), ~.x - target %%.x)
bus[which.min(wait)] * min(wait)

# part 2
dt[2] <- c("1789,37,47,1889")
piece <- str_count(dt[2], ",") + 1
clean <- tibble(value = dt[2]) %>%
  separate(value, into = paste0("V", 1:piece), sep = ",") %>%
  pivot_longer(names_to = "v", values_to = "value", col = paste0("V", 1:piece)) %>%
  mutate(id = row_number()) %>% filter(value != "x") %>% select(-v) %>% mutate(value = parse_number(value))

check <- rep(FALSE, nrow(clean))
i <- 1
j <- 1
while (!all(check)){
  check <- rep(FALSE, nrow(clean))
  for (j in 1: nrow(clean)){
    test_bus <- clean$value[j]
    remainder <- i %% test_bus

    gap <- clean$id[j] - 1

    if (clean$id[j] == 1) {
      if (remainder == gap) check[j] <- TRUE
    }else{
      if (test_bus - remainder == gap) check[j] <- TRUE
    }

    j <- j  +1
  }

  i <- i + 1
}

print(i - 1)
