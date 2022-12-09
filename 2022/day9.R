library(tidyverse)
inst <- readLines(here::here("data-2022/day9.txt")) %>%
  as_tibble() %>%
  separate(value, into = c("dir", "steps")) %>%
  mutate(steps = as.numeric(steps))

rule <- inst %>% rowwise() %>% mutate(new = list(rep(dir, steps))) %>% unnest(new) %>% pull(new)

n <- 10000
raw <- matrix(data = 0, nrow = n, ncol = n)
hi <- ti <- n/2
hj <- tj <- n/2
i <- 1
for(i in 1:length(rule)){
  if (rule[i] == "R") hj <- hj + 1
  if (rule[i] == "L") hj <- hj - 1
  if (rule[i] == "U") hi <- hi - 1
  if (rule[i] == "D") hi <- hi + 1

  if (abs(hi - ti) <= 1 & abs(hj - tj) <= 1){
  } else if (abs(hi - ti) > 1 | abs(hj - tj) > 1){
    ti <- ti + 1 * sign(hi - ti)
    tj <- tj + 1 * sign(hj - tj)
    print(ti)
    print(tj)
    raw[ti, tj] <- 1
  } else if (abs(hi - ti) > 1){
    ti <- ti + 1 * sign(hi - ti)
    print(ti)
    print(tj)
    raw[ti, tj] <- 1
  } else if (abs(hj - tj) > 1){
    tj <- tj + 1 * sign(hj - tj)
    print(ti)
    print(tj)
    raw[ti, tj] <- 1
  }

  i <- i + 1

}

# Part 2
# nah
n <- 30
raw <- matrix(data = 0, nrow = n, ncol = n)
i_vec <- rep(n/2, 9)
j_vec <- rep(n/2, 9)
hi <- i_vec[1] <- n/2
hj <- j_vec[1] <- n/2
i <- 1

for(i in 1:5){
  if (rule[i] == "R") hj <- hj + 1
  if (rule[i] == "L") hj <- hj - 1
  if (rule[i] == "U") hi <- hi - 1
  if (rule[i] == "D") hi <- hi + 1

  if (abs(hi - i_vec[1]) <= 1 & abs(hj - j_vec[1]) <= 1){
  } else if (abs(hi - i_vec[1]) > 1 | abs(hj - j_vec[1]) > 1){
    i_from_sec <- i_vec[1:8]
    j_from_sec <- j_vec[1:8]
    i_vec[1] <- i_vec[1] + 1 * sign(hi - i_vec[1])
    j_vec[1] <- j_vec[1] + 1 * sign(hj - j_vec[1])
    i_vec <- c(i_vec[1], i_from_sec)
    j_vec <- c(j_vec[1], j_from_sec)
    raw[i_vec[9], j_vec[9]] <- 1
  }else if (abs(hi - i_vec[1]) > 1){
    i_from_sec <- i_vec[1:8]
    i_vec[1] <- i_vec[1] + 1 * sign(hi - i_vec[1])
    i_vec <- c(i_vec[1], i_from_sec)
    raw[i_vec[9], j_vec[9]] <- 1
  } else if (abs(hj - j_vec[1]) > 1){
    j_from_sec <- j_vec[1:8]
    j_vec[1] <- j_vec[1] + 1 * sign(hj - j_vec[1])
    j_vec <- c(j_vec[1], j_from_sec)
    raw[i_vec[9], j_vec[9]] <- 1
  }

  i <- i + 1

}
raw
i_vec
j_vec



