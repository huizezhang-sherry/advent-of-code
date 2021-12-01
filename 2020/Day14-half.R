library(tidyverse)
dt <- readLines("data/day14-input.txt")
pos <- c()
out <- c()
int_to_bit <- function(x)  paste0("0000", paste0(rev(intToBits(x)) %>% as.numeric(), collapse = ""), collapse = "")
i <- 1
j <- 1

while (i <= length(dt)) {
  if (str_detect(dt[i], "mask")) {
    mask <- str_match(dt[i], "^.* = (.*)")[2]
  } else{
    pos[i] <- str_match(dt[i], ".*\\[(.*)\\].*")[2] %>% as.numeric()
    num <- str_match(dt[i], ".*\\[.*\\] = (.*)")[2] %>% as.numeric() %>% int_to_bit()
    res <- paste0(rep(0, str_length(mask)), collapse = "")
    for (j in 1:str_length(mask)){
      if (str_sub(mask, j, j) != "X"){
        str_sub(res, j, j) <- str_sub(mask, j, j)
      }else{
        str_sub(res, j, j) <- str_sub(num, j, j)
      }
      j <-  j + 1
    }

    out[i] <- list(36 - str_locate_all(res, "1")[[1]][,1]) %>% map_dbl(~sum(2^.x))


  }
  i <- i + 1
  j <- 1
}

options(scipen=999)

tibble(pos = pos, out = out) %>%
  filter(!is.na(pos)) %>%
  mutate(id = row_number()) %>%
  group_by(pos) %>%
  filter(id == max(id)) %>%
  pull(out) %>%
  sum()
