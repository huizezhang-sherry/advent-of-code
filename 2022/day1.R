# Part 1
raw <- readLines(here::here("data-2022/day1.txt")) %>% as.numeric()
sum <-  rep(0, length = sum(is.na(raw)))
i <- 1; j <- 1
while (i <= length(raw)){
  if (!is.na(raw[i])){
    sum[j] <- sum[j] + raw[i]
  } else{
    j <- j + 1
  }
  i <- i + 1
}

max(sum, na.rm = TRUE)

# Part 2
sort(sum, decreasing = TRUE) %>% head(3) %>% sum()
