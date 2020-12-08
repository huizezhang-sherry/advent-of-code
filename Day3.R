library(tidyverse)
dt <- readLines("data/day3-input.txt")

# Part 1
i <- 1
res <- vector()
dt_app <- dt

while (i <= length(dt)) {
  dt_app[i] <- paste(replicate(3*length(dt) + 1, dt[i]), collapse = "")
  res[i] <- str_sub(dt_app[i], 1 + 3*(i-1), 1 + 3*(i - 1))
  i <- i + 1
}


sum(str_count(res, "#"))


# Part 2
count_tree <- function (right, down) {
  i<- 1
  j <- 1
  res <- vector()
  dt_app <- dt

  while (i <= length(dt)) {
    dt_app[i] <- paste(replicate(right*length(dt), dt[i]), collapse = "")
    res[j] <- str_sub(dt_app[i], 1 + right*(j-1), 1 + right*(j - 1))
    i <- i + down
    j <- j + 1
  }
  #print(str_length(dt_app[1]))

  sum(str_count(res, "#"))
}

one <- count_tree(1, 1)
three <- count_tree(3, 1)
five <- count_tree(5, 1)
seven <- count_tree(7, 1)
double <- count_tree(1, 2)

one * three * five * seven * double


example <- readLines("data/day3-example.txt")
