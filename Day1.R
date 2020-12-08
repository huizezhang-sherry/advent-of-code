dt <-as.numeric(readLines("data/day1-input.txt"))

# Part 1
i <- 1
j <- 1
for (i in 1:length(dt)) {
  for (j in i: length(dt)) {
    if (dt[i] + dt[j] == 2020) {

      res <- dt[i] * dt[j]
      print(res)
    } else {
      j <- j + 1
    }
  }
  i <-  i + 1
}

# Part 2
i <- 1
j <- 1
k <- 1
for (i in 1: length(dt)) {
  for (j in i: length(dt)) {
    for (k in j : length(dt)) {
      if (dt[i] + dt[j] + dt[k] == 2020 ){
        res <- dt[i] *dt[j] *dt[k]
        print(res)
      }
        k <- k + 1
    }
    j <-  j + 1
  }
  i <-  i + 1
}
