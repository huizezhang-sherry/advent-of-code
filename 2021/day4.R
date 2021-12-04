library(tidyverse)
raw <- readLines(here::here("data-2021/day4-input.txt"))

order <- str_extract_all(raw[1], "\\(?[0-9]+\\)?")[[1]] %>% as.numeric()
size <- 100
m_list <- list(length = size) # 100 matrix in total: (601 -1)/(5+ 1)

i <- 1
while (i <= size){
  start <- 3 + 6 * (i-1)
  end <- start + 4

  m_list[[i]] <- paste0(raw[start : end], collapse = " ") %>%
    str_extract_all("\\(?[0-9,.]+\\)?")[[1]] %>%
    as_numeric() %>%
    matrix(nrow = 5, ncol = 5, byrow = TRUE)

  i <- i + 1

}

check_list <- replicate(size, matrix(rep(0, 25), nrow = 5, ncol = 5), simplify = FALSE)

j <- 1
while (j < length(order)){
  i <- 1
  for (i in 1: size){
    idx <- which(m_list[[i]] == order[j], arr.ind = TRUE)
    check_list[[i]][idx] <- 1

    if (any(rowSums(check_list[[i]]) == 5) | any(colSums(check_list[[i]]) == 5)){
      print(paste0("number: ", j))
      res <- list(m = m_list[[i]], check = check_list[[i]])
      return(res)
    }
    i <- i + 1
  }

  j <- j + 1
}

m_sum <- sum(res$m[which(res$check ==0, arr.ind = TRUE)])
m_sum * order[j]


# part 2
check_list <- replicate(size, matrix(rep(0, 25), nrow = 5, ncol = 5), simplify = FALSE)
win <- rep(0, size)
j <- 1
while (j < length(order)){
  i <- 1
  for (i in 1: size){
    idx <- which(m_list[[i]] == order[j], arr.ind = TRUE)
    check_list[[i]][idx] <- 1

    win_by_row <- any(rowSums(check_list[[i]]) == 5)
    win_by_col <- any(colSums(check_list[[i]]) == 5)
    win_already <- win[i] == 1

    if (!win_already & (win_by_col | win_by_row)) win[i] <- 1


    if (sum(win) == size){
      res <- list(m = m_list[[i]], check = check_list[[i]])
      return(res)
    }

    i <- i + 1
  }
  j <- j + 1
}

m_sum <- sum(res$m[which(res$check ==0, arr.ind = TRUE)])
m_sum * order[j]
