library(tidyverse)
raw <- readLines(here::here("data-2022/day7.txt"))
tree <- list()
i <- 1
j <- 1
idx <- NULL
while(i <= length(raw)){
  print(j)

  if (str_detect(raw[i], "cd \\..")){
    idx <- idx[-length(idx)]
  } else if (str_detect(raw[i], "cd ")){
    new_dir <- str_remove(raw[i], "\\$ cd ")
    idx <- c(idx, new_dir)
    tree[[idx]] <- list()
  } else if (str_detect(raw[i], "ls")){
    j <- i + 1

    while (!str_detect(raw[j], "\\$") & j <= length(raw)){

      size <- ifelse(is.na(parse_number(raw[j], na = "NA")),
                     str_remove(raw[j], "dir "),
                     str_extract(raw[j], "\\d+"))
      tree[[c(idx, "")]] <- size

      j <- j + 1
    }
  }

  i <- i + 1
}

dirs <- raw[raw %>% str_detect("dir")] %>% str_remove("dir ") %>% unique()
res <- tibble(size = unlist(tree, use.names = TRUE)) %>%
  mutate(path = names(size) %>% str_remove("\\d+")) %>%
  # don't know what's wrong with the loop above but it seems that there are some duplicates
  # say one of them is "hlgqdqb"
  distinct() %>%
  mutate(size = as.numeric(size)) %>%
  group_by(path) %>%
  summarise(size  = sum(size, na.rm =TRUE))

res <- res %>%
  rowwise() %>%
  mutate(size = sum(res$size[str_detect(res$path,path)]))
sum(res$size[res$size < 1e5])

# Part 2
to_free <- 3e7 - (7e7 - 41609574)
min(res$size[res$size > to_free])

