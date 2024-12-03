library(tidyverse)
raw <- paste0(readLines("data-2024/day3-input.txt"), collapse = "")

# Part 1
parse_string <- function(str){
  matches_info <- gregexpr("mul\\(\\d+,\\d+\\)", str)
  matches <- regmatches(str, matches_info)[[1]]
  map_dbl(matches, ~{
    num_str <- gsub("mul\\((\\d+),(\\d+)\\)", "\\1, \\2", .x)
     as.numeric(strsplit(num_str, ", ")[[1]]) |> prod()
  })
}

sum(parse_string(raw))

# Part 2
parse_string2 <- function(str){
  matches_info <- gregexpr("mul\\(\\d+,\\d+\\)|don\'t\\(\\)|do\\(\\)", str)
  matches <- regmatches(str, matches_info)[[1]]

  new <- c()
  bad <- FALSE
  i <- 1
  while (i %in% 1:length(matches)){
    if (matches[i] == "don't()") {
      bad <- TRUE
    } else if (matches[i] == "do()") {
      bad <- FALSE
      }
    if (!bad) new <- c(new, matches[i])
    i <- i + 1
  }

  map_dbl(new[new != "do()"], ~{
    num_str <- gsub("mul\\((\\d+),(\\d+)\\)", "\\1, \\2", .x)
    as.numeric(strsplit(num_str, ", ")[[1]]) |> prod()
  })
}

sum(parse_string2(raw))
