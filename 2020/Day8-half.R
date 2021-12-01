library(tidyverse)

# part 1
dt <- readLines("data/day8-input.txt") %>%
  as_tibble() %>%
  separate(col = value, into = c("instruct", "steps"), sep = " ") %>%
  mutate(dir = str_sub(steps, end = 1),
         steps = parse_number(steps),
         id = row_number())

acc <- 0
row <- c()
i <- 1
while (!any(duplicated(row))){
  if (dt$instruct[i] == "nop") {
    row <- append(row, dt$id[i])
    i <- i + 1
  } else if (dt$instruct[i] == "acc"){
    row <- append(row, dt$id[i])
    acc <- acc + dt$steps[i]
    i <- i + 1
  } else if (dt$instruct[i] == "jmp" ){
    row <- append(row, dt$id[i])
    i <- i + dt$steps[i]
  }
}

# part 2
# dt_mod <- dt %>% mutate(instruct = ifelse(id == 326, "nop", instruct))
# acc <- 0
# row <- c()
# i <- 1
# while (!any(duplicated(row))){
#   if (dt_mod$instruct[i] == "nop") {
#     row <- append(row, dt_mod$id[i])
#     i <- i + 1
#   } else if (dt_mod$instruct[i] == "acc"){
#     row <- append(row, dt_mod$id[i])
#     acc <- acc + dt_mod$steps[i]
#     i <- i + 1
#   } else if (dt_mod$instruct[i] == "jmp" ){
#     row <- append(row, dt_mod$id[i])
#     i <- i + dt_mod$steps[i]
#   }
# }
