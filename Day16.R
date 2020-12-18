library(tidyverse)
dt <- readLines("data/day16-input.txt")

# part 1
criteria <- dt[1:20] %>% str_match("^(.+): (\\d+)-(\\d+) or (\\d+)-(\\d+)")
valid_num_list <- as_tibble(criteria[,3:4]) %>% bind_rows(as_tibble(criteria[,5:6])) %>%
  mutate(V1 = as.numeric(V1),
         V2 = as.numeric(V2),
         inbetween = map2(V1, V2, ~list(seq(from = .x, to =  .y)))) %>% pull(inbetween)

valid_num <- valid_num_list %>% unlist() %>% unique()

cand <- dt[26:length(dt)] %>% str_split(pattern = ",") %>% unlist() %>% as.numeric()
sum(cand[which(!cand %in% valid_num)])

# part 2
# filter out the valid ticket and criteria table
tickets <- dt[26:length(dt)] %>% str_split(pattern = ",") %>% map(~as.numeric(.x))
valid <- tickets %>% map_lgl(~  .x%in% valid_num %>% all)
valid_tic <- tickets[valid] %>% as_tibble(.name_repair = "unique") %>%
  mutate(id = row_number()) %>%
  pivot_longer(names_to = "ticket", values_to = "val", col = -id) %>%
  mutate(ticket = paste0("T",str_match(ticket, "^...(\\d+)")[,2])) %>%
  pivot_wider(names_from = "ticket", values_from = "val")
cri <- criteria[,2:6] %>% as_tibble() %>%
  mutate_at(c(paste0("V", 2:5)), as.numeric)

# filter out the invalid match between field and position
grid <- expand_grid(cri, valid_tic)
invalid <- grid %>%
  pivot_longer(-c(V1: id), names_to = "T", values_to = "value") %>%
  mutate(valid = (value >= V2 & value <= V3)|(value >= V4 & value <= V5)) %>% filter(!valid)

# loop around to find the match between field and position
valid_match <- grid %>% select(V1, id) %>% anti_join(invalid)
order <- valid_match %>% group_by(V1) %>% count() %>% arrange(n)
res <- tibble(var = character(),
              col = numeric())
i <- 1
for (i in 1: 20){
  var <- order %>% filter(n == i) %>% pull(V1)
  col <- valid_match %>% filter(V1 == var) %>% pull(id)

  valid_match <- valid_match %>% filter(id != col)

  res <- res %>% add_row(var = var, col = col)
  i <- i + 1
}

# find the corresponding value on my ticket and calc prod()
pos <- res %>% filter(str_detect(var, "departure")) %>% pull(col)
my_ticket <- dt[23] %>% str_split(",") %>% .[[1]]
my_ticket[pos] %>% as.numeric() %>% prod()
