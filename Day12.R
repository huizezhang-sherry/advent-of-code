library(tidyverse)
dt <- readLines("data/day12-input.txt")

# part 1
pos <- list(long = 0,
            lat = 0,
            dir = "East")

dir_list_right <- c("East", "South", "West", "North")
dir_list_left <- dir_list_right[4:1]

inst <- list(dir = str_sub(dt, end = 1),
             mag = str_sub(dt, start = 2) %>% as.integer())
dir_idx <- c()
move_dir <- inst$dir
i <- 1
for (i in 1:length(inst$dir)){

  if (inst$dir[i] %in% c("R", "L")){
    if (inst$dir[i] == "R"){
      dir_idx <- (which(dir_list_right == pos$dir) + inst$mag[i]/90)
      if (dir_idx > 4) dir_idx <- dir_idx%%4
      pos$dir <- dir_list_right[dir_idx]
    } else if (inst$dir[i] == "L"){
      dir_idx <- (which(dir_list_left == pos$dir) + inst$mag[i]/90)
      if (dir_idx > 4) dir_idx <- dir_idx%%4
      pos$dir <- dir_list_left[dir_idx]
    }
  } else if (inst$dir[i] == "F"){
    move_dir[i] <- str_sub(pos$dir, end = 1)
  }

  if (move_dir[i] == "E") pos$long <- pos$long + inst$mag[i]
  if (move_dir[i] == "W") pos$long <- pos$long - inst$mag[i]
  if (move_dir[i] == "N") pos$lat <- pos$lat + inst$mag[i]
  if (move_dir[i] == "S") pos$lat <- pos$lat - inst$mag[i]

  i <- i + 1

}

abs(pos$long) + abs(pos$lat)


# part 2
waypoint <- matrix(c(10, 1), ncol = 1)
pos <- matrix(c(0, 0), ncol = 1)
inst <- list(dir = str_sub(dt, end = 1),
             mag = str_sub(dt, start = 2) %>% as.integer())
i <- 1

rotation <- function(angle) {
  deg <- angle * pi / 180
  matrix(c(cos(deg),-sin(deg),
           sin(deg),cos(deg)),
         nrow =2, ncol = 2,byrow = TRUE)
}

for (i in 1:length(inst$dir)){

  if (inst$dir[i] %in% c("R", "L")){
    if (inst$dir[i] == "R"){
      waypoint <- rotation(-inst$mag[i]) %*% waypoint
    } else if (inst$dir[i] == "L"){
      waypoint <- rotation(inst$mag[i]) %*% waypoint
    }
  } else if (inst$dir[i] == "F"){
    pos <- pos + inst$mag[i] * waypoint
  }

  if (inst$dir[i] == "E") waypoint[1] <- waypoint[1] + inst$mag[i]
  if (inst$dir[i] == "W") waypoint[1] <- waypoint[1] - inst$mag[i]
  if (inst$dir[i] == "N") waypoint[2] <- waypoint[2] + inst$mag[i]
  if (inst$dir[i] == "S") waypoint[2] <- waypoint[2] - inst$mag[i]

  i <- i + 1

}

abs(pos[1]) + abs(pos[2])
