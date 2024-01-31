# Add an extra blank line to end of input file
input <- readLines("input.txt")

seeds <- read.table(text = strsplit(input[1], ":")[[1]][2])
num_seeds <- length(seeds)

start_inds <- which(grepl("map", input)) + 1
stop_inds <- which(input == "")[-1] - 1
num_maps <- length(start_inds)

map_maker <- function(start, stop) {
  map <- read.table(text = input[start:stop])
  return(map)
}

mapper <- function(map, num) {
  starts <- map[1]
  low <- map[2]
  high <- map[2] + map[3] - 1
  range_check <- num >= low & num <= high
  if (sum(range_check) == 1) {
    ind <- which(range_check == TRUE)
    output <- num - low[ind, ] + starts[ind, ]
  } else {
    output <- num
  }
  return(output)
}

location <- NULL
for (i in 1:num_seeds) {
  val <- as.numeric(seeds[i])
  for (j in 1:num_maps) {
    map <- map_maker(start_inds[j], stop_inds[j])
    val <- mapper(map, val)
  }
  location <- min(location, val)
}

print(location)

# Part 2
location <- NULL
for (i in 1:num_seeds) {
  print(i)
  if (i %% 2 == 1) {
    low <- as.numeric(seeds[i])
    high <- as.numeric(seeds[i] + seeds[i + 1] - 1)
    for (j in low:high) {
      print(j)
      val <- j
      for (k in 1:num_maps) {
        map <- map_maker(start_inds[k], stop_inds[k])
        val <- mapper(map, val)
      }
      location <- min(location, val)
    }
  }
}

print(location)