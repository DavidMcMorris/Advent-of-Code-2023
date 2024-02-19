# Add an extra blank line to end of input file
input <- readLines("input.txt")

seeds <- read.table(text = strsplit(input[1], ":")[[1]][2])
num_seeds <- length(seeds)

start_inds <- which(grepl("map", input)) + 1
stop_inds <- which(input == "")[-1] - 1
num_maps <- length(start_inds)

map_maker_forward <- function(start, stop) {
  map <- read.table(text = input[start:stop])
  return(map)
}

map_maker_backward <- function(start, stop) {
  map <- read.table(text = input[start:stop])[c(2, 1, 3)]
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
    map <- map_maker_forward(start_inds[j], stop_inds[j])
    val <- mapper(map, val)
  }
  location <- min(location, val)
}

print(location)


endpoints <- NULL
for (i in rev(1:num_maps)) {
  map <- map_maker_backward(start_inds[i], stop_inds[i])
  ends <- cbind(map[1], map[1] + map[3] - 1)
  ends <- cbind(ends[1] - 1, ends, ends[2] + 1)
  ends <- unique(as.numeric(unlist(ends)))
  ends <- ends[order(ends)]
  if (i > 1) {
    new_map <- map_maker_backward(start_inds[i - 1], stop_inds[i - 1])
    for (j in seq_along(ends)) {
      value <- mapper(new_map, ends[j])
      endpoints <- c(endpoints, value)
    }
    endpoints <- unique(endpoints)
  } else {
    endpoints <- unique(c(endpoints, ends))
  }
}


# Part 2
location <- NULL
for (i in 1:num_seeds) {
  if (i %% 2 == 1) {
    low <- as.numeric(seeds[i])
    high <- as.numeric(seeds[i] + seeds[i + 1] - 1)
    test_points <- c(low, high)
    for (k in seq_along(endpoints)) {
      if (endpoints[k] > low && endpoints[k] < high) {
        test_points <- c(test_points, endpoints[k])
      }
    }
    for (j in test_points) {
      val <- j
      for (k in 1:num_maps) {
        map <- map_maker_forward(start_inds[k], stop_inds[k])
        val <- mapper(map, val)
      }
      location <- min(location, val)
    }
  }
}


print(location)