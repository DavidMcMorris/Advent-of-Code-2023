input <- readLines("sample.txt")

seedsoil <- read.table(text = input[4:5])

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