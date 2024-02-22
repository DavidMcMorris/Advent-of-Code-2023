input <- readLines("input.txt")
input <- read.table(text = input)
input <- input[, 2:ncol(input)]

# Uncomment for part 2
# input <- t(t(as.numeric(apply(input, 1, paste, collapse = ""))))

num_ways <- function(a, n) {
  lower <- floor(0.5 * (a - sqrt(a^2 - 4 * n)) + 1)
  upper <- a - lower
  num <- upper - lower + 1
  return(num)
}

product <- 1
for (i in seq_len(ncol(input))) {
  num <- num_ways(input[1, i], input[2, i])
  product <- product * num
}

print(product)