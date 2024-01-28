input <- readLines("input.txt")

input <- strsplit(input, ": ")
input <- sapply(input, "[", 2)
input <- strsplit(input, "\\|")

winners <- lapply(input, function(x) {read.table(text = x[[1]])})
numbers <- lapply(input, function(x) {read.table(text = x[[2]])})

num_wins <- t(mapply(function(x, y) {(x %in% y)}, x = winners, y = numbers))
score <- sum(floor(2^(rowSums(num_wins) - 1)))
print(score)