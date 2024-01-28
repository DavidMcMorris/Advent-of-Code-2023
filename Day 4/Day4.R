input <- readLines("input.txt")
input <- strsplit(input, ": ")
input <- sapply(input, "[", 2)
input <- strsplit(input, "\\|")

# Store lists of winning numbers and numbers on tickets
winners <- lapply(input, function(x) {read.table(text = x[[1]])})
numbers <- lapply(input, function(x) {read.table(text = x[[2]])})

# Find number of winning numbers on each ticket
num_wins_mat <- t(mapply(function(x, y) {(x %in% y)}, x = winners, y = numbers))
num_wins <- rowSums(num_wins_mat)

# Count number of each ticket based on part 2 rules
num_tickets <- rep(1, length(num_wins))
for (i in seq_along(num_tickets)) {
  if (num_wins[i] > 0) {
    ind <- i + num_wins[i]
    num_tickets[(i + 1):ind] <- num_tickets[(i + 1):ind] + num_tickets[i]
  }
}

# Part 1 Answer
score <- sum(floor(2^(num_wins - 1)))
print(score)

# Part 2 Answer
num_winning_tickets <- sum(num_tickets)
print(num_winning_tickets)