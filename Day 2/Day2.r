r_max <- 12
g_max <- 13
b_max <- 14

input <- readLines("input.txt")
input <- strsplit(gsub("[[:punct:] ]+", " ", input), " ")
r <- lapply(lapply(input, grep, pattern = "red"), function(x){x - 1})
b <- lapply(lapply(input, grep, pattern = "blue"), function(x){x - 1})
g <- lapply(lapply(input, grep, pattern = "green"), function(x){x - 1})

r <- lapply(Map("[", input, r), as.numeric)
b <- lapply(Map("[", input, b), as.numeric)
g <- lapply(Map("[", input, g), as.numeric)

r_check <- which(lapply(lapply(r, function(x){x > r_max}), sum) > 0)
g_check <- which(lapply(lapply(g, function(x){x > g_max}), sum) > 0)
b_check <- which(lapply(lapply(b, function(x){x > b_max}), sum) > 0)

r_needed <- unlist(lapply(r, max))
b_needed <- unlist(lapply(b, max))
g_needed <- unlist(lapply(g, max))

val <- sum(setdiff(seq_along(input), c(r_check, g_check, b_check)))
print(val)
print(sum(r_needed * b_needed * g_needed))
