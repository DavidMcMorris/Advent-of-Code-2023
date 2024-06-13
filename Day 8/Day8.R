input <- "input.txt"
inst <- readLines(input, 1)
inst <- strsplit(inst, split = "")[[1]]
inst[which(inst == "L")] <- 2
inst[which(inst == "R")] <- 3
inst <- as.numeric(inst)

nodes <- read.table(input, skip = 2, sep = "=")

lr <- unlist(lapply(nodes[, 2], function(x) {
  strsplit(x, split = "\\,| \\(|\\)| ")[[1]][c(2, 4)]
}))

nodes <- cbind(nodes[, 1], matrix(lr, ncol = 2, byrow = TRUE))

nodes[, 1] <- unlist(lapply(nodes[, 1], function(x) {
  strsplit(x, split = " ")[[1]][1]
}))

len <- length(inst)

# Part 1
i <- 0
current <- "AAA"
while (current != "ZZZ") {
  i <- i + 1
  j <- (i - 1) %% len + 1
  row <- which(nodes[, 1] == current)
  current <- nodes[row, inst[j]]
}

# Part 2
ghost_inds <- grep("*A", nodes[, 1])
num_paths <- length(ghost_inds)
ghost_mat <- data.frame("current" = nodes[ghost_inds, 1])

flag <- 0
i <- 0
while (flag == 0) {
  i <- i + 1
  j <- (i - 1) %% len + 1
  flag2 <- 1
  for (k in 1:num_paths) {
    row <- which(nodes[, 1] == ghost_mat$current[k])
    ghost_mat$current[k] <- nodes[row, inst[j]]
    flag2 <- sum(grepl("*Z", ghost_mat$current[k])) * flag2
  }
  flag <- flag2
}
print(i)