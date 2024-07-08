input <- "input.txt"
space <- readLines(input)
space <- lapply(space, strsplit, split = "")
space <- matrix(unlist(space), nrow = length(space), byrow = TRUE)

gal_inds <- which(space == "#")
num_gal <- length(gal_inds)
space[gal_inds] <- seq_len(num_gal)
dims <- dim(space)

exp_rows <- which(rowSums(space == ".") == ncol(space))
exp_cols <- which(colSums(space == ".") == nrow(space))

total_dist <- 0
for (i in seq_len(num_gal - 1)) {
  current_gal <- arrayInd(gal_inds[i], dims)
  for (j in (i + 1):num_gal) {
    next_gal <- arrayInd(gal_inds[j], dims)
    ext_rows <- sum(current_gal[1] < exp_rows & exp_rows < next_gal[1]) +
      sum(current_gal[1] > exp_rows & exp_rows > next_gal[1])
    ext_cols <- sum(current_gal[2] < exp_cols & exp_cols < next_gal[2]) +
      sum(current_gal[2] > exp_cols & exp_cols > next_gal[2])
    dist <- (1e6 - 1) * (ext_rows + ext_cols) + sum(abs(next_gal - current_gal))
    total_dist <- total_dist + dist
  }
}
print(total_dist)