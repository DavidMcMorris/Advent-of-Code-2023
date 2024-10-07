input <- "input.txt"
mirror <-  as.matrix(read.table(input, sep = ""))
mirror <- strsplit(mirror, "")
mirror <- matrix(unlist(mirror), ncol = length(mirror), byrow = TRUE)
mirror_dim <- dim(mirror)

num_rounds <- sum(mirror == "O")
north <- 1 + mirror_dim[1] * seq(0, mirror_dim[2])
south <- north + mirror_dim[1] - 1

for (i in seq_len(mirror_dim[2])) {
  flag <- 0
  j <- 1
  while (flag == 0) {
    if (mirror[j, i] == "." && mirror[j + 1, i] == "O") {
      mirror[j, i] <- "O"
      mirror[j + 1, i] <- "."
      j <- max(j - 1, 1)
    } else {
      j <- j + 1
    }
    if (j == mirror_dim[1]) {
      flag <- 1
    }
  }
}

ans <- sum(rowSums(mirror == "O") * rev(seq(1:mirror_dim[1])))
print(ans)