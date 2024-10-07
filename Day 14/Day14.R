input <- "sample.txt"
mirror <-  as.matrix(read.table(input, sep = ""))
mirror <- strsplit(mirror, "")
mirror <- matrix(unlist(mirror), ncol = length(mirror), byrow = TRUE)

tilt <- function(mat) {
  mat_dim <- dim(mat)
  for (i in seq_len(mat_dim[2])) {
    flag <- 0
    j <- 1
    while (flag == 0) {
      if (mat[j, i] == "." && mat[j + 1, i] == "O") {
        mat[j, i] <- "O"
        mat[j + 1, i] <- "."
        j <- max(j - 1, 1)
      } else {
        j <- j + 1
      }
      if (j == mat_dim[1]) {
        flag <- 1
      }
    }
  }
  return(mat)
}

spin <- function(mat) {
  mat <- tilt(mat)
  mat <- t(mat[nrow(mat):1, ])
  mat <- tilt(mat)
  mat <- t(mat[, ncol(mat):1])
  mat <- mat[nrow(mat):1, ]
  mat <- tilt(mat)
  mat <- mat[nrow(mat):1, ]
  mat <- t(mat[,ncol(mat):1])
  mat <- tilt(mat)
  mat <- t(mat[nrow(mat):1,]) 
  return(mat)
}

# mirror <- tilt(mirror)
# ans <- sum(rowSums(mirror == "O") * rev(seq(1:mirror_dim[1])))
# print(ans)

k <- 0
for(i in 1:1000000000) {
  mirror <- spin(mirror)
  k <- k + 1
  print(k)
}

ans <- sum(rowSums(mirror == "O") * rev(seq(1:mirror_dim[1])))
print(ans)
