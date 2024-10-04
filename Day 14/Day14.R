input <- "sample.txt"
mirror <-  as.matrix(read.table(input, sep = ""))
mirror <- strsplit(mirror, "")
mirror <- matrix(unlist(mirror), ncol = length(x), byrow = TRUE)
mirror2 <- mirror

num_rounds <- sum(mirror == "O")
inds <- which(mirror == "O"
)
for (i in 1:num_rounds) {
  ind <- inds[i]
  up <- ind - 1
}