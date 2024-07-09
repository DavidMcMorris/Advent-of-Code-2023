input <- "input.txt"
valley <- readLines(input)
breaks <- c(which(valley == ""), length(valley) + 1)
valley_ls <- list()

start <- 1
for (i in seq_len(length(breaks))) {
  temp <- valley[start: (breaks[i] - 1)]
  temp <- lapply(temp, strsplit, split = "")
  temp <- matrix(unlist(temp), nrow = length(temp), byrow = TRUE)
  valley_ls[[i]] <- temp
  start <- breaks[i] + 1
}