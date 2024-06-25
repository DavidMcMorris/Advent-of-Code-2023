input <- "input.txt"
hists <-  as.matrix(read.table(input, sep = " "))
len <- nrow(hists)

next_value <- function(hist_seq) {
  value <- hist_seq[length(hist_seq)]
  if (identical(as.numeric(unique(hist_seq)), 0)) {
    return(value)
  } else {
    hist_seq <- diff(hist_seq)
    return(next_value(hist_seq) + value)
  }
}

total <- 0
for (i in 1:len) {
  total <- total + next_value(hists[i, ])
}

print(total)