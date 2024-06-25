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

total_next <- 0
for (i in 1:len) {
  total_next <- total_next + next_value(hists[i, ])
}

prev_value <- function(hist_seq) {
  value <- hist_seq[1]
  if (identical(as.numeric(unique(hist_seq)), 0)) {
    return(value)
  } else {
    hist_seq <- diff(hist_seq)
    return(value - prev_value(hist_seq))
  }
}

total_prev <- 0
for (i in 1:len) {
  total_prev <- total_prev + prev_value(hists[i, ])
}

print(c(total_next, total_prev))