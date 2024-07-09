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

col_check <- function(pattern) {
  cols <- split(pattern, col(pattern))
  len <- length(cols)
  dups <- duplicated(cols)
  ind <- which(dups == TRUE)[1]
  if (!is.na(ind)) {
    t_vec_1 <- rep(TRUE, len - ind + 1)
    t_vec_2 <- rep(TRUE, ind - 1)
    test_1 <- identical(dups[ind:len], t_vec_1)
    test_2 <- identical(dups[ind:(2 * ind - 1)], t_vec_2)
    if (test_1 + test_2 > 0) {
      return(ind - 1)
    }
  }
}

lin_find <- function(pattern) {
  val <- col_check(pattern)
  if (is.null(val)) {
    val <- 100 * col_check(t(pattern))
  }
  return(val)
}

answer <- sum(unlist(lapply(valley_ls, lin_find)))
print(answer)