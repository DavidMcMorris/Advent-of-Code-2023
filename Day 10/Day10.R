input <- "input.txt"
pipes <- readLines(input)
pipes <- lapply(pipes, strsplit, split = "")
pipes <- matrix(unlist(pipes), nrow = length(pipes), byrow = TRUE)

s <- which(pipes == "S")
pipes[s] <- "|"

adjacent <- function(char, current_ai) {
  if (char == "|") {
    d1 <- c(1, 0)
    d2 <- c(-1, 0)
  } else if (char == "-") {
    d1 <- c(0, 1)
    d2 <- c(0, -1)
  } else if (char == "L") {
    d1 <- c(-1, 0)
    d2 <- c(0, 1)
  } else if (char == "J") {
    d1 <- c(-1, 0)
    d2 <- c(0, -1)
  } else if (char == "7") {
    d1 <- c(1, 0)
    d2 <- c(0, -1)
  } else if (char == "F") {
    d1 <- c(1, 0)
    d2 <- c(0, 1)
  } else {
    d1 <- c(0, 0)
    d2 <- c(0, 0)
  }
  adjacent_inds <- data.frame(rbind(current_ai + d1,
                                    current_ai + d2))
  return(adjacent_inds)
}

dims <- dim(pipes)
distance <- matrix(nrow = dims[1], ncol = dims[2], Inf)
distance[s] <- 0
unvisited <- matrix(nrow = dims[1], ncol = dims[2], 1)

current <- s
current_ai <- arrayInd(current, dims)
adj_inds <- adjacent(pipes[current], current_ai)
adj_inds <- subset(adj_inds, X1 > 0 & X1 <= dims[1] & X2 > 0 & X2 <= dims[2])
connected_verts <- (adj_inds[, 2] - 1) * (dims[1]) + adj_inds[, 1]

while (min(distance[which(unvisited == 1)]) < Inf) {
  current_ai <- arrayInd(current, dims)
  adj_inds <- adjacent(pipes[current], current_ai)
  adj_inds <- subset(adj_inds, X1 > 0 & X1 <= dims[1] & X2 > 0 & X2 <= dims[2])
  connected_verts <- (adj_inds[, 2] - 1) * (dims[1]) + adj_inds[, 1]
  for (i in seq_along(connected_verts)) {
    j <- connected_verts[i]
    distance[j] <- min(distance[j], distance[current] + 1)
  }
  unvisited[current] <- 0
  current <- which(distance == min(distance[which(unvisited == 1)])
                   & unvisited == 1)[1]
}

print(max(distance[which(distance < Inf)]))

inside <- function(x, y) {
  sums <- NULL
  for (i in seq_along(x)) {
    if (y[i] >= Inf) {
      sums[i] <- NaN
    } else {
      a <- sum(x[which(y[1:i] < Inf)] == "|") 
      b <- sum(x[which(y[1:i] < Inf)] == "7")
      c <- sum(x[which(y[1:i] < Inf)] == "F")
      d <- sum(x[which(y[1:i] < Inf)] == "L")
      e <- sum(x[which(y[1:i] < Inf)] == "J")
      match <- min(b + c, d + e)
      unmatch <- max(b + c, d + e) - match
      s <- a + match - unmatch
      s <- abs(s) %% 2
      sums[i] <- s
    }
  }
  rbind(sums, x)

  value <- 0
  total <- 0
  for (i in 1:(length(x) - 1)) {
    if (!is.na(sums[i + 1])) {
      value <- sums[i + 1]
    } else {
      total <- total + value
    }
  }
  return(total)
}

tot <- mapply(inside, split(pipes, row(pipes)), split(distance, row(distance)))
print(sum(tot))
