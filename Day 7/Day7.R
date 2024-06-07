input <- read.table("input.txt", col.names = c("hand", "bid"))
hands <- strsplit(input$hand, split = "")
bids <- input$bid
N <- length(bids)

ranking <- c("2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A")

cardtype <- function(hand) {
  cards <- table(hand)
  numcards <- length(cards)
  if (numcards == 5) {
    type <- 1
  } else if (numcards == 4) {
    type <- 2
  } else if (numcards == 3) {
    if (2 %in% cards) {
      type <- 3
    } else {
      type <- 4
    }
  } else if (numcards == 2) {
    if (2 %in% cards) {
      type <- 5
    } else {
      type <- 6
    }
  } else {
    type <- 7
  }
  return(type)
}

compare <- function(c1, c2) {
  c1type <- cardtype(c1)
  c2type <- cardtype(c2)
  if (c1type > c2type) {
    return(TRUE)
  } else if (c1type < c2type) {
    return(FALSE)
  } else {
    for (i in 1:5) {
      c1rank <- which(ranking == c1[i])
      c2rank <- which(ranking == c2[i])
      if (c1rank > c2rank) {
        return(TRUE)
      } else if (c1rank < c2rank) {
        return(FALSE)
      }
    }
  }
}

for (i in seq_len(N)) {
  for (j in seq_len(N - 1)) {
    if (compare(hands[[j]], hands[[j + 1]])) {
      hands[c(j, j + 1)] <- hands[c(j + 1, j)]
      bids[c(j, j + 1)] <- bids[c(j + 1, j)]
    }
  }
}

winnings <- bids %*% 1:N
print(winnings)