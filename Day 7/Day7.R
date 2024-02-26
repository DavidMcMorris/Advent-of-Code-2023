input <- read.table("sample.txt", col.names = c("hand", "bid"))
hands <- strsplit(input$hand, split = "")
bids <- input$hand

ranking <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "T", "Q", "K", "A")

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
  if (c2type > c1type) {
    return(TRUE)
  } else if (c2type < c1type) {
    return(FALSE)
  } else if (c2type == c2type) {
    for (i in 1:5) {
      c1rank <- which(ranking == c1[i])
      c2rank <- which(ranking == c2[i])
      if (c2rank > c1rank) {
        return(TRUE)
      } else if (c2rank < c1rank) {
        return(FALSE)
      }
    }
  }
}
