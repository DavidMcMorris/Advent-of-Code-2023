input <- read.table("input.txt", col.names = c("hand", "bid"))
hands <- strsplit(input$hand, split = "")
bids <- input$bid
hands_og <- hands
n <- length(bids)

ranking <- c("J", "2", "3", "4", "5", "6", "7", "8", "9", "T", "Q", "K", "A")

cardtype <- function(hand) {
  cards <- table(hand)
  numcards <- length(cards)
  if ("J" %in% hand) {
    if (numcards == 5) {
      type <- 2
    } else if (numcards == 4) {
      type <- 4
    } else if (numcards == 3) {
      if (cards[["J"]] == 1 &&  !(3 %in% cards)) {
        type <- 5
      } else {
        type <- 6
      }
    } else {
      type <- 7
    }
  }
  if (!("J" %in% hand)) {
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
  }
  return(type)
}

compare <- function(c1, c2) {
  c1type <- cardtype(c1)
  c2type <- cardtype(c2)
  if (c1type > c2type) {
    return(FALSE)
  } else if (c1type < c2type) {
    return(TRUE)
  } else {
    for (i in 1:5) {
      c1rank <- which(ranking == c1[i])
      c2rank <- which(ranking == c2[i])
      if (c1rank > c2rank) {
        return(FALSE)
      } else if (c1rank < c2rank) {
        return(TRUE)
      }
    }
  }
}

# Merge sort functions

merge_lists <- function(left, right) {
  result <- list()
  while (length(left) * length(right) > 0) {
    if (compare(left[[1]], right[[1]])) {
      result <- c(result, left[1])
      left <- left[!(left %in% result)]
    } else {
      result <- c(result, right[1])
      right <- right[!(right %in% result)]
    }
  }
  if (length(left) > 0) {
    result <- c(result, left)
  }
  if (length(right) > 0) {
    result <- c(result, right)
  }
  return(result)
}

merge_sort <- function(m) {
  len <- length(m)
  if (length(m) <= 1) {
    return(m)
  } else {
    split_ind <- floor(len / 2)
    left <- m[1:split_ind]
    right <- m[(split_ind + 1) : len]

    left <- merge_sort(left)
    right <- merge_sort(right)

    return(merge_lists(left, right))
  }
}


hands <- merge_sort(hands)
bids <- bids[match(hands_og, hands)]
winnings <- bids %*% 1:n
print(winnings)