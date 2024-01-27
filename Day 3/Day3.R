input <- readLines("input.txt")


len <- length(strsplit(input[1], split = "")[[1]])
input <- matrix(unlist(strsplit(input, split = "")), ncol = len, byrow = TRUE)
inputpt2 <- input

num_inds <- which(input %in% as.character(0:9))
non_period_inds <- which(input != ".")
sym_inds <- setdiff(non_period_inds, num_inds)

adjacent <- function(ind, dims) {
  a_ind <- arrayInd(ind, dims)
  up <- c(max(a_ind[1] - 1, 1), a_ind[2])
  down <- c(min(a_ind[1] + 1, dims[1]), a_ind[2])
  left <- c(a_ind[1], max(a_ind[2] - 1, 1))
  right <- c(a_ind[1], min(a_ind[2] + 1, dims[2]))
  up_left <- c(up[1], left[2])
  up_right <- c(up[1], right[2])
  down_left <- c(down[1], left[2])
  down_right <- c(down[1], right[2])
  all_adjacent <- list(up, down, left, right, up_left, up_right, down_left, down_right)
  adjacent <- setdiff(all_adjacent, list(as.vector(a_ind)))
  return(adjacent)
}

number_finder <- function(a_ind, input) {
  if (is.na(as.numeric(input[a_ind])) == TRUE) {
    return(0)
  }  else {
    dims <- dim(input)
    lind <- a_ind[2]
    rind <- a_ind[2]
    left_check <- 1
    right_check <- 1
    while (left_check == 1 && lind != 1) {
      lind <- lind - 1
      if (!is.na(as.numeric(input[a_ind[1], lind])) == FALSE) {
        lind <- lind + 1
        left_check <- 0
      }
    }
    while (right_check == 1 && rind != dims[2]) {
      rind <- rind + 1
      if (!is.na(as.numeric(input[a_ind[1], rind])) == FALSE) {
        rind <- rind - 1
        right_check <- 0
      }
    }
    return(c(lind, rind))
  }
}

all_nums <- NULL
for (i in seq_along(sym_inds)) {
  adjacent_inds <- adjacent(sym_inds[i], dim(input))
  for (j in seq_along(adjacent_inds)) {
    current_ind <- adjacent_inds[[j]]
    xrange <- number_finder(matrix(current_ind, ncol = 2), input)
    if (!identical(xrange, 0)) {
      xseq <- xrange[1]:xrange[2]
      num <- as.numeric(paste(input[current_ind[1], xseq], collapse = ""))
      input[current_ind[1], xseq] <- rep(".", length(xseq))
      all_nums <- c(all_nums, num)
    }
  }
}
print(sum(all_nums))

# Part 2
all_nums <- NULL
for (i in seq_along(sym_inds)) {
  if (inputpt2[sym_inds[i]] == "*") {
    local_nums <- NULL
    adjacent_inds <- adjacent(sym_inds[i], dim(inputpt2))
    for (j in seq_along(adjacent_inds)) {
      current_ind <- adjacent_inds[[j]]
      xrange <- number_finder(matrix(current_ind, ncol = 2), inputpt2)
      if (!identical(xrange, 0)) {
        xseq <- xrange[1]:xrange[2]
        num <- as.numeric(paste(inputpt2[current_ind[1], xseq], collapse = ""))
        inputpt2[current_ind[1], xseq] <- rep(".", length(xseq))
        local_nums <- c(local_nums, num)
      }
    }
    if (length(local_nums) == 2) {
      all_nums <- c(all_nums, prod(local_nums))
    }
  }
}
print(sum(all_nums))