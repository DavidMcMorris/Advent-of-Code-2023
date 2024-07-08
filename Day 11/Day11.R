input <- "sample.txt"
space <- readLines(input)
space <- lapply(space, strsplit, split = "")
space <- matrix(unlist(space), nrow = length(space), byrow = TRUE)