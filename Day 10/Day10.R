input <- "input.txt"
pipes <- readLines(input)
pipes <- lapply(pipes, strsplit, split = "")
pipes <- matrix(unlist(pipes), nrow = length(pipes), byrow = TRUE)