input <- readLines("Day1Input.txt")
part <- 2 #Specify 1 or 2
words <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
nums <- c("o1e", "t2o", "t3e", "f4r", "f5e", "s6x", "s7n", "e8t", "n9e")

if (part == 2) {
  for (i in 1:9){
    input <- gsub(words[i], nums[i], input)
  }
}

input <- strsplit(input, "")
input <- lapply(input, as.numeric)

na_remove <- function(l) {
  l <- l[!is.na(l)]
}

cal_val <- function(l) {
  len <- length(l)
  tens <- l[1]
  ones <- l[len]
  val <- 10 * tens + ones
  return(val)
}

input <- lapply(input, na_remove)
input <- lapply(input, cal_val)
answer <- sum(unlist(input))
print(answer)