rMax <- 12
gMax <- 13
bMax <- 14

input <- readLines("Day2Input.txt")
input <- strsplit(gsub('[[:punct:] ]+', ' ', input), " ")
r <- lapply(lapply(input, grep, pattern = "red"), function(x){x-1})
b <- lapply(lapply(input, grep, pattern = "blue"), function(x){x-1}) 
g <- lapply(lapply(input, grep, pattern = "green"), function(x){x-1})

r <- lapply(Map('[', input, r), as.numeric)
b <- lapply(Map('[', input, b), as.numeric)
g <- lapply(Map('[', input, g), as.numeric)

rCheck <- which(lapply(lapply(r,function(x){x > rMax}),sum) > 0)
gCheck <- which(lapply(lapply(g,function(x){x > gMax}),sum) > 0)
bCheck <- which(lapply(lapply(b,function(x){x > bMax}),sum) > 0)

rNeeded <- unlist(lapply(r,max))
bNeeded <- unlist(lapply(b,max))
gNeeded <- unlist(lapply(g,max))


val <- sum(setdiff(1:length(input), c(rCheck, gCheck, bCheck)))
print(val)
print(sum(rNeeded * bNeeded * gNeeded))
