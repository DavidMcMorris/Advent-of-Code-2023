inst <- readLines("input.txt", 1)
inst <- strsplit(inst, split = "")[[1]]
inst[which(inst == "L")] <- 1
inst[which(inst == "R")] <- 2

nodes <- 