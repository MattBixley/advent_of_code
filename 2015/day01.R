#part 1
input <- readLines("2015/input01.txt")
chars <- strsplit(input, "")[[1]]

key <- c("(" = 1, ")" = -1)

sum(key[chars])

#part 2
input <- readLines("2015/input01.txt")
chars <- strsplit(input, "")[[1]]

key <- c("(" = 1, ")" = -1)

min(which(cumsum(key[chars]) < 0))
