library(tidyverse)

input <- read_lines("2022/day06_input.txt") |>
  str_split("") |>
  unlist()

#mjqjpqmgbljsphdztnvjfqwrcgsmlb: first marker after character 7
#bvwbjplbgvbhsrlpgdmjqwftvncz: first marker after character 5
#nppdvjthqldpwncqszvftbrmjlhg: first marker after character 6
#nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg: first marker after character 10
#zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw: first marker after character 11

#part 1
i <- x <- 5 

while(length(x) < 4){
  x <- unique(input[(i-4):(i-1)])
  length(x)
  print(i)
  i = i + 1
}

#part 2
i <- x <- 15 

while(length(x) < 14){
  x <- unique(input[(i-14):(i-1)])
  length(x)
  print(i)
  i = i + 1
}
