library(tidyverse)

input <- data.frame(read_table("day02_input.txt", col_names = c("opp","me")))

#part 1 coding
#paper B 2
#Scissors C 3
#rock A 1

score <- input |>
  mutate(my_value = recode(me, "Y" = 2, "X" = 1, "Z" =  3 )) |>
  mutate(result = if_else(my_value == 2 & opp == "A", 6, 
                          if_else(my_value == 3 & opp == "B", 6,
                          if_else(my_value == 1 & opp == "C", 6,
                          if_else(my_value == 2 & opp == "B", 3,
                          if_else(my_value == 3 & opp == "C", 3,
                          if_else(my_value == 1 & opp == "A", 3, 0))))))) |>
  mutate(score = my_value + result)

#part 2 coding
#draw Y 3
#win Z 6
#loss X 0

score <- score |> 
  mutate(right_score = if_else(me == "Y" & opp == "A", 1 + 3,
                          if_else(me == "Y" & opp == "B", 2 + 3,
                          if_else(me == "Y" & opp == "C", 3 + 3,
                          if_else(me == "Z" & opp == "A", 2 + 6, 
                          if_else(me == "Z" & opp == "B", 3 + 6,
                          if_else(me == "Z" & opp == "C", 1 + 6,
                          if_else(me == "X" & opp == "A", 3 + 0, 
                          if_else(me == "X" & opp == "B", 1 + 0,
                          if_else(me == "X" & opp == "C", 2 + 0,0))))))))))
  
# Part 1 
out1 <- sum(score$score)
  
cat("Puzzle 1 solution:", out1, fill = TRUE)

# Part 2 
out2 <- sum(score$right_score)
  
cat("Puzzle 2 solution:", out2, fill = TRUE)



