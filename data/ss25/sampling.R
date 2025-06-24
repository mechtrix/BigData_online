library(tidyverse)

dataset <- data.frame(
  gender = c("M","M","M","F","M","M","F","F","M","M","M"),
  age = c(30,25,27,27,25,27,27,28,28,24,27)
)

# random sampling

set1 <- dataset |> 
  slice_sample(n = 4)

mean(set1$age)
sd(set1$age)

# stratified sampling

set2 <- dataset |> 
  slice_sample(by = "gender", n = 2)

mean(set2$age)
sd(set2$age)

# systematic sampling

start_p = 3
end_p = 12
k = 3

idx = seq(start_p, end_p, by = 3)


set3 <- dataset |> 
  rowid_to_column() |> 
  filter(rowid %in% c(3,6,9,1))

mean(set3$age)
sd(set3$age)
