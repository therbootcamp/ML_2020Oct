library(tidyverse)

# read baselers data
baselers <- read_csv("1_Data/baselers.csv")

baselers

view(baselers)

tidyverse::read_csv

# regression

my_lm <- lm(formula = weight ~ age + sex + height,
            data = baselers)

names(my_lm)
summary(my_lm)
predict(my_lm)

names(my_lm)


