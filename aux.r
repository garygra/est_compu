#!/usr/bin/Rscript
library(datasets)
library(tidyverse)
data(iris)

df_p1 <- gather(data = iris, key = "part", value = "measurement", -Species )

df_p1 <- separate(df_p1, "part", c("part", "dimension"))

summary(filter(df_p1, part == "Petal", dimension == "Width", Species == "setosa"))
summary(filter(df_p1, part == "Petal", dimension == "Width", Species == "versicolor"))
summary(filter(df_p1, part == "Petal", dimension == "Width", Species == "virginica"))
# head(df_p1)

class(iris$Species)
typeof(class(iris$Species))
class(iris$part)
class(iris$dimension)
class(iris$measurement)

# add_column(df_p1, z = df_p1$measurement * 10)
# head(add_column(df_p1, z = df_p1$measurement * 10))