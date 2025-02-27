library(dplyr)
library(ggplot2)
# Generative aditive model for the iris dataset
iris %>% 
  ggplot(aes(x = Sepal.Length, y = Petal.Length)) +
    geom_point(aes(color = Species)) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
    theme_minimal() +
    labs(title = "Generative aditive model for the iris dataset") +
  facet_wrap(~Species)


iris_1 = iris %>% filter(Species == "setosa")
iris_2 = iris %>% filter(Species == "virginica")
iris_3 = iris %>% filter(Species == "versicolor")
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point(aes(color = Species)) +
  geom_smooth(data = iris_1, aes(x = Sepal.Length, y = Petal.Length), dmethod = "gam") +
  geom_smooth(data = iris_2, aes(x = Sepal.Length, y = Petal.Length), dmethod = "gam") +
  geom_smooth(data = iris_3, aes(x = Sepal.Length, y = Petal.Length), dmethod = "gam") +
  theme_minimal() +
  labs(title = "Generative aditive model for the iris dataset")
