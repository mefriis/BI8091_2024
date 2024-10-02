
install.packages("ggplot2")

library(ggplot2)

data(mtcars)

data <- data.frame(x = 1:10, y = 1:10)
data2 <- data.frame(x = 1:10, y = 10:1)

ggplot(data, aes(x = x, y = y)) + geom_point()
ggplot(data2, aes(x = x, y = y)) + geom_point()

# Ser bra ut ditta
