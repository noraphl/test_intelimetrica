#diamonds

#libraries
library(dplyr)
library(caret)

#import data
diamond_raw <- read.csv("diamonds/diamonds_data.csv")

diamonds <- diamond_raw
diamonds$cut <- factor(diamond_raw$cut)
diamond$color <- as.factor(diamond$color)
diamond$clarity <- as.factor(diamond$clarity)

#summary
diM(diamond)
head(diamond)
summary(diamond)






#correlation
featurePlot(y = diamond$price, x = select(diamond, -price))

plot(log(diamond$price), diamond$carat)

diamond %>%
        ggplot(aes(x = clarity, y = carat)) + 
        geom_point(aes(color = color)) + 
        facet_wrap(~cut) + 
        scale_fill_brewer(palette = "Dark2")

#model
set.seed(12)
train_size <- 0.80
train <- sample(seq(nrow(diamond)), nrow(diamond)*train_size, replace = F)

model <- train(price ~ .,
               data = diamond[train,],
               method = "ranger")
