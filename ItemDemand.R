library(tidymodels)
library(tidyverse)
library(vroom)
library(timetk)
library(patchwork)

train <- vroom("./train.csv")
test <- vroom("./test.csv")

nStores <- max(train$store)
nItems <- max(train$item)
for(s in 1:nStores) {
  for(i in 1:nItems) {
    storeItemTrain[i,] <- train %>%
      filter(store==s, item==i)
    storeItemTest <- test %>%
      filter(store==s, item==i)
  }
}
storeItem1 <- train[train$item == 10 & train$store == 1,]
storeItem2 <- train[train$item == 20 & train$store == 3,]
storeItem3 <- train[train$item == 30 & train$store == 5,]
storeItem4 <- train[train$item == 40 & train$store == 7,]

p1 <- storeItem1 %>%
  pull(sales) %>%
  forecast::ggAcf(.) +
  ggtitle("Item 10, Store 1")
p2 <- storeItem2 %>%
  pull(sales) %>%
  forecast::ggAcf(.) +
  ggtitle("Item 20, Store 3")
p3 <- storeItem3 %>%
  pull(sales) %>%
  forecast::ggAcf(.) +
  ggtitle("Item 30, Store 5")
p4 <- storeItem4 %>%
  pull(sales) %>%
  forecast::ggAcf(.) +
  ggtitle("Item 40, Store 7")

p <- (p1 | p2) /
  (p3 | p4)
p <- p &
  xlim(0, 32) & 
  ylim(-0.1, 0.8)
p
ggsave('EDAfigures.png', p)
