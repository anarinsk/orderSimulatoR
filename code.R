
# Read Cannodale orders data  ---------------------------------------------

library(xlsx) 
customers <- read.xlsx("./data/bikeshops.xlsx", sheetIndex = 1)
products <- read.xlsx("./data/bikes.xlsx", sheetIndex = 1) 
orders <- read.xlsx("./data/orders.xlsx", sheetIndex = 1) 

library(dplyr)
orders.extended <- merge(orders, customers, by.x = "customer.id", by.y="bikeshop.id")
orders.extended <- merge(orders.extended, products, by.x = "product.id", by.y = "bike.id")

orders.extended <- orders.extended %>%
  mutate(price.extended = price * quantity) %>%
  select(order.date, order.id, order.line, bikeshop.name, model,
         quantity, price, price.extended, category1, category2, frame) %>%
  arrange(order.id, order.line)

knitr::kable(sample_n(orders.extended, 10)) # Preview the data


# Group by model & model features, summarize by quantity purchased --------

library(tidyr)

customerTrends <- orders.extended %>%
  group_by(bikeshop.name, model, category1, category2, frame, price) %>%
  summarise(total.qty = sum(quantity)) %>%
  spread(bikeshop.name, total.qty)
customerTrends[is.na(customerTrends)] <- 0  # Remove NA's


# Convert price to binary high/low category -------------------------------

#install.packages("Hmisc")
library(Hmisc)
customerTrends$price <- cut2(customerTrends$price, g=2)


# Convert customer pruchase quantity to percentage of total quantity --------

customerTrends.mat <- as.matrix(customerTrends[,-(1:5)])
customerTrends.mat <- prop.table(customerTrends.mat, margin = 2)

t1 <- customerTrends[, c(1:5)]
t2 <- as.data.frame(customerTrends.mat)
customerTrends <- bind_cols(t1,t2)
knitr::kable(head(customerTrends))


# Running the k-means algorithm  ------------------------------------------

library(cluster) # Needed for silhouette function
kmeansDat <- customerTrends[,-(1:5)]
kmeansDat.t <- t(kmeansDat) 

# Setup for k-means loop 
km.out <- list()
sil.out <- list()
x <- vector()
y <- vector()
minClust <- 4      # Hypothesized minimum number of segments
maxClust <- 8      # Hypothesized maximum number of segments

