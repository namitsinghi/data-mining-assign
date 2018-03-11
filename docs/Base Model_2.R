library(ggplot2)
library(dplyr)
library(arules)
library(plyr)

setwd("G:\\DA Assignment")
sample_data <- read.csv("sample_data.csv")
products <- read.csv("products.csv")


df_sorted <- sample_data[order(sample_data$user_id),]
df_sorted$user_id <- as.numeric(df_sorted$user_id)


sample_data <- sample_data %>% left_join(products)

df_itemList <- ddply(sample_data,c("user_id","order_id"), 
                     function(df1)paste(df1$product_name, 
                                        collapse = ","))
df_itemList$user_id <- NULL
df_itemList$order_id <- NULL

colnames(df_itemList) <- c("itemList")

write.csv(df_itemList,"itemList.csv", row.names = TRUE)
itemlist <- read.csv("itemlist.csv")

txn = read.transactions(file="itemList.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1)

txn@itemInfo$labels <- gsub("\"","",txn@itemInfo$labels)

basket_rules <- apriori(txn,parameter = list(sup = 0.01, conf = 0.5,target="rules"));

inspect(basket_rules)

basket_rules <- apriori(txn,parameter = list(sup = 0.001, conf = 0.75,target="rules"));

inspect(basket_rules)

library(arulesViz)
plot(basket_rules)
plot(basket_rules, method = "grouped", control = list(k = 5))
plot(basket_rules, method="graph", control=list(type="items"))
plot(basket_rules, method="paracoord",  control=list(alpha=.5, reorder=TRUE))
plot(basket_rules,measure=c("support","lift"),shading="confidence",interactive=T)
