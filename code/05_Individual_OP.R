'INDIVIDUAL OPTIMAL PRICE'

#WTP
WTP_total <- mxl_betai/(-mxl_betai$price)

WTP_cl1 <- cl1/(-cl1$price)
WTP_cl2 <- cl2/(-cl2$price)
WTP_cl3 <- cl3/(-cl3$price)

#Individual optimal pricing
profit_WTP <- matrix(0, length(prices), nrow(X))

list <- matrix(0, 2, nrow(mxl_betai))

list_price <- numeric()
list_profit <- numeric()
  
  for(i in 1:nrow(mxl_betai)) {
  
    for (k in seq_along(prices)) {
    X[brand, "price"] <- prices[k]
    profit_WTP[k,] <- (prices[k]-0.44)*predict.mxl(X, mxl_betai[i,])
    }
    
    profit_WTP_total <- cbind(profit_WTP, prices, rowSums(profit_WTP[,1:5]))
    #list contains optimal prices per customer
    list_price[i] <- profit_WTP_total[which(profit_WTP_total[,brand] == max(profit_WTP_total[,brand])), 6]
    list_profit[i] <- profit_WTP_total[which(profit_WTP_total[,brand] == max(profit_WTP_total[,brand])), 1]
}
list_profit <- round(list_profit*10000)
list_price <- list_price*100
indivOP <- as.data.frame(cbind(list_price, list_profit))

hist(list_profit, breaks = length(unique(list_profit))/8)
hist(list_price, breaks = length(unique(list_price)))

#relationship of profit and price
cor(indivOP)
plot(list_profit, list_price, xlab = "Profit in a market with 100k participants", ylab = "Price", main = "Relationship between Individual Price and Profit")
  #-0.4116434

#individual shares

brand <- 1
prices <- seq(0, 2, 0.01)

head(prices)
X <- MarketSimulation
shares_indiv <- matrix(0, length(prices), nrow(X))
X

  #respondent with 
    #highest price: 164
    #lowest price: 428/466
    #highest profit: 404
    #lowest profit: 104

x <- 104
for (k in seq_along(prices)) {
  X[brand, "price"] <- prices[k]
  shares_indiv[k,] <- predict.mxl(X, mxl_betai[x,])
}

shares_indiv_1 <- cbind(shares_indiv, prices*100)
(indivOP[x,1]-44)*shares_indiv_1[indivOP[x,1]+1,1]*100
shares_indiv_1[indivOP[x,1]+1,1]
y <- which(colnames(indivData) == "jeans_clust_seg")
indivData[x,y[1]]

#profit advantage of targeted pricing vs. uniform pricing

profit_diff <- numeric() #shows much more money the individual targeting rises
for(i in 1:nrow(indivOP)) {
  profit_diff[i] <- list_profit[i]-profit_total[1]
}

mean(profit_diff) #profit advantage in total market
(sum(list_profit)-profit_total[1]*650)/100 #average profit advantage of customer

hist(test$list_price)

profit_WTP_total <- cbind(profit_WTP_total, prices, rowSums(profit_WTP_total[,1:5]))

plot(test$list_price, test$list_profit, type = "l", ylim = c(0, 1000), xaxt="n",
     xlab = "Price", ylab = "Profit", main = "Profit of Mystery Product (Total Market)")
axis(1, at = seq(0, 200, by = 5), las=2)
abline(v=profit_WTP_total[which(profit_WTP_total[,brand] == max(profit_WTP_total[,brand])), 6]*100, col = "grey")'

#determine unique value to set the breaks of the hist
no_of_breaks <- length(unique(list))
#hist with individual optimal prices
hist(list*100, breaks = no_of_breaks, xaxt = "n", yaxt = "n", xlab = "Prices", main = "Optimal Individual Prices (Total Market)")
axis(1, at = seq(0, 200, by = 5), las=2)
axis(2, at = seq(0,50, by = 5))

#compute optimal price per cluster

  #cl1
profit_WTP <- matrix(0, length(prices), nrow(X))

list_cl1 <- numeric()

for(i in 1:nrow(cl1)) {
  
  for (k in seq_along(prices)) {
    X[brand, "price"] <- prices[k]
    profit_WTP[k,] <- (prices[k]-0.44)*predict.mxl(X, cl1[i,])
  }
  
  profit_WTP_cl1 <- cbind(profit_WTP, prices, rowSums(profit_WTP[,1:5]))
  #list contains optimal prices per customer in cl1
  list_cl1[i] <- profit_WTP_cl1[which(profit_WTP_cl1[,brand] == max(profit_WTP_cl1[,brand])), 6]
}

#determine unique value to set the breaks of the hist
no_of_breaks <- length(unique(list_cl1))
#hist with individual optimal prices
hist(list_cl1*100, breaks = no_of_breaks, xaxt = "n", yaxt = "n", xlab = "Prices", main = "Optimal Individual Prices (cl1)")
axis(1, at = seq(0, 200, by = 5), las=2)
axis(2, at = seq(0,50, by = 5))

  #cl2
profit_WTP <- matrix(0, length(prices), nrow(X))

list_cl2 <- numeric()

for(i in 1:nrow(cl2)) {
  
  for (k in seq_along(prices)) {
    X[brand, "price"] <- prices[k]
    profit_WTP[k,] <- (prices[k]-0.44)*predict.mxl(X, cl2[i,])
  }
  
  profit_WTP_cl2 <- cbind(profit_WTP, prices, rowSums(profit_WTP[,1:5]))
  #list contains optimal prices per customer in cl2
  list_cl2[i] <- profit_WTP_cl2[which(profit_WTP_cl2[,brand] == max(profit_WTP_cl2[,brand])), 6]
}

#determine unique value to set the breaks of the hist
no_of_breaks <- length(unique(list_cl2))
#hist with individual optimal prices
hist(list_cl2*100, breaks = no_of_breaks, xaxt = "n", yaxt = "n", xlab = "Prices", main = "Optimal Individual Prices (cl2)")
axis(1, at = seq(0, 200, by = 5), las=2)
axis(2, at = seq(0,50, by = 5))

  #cl3
profit_WTP <- matrix(0, length(prices), nrow(X))

list_cl3 <- numeric()

for(i in 1:nrow(cl3)) {
  
  for (k in seq_along(prices)) {
    X[brand, "price"] <- prices[k]
    profit_WTP[k,] <- (prices[k]-0.44)*predict.mxl(X, cl3[i,])
  }
  
  profit_WTP_cl3 <- cbind(profit_WTP, prices, rowSums(profit_WTP[,1:5]))
  #list contains optimal prices per customer in cl3
  list_cl3[i] <- profit_WTP_cl3[which(profit_WTP_cl3[,brand] == max(profit_WTP_cl3[,brand])), 6]
}

#determine unique value to set the breaks of the hist
no_of_breaks <- length(unique(list_cl3))
#hist with individual optimal prices
hist(list_cl3*100, breaks = no_of_breaks, xaxt = "n", yaxt = "n", xlab = "Prices", main = "Optimal Individual Prices (cl3)")
axis(1, at = seq(0, 200, by = 5), las=2)
axis(2, at = seq(0,50, by = 5))

#plot all together
par(mfrow=c(2,2))
hist(list*100, breaks = no_of_breaks, xaxt = "n", yaxt = "n", xlab = "OP Prices", main = "Total Market IndivOP")
axis(1, at = seq(0, 200, by = 5), las=2)
axis(2, at = seq(0,50, by = 5))
hist(list_cl1*100, breaks = no_of_breaks, xaxt = "n", yaxt = "n", xlab = "OP Prices", main = "Open Minds IndivOP")
axis(1, at = seq(0, 200, by = 5), las=2)
axis(2, at = seq(0,50, by = 5))
hist(list_cl2*100, breaks = no_of_breaks, xaxt = "n", yaxt = "n", xlab = "OP Prices", main = "Fashionistas IndivOP")
axis(1, at = seq(0, 200, by = 5), las=2)
axis(2, at = seq(0,50, by = 5))
hist(list_cl3*100, breaks = no_of_breaks, xaxt = "n", yaxt = "n", xlab = "OP Prices", main = "Originals IndivOP")
axis(1, at = seq(0, 200, by = 5), las=2)
axis(2, at = seq(0,50, by = 5))


