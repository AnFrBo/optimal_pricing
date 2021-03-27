'OPTIMAL PRICING/ MAXIMIZED PROFIT'

  #prep
brand <- 1
prices <- seq(0, 2, 0.01)

head(prices)
X <- MarketSimulation

  #total market
profit_total <- matrix(0, length(prices), nrow(X))
for (k in seq_along(prices)) {
  X[brand, "price"] <- prices[k]
  profit_total[k,] <- (prices[k]-0.44)*predict.mxl(X, mxl_betai)
}

prices_profit <- prices*100
#2x 100 für share (market size) und profit
profit_total <- cbind(profit_total, prices, rowSums(profit_total[,1:5]))
plot(prices_profit, profit_total[, brand]*10000, type = "l", ylim = c(0, 1000), xaxt='n',
     xlab = "Price", ylab = "Profit (in k)", main = "Profit of Mystery Product (Total Market)")
axis(1, at = seq(0, 200, by = 5), las=2)
abline(v=profit_total[which(profit_total[,brand] == max(profit_total[,brand])), 6]*100, col = "grey")

  #cl1
X <- MarketSimulation
profit_cl1 <- matrix(0, length(prices), nrow(X))
for (k in seq_along(prices)) {
  X[brand, "price"] <- prices[k]
  profit_cl1[k,] <- (prices[k]-0.44)*predict.mxl(X, cl1)
}

profit_cl1 <- cbind(profit_cl1, prices, rowSums(profit_cl1[,1:5]))

plot(prices_profit, profit_cl1[, brand]*10000, type = "l", ylim = c(0, 1000), xaxt='n',
     xlab = "Price", ylab = "Profit", main = "Profit of Mystery Product (Total Market)")
axis(1, at = seq(0, 200, by = 5), las=2)
abline(v=profit_cl1[which(profit_cl1[,brand] == max(profit_cl1[,brand])), 6]*100, col = "grey")

  #cl2
profit_cl2 <- matrix(0, length(prices), nrow(X))
for (k in seq_along(prices)) {
  X[brand, "price"] <- prices[k]
  profit_cl2[k,] <- (prices[k]-0.44)*predict.mxl(X, cl2)
}

profit_cl2 <- cbind(profit_cl2, prices, rowSums(profit_cl2[,1:5]))

plot(prices_profit, profit_cl2[, brand]*10000, type = "l", ylim = c(0, 1000), xaxt='n',
     xlab = "Price", ylab = "Profit", main = "Profit of Mystery Product (Total Market)")
axis(1, at = seq(0, 200, by = 5), las=2)
abline(v=profit_cl2[which(profit_cl2[,brand] == max(profit_cl2[,brand])), 6]*100, col = "grey")

  #cl3
profit_cl3 <- matrix(0, length(prices), nrow(X))
for (k in seq_along(prices)) {
  X[brand, "price"] <- prices[k]
  profit_cl3[k,] <- (prices[k]-0.44)*predict.mxl(X, cl3)
}

profit_cl3 <- cbind(profit_cl3, prices, rowSums(profit_cl3[,1:5]))

plot(prices_profit, profit_cl3[, brand]*10000, type = "l", ylim = c(0, 1000), xaxt='n',
     xlab = "Price", ylab = "Profit", main = "Profit of Mystery Product (Total Market)")
axis(1, at = seq(0, 200, by = 5), las=2)
abline(v=profit_cl3[which(profit_cl3[,brand] == max(profit_cl3[,brand])), 6]*100, col = "grey")

  #results
profit_total[,1] <- profit_total[,1]*10000
profit_total[,6] <- profit_total[,6]*100
profit_total <- round(profit_total[which(profit_total[,brand] == max(profit_total[,brand])), c(1,6)],2)
profit_cl1[,1] <- profit_cl1[,1]*10000
profit_cl1[,6] <- profit_cl1[,6]*100
round(profit_cl1[which(profit_cl1[,brand] == max(profit_cl1[,brand])), c(1,6)],2)
profit_cl2[,1] <- profit_cl2[,1]*10000
profit_cl2[,6] <- profit_cl2[,6]*100
round(profit_cl2[which(profit_cl2[,brand] == max(profit_cl2[,brand])), c(1,6)],2)
profit_cl3[,1] <- profit_cl3[,1]*10000
profit_cl3[,6] <- profit_cl3[,6]*100
round(profit_cl3[which(profit_cl3[,brand] == max(profit_cl3[,brand])), c(1,6)],2)

rm(k, prices_ms, sharenew, share_ms, share)


