'SHARES'

#all products of given market simulation

  #total market
MarketSimulation$price <- MarketSimulation$price/100
MarketSimulation

mxl_betai <- mxl_betai[,-1]
head(mxl_betai)
head(MarketSimulation)

predict.mxl <- function(X, theta) {
  eu <- exp(as.matrix(X) %*% t(theta))
  p <- t(eu) / colSums(eu)
  #  return(p)
  return(colMeans(p))
}

tmp<-predict.mxl(MarketSimulation, mxl_betai)
tmp

  #per cluster
mxl_cl <- cbind(mxl_betai, jeans_clust_seg)

cl1 <- mxl_cl[which(mxl_cl$jeans_clust_seg == 1),]
cl1 <- cl1[,-11]
cl2 <- mxl_cl[which(mxl_cl$jeans_clust_seg == 2),]
cl2 <- cl2[,-11]
cl3 <- mxl_cl[which(mxl_cl$jeans_clust_seg == 3),]
cl3 <- cl3[,-11]

tmp_cl1 <-predict.mxl(MarketSimulation, cl1)
tmp_cl1

tmp_cl2 <-predict.mxl(MarketSimulation, cl2)
tmp_cl2

tmp_cl3 <-predict.mxl(MarketSimulation, cl3)
tmp_cl3

  #total share function

brand <- 1
prices <- seq(0, 2, 0.01)

head(prices)
X <- MarketSimulation
share <- matrix(0, length(prices), nrow(X))
X

for (k in seq_along(prices)) {
  X[brand, "price"] <- prices[k]
  share[k,] <- predict.mxl(X, mxl_betai)
}

prices_ms <- prices*100
share_ms <- share*100

a <- c("Mystery", "Hilfiger", "Guess", "Levi's", "None")
row.names(MarketSimulation) <- a
matplot(prices_ms, share_ms, type = "l", lty = 1, yaxt = "n", xaxt= "n", xlab = "Price", ylab = "Share in %", 
        main = "Market Share of All Products", cex.main=1.5, cex.lab=1.5, ylim = c(0,100))
legend("top", rownames(MarketSimulation), col = 1:5, lty = 1, cex=0.595, horiz = T)

#
axis(1, at = seq(0, 200, by = 10), las=2, cex.axis=1.2)
axis(2, at = seq(0, 100, by = 10), cex.axis=1.2)

head(share_ms,5)
sharenew <- cbind(share_ms[,1], prices_ms)
round(apply(share_ms,2,mean),2)

  #share of cluster 1 (needed for prove for interpretation of optimal price)
brand <- 1
prices <- seq(0, 2, 0.01)

head(prices)
X <- MarketSimulation
share_cl1 <- matrix(0, length(prices), nrow(X))
X

for (k in seq_along(prices)) {
  X[brand, "price"] <- prices[k]
  share_cl1[k,] <- predict.mxl(X, cl1)
}

prices_ms <- prices*100
share_ms <- share_cl1*100

plot(prices_ms, share_ms[, brand], type = "l", yaxt = "n", xaxt= "n", xlab = "Price", ylab = "Share in %", 
     main = "Market Share of Mystery Product", cex.main=1.5, cex.lab=1.5)
axis(1, at = seq(0, 200, by = 10), las=2, cex.axis=1.2)
axis(2, at = seq(0, 100, by = 10), cex.axis=1.2)

head(share_ms,5)
sharenew_cl1 <- cbind(share_ms[,c(1)], prices_ms)
round(apply(share_ms,2,mean),2)


  #share of cluster 2 (needed for prove for interpretation of optimal price)
brand <- 1
prices <- seq(0, 2, 0.01)

head(prices)
X <- MarketSimulation
share_cl2 <- matrix(0, length(prices), nrow(X))
X

for (k in seq_along(prices)) {
  X[brand, "price"] <- prices[k]
  share_cl2[k,] <- predict.mxl(X, cl2)
}

prices_ms <- prices*100
share_ms <- share_cl2*100

plot(prices_ms, share_ms[, brand], type = "l", yaxt = "n", xaxt= "n", xlab = "Price", ylab = "Share in %", 
     main = "Market Share of Mystery Product", cex.main=1.5, cex.lab=1.5)
axis(1, at = seq(0, 200, by = 10), las=2, cex.axis=1.2)
axis(2, at = seq(0, 100, by = 10), cex.axis=1.2)

head(share_ms,5)
sharenew_cl2 <- cbind(share_ms[,c(1,3)], prices_ms)
round(apply(share_ms,2,mean),2)

