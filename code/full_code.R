'This document is structured as follows:
  - general setup
  - data preparation
  - feature engineering
  - cluster analysis
  - shares
  - optimal price and profit
  - individual optimal price'

#################################################################################################

'GENERAL SETUP'

#please set the work directory
setwd("")

#please install/load the following libraries
library(fpc) #for cluster statistics
library(mlr) #for creating dummy variables
library(psych) #for factor analysis
library(cluster) #for silhouette plot
library(RColorBrewer) #for coloring silhouette plot

'DATA PREPARATION'

#read in data
idList <- read.csv("idListJeans.csv")
mxl_betai <- read.csv("mxl_betaiJeans.csv")
indivData <- read.csv("indivDataJeans.csv")
MarketSimulation <- read.csv("marketsimulationJeans.csv")
id <- idList$X576309 # REPLACE WITH YOUR STUDENT ID 
id <- as.data.frame(id)
mxl_betai <- merge(mxl_betai,id, by="id")
indivData <- merge(indivData, id, by="id")
# check the dimensions
nrow(indivData) == 650 # the answer must be “TRUE”
nrow(mxl_betai) == 650 # the answer must be “TRUE”

mxl_betai <- cbind(mxl_betai[,1], mxl_betai[,10:11],mxl_betai[,2:9])
colnames(mxl_betai)[1] <- "id"

rm(idList, id)

'FEATURE ENGINEERING'

#topbrands
for(i in 1:nrow(indivData)){
  indivData$topbrands[i] <- indivData$Q2Aw_1[i]+indivData$Q2Aw_4[i]+indivData$Q2Aw_5[i]+indivData$Q2Aw_9[i]
}

dummy_top <- createDummyFeatures(as.factor(indivData$topbrands), target = character(0L),
                                 cols = NULL)
indivData <- cbind(indivData, dummy_top)

colnames(indivData)[62:66] <- c("AW_Topbrand_None", "AW_Topbrand_1", "AW_Topbrand_2", "AW_Topbrand_3", "AW_Topbrand_All")

#purchase prob of all brands
colnames(indivData)[14:23] <- c("Levi's", "Wrangler", "Lee", "Tommy", "Diesel", "GAP", "American Eagle",
                                "Guess", "Esprit", "S.Oliver")

#age groups
indivData$agegroup <- ifelse(indivData$Q15Demo <= 23, "below Q1", 
                             ifelse(indivData$Q15Demo > 23 & indivData$Q15Demo <= 24, "Q1-Median",
                                    ifelse(indivData$Q15Demo > 24 & indivData$Q15Demo <= 27, "Median-Q3",
                                           ifelse(indivData$Q15Demo >27 & indivData$Q15Demo <= 35, "Age Q3-35", "Elders"))))

#agegroups regarding purchase probability (brands: Wrangler, Tommy, American Eagle, Esprit --> select two)
indivData$agegroup <- as.factor(indivData$agegroup)
indivData$agegroup <- factor(indivData$agegroup,
                             levels = levels(indivData$agegroup)[c(2,5,4,1,3)])

par(mfrow=c(2,2))
list <- list(indivData[,c(15,17,20,22)])
for(i in 1:4){
  boxplot(list[[1]][[i]]~indivData$agegroup,
          data=indivData,
          main="Purchase Probability",
          xlab=paste(colnames(list[[1]][i])),
          ylab="Prob. of Purchase",
          col="grey",
          border="brown")
}

#aggregated cred
for(i in 1:nrow(indivData)){
  indivData$CredL[i] <- sum(mean(indivData$Q4CredL_r1[i]),mean(indivData$Q4CredL_r2[i]),mean(indivData$Q4CredL_r3[i]),mean(indivData$Q4CredL_r4))/4
  indivData$CredG[i] <- sum(mean(indivData$Q5CredG_r1[i]),mean(indivData$Q5CredG_r2[i]),mean(indivData$Q5CredG_r3[i]),mean(indivData$Q5CredG_r4[i]))/4
  indivData$CredT[i] <- sum(mean(indivData$Q6CredT_r1[i]),mean(indivData$Q6CredT_r2[i]),mean(indivData$Q6CredT_r3[i]),mean(indivData$Q6CredT_r4[i]))/4
}

#optimistic scale (factor analysis)
#ML1: future optimist
#ML2: past optimist

a.fa <- fa(indivData[,42:50], fm = "ml", nfactors = 2, 
           rotate = "varimax")
a.fa

indivData$future <- a.fa$scores[,1]
indivData$past <- a.fa$scores[,2]
head(a.fa,10)

#aggregation of regions/europeans-non-europeans/cultures
indivData$regions <- ifelse(indivData$Q16Demo == "Canada" | indivData$Q16Demo == "United States", "North America",
                            ifelse(indivData$Q16Demo == "Argentina" | indivData$Q16Demo == "Brazil" |
                                     indivData$Q16Demo == "Colombia" | indivData$Q16Demo == "Guatemala" |
                                     indivData$Q16Demo == "Trinidad & Tobago" |
                                     indivData$Q16Demo == "Peru" | indivData$Q16Demo == "Venezuela", "South America", 
                                   ifelse(indivData$Q16Demo == "Afghanistan" | indivData$Q16Demo == "Pakistan" |
                                            indivData$Q16Demo == "India" |
                                            indivData$Q16Demo == "Sri Lanka", "South Asia",
                                          ifelse(indivData$Q16Demo == "China" | indivData$Q16Demo == "Taiwan" | indivData$Q16Demo == "Vietnam" |
                                                   indivData$Q16Demo == "Malaysia" | indivData$Q16Demo == "Thailand" | 
                                                   indivData$Q16Demo == "Philippines" |
                                                   indivData$Q16Demo == "Korea South", "South-East Asia",
                                                 ifelse(indivData$Q16Demo == "Russian Federation" | indivData$Q16Demo == "Kazakhstan", 
                                                        "North-Central Asia",
                                                        ifelse(indivData$Q16Demo == "Algeria" | indivData$Q16Demo == "Azerbaijan" | indivData$Q16Demo == "Egypt" | indivData$Q16Demo == "Algeria" |
                                                                 indivData$Q16Demo == "Iran" | indivData$Q16Demo == "Lebanon" | 
                                                                 indivData$Q16Demo == "Turkey" |
                                                                 indivData$Q16Demo == "Morocco" | indivData$Q16Demo == "Syria",
                                                               "Arab States/ Western Asia",
                                                               ifelse(indivData$Q16Demo == "Australia", "Australia",
                                                                      ifelse(indivData$Q16Demo == "Angola" | indivData$Q16Demo == "Ghana" |
                                                                               indivData$Q16Demo == "Nigeria" | indivData$Q16Demo == "Togo" |
                                                                               indivData$Q16Demo == "Zambia", "Africa excl. North",
                                                                             ifelse(indivData$Q16Demo == "Austria" | indivData$Q16Demo == "Belgium" |
                                                                                      indivData$Q16Demo == "Denmark" | indivData$Q16Demo == "France" |
                                                                                      indivData$Q16Demo == "Germany" | indivData$Q16Demo == "Netherlands" |
                                                                                      indivData$Q16Demo == "Sweden" | indivData$Q16Demo == "Switzerland" |
                                                                                      indivData$Q16Demo == "United Kingdom", "North-West Europe", "South-East Europe")))))))))



indivData$europe <- ifelse(indivData$regions == "North-West Europe" | indivData$regions == "South-East Europe", "European", "Non-Europeans")

indivData$cultural <- ifelse(indivData$regions == "North-West Europe" | indivData$regions == "South-East Europe" | indivData$regions == "Australia" | indivData$regions == "North America", 
                             "Western",
                             ifelse(indivData$regions == "Arab States/ Western Asia", 
                                    "Arabic",
                                    ifelse(indivData$regions == "North-Central Asia" | indivData$regions == "South-East Asia" | indivData$regions == "South Asia",
                                           "Asian", "Others")))

  #western
  #arabic
  #asian
  #other (latin, african)

dummy_europe <- createDummyFeatures(indivData$europe, target = character(0L),
                                    cols = NULL)
dummy_cultural <- createDummyFeatures(indivData$cultural, target = character(0L),
                                      cols = NULL)

indivData <- cbind(indivData, dummy_europe, dummy_cultural)

#dummy coding of gender (create ability to interpret percentages of gender in cluster)
indivData$female <- ifelse(indivData$Q14Demo == 1, 1, 0)
indivData$male <- ifelse(indivData$Q14Demo == 2, 1, 0)
indivData$divers <- ifelse(indivData$Q14Demo == 3, 1, 0)

#randomly dummy coded variables, check out if impact; otherwise, delete
dummy_freqpur <- createDummyFeatures(as.factor(indivData$Q1Pur), target = character(0L),
                                     cols = NULL)
colnames(dummy_freqpur) <- c("<=1", "3-4times", "4-6times", ">=12times")

dummy_belief <- createDummyFeatures(as.factor(indivData$Q7Belief), target = character(0L),
                                    cols = NULL)
colnames(dummy_belief) <- c("TisM", "GisM", "LisM")

dummy_income <- createDummyFeatures(as.factor(indivData$Q18Demo), target = character(0L),
                                    cols = NULL)
colnames(dummy_income) <- c("<20k", "20-40k", "40-60", "60-80", "80-100", ">100")

dummy_employment <- createDummyFeatures(as.factor(indivData$Q19Demo), target = character(0L),
                                        cols = NULL)
colnames(dummy_employment) <- c("fulltime", "parttime", "self", "notemployed", "retired")

dummy_edu <- createDummyFeatures(as.factor(indivData$Q20Demo), target = character(0L),
                                 cols = NULL)
colnames(dummy_edu) <- c("<highschool", "highschool", "somedegree", "BA", "MA/PhD")

dummy_agegroups <- createDummyFeatures(indivData$agegroup, target = character(0L),
                                       cols = NULL)

indivData <- cbind(indivData, dummy_freqpur, dummy_belief, dummy_income, dummy_employment, dummy_edu, dummy_agegroups)

#recode variables
indivData$Q22Demo <- ifelse(indivData$Q22Demo == 1, 1, 0)
indivData$Q8Myst <-  ifelse(indivData$Q8Myst == 1, 1, 0)
indivData$Q12Myst <-  ifelse(indivData$Q12Myst == 1, 1, 0)

#delete all variables that are not needed
remove(dummy_agegroups, dummy_belief, dummy_cultural, dummy_edu, dummy_employment, dummy_europe,
       dummy_freqpur, dummy_income, dummy_top, i, list, a.fa)
indivData <- indivData[,-c(1,2,24:36,38,40,42:51,53:58,60,61,67,73:75)]

par(mfrow=c(1,1))

'CLUSTER ANALYSIS'

mxl_betai_cl <- mxl_betai[,-1]
jeans_dist <-dist(apply(mxl_betai_cl,2,scale))
jeans_clust <- hclust(jeans_dist, method ="ward.D2")
plot(jeans_clust)
plot(rev(jeans_clust$height^2)[1:50], type="b")

jeans_clust_seg <- cutree(jeans_clust, k=3)
str(jeans_clust_seg)
table(jeans_clust_seg)

seg.summ <- function (data , groups) 
{aggregate (data , list(groups), function (x) mean(as.numeric (x)))}

seg.summ(mxl_betai_cl,jeans_clust_seg)

cluster.stats(d = dist(apply(mxl_betai_cl,2,scale)), clustering = jeans_clust_seg) #check dunn and clus.avg.silwidths
#sil plot
sil <- silhouette(jeans_clust_seg, jeans_dist)
quartz()
plot(silhouette(jeans_clust_seg, jeans_dist), col= c("red", "blue", "darkgreen"), main = "Silhouette Plot")
#https://www.datanovia.com/en/lessons/cluster-validation-statistics-must-know-methods/#computing-cluster-validation-statistics-in-r
#Silouette
#Observations with a large Si (almost 1) are very well clustered.
#A small Si (around 0) means that the observation lies between two clusters.
#Negative S_i are probably placed in the wrong cluster.

#description of cluster, merge results with individual data set
indivData <- cbind(indivData, jeans_clust_seg)

absolutes <- function (data , groups) {aggregate (data , list(groups), function (x) sum(as.numeric (x)))}
absolutes(indivData,jeans_clust_seg)
seg.summ(indivData,jeans_clust_seg)

#prep clusters for market simulation
mxl_cl <- cbind(mxl_betai_cl, jeans_clust_seg)

cl1 <- mxl_cl[which(mxl_cl$jeans_clust_seg == 1),]
cl1 <- cl1[,-c(11)]
cl2 <- mxl_cl[which(mxl_cl$jeans_clust_seg == 2),]
cl2 <- cl2[,-c(11)]
cl3 <- mxl_cl[which(mxl_cl$jeans_clust_seg == 3),]
cl3 <- cl3[,-c(11)]

rm(jeans_dist, jeans_clust, seg.summ, absolutes)

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
mean(profit_diff)/100

profit_WTP_total <- cbind(profit_WTP_total, prices, rowSums(profit_WTP_total[,1:5]))

#determine unique value to set the breaks of the hist
no_of_breaks <- length(unique(list))
#hist with individual optimal prices
hist(list_price, breaks = no_of_breaks, xaxt = "n", yaxt = "n", xlab = "Prices", main = "Optimal Individual Prices (Total Market)")
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

