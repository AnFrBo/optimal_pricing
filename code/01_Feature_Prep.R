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

