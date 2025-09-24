
library(jagsUI)
library(dplyr)
library(ggplot2)
library(patchwork)

#load jags data
load("C:/Users/dylan.bakner/Documents/manuscripts/turkeySurvival/secondRoundRevisions/revisedAnalyses/analysis9.24.25/data/model1data.RData") 

#load jags out
model1output <- readRDS("C:/Users/dylan.bakner/Documents/manuscripts/turkeySurvival/secondRoundRevisions/revisedAnalyses/analysis9.24.25/out/model1output.rds") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#breeding season survival for adults and juvs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#states
states <- c(1, 2, 3, 4)

#define age groups (row indices)
ageGroups <- list(adult = which(jags.data$age == 1), juvenile = which(jags.data$age == 2))

#define period groups (column indices)
periodGroups <- list(
  early = which(jags.data$period == 1),
  mid   = which(jags.data$period == 2),
  late  = which(jags.data$period == 3)
)

#storage
stateAgePeriodMu <- list()

#loop through states, age, and period
for(i in states){
  for(a in names(ageGroups)){
    for(p in names(periodGroups)){
      rows <- ageGroups[[a]]
      cols <- periodGroups[[p]]
      #filter out rows that are all 1s (individuals that never initiated breeding)
      validRows <- rows[rowSums(jags.data$x[rows, , drop = FALSE] != 1) > 0]
      if(length(validRows) > 0){
        tmp <- rowSums(jags.data$x[validRows, cols, drop = FALSE] == i)
        stateAgePeriodMu[[paste0("state", i, "_", a, "_", p)]] <- mean(tmp, na.rm = TRUE)
      } else {
        stateAgePeriodMu[[paste0("state", i, "_", a, "_", p)]] <- NA
      }
    }
  }
}
stateAgePeriodMu
s <- stateAgePeriodMu

#now used these means to produce annual survival estimates
#early-reproduction (days 1-36 (36 exposure days))
#mid-reproduction (days 37-95 (59 exposure days))
#late-reproduction (days 96-184)
#[,age,state,period]

#adult breeding season survival
bsPhiA <- plogis(model1output$sims.list$alpha[, 1, 1, 1])^((36-s$state1_adult_early-s$state2_adult_early-s$state3_adult_early-s$state4_adult_early)+s$state1_adult_early) *      #assign any unaccounted for days to the non-breeding state 
          plogis(model1output$sims.list$alpha[, 1, 1, 2])^((59-s$state1_adult_mid-s$state2_adult_mid-s$state3_adult_mid-s$state4_adult_mid)+s$state1_adult_mid) *  #same here
          plogis(model1output$sims.list$alpha[, 1, 1, 3])^((89-s$state1_adult_late-s$state2_adult_late-s$state3_adult_late-s$state4_adult_late)+s$state1_adult_late) *    #and here
  
          plogis(model1output$sims.list$alpha[, 1, 2, 1])^s$state2_adult_early * 
          plogis(model1output$sims.list$alpha[, 1, 2, 2])^s$state2_adult_mid * 
          plogis(model1output$sims.list$alpha[, 1, 2, 3])^s$state2_adult_late * 
  
          plogis(model1output$sims.list$alpha[, 1, 3, 1])^s$state3_adult_early *  
          plogis(model1output$sims.list$alpha[, 1, 3, 2])^s$state3_adult_mid * 
          plogis(model1output$sims.list$alpha[, 1, 3, 3])^s$state3_adult_late * 
  
          plogis(model1output$sims.list$alpha[, 1, 4, 1])^s$state4_adult_early * 
          plogis(model1output$sims.list$alpha[, 1, 4, 2])^s$state4_adult_mid * 
          plogis(model1output$sims.list$alpha[, 1, 4, 3])^s$state4_adult_late

round(quantile(bsPhiA, c(0.025,0.5,0.975)),2)
round(mean(bsPhiA),2)

#juv breeding season survival
bsPhiJ <- plogis(model1output$sims.list$alpha[, 1, 1, 1])^((36-s$state1_juvenile_early-s$state2_juvenile_early-s$state3_juvenile_early-s$state4_juvenile_early)+s$state1_juvenile_early) *      #assign any unaccounted for days to the non-breeding state 
  plogis(model1output$sims.list$alpha[, 2, 1, 2])^((59-s$state1_juvenile_mid-s$state2_juvenile_mid-s$state3_juvenile_mid-s$state4_juvenile_mid)+s$state1_juvenile_mid) *  #same here
  plogis(model1output$sims.list$alpha[, 2, 1, 3])^((89-s$state1_juvenile_late-s$state2_juvenile_late-s$state3_juvenile_late-s$state4_juvenile_late)+s$state1_juvenile_late) *    #and here
  
  plogis(model1output$sims.list$alpha[, 2, 2, 1])^s$state2_juvenile_early * 
  plogis(model1output$sims.list$alpha[, 2, 2, 2])^s$state2_juvenile_mid * 
  plogis(model1output$sims.list$alpha[, 2, 2, 3])^s$state2_juvenile_late * 
  
  plogis(model1output$sims.list$alpha[, 2, 3, 1])^s$state3_juvenile_early *  
  plogis(model1output$sims.list$alpha[, 2, 3, 2])^s$state3_juvenile_mid * 
  plogis(model1output$sims.list$alpha[, 2, 3, 3])^s$state3_juvenile_late * 
  
  plogis(model1output$sims.list$alpha[, 2, 4, 1])^s$state4_juvenile_early * 
  plogis(model1output$sims.list$alpha[, 2, 4, 2])^s$state4_juvenile_mid * 
  plogis(model1output$sims.list$alpha[, 2, 4, 3])^s$state4_juvenile_late

round(quantile(bsPhiJ, c(0.025,0.5,0.975)),2)
round(mean(bsPhiJ),2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#breeding season survival for non-breeding adults and juvs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#non-breeding season survival for adults
bsPhiAnb <- plogis(model1output$sims.list$alpha[,1,1,1])^36 * plogis(model1output$sims.list$alpha[,1,1,2])^59 *  plogis(model1output$sims.list$alpha[,1,1,3])^89
round(quantile(bsPhiAnb, c(0.025,0.5,0.975)),2)
round(mean(bsPhiAnb),2)

#non-breeding season survival for juveniles
bsPhiJnb <- plogis(model1output$sims.list$alpha[,2,1,1])^36 * plogis(model1output$sims.list$alpha[,2,1,2])^59 *  plogis(model1output$sims.list$alpha[,2,1,3])^89
round(quantile(bsPhiJnb, c(0.025,0.5,0.975)),2)
round(mean(bsPhiJnb),2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#p-value table comparing breeding season survival for age-speciifc breeders and non-breeding individuals
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#extract posterior samples for each age, state, and period
ba = bsPhiA     #adult breeder
bj = bsPhiJ     #juvenile breeder
nba = bsPhiAnb  #adult non-breeder
nbj = bsPhiJnb  #juvenile non-breeder

#choose threshold to test probability based on sims of having a significant difference in survival
threshold = 0.05 

#put estimates list
estimates <- list(ba = ba, bj = bj, nba = nba, nbj = nbj)

#group names
groupNames <- names(estimates)
nGroups <- length(estimates)

#empty matrix to store results
resultsMatrix <- matrix(NA, nrow = nGroups, ncol = nGroups,
                        dimnames = list(groupNames, groupNames))

#loop through all combinations and calculate the desired proportion
for (i in seq_along(estimates)) {
  for (j in seq_along(estimates)) {
    #ensure each pair is only calculated once
    if (i < j) {  
      if (mean(estimates[[i]]) > mean(estimates[[j]])) {
        #calculate when i > j
        resultsMatrix[i, j] <- mean((estimates[[i]] - estimates[[j]]) >= threshold)
      } else {
        #calculate when j > i
        resultsMatrix[i, j] <- mean((estimates[[j]] - estimates[[i]]) >= threshold)
      }
    } else {
      #skip self-comparisons
      resultsMatrix[i, j] <- NA  
    }
  }
}

#store as data frame and clean up
table1 <- as.data.frame(print(round(resultsMatrix, 2)))
row.names(table1) <- c("Breeding Adult", "Breeding juvenile", "Non-breeding Adult", "Non-breeding juvenile")
colnames(table1) <- c("Breeding Adult", "Breeding juvenile", "Non-breeding Adult", "Non-breeding juvenile")
table1

setwd("C:/Users/dylan.bakner/Documents/manuscripts/turkeySurvival/secondRoundRevisions/revisedAnalyses/analysis9.24.25/out")
write.csv(table1, file = "appendixTable1.csv")


