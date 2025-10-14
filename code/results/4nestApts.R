
library(jagsUI)
library(dplyr)
library(ggplot2)
library(patchwork)

#load jags data
load("model1data.RData") 

#load jags out
model1output <- readRDS("model1output.rds") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#breeding season survival for more multiple nesting attempts
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#attempts for each bird
brdAtts <- function(x, val = 2) {
  r <- rle(x)
  sum(r$values == val, na.rm=T)}
brdAtts <- apply(jags.data$x, 1, brdAtts)

#row sums of days in state for each period
oneE <- rowSums(jags.data$x[,1:36] == 1, na.rm=T)
oneM <- rowSums(jags.data$x[,37:95] == 1, na.rm=T)
oneL <- rowSums(jags.data$x[,96:184] == 1, na.rm=T)
twoE <- rowSums(jags.data$x[,1:36] == 2, na.rm=T)
twoM <- rowSums(jags.data$x[,37:95] == 2, na.rm=T)
twoL <- rowSums(jags.data$x[,96:184] == 2, na.rm=T)
threeE <- rowSums(jags.data$x[,1:36] == 3, na.rm=T)
threeM <- rowSums(jags.data$x[,37:95] == 3, na.rm=T)
threeL <- rowSums(jags.data$x[,96:184] == 3, na.rm=T)
fourE <- rowSums(jags.data$x[,1:36] == 4, na.rm=T)
fourM <- rowSums(jags.data$x[,37:95] == 4, na.rm=T)
fourL <- rowSums(jags.data$x[,96:184] == 4, na.rm=T)

#generate data frame
attMeans1 <- data.frame(age = jags.data$age, atts = brdAtts,
                        tot1E = oneE, tot1M = oneM, tot1L = oneL,
                        tot2E = twoE, tot2M = twoM, tot2L = twoL,
                        tot3E = threeE, tot3M = threeM, tot3L = threeL,
                        tot4E = fourE, tot4M = fourM, tot4L = fourL)

#save sims for different breeding attempts
ageAttsims <- vector("list", 2)  # 2 ages
for (a in 1:2) {
  ageAttsims[[a]] <- vector("list", 3)  # 3 attempts
}

#save quants
ageAtt <- list()

for (a in 1:2) {
  for (t in 1:3) {
    #compute the number of days spent in each state each period
    attMeans <- attMeans1 %>% filter(age == a, atts == t) %>% summarise(
      tot1E = mean(tot1E), 
      tot1M = mean(tot1M),
      tot1L = mean(tot1L),
      tot2E = mean(tot2E),
      tot2M = mean(tot2M), 
      tot2L = mean(tot2L),
      tot3E = mean(tot3E),
      tot3M = mean(tot3M),
      tot3L = mean(tot3L),
      tot4E = mean(tot4E), 
      tot4M = mean(tot4M), 
      tot4L = mean(tot4L)
    )
    
    out <- plogis(model1output$sims.list$alpha[, 1, 1, 1])^((36-attMeans$tot1E-attMeans$tot2E-attMeans$tot3E-attMeans$tot4E)+attMeans$tot1E) *      #assign any unaccounted for days to the non-breeding state 
      plogis(model1output$sims.list$alpha[, 1, 1, 2])^((59-attMeans$tot1M-attMeans$tot2M-attMeans$tot3M-attMeans$tot4M)+attMeans$tot1M) *      #same here
      plogis(model1output$sims.list$alpha[, 1, 1, 3])^((89-attMeans$tot1L-attMeans$tot2L-attMeans$tot3L-attMeans$tot4L)+attMeans$tot1L) *      #and here
      
      plogis(model1output$sims.list$alpha[, 1, 2, 1])^(attMeans$tot2E) * 
      plogis(model1output$sims.list$alpha[, 1, 2, 2])^(attMeans$tot2M) *
      plogis(model1output$sims.list$alpha[, 1, 2, 3])^(attMeans$tot2L) * 
      
      plogis(model1output$sims.list$alpha[, 1, 3, 1])^(attMeans$tot3E) *  
      plogis(model1output$sims.list$alpha[, 1, 3, 2])^(attMeans$tot3M) * 
      plogis(model1output$sims.list$alpha[, 1, 3, 3])^(attMeans$tot3L) * 
      
      plogis(model1output$sims.list$alpha[, 1, 4, 1])^(attMeans$tot4E) *
      plogis(model1output$sims.list$alpha[, 1, 4, 2])^(attMeans$tot4M) *
      plogis(model1output$sims.list$alpha[, 1, 4, 3])^(attMeans$tot4L)
    
    q <- quantile(out,c(0.025,0.5,0.975))
    
    #store quantiles
    ageAtt[[length(ageAtt) + 1]] <- data.frame(
      Age = a,
      Attempt = t,
      lower = q[1],
      mean  = mean(out),
      upper = q[3]
    )
    
    #store sims
    ageAttsims[[a]][[t]] <- out
    
  }
}

#make data frame
ageAtt <- as.data.frame(do.call(rbind, ageAtt))

#assign names
ageAtt$Age[ageAtt$Age=="1"]<-"Adult"
ageAtt$Age[ageAtt$Age=="2"]<-"Juvenile"

#view
ageAtt
rownames(ageAtt) <- NULL

#add values for 0 nesting attempts
#ageAtt[7,] <- c("Adult", "0", quantile(bsPhiAnb, c(0.025,0.5,0.975)))
#ageAtt[8,] <- c("Juvenile", "0", quantile(bsPhiJnb, c(0.025,0.5,0.975)))

#order rows for table below 
ageAtt <- ageAtt[order(ageAtt$Age, ageAtt$Attempt), ]
ageAtt[, 3:5] <- lapply(ageAtt[, 3:5], as.numeric)

#attempts as a factor
ageAtt$Attempt=factor(ageAtt$Attempt,levels=c("0","1","2","3"))

#view
ageAtt
for (i in 1:6) {
  for (j in 3:5) {
    ageAtt[i,j] <- round(ageAtt[i,j],digits = 3)
  } 
}
ageAtt

p4<-ggplot() +
  geom_point(data = ageAtt, aes(x = Attempt, y = mean, shape = Age),position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(data = ageAtt, aes(x = Attempt, y = mean, ymin = lower, ymax = upper, group = Age),position = position_dodge(width = 0.3), width = 0, size = 0.5, linetype="dashed") +
  theme_classic(base_size = 12) +
  scale_y_continuous(limits = c(0.45,0.9))+
  labs(x = "Number of nesting attempts", y="Breeding season survival probability") +
  theme(legend.position = "right", axis.text = element_text(color="black"))
p4

ggsave(filename = "figure1.png", plot = p4, width = 5, height = 5, units = "in", dpi = 300)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#p-value table comparing breeding season survival for different nesting attempts
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#add sims for non-breeding individuals
ageAttsims[[1]][[4]] <- bsPhiAnb
ageAttsims[[2]][[4]] <- bsPhiJnb

#shift so the new element becomes [[1]] and the others move down
ageAttsims[[1]] <- c(ageAttsims[[1]][4], ageAttsims[[1]][1:3])
ageAttsims[[2]] <- c(ageAttsims[[2]][4], ageAttsims[[2]][1:3])

#choose threshold to test probability based on sims of having a significant difference in survival
threshold = 0.05 

#place estimates list
estimates <- list()
idx <- 1
for (a in 1:2) {
  for (att in 1:4) {
    estimates[[idx]] <- ageAttsims[[a]][[att]] 
    names(estimates)[idx] <- paste0("age", a, "_att", att)
    idx <- idx + 1
  }
}

#get group names
groupNames <- names(estimates)
nGroups <- length(estimates)

#empty matrix to store results
resultsMatrix <- matrix(NA, nrow = nGroups, ncol = nGroups,
                        dimnames = list(groupNames, groupNames))

#loop through all combinations and calculate the desired proportion
for (i in seq_along(estimates)) {
  for (j in seq_along(estimates)) {
    if (i < j) {  
      #ensure each pair is only calculated once
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
table4 <- as.data.frame(print(round(resultsMatrix, 2)))
row.names(table4) <- c("aa0","aa1","aa2","aa3","ja0","ja1","ja2","ja3")
colnames(table4) <- c("aa0","aa1","aa2","aa3","ja0","ja1","ja2","ja3")

#view
table4

write.csv(table4, file = "appendixTable2.csv")


