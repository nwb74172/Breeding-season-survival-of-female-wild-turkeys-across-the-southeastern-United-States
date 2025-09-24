
library(jagsUI)
library(dplyr)
library(ggplot2)
library(patchwork)

#load jags data
load("C:/Users/dylan.bakner/Documents/manuscripts/turkeySurvival/secondRoundRevisions/revisedAnalyses/analysis9.24.25/data/model1data.RData") 

#load jags out
model1output <- readRDS("C:/Users/dylan.bakner/Documents/manuscripts/turkeySurvival/secondRoundRevisions/revisedAnalyses/analysis9.24.25/out/model1output.rds") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#dsp table for all age-state-period combos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#save mean dsp
phis <- numeric(24)  
idx <- 1
for (p in 1:3) {
  for (a in 1:2) {
    for (s in 1:4) {
      phis[idx] <- plogis(model1output$mean$alpha[,,p][a, s])
      idx <- idx + 1
    }
  }
}

#save dsp 0.025%
phisL <- numeric(24)  
idxL <- 1
for (p in 1:3) {
  for (a in 1:2) {
    for (s in 1:4) {
      phisL[idxL] <- plogis(model1output$q2.5$alpha[,,p][a, s])
      idxL <- idxL + 1
    }
  }
}

#save dsp 97.5%
phisU <- numeric(24)  
idxU <- 1
for (p in 1:3) {
  for (a in 1:2) {
    for (s in 1:4) {
      phisU[idxU] <- plogis(model1output$q97.5$alpha[,,p][a, s])
      idxU <- idxU + 1
    }
  }
}

#generate data frame
dspDF <- data.frame(mean = phis, lower = phisL, upper = phisU)
dspDF$age <- c("Adult","Adult","Adult","Adult","Juvenile","Juvenile","Juvenile","Juvenile")
dspDF$`Phenological-specific state` <- c("Not currently breeding","Egg-laying","Incubation","Brood-rearing")
dspDF$Period <- c("Early","Early","Early","Early","Early","Early","Early","Early",
                  "Mid","Mid","Mid","Mid","Mid","Mid","Mid","Mid","Late","Late","Late","Late",
                  "Late","Late","Late","Late")

#view
dspDF

#earliest brood
min(apply(jags.data$x, 1, function(x) {
  idx <- which(x == 4)
  if (length(idx) == 0) NA else min(idx)
}),na.rm=T)

#remove brooding from early period as this did not occur
dspDF <- dspDF[-c(4,8),]

#view
dspDF

#reorder
dspDF <- dspDF %>% select(age, `Phenological-specific state`, Period, mean, lower, upper)

#view
table2 <- dspDF
for (i in 1:22) {
 for (j in 4:6) {
  table2[i,j] <- round(table2[i,j],3)
  } 
}
table2

setwd("C:/Users/dylan.bakner/Documents/manuscripts/turkeySurvival/secondRoundRevisions/revisedAnalyses/analysis9.24.25/out")
write.csv(table2, file = "table1.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#dsp plots 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#set factor levels
dspDF$Period=factor(dspDF$Period,levels=c("Early","Mid","Late"))
dspDF$`Phenological-specific state`=factor(dspDF$`Phenological-specific state`,levels=c("Not currently breeding","Egg-laying","Incubation","Brood-rearing"))

p3 <- dspDF %>%
  ggplot() +
  geom_point(aes(x = Period, y = mean, shape = `Phenological-specific state`), 
             position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(x = Period, y = mean, ymin = lower, ymax = upper, group = `Phenological-specific state`),
                position = position_dodge(width = 0.5), width = 0, size = 0.5, linetype = "dashed") +
  theme_classic(base_size = 12) +
  scale_shape_manual(values = c(16, 1, 17, 2)) +
  scale_y_continuous(limits = c(0.94, 1), labels = scales::number_format(accuracy = 0.001)) +
  labs(x = "Reproductive period", y = "Daily survival probability") +
  theme(
    legend.position = "right",
    axis.text = element_text(color = "black"),
    strip.background = element_blank(), 
    strip.text = element_text(size = 14, face = "plain")
  ) +
  facet_wrap(~age, ncol = 1, scales = "fixed")
p3

setwd("C:/Users/dylan.bakner/Documents/manuscripts/turkeySurvival/secondRoundRevisions/revisedAnalyses/analysis9.24.25/out")
ggsave(filename = "figure2.png", plot = p3, width = 6.5, height = 6, units = "in", dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#how many daily observations for each state-period-age combination (not in manuscript)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (i in 1:4) {
  for (j in 1:3) {
    for (k in 1:2) {
      print(paste("i =", i, "j =", j, "k =", k))
      print(table(jags.data$x == i & jags.data$period == j & jags.data$age == k))
    }
  }
}
