
library(jagsUI)
library(dplyr)
library(ggplot2)
library(patchwork)

#load jags data
load("model1data.RData") 

#load jags out
model1output <- readRDS("model1output.rds") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#spatial and temporal variance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#spatial variance
round(mean(plogis(model1output$mean$sigma[1])),2);round(mean(plogis(model1output$q2.5$sigma[1])),2);round(mean(plogis(model1output$q97.5$sigma[1])),2)

#temporal variance
round(mean(plogis(model1output$mean$sigma[2])),2);round(mean(plogis(model1output$q2.5$sigma[2])),2);round(mean(plogis(model1output$q97.5$sigma[2])),2)


