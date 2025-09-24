
library(jagsUI)
library(dplyr)
library(ggplot2)
library(patchwork)

#load jags data
load("C:/Users/dylan.bakner/Documents/manuscripts/turkeySurvival/secondRoundRevisions/revisedAnalyses/analysis9.24.25/data/model2data.RData") 

#set working directory to save the jags script
setwd("C:/Users/dylan.bakner/Documents/manuscripts/turkeySurvival/secondRoundRevisions/revisedAnalyses/analysis9.24.25/jagsScripts")


#jags model~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sink("m2.jags")
cat("model {

#likelihood~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      for (i in 1:n.inds){
        for(t in (first[i]):(last[i]-1)){
          y[i,t+1] ~ dbern(y[i,t] * phi[i,t])
          logit(phi[i,t]) <- (1 - x[i,t]) * alpha +
                             x[i,t] * alphaB[day[i,t]]
            }
        }
    
#priors~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    #intercept prior
    alpha ~ dnorm(5.5,1) 
    
    #day priors
    
    #laying~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    alphaB[1] ~ dnorm(5.5, 1) 
    sigma ~ dgamma(1,1)
    tau = pow(sigma, -2)
    for (j in 2:20){
      alphaB[j] ~ dnorm(alphaB[j-1], tau)
    }
    
    #incubation~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    alphaB[21] ~ dnorm(5.5, 1) 
    sigma1 ~ dgamma(1,1)
    tau1 = pow(sigma1, -2) 
    for (k in 22:55){
      alphaB[k] ~ dnorm(alphaB[k-1], tau1)
    }
    
    #brooding~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    alphaB[56] ~ dnorm(5.5, 1) 
    sigma2 ~ dgamma(1,1)
    tau2 = pow(sigma2, -2) 
    for (l in 57:86){
      alphaB[l] ~ dnorm(alphaB[l-1], tau2)
    }

    
}",fill = TRUE)
sink()

#number of chains (nc), thinning rate (nt), number of iterations (ni), and number to burn-in
nc <- 3
nt <- 1
ni <- 30000
nb <- 5000

#initial values
inits <- function(){list()}  

#parameters to monitor
parameters <- c('alpha','alphaB')

#fit
m2 <- jags(jags.data1, inits, parameters, "m2.jags", parallel = T, 
          n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

#quick look at output
summary(m2)

#set working directory to save the jags script
setwd("C:/Users/dylan.bakner/Documents/manuscripts/turkeySurvival/secondRoundRevisions/revisedAnalyses/analysis9.24.25/out")
saveRDS(m2, file = "model2output.rds")
