
library(jagsUI)
library(dplyr)
library(ggplot2)
library(patchwork)

#load jags data
load("model1data.RData") 

#set working directory to save the jags script
setwd("jagsScripts")

#jags model~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sink("m1.jags")
cat("model {

#likelihood~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      for (i in 1:n.inds){
        for(t in (first[i]):(last[i]-1)){
          y[i,t+1] ~ dbern(phi[i,t] * y[i,t])
          logit(phi[i,t]) <- alpha[age[i], x[i,t], period[t]] + 
                             epsilon[site[i]] + delta[year[i]]
  }
}

#priors~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      for(a in 1:2){        #for each age
        for(s in 1:4){      #for each state
          for (p in 1:3){   #for each period
          #biologically informative priors suggest dsp is skewed left (i.e., closer to 0.99)
          alpha[a,s,p] ~ dnorm(5.5, 1) 
    }
  }
}

#non-centered random effects to aid convergence
#epsilon: site
#delta: year

      #epsilon

      for(k in 1:nSite){
      #normally distributed between 0-1
       epsilon.star[k] ~ dnorm(0,1)
        epsilon[k] = epsilon.star[k] * sigma[1]
}
    sigma[1] ~ dgamma(1,1) #skewed right

      #delta

      for(k in 1:nYear){
        #normally distributed between 0-1
        delta.star[k] ~ dnorm(0, 1)
        delta[k] = delta.star[k] * sigma[2]
}
    sigma[2] ~ dgamma(1,1) #skewed right

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
parameters <- c('alpha', 'sigma')

#fit
m1 <- jags(jags.data, inits, parameters, "m1.jags", parallel = T, 
          n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

#quick look at output
summary(m1)

#set working directory to save the jags script
saveRDS(m1, file = "model1output.rds")

