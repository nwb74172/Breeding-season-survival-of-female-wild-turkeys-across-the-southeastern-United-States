
library(jagsUI)
library(dplyr)
library(ggplot2)
library(patchwork)

#load jags data
load("model2data.RData") 

#load jags out
model2output <- readRDS("model2output.rds") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#assess temporal variation in dsp during laying, incubation, and brooding
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#make data frames
eggLaying <- data.frame(matrix(ncol = 3, nrow = 11))
for(i in 1:11){
  eggLaying[i,] <- quantile(plogis(model2output$sims.list$alphaB[,i]), c(0.025,0.5,0.975))
}
incubation <- data.frame(matrix(ncol = 3, nrow = 30))
for(i in 21:50){
  incubation[i-20,] <- quantile(plogis(model2output$sims.list$alphaB[,i]), c(0.025,0.5,0.975))
}
brooding <- data.frame(matrix(ncol = 3, nrow = 28))
for(i in 56:83){
  brooding[i-55,] <- quantile(plogis(model2output$sims.list$alphaB[,i]), c(0.025,0.5,0.975))
}
eggLaying$day <- 1:11
colnames(eggLaying) <- c("lower","mean","upper","day")
incubation$day <- 1:30
colnames(incubation) <- c("lower","mean","upper","day")
brooding$day <- 1:28
colnames(brooding) <- c("lower","mean","upper","day")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#mean dsr for each stateof interest
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#incubation
round(mean(incubation$lower),3);round(mean(incubation$mean),3);round(mean(incubation$upper),3)

#brooding
round(mean(brooding$lower),3);round(mean(brooding$mean),3);round(mean(brooding$upper),3)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#plots for each
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#data frame that tells what state death occured
deaths <- as.data.frame(which(jags.data1$y == 0, arr.ind = TRUE))
for (i in 1:nrow(deaths)) {
  deaths$day[i] <- jags.data1$day[deaths$row[i],deaths$col[i]]
}

deaths <- deaths %>%
  mutate(state = case_when(
    day < 21 ~ "laying",
    day > 20 & day < 56 ~ "incubation",
    day > 55 & day < 86 ~ "brooding",
    day == 86 ~ "not breeding"
  ))

#egg-laying
#eggLaying%>%
#  ggplot() +
#  geom_point(aes(x = (day), y = mean)) +
#  geom_errorbar(aes( x = day, y = mean, ymin = lower, ymax = upper), width = 0, size = 0.5, linetype="dashed") +
#  theme_classic(base_size = 12) +
#  scale_y_continuous(limits = c(0.96,1))+
#  scale_x_continuous(limits = c(1,11), breaks = seq(1:11))+
#  labs(x="Day", y="Daily survival probability") +
#  theme(legend.position = "none", axis.text = element_text(color="black"))

#incubation
p5<-incubation%>%
  ggplot() +
  geom_point(aes(x = day, y = mean)) +
  geom_errorbar(aes(x = day, y = mean, ymin = lower, ymax = upper), width = 0, size = 0.5, linetype="dashed") +
  theme_classic(base_size = 12) +
  scale_y_continuous(limits = c(0.955,1), labels = scales::number_format(accuracy = 0.001))+
  scale_x_continuous(limits = c(1,30), breaks = c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)) +
  labs(x="Day", y="Daily survival probability") +
  theme(legend.position = "none", axis.text = element_text(color="black")) 

#incubation deaths
p6<-deaths %>% filter(state == "incubation")%>%
  ggplot() +
  geom_boxplot(aes(x = (day-20), y = state), size = 0.75) +
  geom_point(aes(x = (day-20), y = state), position = position_jitter(width = 0.5, height = 0.2), alpha = 0.6, size = 1.5) + 
  scale_x_continuous(limits = c(1,30), breaks = c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)) +
  theme_void(base_size = 12)+
  ggtitle("Incubation")

p7<-p6/p5 + plot_layout(heights = c(1, 2))
p7

#brooding
p8<-brooding%>%
  ggplot() +
  geom_point(aes(x = day, y = mean)) +
  geom_errorbar(aes( x = day, y = mean, ymin = lower, ymax = upper), width = 0, size = 0.5, linetype="dashed") +
  theme_classic(base_size = 12) +
  scale_y_continuous(limits = c(0.955,1), labels = scales::number_format(accuracy = 0.001))+
  scale_x_continuous(limits = c(1,28), breaks = c(1,3,5,7,9,11,13,15,17,19,21,23,25,27)) +
  labs(x="Day", y="Daily survival probability") +
  theme(legend.position = "none", axis.text = element_text(color="black")) 

#brooding deaths
p9<-deaths %>% filter(state == "brooding")%>%
  ggplot() +
  geom_boxplot(aes(x = (day-55), y = state), size = 0.75) +
  geom_point(aes(x = (day-55), y = state), position = position_jitter(width = 0.5, height = 0.2), alpha = 0.6, size = 1.5) + 
  scale_x_continuous(limits = c(1,28), breaks = c(1,3,5,7,9,11,13,15,17,19,21,23,25,27)) +
  theme_void(base_size = 12)+
  ggtitle("Brooding-rearing")

p10<-p9/p8 + plot_layout(heights = c(1, 2))
p10

p11 <- (p7 | p10) + plot_layout(ncol = 2)
p11

#not currently breeding state deaths
p12 <- deaths %>% filter(state == "not breeding")%>%
  ggplot() +
  geom_boxplot(aes(x = (col), y = state), size = 0.75) +
  geom_point(aes(x = (col), y = state), position = position_jitter(width = 0.5, height = 0.2), alpha = 0.6, size = 1.5) + 
  geom_vline(xintercept = 37, size = 1.25, linetype = "dashed",) + #start mid reproduction
  geom_vline(xintercept = 96, size = 1.25, linetype = "dashed") + #start late reproduction
  annotate("rect", xmin = 37, xmax = 96, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "goldenrod") +
  scale_x_continuous(limits = c(1,160)) +
  theme_classic(base_size = 12) +
  labs(x = "Day of breeding season") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(color="black")) +
  ggtitle("Not currently breeding mortalities")
p12

ggsave(filename = "figure3.png", plot = p11, width = 9, height = 5, units = "in", dpi = 300)
ggsave(filename = "appendixfigure1.png", plot = p12, width = 5, height = 5, units = "in", dpi = 300)





