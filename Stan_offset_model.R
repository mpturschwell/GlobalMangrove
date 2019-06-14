#Stan Model 26 NOV 2018

#Mangrove Loss Stan Models
setwd("C:/POSTDOC_2018-2021/Mangrove_trends/Data/Sebastian")

library(rstan)
library(rethinking)
library(purrr)
library(ggplot2)
Sys.setenv(USE_CXX14 = 1)

mangroves <- read.csv("GlobalMangroveData_Nov26.csv", header = T)

# sites with GDP info 
mangroves <- mangroves[!is.na(mangroves["GDP_Perc_Inc2000_2012"]),]
mangroves$NATION <- droplevels(mangroves$NATION)

# Using whole dataset
# Response 
addbit <- min(mangroves$area_2012[mangroves$area_2012>0])*0.1
y <- log(mangroves$area_2012 + addbit) 

# Lower level predictors
#PORT <- as.numeric(mangroves$SCPort_Dist_KM)
PROTECTION <- as.numeric(mangroves$SCProtectedArea)
#FRAG <- as.numeric(scale(mangroves$CLUMPY))
#FRAG1 <- as.numeric(scale(mangroves$ENN_RA))
# frag metrics to look at: PLAND = percentage of landscape, ENN_RA
# LSI - the greater the value of LSI, the more dispersed are the patch types.
# AREA_CV
# SHAPE_AM/RA



# Offset
offset <- log(mangroves$area_2000 + addbit)

# Random Effets 
mangroves$NATION_number <- as.numeric(mangroves$NATION)
Nation_ID <- as.integer(mangroves$NATION_number)
Unique_Nations <- unique(Nation_ID)

# Upper-level Predictors
GDP <- as.numeric(mangroves$SC_GDP)

newdata<- data.frame(y, PROTECTION, PORT, FRAG, FRAG1, Nation_ID, Unique_Nations, offset)

newdata<- data.frame(y, PROTECTION, Nation_ID, offset)

newdata <- list(y = y, PROTECTION = PROTECTION, Nation_ID = Nation_ID, Unique_Nations = Unique_Nations,
                offset = offset, GDP = GDP)

#############################################################################################
#############################################################################################
#
#
# multi-level varying intercepts and slopes model 
#
#
#############################################################################################
#############################################################################################

stan_mod2 <- map2stan(alist( 
  y ~ dnorm(mu,sigma),
  #DPSIR
  #Impacts/state changes esimated from Pressures and mgmt responses. 
  mu <- offset +(a + a_Nation[Nation_ID]) + 
                (b + b_Nation[Nation_ID]) * PROTECTION,
      #        (c + c_Nation[Nation_ID]) * PORT +
      #        (d + d_Nation[Nation_ID]) * FRAG, 
    
  a_Nation[Nation_ID] ~ dnorm(0, sigma1),
  b_Nation[Nation_ID] ~ dnorm(0, sigma2), #uncorrelated slopes and intercepts
# c_Nation[Nation_ID] ~ dnorm(0, sigma3), #uncorrelated slopes and intercepts
# d_Nation[Nation_ID] ~ dnorm(0, sigma4), #uncorrelated slopes and intercepts
#  mu_b <-  ab1 +gb1 *GDP,
#mu_c <- gc1 *GDP,
 
  #drivers model, how do pressure vary by drivers at country scale
  #c(a_Nation, b_Nation, c_Nation, d_Nation)[Nation_ID] ~ dmvnorm2(0, tau, Rho),     # correlated slopes and intercepts
  a ~ dnorm(0, 10),
  b ~ dnorm(0, 1),

 #gb1 ~ dnorm(0, 1),
# c ~ dnorm(0, 1),
# d ~ dnorm(0, 1),
 
#c(g0_Nation, g1_Nation)[Nation_ID] ~ dmvnorm2(0, tau, Rho1),     # correlated slopes and intercepts

  sigma ~ dcauchy(0, 2.5),
 sigma1 ~ dcauchy(0, 2.5),
 sigma2 ~ dcauchy(0, 2.5)
 #sigma3 ~ dcauchy(0, 2.5),
 #sigma4 ~ dcauchy(0, 2.5)
  #  tau ~ dcauchy(0, 2.5),
   # Rho ~ dlkjcorr(2)
   # Rho1 ~ dlkjcorr(2)

  ),
data = newdata, iter = 2000, warmup = 500, chains = 2,
control=list(adapt_delta = 0.8, max_treedepth = 10))


##############################################################
plot(stan_mod2) # gives you MCMC chains 
precis(stan_mod2,depth=3)
sp <- dashboard(stan_mod2)
postcheck(stan_mod2)


stancode(stan_mod2)

stanmod <- stan_mod2@stanfit
stan_mod <- as.shinystan(stanmod)
shinystan::launch_shinystan(stan_mod)

################################################
#
# caterpillar plots of intercepts ad slopes 
#
################################################

# extract samples from STAN model 
x <- data.frame(extract.samples(stan_mod2))
mod_summary <- map_df(x, ~quantile(.x, c(.055, 0.5, .945))) # calculate 89% CI 
mod_summary <- as.data.frame(mod_summary)
row.names(mod_summary) <- c("lwr", "mean", "upr")

mod_summary <- as.data.frame(t(mod_summary)) # transpose df
mod_summary$NATION <- rownames(mod_summary)

# get unique nations to merge 
nations <- data.frame(unique(mangroves$NATION))
nations$NATION_number <- as.numeric(nations$unique.mangroves.NATION.)
nations$NATION_number <- as.integer(nations$NATION_number)
nations$NATION_number <- nations[order(nations$NATION_number),]

#
# extract and plot intercepts 
#
ints <- mod_summary[1:108,] # these numbers will depend on how many countries you have as your index 
ints2 <- cbind(ints, nations)
ints2 <- ints2[,c(1:4,6)]
ints2 <- ints2[order(-ints2$mean), ]

g1 <- ggplot(ints2, aes(x = reorder(NATION_number$unique.mangroves.NATION., -mean), y = mean)) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr))+
  geom_point() +
  theme_bw() + 
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(size = 8, angle = 75, hjust = 1)) +
  xlab("NATION")+
  ylab("Log Proportion of area in 2000")
g1
ggsave("Log Proportion of area in 2000.png", g1, width = 12, height = 6, units = c("in"), dpi = 300)

#
# Extract and plot slopes of Protection 
#
beta1 <- mod_summary[109:216,]
beta1 <- cbind(beta1, nations)
beta1 <- beta1[,c(1:4,6)]
beta1 <- beta1[order(-beta1$mean), ]


g2 <- ggplot(beta1, aes(x = reorder(NATION_number$unique.mangroves.NATION., -mean), y = mean)) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr))+
  geom_point() +
  theme_bw() + 
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(size = 8, angle = 75, hjust = 1)) +
  xlab("NATION")+
  ylab("Effect of Protection")

g2
ggsave("Effect of protection area on loss.png", g2, width = 10, height = 6, units = c("in"), dpi = 300)


#
# extract and plot slopes of Port effects 
#
beta2 <- mod_summary[217:324,]
beta2 <- cbind(beta2, nations)
beta2 <- beta2[,c(1:4,6)]
beta2 <- beta2[order(-beta2$mean), ]

g3 <- ggplot(beta2, aes(x = reorder(NATION_number$unique.mangroves.NATION., -mean), y = mean)) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr))+
  geom_point() +
  theme_bw() + 
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(size = 8, angle = 75, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("NATION")+
  ylab("Effect of Distance to Port")

g3
ggsave("Effect of distance to port on loss.png", g3, width = 10, height = 6, units = c("in"), dpi = 300)


#
beta3 <- mod_summary[325:432,]
beta3 <- cbind(beta3, nations)
beta3 <- beta3[,c(1:4,6)]
beta3 <- beta3[order(-beta3$mean), ]

g4 <- ggplot(beta3, aes(x = reorder(NATION_number$unique.mangroves.NATION., -mean), y = mean)) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr))+
  geom_point() +
  theme_bw() + 
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(size = 8, angle = 75, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("NATION")+
  ylab("Effect of Fragmentation")

g4
ggsave("Effect of distance to port on loss.png", g3, width = 10, height = 6, units = c("in"), dpi = 300)

#
# plot mu 
#
p <- link(stan_mod2, n = 8000)
preds <- colMeans(p)
plot(y~ preds)
abline(0,1, col = 2)
cor(y, preds)^2

residuals <- y-preds
plot(residuals)

