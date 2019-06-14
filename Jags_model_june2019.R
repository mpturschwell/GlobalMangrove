###############################################
# JAGS Model for Global Mangrove Trends 
# code by Mischa Turschwell 
###############################################

setwd("C:/POSTDOC_2018-2021/Mangrove_trends/Data/Sebastian")
library(R2jags)
library(ggplot2)
options(max.print = 10000)
mangroves <- read.csv("GlobalMangroveData_JUNE.csv", header = T)


# select cells with GDP info 
mangroves <- mangroves[!is.na(mangroves["SC_PCGDP_trend_2000.2012"]),]
mangroves <- mangroves[!is.na(mangroves["SC_PCGDP_mean_1990.2012"]),]
mangroves <- mangroves[!is.na(mangroves["SC_PCGDP_mean_2000.2012"]),]
mangroves <- mangroves[!is.na(mangroves["SC_PCGDP_mean_1990.2000"]),]


mangroves <- mangroves[!is.na(mangroves["tcombo"]),]
mangroves <- mangroves[!is.na(mangroves["humanfootprintindex"]),]
mangroves <- mangroves[!is.na(mangroves["popdens2000"]),]
mangroves <- mangroves[!is.na(mangroves["AREA_AM"]),]
mangroves <- mangroves[!is.na(mangroves["protectedpercentage"]),]
mangroves <- mangroves[!is.na(mangroves["Port_Dist_KM"]),]

# get new total nation numbers 
mangroves$NATION <- droplevels(mangroves$NATION)
mangroves$NATION_number <- as.numeric(mangroves$NATION)

# scale and mean centre variables by country 
Cum.impact.scale <- vector(length=nrow(mangroves))
for(i in 1:max(mangroves$NATION_number)){
  Cum.impact.scale[mangroves$NATION_number==i] <- (mangroves$tcombo[which(mangroves$NATION_number==i)] - mean(mangroves$tcombo[which(mangroves$NATION_number==i)])) / sd(mangroves$tcombo[which(mangroves$NATION_number==i)])
}

Human.footprint <- vector(length=nrow(mangroves))
for(i in 1:max(mangroves$NATION_number)){
  Human.footprint[mangroves$NATION_number==i] <- (mangroves$humanfootprintindex[which(mangroves$NATION_number==i)] - mean(mangroves$humanfootprintindex[which(mangroves$NATION_number==i)])) / sd(mangroves$humanfootprintindex[which(mangroves$NATION_number==i)])
}

Pop.dens.2000 <- vector(length=nrow(mangroves))
  for(i in 1:max(mangroves$NATION_number)){
    Pop.dens.2000[mangroves$NATION_number==i] <- (mangroves$popdens2000[which(mangroves$NATION_number==i)] - mean(mangroves$popdens2000[which(mangroves$NATION_number==i)])) / sd(mangroves$popdens2000[which(mangroves$NATION_number==i)])
  }

Port.scale <- vector(length=nrow(mangroves))
for(i in 1:max(mangroves$NATION_number)){
  Port.scale[mangroves$NATION_number==i] <- (mangroves$Port_Dist_KM[which(mangroves$NATION_number==i)] - mean(mangroves$Port_Dist_KM[which(mangroves$NATION_number==i)])) / sd(mangroves$Port_Dist_KM[which(mangroves$NATION_number==i)])
}

Protection.scale <- vector(length=nrow(mangroves))
for(i in 1:max(mangroves$NATION_number)){
  Protection.scale[mangroves$NATION_number==i] <- (mangroves$protectedpercentage[which(mangroves$NATION_number==i)] - mean(mangroves$protectedpercentage[which(mangroves$NATION_number==i)])) / sd(mangroves$protectedpercentage[which(mangroves$NATION_number==i)])
}

frag.scale <- vector(length=nrow(mangroves))
for(i in 1:max(mangroves$NATION_number)){
  frag.scale[mangroves$NATION_number==i] <- (mangroves$AREA_AM[which(mangroves$NATION_number==i)] - mean(mangroves$AREA_AM[which(mangroves$NATION_number==i)])) / sd(mangroves$AREA_AM[which(mangroves$NATION_number==i)])
}

mangroves <- cbind(mangroves, Cum.impact.scale, Human.footprint, Pop.dens.2000, frag.scale, Protection.scale, Port.scale)
mangroves <- mangroves[!is.na(mangroves["Cum.impact.scale"]),]
mangroves <- mangroves[!is.na(mangroves["Human.footprint"]),]
mangroves <- mangroves[!is.na(mangroves["frag.scale"]),]
mangroves <- mangroves[!is.na(mangroves["Protection.scale"]),]
mangroves <- mangroves[!is.na(mangroves["Port.scale"]),]
mangroves <- mangroves[!is.na(mangroves["Pop.dens.2000"]),]


#drop suriname & equitorial guinea outlier
sur <- mangroves[which(mangroves$NATION=='SURINAME'),]
mangroves <- mangroves[!(mangroves$NATION %in% sur$NATION),]

mangroves$NATION <- droplevels(mangroves$NATION)
mangroves$NATION_number <- as.numeric(mangroves$NATION)

# Using whole dataset
# Response 
addbit <- min(mangroves$area_2012[mangroves$area_2012>0])*0.1
y <- log(mangroves$area_2012 + addbit) 

#y <- mangroves$Proportion
# Lower level predictors
PORT <- mangroves$Port.scale
PROTECTION <- mangroves$Protection.scale
FRAG <- mangroves$frag.scale
CUM <- mangroves$Cum.impact.scale
HF <- mangroves$Human.footprint
POP <- mangroves$Pop.dens.2000
# Offset
offset <- log(mangroves$area_2000 + addbit)

# Random Effets 
cellID <- as.integer(mangroves$NATION_number)

n <- as.numeric(length(cellID))
N <- as.numeric(length(unique(cellID)))

# Upper-level Predictors

#GDP <- as.numeric(tapply(mangroves$SC_PCGDP_trend_2000.2012, mangroves$NATION, mean))
#GDP <- as.numeric(tapply(mangroves$SC_PCGDP_mean_1990.2012, mangroves$NATION, mean))
#GDP <- as.numeric(tapply(mangroves$SC_PCGDP_mean_2000.2012, mangroves$NATION, mean))
GDP <- as.numeric(tapply(mangroves$SC_PCGDP_mean_1990.2000, mangroves$NATION, mean))

parameters <- list("b","g","tau", "sigma") # add sigma in too
parms <- as.character(parameters)

newdata <- list(y = y, 
                HF = HF, CUM = CUM, FRAG = FRAG, POP = POP, PORT = PORT, PROTECTION = PROTECTION,
                cellID = cellID, N = N,  n = n, offset = offset, GDP = GDP)

program.file.name ="BHM_Mangrove_RandomEffects-GDP_priors.txt" 

#################################################################################
# Run model in JAGS
start.time <- Sys.time()

Rjags.mod <- jags(data = newdata, 
                  parameters.to.save = parms,
                  model.file = program.file.name, n.chains = 3, n.iter = 100000, 
                  n.burnin = 10000, n.thin = 10, DIC = TRUE)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

################################################################################

#
#plot rhat values - indicator of convergence 
#
plot(Rjags.mod$BUGSoutput$summary[,c(8)])

Rjags.mod$BUGSoutput$DIC

#
# Plot MCMC chains 
#library('MCMCvis')
#jagsfit.mcmc <- as.mcmc(Rjags.model)
#MCMCtrace(jagsfit.mcmc)

#
# Extract 2.5, 50 and 97.5% estimates
#
mod_summary <- data.frame(round(Rjags.mod$BUGSoutput$summary[,c(3,5,7)],3))
colnames(mod_summary) <- c("lwr", "mean", "upr")

mod_summary$NATION <- rownames(mod_summary)

#
# get unique nations to merge 
#
nations <- data.frame(unique(mangroves$NATION))
nations$NATION_number <- as.numeric(nations$unique.mangroves.NATION.)
nations$NATION_number <- as.integer(nations$NATION_number)
nations$NATION_number <- nations[order(nations$NATION_number),]

###############################################################################################################
###############################################################################################################
#
#       PLOTTING IT UP
#
# extract and plot intercepts 
#
###############################################################################################################
###############################################################################################################

# Change directory for plots to go in
setwd("C:/POSTDOC_2018-2021/Mangrove_trends/Data/Sebastian/June")

###################
# Gamma's 
#################
gs <- data.frame(round(Rjags.mod$BUGSoutput$summary[((N*7+2):(N*7+15)),c(3,5,7)],5))
gs$Gamma <- rownames(gs)
colnames(gs) <- c("lwr", "mean", "upr", "Gamma")
gs

gg1 <- ggplot(gs, aes(x = Gamma, y = mean)) + 
  geom_pointrange(aes(ymin = lwr, ymax = upr))+
  geom_point() +
  theme_bw() + 
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(size = 8, angle = 75, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Upper level coefficient")+
  ylab("")
gg1
ggsave("Effects of upper level predictor (GDP) on lower level relationships.png", gg1, width = 10, height = 6, units = c("in"), dpi = 300)

###############################################
#        INTERCEPTS - COUNTRY LEVEL AVERAGE
###############################################

ints <- mod_summary[1:N,]   # these numbers will depend on how many countries you have as your index 
ints <- cbind(ints, nations)
ints <- ints[,c(1:4,6)]
ints2 <- ints[order(-ints$mean), ]

g1 <- ggplot(ints2, aes(x = reorder(NATION_number$unique.mangroves.NATION., -mean), y = mean)) + 
  geom_pointrange(aes(ymin = lwr, ymax = upr))+
  geom_point() +
  theme_bw() + 
  geom_hline(yintercept = 0, col = 'black', linetype ="solid", size = 1)+ 
  
  geom_hline(yintercept = gs[1,1], col = 'red', linetype ="dotted", size = 1.1)+ 
  geom_hline(yintercept = gs[1,2], col = 'red', size = 1.1) +
  geom_hline(yintercept = gs[1,3], col = 'red', linetype ="dotted", size = 1.1) +
  theme(axis.text.x = element_text(size = 10, hjust = 0.5)) +
  xlab("NATION")+
  ylab("Mean estimated proportion of Mangrove Area in 2012 relative to 2000 (log)") +
  scale_y_continuous(position = "left") + coord_flip() 
g1

ggsave("Mean estimated log Proportion of Mangroves Area in 2012 vs 2000 by NATION.png", g1, width = 12, height = 12, units = c("in"), dpi = 300)


###########  NATIONAL LEVEL VARIABLES ###############
#
# Plot effect of GDP on mean estimated loss 
#


# Change this if you want to plot against log, not scaled. 

mangroves$logGDP <- sqrt(mangroves$mean1990.2000)
GDP2 <- as.numeric(tapply(mangroves$logGDP, mangroves$NATION, mean))

test <- cbind(ints, GDP2)
q <- ggplot(test, aes(x = GDP2, y = mean)) + 
  geom_pointrange(aes(ymin = lwr, ymax = upr))+
  geom_point() +
#  ylim(-0.25,0.25)+
#  xlim(-1,15) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = ifelse(mean< -0.035 ,as.character(NATION_number$unique.mangroves.NATION.),'')), 
              angle = 0, hjust=-0.25, vjust=1, size = 4, colour = "blue", position=position_jitter())+
  geom_abline(colour = "red", size = 1, intercept = Rjags.mod$BUGSoutput$summary[(N*7+2),5], slope = Rjags.mod$BUGSoutput$summary[(N*7+9),5])+
  xlab("Scaled mean per capita GDP 1990-2000")+
  ylab("Log Proportion of Mangroves Area in 2012 relative to 2000")
q
ggsave("Mean per capita GDP 1990-2000 and estimated Mangrove loss.png", q, width = 12, height = 8, units = c("in"), dpi = 300)


#
# extract HUMAN FOOTPRINT 
#
beta1 <- mod_summary[(N+1):(N*2),]
beta1 <- cbind(beta1, nations)
beta1 <- beta1[,c(1:4,6)]
beta1 <- beta1[order(-beta1$mean), ]

g2 <- ggplot(beta1, aes(x = reorder(NATION_number$unique.mangroves.NATION., -mean), y = mean)) + 
  geom_pointrange(aes(ymin = lwr, ymax = upr))+
  geom_point() +
  theme_bw() + 
  geom_hline(yintercept = 0, col = 'black', linetype ="solid", size = 1)+ 
  
  geom_hline(yintercept = gs[2,1], col = 'red', linetype ="dotted", size = 1.1)+ 
  geom_hline(yintercept = gs[2,2], col = 'red', size = 1.1) +
  geom_hline(yintercept = gs[2,3], col = 'red', linetype ="dotted", size = 1.1) +
  theme(axis.text.x = element_text(size = 10, hjust = 0.5)) +
  xlab("NATION")+
  ylab("Effect of Human Footprint")+
scale_y_continuous(position = "left") + coord_flip() 

g2
ggsave("Fig S7.png", g2, width = 12, height = 12, units = c("in"), dpi = 300)


#
# extract and plot slopes of CUMULATIVE PRESSURES  
#
beta2 <- mod_summary[(N*2+1):(N*3),]
beta2 <- cbind(beta2, nations)
beta2 <- beta2[,c(1:4,6)]
beta2 <- beta2[order(-beta2$mean), ]

g3 <- ggplot(beta2, aes(x = reorder(NATION_number$unique.mangroves.NATION., -mean), y = mean)) + 
  geom_pointrange(aes(ymin = lwr, ymax = upr))+
  geom_point() +
  theme_bw() + 
  geom_hline(yintercept = 0, col = 'black', linetype ="solid", size = 1)+ 
  
  geom_hline(yintercept = gs[3,1], col = 'red', linetype ="dotted", size = 1.1)+ 
  geom_hline(yintercept = gs[3,2], col = 'red', size = 1.1) +
  geom_hline(yintercept = gs[3,3], col = 'red', linetype ="dotted", size = 1.1) +
  theme(axis.text.x = element_text(size = 10, hjust = 0.5),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("NATION")+
  ylab("Effect of Cumulative Pressures")+
  scale_y_continuous(position = "left") + coord_flip() 

g3
ggsave("Fig S4.png", g3, width = 12, height = 12, units = c("in"), dpi = 300)

#
# Effect of fragmentation
#
beta3 <- mod_summary[(N*3+1):(N*4),]
beta3 <- cbind(beta3, nations)
beta3 <- beta3[,c(1:4,6)]
beta3 <- beta3[order(-beta3$mean), ]

g4 <- ggplot(beta3, aes(x = reorder(NATION_number$unique.mangroves.NATION., -mean), y = mean)) + 
  geom_pointrange(aes(ymin = lwr, ymax = upr))+
  geom_point() +
  theme_bw() + 
  geom_hline(yintercept = 0, col = 'black', linetype ="solid", size = 1)+ 
  geom_hline(yintercept = gs[4,1], col = 'red', linetype ="dotted", size = 1.1)+ 
  geom_hline(yintercept = gs[4,2], col = 'red', size = 1.1) +
  geom_hline(yintercept = gs[4,3], col = 'red', linetype ="dotted", size = 1.1) +
  theme(axis.text.x = element_text(size = 10, hjust = 0.5),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("NATION")+
  ylab("Effect of Patch Size")+
  scale_y_continuous(position = "left") + coord_flip() 


g4
ggsave("Fig S3.png", g4, width = 12, height = 12, units = c("in"), dpi = 300)

#
# Effect of POPULATION DENSITY 2000
#
beta4 <- mod_summary[(N*4+1):(N*5),]
beta4 <- cbind(beta4, nations)
beta4 <- beta4[,c(1:4,6)]
beta4 <- beta4[order(-beta4$mean), ]

g5 <- ggplot(beta4, aes(x = reorder(NATION_number$unique.mangroves.NATION., -mean), y = mean)) + 
  geom_pointrange(aes(ymin = lwr, ymax = upr))+
  geom_point() +
  theme_bw() + 
  geom_hline(yintercept = 0, col = 'black', linetype ="solid", size = 1)+ 
  
  geom_hline(yintercept = gs[5,1], col = 'red', linetype ="dotted", size = 1.1)+ 
  geom_hline(yintercept = gs[5,2], col = 'red', size = 1.1) +
  geom_hline(yintercept = gs[5,3], col = 'red', linetype ="dotted", size = 1.1) +
  theme(axis.text.x = element_text(size = 10, hjust = 0.5),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("NATION")+
  ylab("Effect of Population Density")+
  scale_y_continuous(position = "left") + coord_flip() 
g5
ggsave("Fig S5.png", g5, width = 12, height = 12, units = c("in"), dpi = 300)


#
# Effect of DISTANCE TO PORT
#
beta5 <- mod_summary[(N*5+1):(N*6),]
beta5 <- cbind(beta5, nations)
beta5 <- beta5[,c(1:4,6)]
beta5 <- beta5[order(-beta5$mean), ]

g6 <- ggplot(beta5, aes(x = reorder(NATION_number$unique.mangroves.NATION., -mean), y = mean)) + 
  geom_pointrange(aes(ymin = lwr, ymax = upr))+
  geom_point() +
  theme_bw() + 
  geom_hline(yintercept = 0, col = 'black', linetype ="solid", size = 1)+ 
  
  geom_hline(yintercept = gs[6,1], col = 'red', linetype ="dotted", size = 1.1)+ 
  geom_hline(yintercept = gs[6,2], col = 'red', size = 1.1) +
  geom_hline(yintercept = gs[6,3], col = 'red', linetype ="dotted", size = 1.1) +
  theme(axis.text.x = element_text(size = 10, hjust = 0.5),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("NATION")+
  ylab("Effect of Distance to Port")+
  scale_y_continuous(position = "left") + coord_flip() 

g6
ggsave("Fig S6.png", g6, width = 12, height = 12, units = c("in"), dpi = 300)


#
# Effect of PROTECTED AREAS
#
beta6 <- mod_summary[(N*6+1):(N*7),]
beta6 <- cbind(beta6, nations)
beta6 <- beta6[,c(1:4,6)]
beta6 <- beta6[order(-beta6$mean), ]

g7 <- ggplot(beta6, aes(x = reorder(NATION_number$unique.mangroves.NATION., -mean), y = mean)) + 
  geom_pointrange(aes(ymin = lwr, ymax = upr))+
  geom_point() +
  theme_bw() + 
  geom_hline(yintercept = 0, col = 'black', linetype ="solid", size = 1)+ 
  
  geom_hline(yintercept = gs[7,1], col = 'red', linetype ="dotted", size = 1.1)+ 
  geom_hline(yintercept = gs[7,2], col = 'red', size = 1.1) +
  geom_hline(yintercept = gs[7,3], col = 'red', linetype ="dotted", size = 1.1) +
  theme(axis.text.x = element_text(size = 10, hjust = 0.5),
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("NATION")+
  ylab("Effect of Protected Area")+
  scale_y_continuous(position = "left") + coord_flip() 
g7
ggsave("Fig S2.png", g7, width = 12, height = 12, units = c("in"), dpi = 300)



# Residuals 
preds <- Rjags.mod$BUGSoutput$median$mu
residuals <- y-preds
plot(residuals)

plot(preds, y)
cor(preds, y)
0.9989911
0.9990011
#########################################################################################################################
#########################################################################################################################
#
# Probability of parameters 
#
#########################################################################################################################
#########################################################################################################################

P_PostDens <- matrix(nrow=length(mod_summary$NATION), ncol=2)
colnames(P_PostDens) <-c("Parameter","Probability")

for(i in 1:length(P_PostDens[,1])) {
    P_PostDens[i,1] <- row.names(mod_summary[i,])
    #plot(density(Rjags.mod$BUGSoutput$sims.matrix[,i])) 
    #abline(v=0)   
    P <- round(length(Rjags.mod$BUGSoutput$sims.matrix[Rjags.mod$BUGSoutput$sims.matrix[,i]<0,(i)])/nrow(Rjags.mod$BUGSoutput$sims.matrix),3)
    P_PostDens[i,2]<- P[1]               
}

write.csv(P_PostDens, "Parameter_Probabilities_may13.csv", row.names = T)
write.csv(ints, "Nation_names.csv", row.names = T)
write.csv(gs, "Gamma_probabilities_may13.csv", row.names = T)



#########################################################################################################################
#########################################################################################################################
#
# Cross validation using only offset model
#
#########################################################################################################################
#########################################################################################################################


addbit <- min(mangroves$area_2012[mangroves$area_2012>0])*0.1
y <- log(mangroves$area_2012 + addbit) 
offset <- log(mangroves$area_2000 + addbit)

cellID <- as.integer(mangroves$NATION_number)

n <- as.numeric(length(cellID))
N <- as.numeric(length(unique(cellID)))
GDP <- as.numeric(tapply(mangroves$SCGDP_mean_2000.2012, mangroves$NATION, mean))

parameters <- list("mu","b","g","tau")
parms <- as.character(parameters)

newdata <- list(y = y, cellID = cellID, N = N,  n = n, offset = offset, GDP = GDP)

program.file.name ="BHM_Mangrove_RandomEffects-GDP_offset_only.txt" 

#################################################################################
# Run model in JAGS

Rjags.mod <- jags(data = newdata, 
                  parameters.to.save = parms,
                  model.file = program.file.name, n.chains = 3, n.iter = 10000, 
                  n.burnin = 1000, n.thin = 3, DIC = TRUE)



Rjags.mod$BUGSoutput$DIC
-10051.55
### CHECK COLLINEARITY
# select covars 
covars <- mangroves[,c(19, 80, 121:124)]

library(Hmisc)

# on just covariates
covar.mat<- as.matrix(covars)
spearman <- rcorr(covar.mat, type = "pearson")

spear.r <- data.frame(spearman$r)
spear.p <- data.frame(spearman$P)     

as.dist(spear.r)
as.dist(spear.p)

which(spear.p <0.01, arr.ind=TRUE)
write.csv(spear.r, "spearman.fish.covariates.r.vals.csv", row.names = TRUE)

###############################################################################################
#
#
#
#
#       CHEEKY PLOT FOR MAP 
#
#
###############################################################################################

setwd("C:/POSTDOC_2018-2021/Mangrove_trends/Data")

library(leaflet)
library(rgdal)
library(rgeos)
library(sp)
library(maptools)
library(sf)
library(rmapshaper)
library(taxize)
library(dplyr)
library(tibble)
library(RColorBrewer)

# read in megafauna shp file 
MANGROVES <- readOGR("MarineEEZ_Mangrove_loss.shp")



