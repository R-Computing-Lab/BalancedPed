# script to plot power plots
library(ggplot2)

powerCal <- function(lamda,df){
    1- pchisq(qchisq(1-.05, df), df, lamda)
    
}

### create a new enviroment for one condition
env_p2c2 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p2/p2c2/modelSmr.Rdata", envir = env_p2c2)
#load("~/BalancedPed/Simulations/p2/p2c2/modelSmr.Rdata", envir = env_p2c2)

meanDiffLL_mtam_p2c2 <- env_p2c2$smr2$Minus2LogLikelihood - env_p2c2$smr1$Minus2LogLikelihood
lamdaUnit_p2c2 <- meanDiffLL_mtam_p2c2/10000
SSize_p2c2 <- 1: 10000
LamdaVec_p2c2 <- lamdaUnit_p2c2*SSize_p2c2
powVec_p2c2 <- as.numeric(lapply(LamdaVec_p2c2,powerCal, df = 2))

### create a new enviroment for one condition
env_p5c2 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p5/p5c2/modelSmr.Rdata", envir = env_p5c2)
#load("~/BalancedPed/Simulations/p5/p5c2/modelSmr.Rdata", envir = env_p5c2)

meanDiffLL_mtam_p5c2 <- env_p5c2$smr2$Minus2LogLikelihood - env_p5c2$smr1$Minus2LogLikelihood
lamdaUnit_p5c2 <- meanDiffLL_mtam_p5c2/10000
SSize_p5c2 <- 1: 10000
LamdaVec_p5c2 <- lamdaUnit_p5c2*SSize_p5c2
powVec_p5c2 <- as.numeric(lapply(LamdaVec_p5c2,powerCal, df = 2))

### create a new enviroment for one condition
env_p8c2 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c2/modelSmr.Rdata", envir = env_p8c2)
#load("~/BalancedPed/Simulations/p8/p8c2/modelSmr.Rdata", envir = env_p8c2)

meanDiffLL_mtam_p8c2 <- env_p8c2$smr2$Minus2LogLikelihood - env_p8c2$smr1$Minus2LogLikelihood
lamdaUnit_p8c2 <- meanDiffLL_mtam_p8c2/10000
SSize_p8c2 <- 1: 10000
LamdaVec_p8c2 <- lamdaUnit_p8c2*SSize_p8c2
powVec_p8c2 <- as.numeric(lapply(LamdaVec_p8c2,powerCal, df = 2))

# ### create a new enviroment for one condition
# env_p8c2 <- new.env()
# load("~/R-Project/BalancedPed/Simulations/p8/p8c2/modelSmr.Rdata", envir = env_p8c2)
# #load("~/BalancedPed/Simulations/p8/p8c2/modelSmr.Rdata", envir = env_p8c2)
# 
# meanDiffLL_mtam_p8c2 <- env_p8c2$smr2$Minus2LogLikelihood - env_p8c2$smr1$Minus2LogLikelihood
# lamdaUnit_p8c2 <- meanDiffLL_mtam_p8c2/env_p8c2$smr1$numObs
# SSize_p8c2 <- 1: 10
# LamdaVec_p8c2 <- lamdaUnit_p8c2*SSize_p8c2
# powVec_p8c2 <- as.numeric(lapply(LamdaVec_p8c2,powerCal, df = 2))


ggplot()+ 
    geom_line(mapping = aes(x = SSize_p2c2, y = powVec_p2c2), size = 1.5, color = "red") +
    geom_line(mapping = aes(x = SSize_p5c2, y = powVec_p5c2), size = 1.5, color = "blue") +
    geom_line(mapping = aes(x = SSize_p8c2, y = powVec_p8c2), size = 1.5, color = "green") +
    #geom_line(mapping = aes(x = SSize_p8c2, y = powVec_p8c2), size = 1.5, color = "purple") +
    theme(panel.background = element_rect(fill = "transparent"),
          panel.grid = element_line(color = "transparent"),
          axis.line = element_line(size = 1, colour = "black"),
          #axis.line.y = element_blank(),
          axis.text = element_text( color = "black"),
          #axis.text.y = element_blank(),
          #axis.ticks.y = element_blank(),
          text=element_text( family="Calibri",  size = 8),
          legend.spacing = unit(-17,'pt'),
          legend.margin = margin(t=0,b=0,unit='pt'),
          legend.background = element_blank())+
    xlab("N of PP")+
    scale_y_continuous(n.breaks = 6)+
    ylab("Power:Mt+Am")+
    geom_hline(yintercept = .8, linetype = 5, size = .8, color = "grey")
#annotate(geom = "text",x = 0.62, y =.92, label = "A = .7, C = .2", family="Calibri", color = "gray40",size = 3)+
#annotate(geom = "text",x = 0.73, y =.6, label = "A = .5, C = .2", family="Calibri", color = "gray40",size = 3)+
#annotate(geom = "text",x = 0.9, y =.6, label = "A = .3, C = .2", family="Calibri", color = "gray40",size = 3)
ggsave( "~/R-Project/BalancedPed/Simulations/p8/p8c2/graph1.png",g1,width = 6, height = 4.5,  type = "cairo-png", dpi = 900)
