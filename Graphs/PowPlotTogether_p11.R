# script to plot power plots
library(ggplot2)

powerCal <- function(lamda,df){
      1- pchisq(qchisq(1-.05, df), df, lamda)
      
}
my_palette <- c("#377EB8", "#E69F00", "#4DAF4A", "#984EA3",  "#56B4E9", "#A6CE39", "#A9A9A9", "#E41A1C")
num_ped <- 5

### create a new enviroment for one condition
env_p11c2 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p11/p11c2/modelSmr.Rdata", envir = env_p11c2)

meanDiffLL_mtam_p11c2 <- env_p11c2$smr2$Minus2LogLikelihood - env_p11c2$smr1$Minus2LogLikelihood
lamdaUnit_p11c2 <- meanDiffLL_mtam_p11c2/env_p11c2$smr1$numObs
SSize_p11c2 <- 1: num_ped
LamdaVec_p11c2 <- lamdaUnit_p11c2*SSize_p11c2
powVec_p11c2 <- as.numeric(lapply(LamdaVec_p11c2,powerCal, df = 2))

df_p11c2 <- data.frame(Nped = SSize_p11c2, 
                      power = powVec_p11c2, 
                      Combination = rep("power1", num_ped))

### create a new enviroment for one condition
env_p11c5 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p11/p11c5/modelSmr.Rdata", envir = env_p11c5)

meanDiffLL_mtam_p11c5 <- env_p11c5$smr2$Minus2LogLikelihood - env_p11c5$smr1$Minus2LogLikelihood
lamdaUnit_p11c5 <- meanDiffLL_mtam_p11c5/env_p11c5$smr1$numObs
SSize_p11c5 <- 1: num_ped
LamdaVec_p11c5 <- lamdaUnit_p11c5*SSize_p11c5
powVec_p11c5 <- as.numeric(lapply(LamdaVec_p11c5,powerCal, df = 2))

df_p11c5 <- data.frame(Nped = SSize_p11c5, 
                      power = powVec_p11c5, 
                      Combination = rep("power2", num_ped))

### create a new enviroment for one condition
env_p11c8 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p11/p11c8/modelSmr.Rdata", envir = env_p11c8)

meanDiffLL_mtam_p11c8 <- env_p11c8$smr2$Minus2LogLikelihood - env_p11c8$smr1$Minus2LogLikelihood
lamdaUnit_p11c8 <- meanDiffLL_mtam_p11c8/env_p11c8$smr1$numObs
SSize_p11c8 <- 1: num_ped
LamdaVec_p11c8 <- lamdaUnit_p11c8*SSize_p11c8
powVec_p11c8 <- as.numeric(lapply(LamdaVec_p11c8,powerCal, df = 2))

df_p11c8 <- data.frame(Nped = SSize_p11c8, 
                      power = powVec_p11c8, 
                      Combination = rep("power3", num_ped))

### create a new enviroment for one condition
env_p11c11 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p11/p11c11/modelSmr.Rdata", envir = env_p11c11)

meanDiffLL_mtam_p11c11 <- env_p11c11$smr2$Minus2LogLikelihood - env_p11c11$smr1$Minus2LogLikelihood
lamdaUnit_p11c11 <- meanDiffLL_mtam_p11c11/env_p11c11$smr1$numObs
SSize_p11c11 <- 1: num_ped
LamdaVec_p11c11 <- lamdaUnit_p11c11*SSize_p11c11
powVec_p11c11 <- as.numeric(lapply(LamdaVec_p11c11,powerCal, df = 2))

df_p11c11 <- data.frame(Nped = SSize_p11c11, 
                       power = powVec_p11c11, 
                       Combination = rep("power4", num_ped))

### create a data frame for graphs
df_p11 <- rbind(df_p11c2, df_p11c5, df_p11c8, df_p11c11)
df_p11$Combination <- as.factor(df_p11$Combination)


g1 <-ggplot(data = df_p11)+ geom_line(mapping = aes(x = Nped, y = power, color= Combination), size = 1.5) +
      scale_color_manual(values=my_palette[1:4],
                         name="Variance Combinations",
                         breaks=c("power1", "power2", "power3","power4"),
                         labels=c("a\u00B2 = .6, mt\u00B2 = .05, j\u00B2 = .05",
                                  "a\u00B2 = .4, mt\u00B2 = .05, j\u00B2 = .05", 
                                  "a\u00B2 = .2, mt\u00B2 = .05, j\u00B2 = .05", 
                                  "a\u00B2 = .4, mt\u00B2 = .05, j\u00B2 = .05, d\u00B2 = .1")
      )+
      theme(panel.background = element_rect(fill = "transparent"),
            panel.grid = element_line(color = "transparent"),
            axis.line = element_line(size = 1, colour = "black"),
            #axis.line.y = element_blank(),
            axis.text = element_text( color = "black"),
            #axis.text.y = element_blank(),
            #axis.ticks.y = element_blank(),
            text=element_text( family="Calibri",  size = 12),
            legend.spacing = unit(-17,'pt'),
            legend.margin = margin(t=0,b=0,unit='pt'),
            legend.background = element_rect(),
            legend.position=c(.8,.2))+
      xlab("N of Pedigrees")+
      scale_y_continuous(n.breaks = 6)+
      ylab("Power (mt\u00B2+j\u00B2)")+
      geom_hline(yintercept = .8, linetype = 5, size = .8, color = "grey")
# +
# annotate(geom = "text",x = 0.62, y =.92, label = "a\u00B2 = .6", family="Calibri", color = "gray40",size = 3)+
# annotate(geom = "text",x = 0.73, y =.6, label = "a\u00B2 = .4", family="Calibri", color = "gray40",size = 3)+
# annotate(geom = "text",x = 0.9, y =.6, label = "a\u00B2 = .2", family="Calibri", color = "gray40",size = 3)+ 
# annotate(geom = "text",x = 0.9, y =.6, label = "a\u00B2 = .4, d\u00B2 = .1", family="Calibri", color = "gray40",size = 3)
g1

ggsave( "~/R-Project/BalancedPed/Simulations/p11/graph1.png",g1, width = 6, height = 4.5,  type = "cairo-png", dpi = 900)
rm(list = ls())