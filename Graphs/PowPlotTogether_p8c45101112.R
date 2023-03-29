# script to plot power plots
library(ggplot2)

powerCal <- function(lamda,df){
      1- pchisq(qchisq(1-.05, df), df, lamda)
      
}
my_palette <- c(
      
      "#A6CE39",
      "#4DAF4A",            
      "#117A65",
      "#E41A1C",
      "#882255", 
      "#984EA3",
      "#AA0499", 
      "#CC6677", 
      "#377EB8",
      "#56B4E9", 
      "#88CCEE",
      "#A9A9A9",
      "#E69F00", 
      "#DDCC77", 
      "#999933" 
)
num_ped <- 20

### create a new enviroment for one condition
env_p8c4 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c4/modelSmr.Rdata", envir = env_p8c4)

meanDiffLL_mtam_p8c4 <- env_p8c4$smr2$Minus2LogLikelihood - env_p8c4$smr1$Minus2LogLikelihood
lamdaUnit_p8c4 <- meanDiffLL_mtam_p8c4/env_p8c4$smr1$numObs
SSize_p8c4 <- 0: (num_ped-1)
LamdaVec_p8c4 <- lamdaUnit_p8c4*SSize_p8c4
powVec_p8c4 <- as.numeric(lapply(LamdaVec_p8c4,powerCal, df = 2))

df_p8c4 <- data.frame(Nped = SSize_p8c4, 
                      power = powVec_p8c4, 
                      Combination = rep("power1", num_ped))

### create a new enviroment for one condition
env_p8c5 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c5/modelSmr.Rdata", envir = env_p8c5)

meanDiffLL_mtam_p8c5 <- env_p8c5$smr2$Minus2LogLikelihood - env_p8c5$smr1$Minus2LogLikelihood
lamdaUnit_p8c5 <- meanDiffLL_mtam_p8c5/env_p8c5$smr1$numObs
SSize_p8c5 <- 0: (num_ped-1)
LamdaVec_p8c5 <- lamdaUnit_p8c5*SSize_p8c5
powVec_p8c5 <- as.numeric(lapply(LamdaVec_p8c5,powerCal, df = 2))

df_p8c5 <- data.frame(Nped = SSize_p8c5, 
                      power = powVec_p8c5, 
                      Combination = rep("power2", num_ped))

### create a new enviroment for one condition
env_p8c10 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c10/modelSmr.Rdata", envir = env_p8c10)

meanDiffLL_mtam_p8c10 <- env_p8c10$smr2$Minus2LogLikelihood - env_p8c10$smr1$Minus2LogLikelihood
lamdaUnit_p8c10 <- meanDiffLL_mtam_p8c10/env_p8c10$smr1$numObs
SSize_p8c10 <- 0: (num_ped-1)
LamdaVec_p8c10 <- lamdaUnit_p8c10*SSize_p8c10
powVec_p8c10 <- as.numeric(lapply(LamdaVec_p8c10,powerCal, df = 2))

df_p8c10 <- data.frame(Nped = SSize_p8c10, 
                       power = powVec_p8c10, 
                       Combination = rep("power3", num_ped))

### create a new enviroment for one condition
env_p8c11 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c11/modelSmr.Rdata", envir = env_p8c11)

meanDiffLL_mtam_p8c11 <- env_p8c11$smr2$Minus2LogLikelihood - env_p8c11$smr1$Minus2LogLikelihood
lamdaUnit_p8c11 <- meanDiffLL_mtam_p8c11/env_p8c11$smr1$numObs
SSize_p8c11 <- 0: (num_ped-1)
LamdaVec_p8c11 <- lamdaUnit_p8c11*SSize_p8c11
powVec_p8c11 <- as.numeric(lapply(LamdaVec_p8c11,powerCal, df = 2))

df_p8c11 <- data.frame(Nped = SSize_p8c11,
                       power = powVec_p8c11,
                       Combination = rep("power4", num_ped))

### create a new enviroment for one condition
env_p8c12 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c12/modelSmr.Rdata", envir = env_p8c12)

meanDiffLL_mtam_p8c12 <- env_p8c12$smr2$Minus2LogLikelihood - env_p8c12$smr1$Minus2LogLikelihood
lamdaUnit_p8c12 <- meanDiffLL_mtam_p8c12/env_p8c12$smr1$numObs
SSize_p8c12 <- 0: (num_ped-1)
LamdaVec_p8c12 <- lamdaUnit_p8c12*SSize_p8c12
powVec_p8c12 <- as.numeric(lapply(LamdaVec_p8c12,powerCal, df = 2))

df_p8c12 <- data.frame(Nped = SSize_p8c12,
                       power = powVec_p8c12,
                       Combination = rep("power5", num_ped))

### create a data frame for graphs
df_p8 <- rbind(df_p8c4, df_p8c5, df_p8c10, df_p8c11, df_p8c12)
df_p8$Combination <- as.factor(df_p8$Combination)


g1 <-ggplot(data = df_p8)+ geom_line(mapping = aes(x = Nped, y = power, color= Combination), size = 1.5) +
      scale_color_manual(values=my_palette[1:5],
                         name="Variance Combinations",
                         breaks=c("power2", "power3", "power4", "power5", "power1"),
                         labels=c("mt\u00B2 = .05, cn\u00B2 = .10, d\u00B2 = 0, j\u00B2 = .05",
                                  "mt\u00B2 = .05, cn\u00B2 = .20, d\u00B2 = 0, j\u00B2 = .05", 
                                  "mt\u00B2 = .05, cn\u00B2 = .10, d\u00B2 = .10, j\u00B2 = .05",
                                  "mt\u00B2 = .10, cn\u00B2 = .10, d\u00B2 = 0, j\u00B2 = .10",
                                  "mt\u00B2 = .10, cn\u00B2 = .10, d\u00B2 = 0, j\u00B2 = .05")
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

ggsave( "~/R-Project/BalancedPed/Simulations/p8/graph3.png",g1, width = 6, height = 4.5,  type = "cairo-png", dpi = 900)
#rm(list = ls())