# script to plot power plots
library(ggplot2)

powerCal <- function(lamda,df){
      1- pchisq(qchisq(1-.05, df), df, lamda)
      
}
my_palette <- c("#E69F00", 
                "#DDCC77", 
                "#999933", 
                "#A6CE39",
                
                "#E41A1C",
                "#882255", 
                "#984EA3",
                "#AA0499", 
                "#CC6677", 
                "#377EB8",
                "#56B4E9", 
                "#88CCEE",
                
                
                "#4DAF4A",            
                "#A9A9A9", 
                "#117A65")

num_ped <- 30
num_ped1 <- num_ped + 1

### create a new enviroment for one condition
env_p8c1 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c1/modelSmr.Rdata", envir = env_p8c1)

meanDiffLL_mtam_p8c1 <- env_p8c1$smr2$Minus2LogLikelihood - env_p8c1$smr1$Minus2LogLikelihood
lamdaUnit_p8c1 <- (meanDiffLL_mtam_p8c1/env_p8c1$smr1$numObs)  
SSize_p8c1 <- 0: num_ped
LamdaVec_p8c1 <- lamdaUnit_p8c1*SSize_p8c1
powVec_p8c1 <- as.numeric(lapply(LamdaVec_p8c1,powerCal, df = 2))

df_p8c1 <- data.frame(Nped = SSize_p8c1, 
                      power = powVec_p8c1, 
                      Combination = rep("power1", num_ped1))

### create a new enviroment for one condition
env_p8c2 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c2/modelSmr.Rdata", envir = env_p8c2)

meanDiffLL_mtam_p8c2 <- env_p8c2$smr2$Minus2LogLikelihood - env_p8c2$smr1$Minus2LogLikelihood
lamdaUnit_p8c2 <- meanDiffLL_mtam_p8c2/env_p8c2$smr1$numObs
SSize_p8c2 <- 0: num_ped
LamdaVec_p8c2 <- lamdaUnit_p8c2*SSize_p8c2
powVec_p8c2 <- as.numeric(lapply(LamdaVec_p8c2,powerCal, df = 2))

df_p8c2 <- data.frame(Nped = SSize_p8c2, 
                      power = powVec_p8c2, 
                      Combination = rep("power2", num_ped1))

### create a new enviroment for one condition
env_p8c3 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c3/modelSmr.Rdata", envir = env_p8c3)

meanDiffLL_mtam_p8c3 <- env_p8c3$smr2$Minus2LogLikelihood - env_p8c3$smr1$Minus2LogLikelihood
lamdaUnit_p8c3 <- (meanDiffLL_mtam_p8c3 * 1)/env_p8c3$smr1$numObs
SSize_p8c3 <- 0: num_ped
LamdaVec_p8c3 <- lamdaUnit_p8c3*SSize_p8c3
powVec_p8c3 <- as.numeric(lapply(LamdaVec_p8c3,powerCal, df = 2))

df_p8c3 <- data.frame(Nped = SSize_p8c3, 
                      power = powVec_p8c3, 
                      Combination = rep("power3", num_ped1))

### create a new enviroment for one condition
env_p8c4 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c4/modelSmr.Rdata", envir = env_p8c4)

meanDiffLL_mtam_p8c4 <- env_p8c4$smr2$Minus2LogLikelihood - env_p8c4$smr1$Minus2LogLikelihood
lamdaUnit_p8c4 <- meanDiffLL_mtam_p8c4/env_p8c4$smr1$numObs
SSize_p8c4 <- 0: num_ped
LamdaVec_p8c4 <- lamdaUnit_p8c4*SSize_p8c4
powVec_p8c4 <- as.numeric(lapply(LamdaVec_p8c4,powerCal, df = 2))

df_p8c4 <- data.frame(Nped = SSize_p8c4, 
                      power = powVec_p8c4, 
                      Combination = rep("power4", num_ped1))

### create a new enviroment for one condition
env_p8c5 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c5/modelSmr.Rdata", envir = env_p8c5)

meanDiffLL_mtam_p8c5 <- env_p8c5$smr2$Minus2LogLikelihood - env_p8c5$smr1$Minus2LogLikelihood
lamdaUnit_p8c5 <- meanDiffLL_mtam_p8c5* .965/env_p8c5$smr1$numObs
SSize_p8c5 <- 0: num_ped
LamdaVec_p8c5 <- lamdaUnit_p8c5*SSize_p8c5
powVec_p8c5 <- as.numeric(lapply(LamdaVec_p8c5,powerCal, df = 2))

df_p8c5 <- data.frame(Nped = SSize_p8c5, 
                      power = powVec_p8c5, 
                      Combination = rep("power5", num_ped1))

### create a new enviroment for one condition
env_p8c6 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c6/modelSmr.Rdata", envir = env_p8c6)

meanDiffLL_mtam_p8c6 <- env_p8c6$smr2$Minus2LogLikelihood - env_p8c6$smr1$Minus2LogLikelihood
lamdaUnit_p8c6 <- meanDiffLL_mtam_p8c6/env_p8c6$smr1$numObs
SSize_p8c6 <- 0: num_ped
LamdaVec_p8c6 <- lamdaUnit_p8c6*SSize_p8c6
powVec_p8c6 <- as.numeric(lapply(LamdaVec_p8c6,powerCal, df = 2))

df_p8c6 <- data.frame(Nped = SSize_p8c6, 
                      power = powVec_p8c6, 
                      Combination = rep("power6", num_ped1))

### create a new enviroment for one condition
env_p8c7 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c7/modelSmr.Rdata", envir = env_p8c7)

meanDiffLL_mtam_p8c7 <- env_p8c7$smr2$Minus2LogLikelihood - env_p8c7$smr1$Minus2LogLikelihood
lamdaUnit_p8c7 <- meanDiffLL_mtam_p8c7* .95/env_p8c7$smr1$numObs
SSize_p8c7 <- 0: num_ped
LamdaVec_p8c7 <- lamdaUnit_p8c7*SSize_p8c7
powVec_p8c7 <- as.numeric(lapply(LamdaVec_p8c7,powerCal, df = 2))

df_p8c7 <- data.frame(Nped = SSize_p8c7, 
                      power = powVec_p8c7, 
                      Combination = rep("power7", num_ped1))

### create a new enviroment for one condition
env_p8c8 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c8/modelSmr.Rdata", envir = env_p8c8)

meanDiffLL_mtam_p8c8 <- env_p8c8$smr2$Minus2LogLikelihood - env_p8c8$smr1$Minus2LogLikelihood
lamdaUnit_p8c8 <- meanDiffLL_mtam_p8c8* .95/env_p8c8$smr1$numObs
SSize_p8c8 <- 0: num_ped
LamdaVec_p8c8 <- lamdaUnit_p8c8*SSize_p8c8
powVec_p8c8 <- as.numeric(lapply(LamdaVec_p8c8,powerCal, df = 2))

df_p8c8 <- data.frame(Nped = SSize_p8c8, 
                      power = powVec_p8c8, 
                      Combination = rep("power8", num_ped1))

### create a new enviroment for one condition
env_p8c9 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c9/modelSmr.Rdata", envir = env_p8c9)

meanDiffLL_mtam_p8c9 <- env_p8c9$smr2$Minus2LogLikelihood - env_p8c9$smr1$Minus2LogLikelihood
lamdaUnit_p8c9 <- meanDiffLL_mtam_p8c9/env_p8c9$smr1$numObs
SSize_p8c9 <- 0: num_ped
LamdaVec_p8c9 <- lamdaUnit_p8c9*SSize_p8c9
powVec_p8c9 <- as.numeric(lapply(LamdaVec_p8c9,powerCal, df = 2))

df_p8c9 <- data.frame(Nped = SSize_p8c9, 
                      power = powVec_p8c9, 
                      Combination = rep("power9", num_ped1))

### create a new enviroment for one condition
env_p8c10 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c10/modelSmr.Rdata", envir = env_p8c10)

meanDiffLL_mtam_p8c10 <- env_p8c10$smr2$Minus2LogLikelihood - env_p8c10$smr1$Minus2LogLikelihood
lamdaUnit_p8c10 <- meanDiffLL_mtam_p8c10/env_p8c10$smr1$numObs
SSize_p8c10 <- 0: num_ped
LamdaVec_p8c10 <- lamdaUnit_p8c10*SSize_p8c10
powVec_p8c10 <- as.numeric(lapply(LamdaVec_p8c10,powerCal, df = 2))

df_p8c10 <- data.frame(Nped = SSize_p8c10, 
                       power = powVec_p8c10, 
                       Combination = rep("power10", num_ped1))


### create a new enviroment for one condition
env_p8c11 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c11/modelSmr.Rdata", envir = env_p8c11)

meanDiffLL_mtam_p8c11 <- env_p8c11$smr2$Minus2LogLikelihood - env_p8c11$smr1$Minus2LogLikelihood
lamdaUnit_p8c11 <- meanDiffLL_mtam_p8c11/env_p8c11$smr1$numObs
SSize_p8c11 <- 0: num_ped
LamdaVec_p8c11 <- lamdaUnit_p8c11*SSize_p8c11
powVec_p8c11 <- as.numeric(lapply(LamdaVec_p8c11,powerCal, df = 2))

df_p8c11 <- data.frame(Nped = SSize_p8c11, 
                       power = powVec_p8c11, 
                       Combination = rep("power11", num_ped1))

### create a new enviroment for one condition
env_p8c12 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c12/modelSmr.Rdata", envir = env_p8c12)

meanDiffLL_mtam_p8c12 <- env_p8c12$smr2$Minus2LogLikelihood - env_p8c12$smr1$Minus2LogLikelihood
lamdaUnit_p8c12 <- meanDiffLL_mtam_p8c12/env_p8c12$smr1$numObs
SSize_p8c12 <- 0: num_ped 
LamdaVec_p8c12 <- lamdaUnit_p8c12*SSize_p8c12
powVec_p8c12 <- as.numeric(lapply(LamdaVec_p8c12,powerCal, df = 2))

df_p8c12 <- data.frame(Nped = SSize_p8c12, 
                       power = powVec_p8c12, 
                       Combination = rep("power12", num_ped1))

### create a data frame for graphs
df_p8 <- rbind(df_p8c1, df_p8c2,df_p8c3,df_p8c4, df_p8c5,df_p8c6,df_p8c7, df_p8c8,df_p8c9,df_p8c10, df_p8c11, df_p8c12)
df_p8$Combination <- as.factor(df_p8$Combination)


g1 <-ggplot(data = df_p8)+ geom_line(mapping = aes(x = Nped, y = power, color= Combination), size = 1, alpha = .8) +
      scale_color_manual(values=my_palette[1:12],
                         name="Variance Combinations",
                         breaks=c("power1", "power4", "power7", "power12", "power2","power11","power5","power8","power10","power3","power6","power9"),
                         labels=c("a\u00B2 = .60, mt\u00B2 = .10, e\u00B2 = .05",
                                  "a\u00B2 = .40, mt\u00B2 = .10, e\u00B2 = .25",
                                  "a\u00B2 = .20, mt\u00B2 = .10, e\u00B2 = .45",
                                  "a\u00B2 = .40, mt\u00B2 = .10, e\u00B2 = .20, j\u00B2 = .1",
                                  "a\u00B2 = .60, mt\u00B2 = .05, e\u00B2 = .10",
                                  "a\u00B2 = .40, mt\u00B2 = .05, e\u00B2 = .20, d\u00B2 = .10", 
                                  "a\u00B2 = .40, mt\u00B2 = .05, e\u00B2 = .30",
                                  "a\u00B2 = .20, mt\u00B2 = .05, e\u00B2 = .50",
                                  "a\u00B2 = .40, mt\u00B2 = .05, e\u00B2 = .20, cn\u00B2 = .10",
                                  "a\u00B2 = .60, mt\u00B2 = .01, e\u00B2 = .14",
                                  "a\u00B2 = .40, mt\u00B2 = .01, e\u00B2 = .34",
                                  "a\u00B2 = .20, mt\u00B2 = .01, e\u00B2 = .54"
                         )
      )+
      theme(plot.margin = margin(5,200,5,5,"pt"),
            panel.background = element_rect(fill = "transparent"),
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
            legend.position=c(1.2,.5))+
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

ggsave( "~/R-Project/BalancedPed/Graphs/Appendix/p8all.png",g1, width = 10, height = 4.5,  type = "cairo-png", dpi = 900)

rm(list = ls())
