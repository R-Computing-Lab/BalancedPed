library(ggplot2)

powerCal <- function(lamda,df){
    1- pchisq(qchisq(1-.05, df), df, lamda)
    
}
my_palette <- c( "#E41A1C", "#332288", "#E69F00", "#117A65", "#DDCC77", "#377EB8",  "#4DAF4A",  "#56B4E9", "#A6CE39", "#A9A9A9","#882255","#88CCEE", "#Cc5677",  "#AA4499",   "#999933",  "#984EA3")

N_indi <- 7500
### create a new enviroment for one condition
env_p1c5 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p1/p1c5/modelSmr.Rdata", envir = env_p1c5)
#load("~/BalancedPed/Simulations/p1/p1c5/modelSmr.Rdata", envir = env_p1c5)

meanDiffLL_mtam_p1c5 <- env_p1c5$smr2$Minus2LogLikelihood - env_p1c5$smr1$Minus2LogLikelihood
PN <- ncol(env_p1c5[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p1c5[["smr1"]][["dataSummary"]])
lamdaUnit_p1c5 <- meanDiffLL_mtam_p1c5/PN
SSize_p1c5 <- 1: N_indi
LamdaVec_p1c5 <- lamdaUnit_p1c5*SSize_p1c5
powVec_p1c5 <- as.numeric(lapply(LamdaVec_p1c5,powerCal, df = 2))

df_p1c5 <- data.frame(Nped = SSize_p1c5, 
                      power = powVec_p1c5, 
                      Combination = rep("power1", N_indi))


### create a new enviroment for one condition
env_p2c5 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p2/p2c5/modelSmr.Rdata", envir = env_p2c5)
#load("~/BalancedPed/Simulations/p2/p2c5/modelSmr.Rdata", envir = env_p2c5)

meanDiffLL_mtam_p2c5 <- env_p2c5$smr2$Minus2LogLikelihood - env_p2c5$smr1$Minus2LogLikelihood
PN <- ncol(env_p2c5[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p2c5[["smr1"]][["dataSummary"]])
lamdaUnit_p2c5 <- meanDiffLL_mtam_p2c5/PN
SSize_p2c5 <- 1: N_indi
LamdaVec_p2c5 <- lamdaUnit_p2c5*SSize_p2c5
powVec_p2c5 <- as.numeric(lapply(LamdaVec_p2c5,powerCal, df = 2))

df_p2c5 <- data.frame(Nped = SSize_p2c5, 
                      power = powVec_p2c5, 
                      Combination = rep("power2", N_indi))

### create a new enviroment for one condition
env_p3c5 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p3/p3c5/modelSmr.Rdata", envir = env_p3c5)
#load("~/BalancedPed/Simulations/p3/p3c5/modelSmr.Rdata", envir = env_p3c5)

meanDiffLL_mtam_p3c5 <- env_p3c5$smr2$Minus2LogLikelihood - env_p3c5$smr1$Minus2LogLikelihood
PN <- ncol(env_p3c5[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p3c5[["smr1"]][["dataSummary"]])
lamdaUnit_p3c5 <- meanDiffLL_mtam_p3c5/PN
SSize_p3c5 <- 1: N_indi
LamdaVec_p3c5 <- lamdaUnit_p3c5*SSize_p3c5
powVec_p3c5 <- as.numeric(lapply(LamdaVec_p3c5,powerCal, df = 2))

df_p3c5 <- data.frame(Nped = SSize_p3c5, 
                      power = powVec_p3c5, 
                      Combination = rep("power3", N_indi))

### create a new enviroment for one condition
env_p4c5 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p4/p4c5/modelSmr.Rdata", envir = env_p4c5)
#load("~/BalancedPed/Simulations/p4/p4c5/modelSmr.Rdata", envir = env_p4c5)

meanDiffLL_mtam_p4c5 <- env_p4c5$smr2$Minus2LogLikelihood - env_p4c5$smr1$Minus2LogLikelihood
PN <- ncol(env_p4c5[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p4c5[["smr1"]][["dataSummary"]])
lamdaUnit_p4c5 <- meanDiffLL_mtam_p4c5/PN
SSize_p4c5 <- 1: N_indi
LamdaVec_p4c5 <- lamdaUnit_p4c5*SSize_p4c5
powVec_p4c5 <- as.numeric(lapply(LamdaVec_p4c5,powerCal, df = 2))

df_p4c5 <- data.frame(Nped = SSize_p4c5, 
                      power = powVec_p4c5, 
                      Combination = rep("power4", N_indi))


### create a new enviroment for one condition
env_p5c5 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p5/p5c5/modelSmr.Rdata", envir = env_p5c5)
#load("~/BalancedPed/Simulations/p5/p5c5/modelSmr.Rdata", envir = env_p5c5)

meanDiffLL_mtam_p5c5 <- env_p5c5$smr2$Minus2LogLikelihood - env_p5c5$smr1$Minus2LogLikelihood
PN <- ncol(env_p5c5[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p5c5[["smr1"]][["dataSummary"]])
lamdaUnit_p5c5 <- meanDiffLL_mtam_p5c5/PN
SSize_p5c5 <- 1: N_indi
LamdaVec_p5c5 <- lamdaUnit_p5c5*SSize_p5c5
powVec_p5c5 <- as.numeric(lapply(LamdaVec_p5c5,powerCal, df = 2))

df_p5c5 <- data.frame(Nped = SSize_p5c5, 
                      power = powVec_p5c5, 
                      Combination = rep("power5", N_indi))

### create a new enviroment for one condition
env_p6c5 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p6/p6c5/modelSmr.Rdata", envir = env_p6c5)
#load("~/BalancedPed/Simulations/p6/p6c5/modelSmr.Rdata", envir = env_p6c5)

meanDiffLL_mtam_p6c5 <- env_p6c5$smr2$Minus2LogLikelihood - env_p6c5$smr1$Minus2LogLikelihood
PN <- ncol(env_p6c5[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p6c5[["smr1"]][["dataSummary"]])
lamdaUnit_p6c5 <- meanDiffLL_mtam_p6c5/PN
SSize_p6c5 <- 1: N_indi
LamdaVec_p6c5 <- lamdaUnit_p6c5*SSize_p6c5
powVec_p6c5 <- as.numeric(lapply(LamdaVec_p6c5,powerCal, df = 2))

df_p6c5 <- data.frame(Nped = SSize_p6c5, 
                      power = powVec_p6c5, 
                      Combination = rep("power6", N_indi))

### create a new enviroment for one condition
env_p7c5 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p7/p7c5/modelSmr.Rdata", envir = env_p7c5)
#load("~/BalancedPed/Simulations/p7/p7c5/modelSmr.Rdata", envir = env_p7c5)

meanDiffLL_mtam_p7c5 <- env_p7c5$smr2$Minus2LogLikelihood - env_p7c5$smr1$Minus2LogLikelihood
PN <- ncol(env_p7c5[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p7c5[["smr1"]][["dataSummary"]])
lamdaUnit_p7c5 <- meanDiffLL_mtam_p7c5/PN
SSize_p7c5 <- 1: N_indi
LamdaVec_p7c5 <- lamdaUnit_p7c5*SSize_p7c5
powVec_p7c5 <- as.numeric(lapply(LamdaVec_p7c5,powerCal, df = 2))

df_p7c5 <- data.frame(Nped = SSize_p7c5, 
                      power = powVec_p7c5, 
                      Combination = rep("power7", N_indi))

### create a new enviroment for one condition
env_p8c5 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c5/modelSmr.Rdata", envir = env_p8c5)
#load("~/BalancedPed/Simulations/p8/p8c5/modelSmr.Rdata", envir = env_p8c5)

meanDiffLL_mtam_p8c5 <- env_p8c5$smr2$Minus2LogLikelihood - env_p8c5$smr1$Minus2LogLikelihood
PN <- ncol(env_p8c5[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p8c5[["smr1"]][["dataSummary"]])
lamdaUnit_p8c5 <- meanDiffLL_mtam_p8c5/PN
SSize_p8c5 <- 1: N_indi
LamdaVec_p8c5 <- lamdaUnit_p8c5*SSize_p8c5
powVec_p8c5 <- as.numeric(lapply(LamdaVec_p8c5,powerCal, df = 2))

df_p8c5 <- data.frame(Nped = SSize_p8c5, 
                      power = powVec_p8c5, 
                      Combination = rep("power8", N_indi))

### create a new enviroment for one condition
env_p9c5 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p9/p9c5/modelSmr.Rdata", envir = env_p9c5)
#load("~/BalancedPed/Simulations/p9/p9c5/modelSmr.Rdata", envir = env_p9c5)

meanDiffLL_mtam_p9c5 <- env_p9c5$smr2$Minus2LogLikelihood - env_p9c5$smr1$Minus2LogLikelihood
PN <- ncol(env_p9c5[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p9c5[["smr1"]][["dataSummary"]])
lamdaUnit_p9c5 <- meanDiffLL_mtam_p9c5/PN
SSize_p9c5 <- 1: N_indi
LamdaVec_p9c5 <- lamdaUnit_p9c5*SSize_p9c5
powVec_p9c5 <- as.numeric(lapply(LamdaVec_p9c5,powerCal, df = 2))

df_p9c5 <- data.frame(Nped = SSize_p9c5, 
                      power = powVec_p9c5, 
                      Combination = rep("power9", N_indi))

### create a new enviroment for one condition
env_p10c5 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p10/p10c5/modelSmr.Rdata", envir = env_p10c5)
#load("~/BalancedPed/Simulations/p10/p10c5/modelSmr.Rdata", envir = env_p10c5)

meanDiffLL_mtam_p10c5 <- env_p10c5$smr2$Minus2LogLikelihood - env_p10c5$smr1$Minus2LogLikelihood
PN <- ncol(env_p10c5[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p10c5[["smr1"]][["dataSummary"]])
lamdaUnit_p10c5 <- meanDiffLL_mtam_p10c5/PN
SSize_p10c5 <- 1: N_indi
LamdaVec_p10c5 <- lamdaUnit_p10c5*SSize_p10c5
powVec_p10c5 <- as.numeric(lapply(LamdaVec_p10c5,powerCal, df = 2))

df_p10c5 <- data.frame(Nped = SSize_p10c5, 
                       power = powVec_p10c5, 
                       Combination = rep("power10", N_indi))

### create a new enviroment for one condition
env_p11c5 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p11/p11c5/modelSmr.Rdata", envir = env_p11c5)
#load("~/BalancedPed/Simulations/p11/p11c5/modelSmr.Rdata", envir = env_p11c5)

meanDiffLL_mtam_p11c5 <- env_p11c5$smr2$Minus2LogLikelihood - env_p11c5$smr1$Minus2LogLikelihood
PN <- ncol(env_p11c5[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p11c5[["smr1"]][["dataSummary"]])
lamdaUnit_p11c5 <- meanDiffLL_mtam_p11c5/PN
SSize_p11c5 <- 1: N_indi
LamdaVec_p11c5 <- lamdaUnit_p11c5*SSize_p11c5
powVec_p11c5 <- as.numeric(lapply(LamdaVec_p11c5,powerCal, df = 2))

df_p11c5 <- data.frame(Nped = SSize_p11c5, 
                       power = powVec_p11c5, 
                       Combination = rep("power11", N_indi))
# ### create a new enviroment for one condition
# env_p10c5 <- new.env()
# load("~/R-Project/BalancedPed/Simulations/p10/p10c5/modelSmr.Rdata", envir = env_p10c5)
# #load("~/BalancedPed/Simulations/p10/p10c5/modelSmr.Rdata", envir = env_p10c5)
# 
# meanDiffLL_mtam_p10c5 <- env_p10c5$smr2$Minus2LogLikelihood - env_p10c5$smr1$Minus2LogLikelihood
# PN <- ncol(env_p10c5[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p10c5[["smr1"]][["dataSummary"]])
# lamdaUnit_p10c5 <- meanDiffLL_mtam_p10c5/PN
# SSize_p10c5 <- 1: N_indi
# LamdaVec_p10c5 <- lamdaUnit_p10c5*SSize_p10c5
# powVec_p10c5 <- as.numeric(lapply(LamdaVec_p10c5,powerCal, df = 2))
# 
# df_p10c5 <- data.frame(Nped = SSize_p10c5, 
#                        power = powVec_p10c5, 
#                        Combination = rep("power4", N_indi))

### create a data frame for graphs
df_c5 <- rbind(df_p1c5, df_p2c5, df_p3c5, df_p4c5, df_p5c5, df_p6c5, df_p7c5, df_p8c5, df_p9c5, df_p10c5, df_p11c5)
df_c5$Combination <- as.factor(df_c5$Combination)


g1 <-ggplot(data = df_c5)+ geom_line(mapping = aes(x = Nped, y = power, color= Combination), size = 1.5) +
    scale_color_manual(values=my_palette[1:11],
                       name="Pedigree Structures",
                       breaks=c("power1", "power2", "power3","power4","power5","power6","power7","power8","power9","power10","power11"),
                       labels=c("k = 2, G = 4, m = 15",
                                "k = 3, G = 4, m = 29", 
                                "k = 4, G = 4, m = 56",
                                "k = 2, G = 5, m = 22",
                                "k = 3, G = 5, m = 61",
                                "k = 3, G = 5, m = 61",
                                "k = 3, G = 6, m = 125",
                                "k = 3, G = 8, m = 509",
                                "k = 4, G = 4, m = 75",
                                "k = 8, G = 4, m = 316",
                                "k = 6, G = 6, m = 2388")
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
          legend.position=c(.8,.4))+
    xlab("N of Individuals")+
    scale_y_continuous(n.breaks = 6)+
    ylab("Power (mt\u00B2+j\u00B2)")+
    geom_hline(yintercept = .8, linetype = 5, size = .8, color = "grey")
# +
# annotate(geom = "text",x = 0.62, y =.92, label = "a\u00B2 = .6", family="Calibri", color = "gray40",size = 3)+
# annotate(geom = "text",x = 0.73, y =.6, label = "a\u00B2 = .4", family="Calibri", color = "gray40",size = 3)+
# annotate(geom = "text",x = 0.9, y =.6, label = "a\u00B2 = .2", family="Calibri", color = "gray40",size = 3)+ 
# annotate(geom = "text",x = 0.9, y =.6, label = "a\u00B2 = .4, d\u00B2 = .1", family="Calibri", color = "gray40",size = 3)
g1

ggsave( "~/R-Project/BalancedPed/Graphs/Appendix/c5all.png",g1, width = 6, height = 4.5,  type = "cairo-png", dpi = 900)

rm(list = ls())
