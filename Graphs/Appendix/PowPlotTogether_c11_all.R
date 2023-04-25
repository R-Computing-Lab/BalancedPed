library(ggplot2)

powerCal <- function(lamda,df){
    1- pchisq(qchisq(1-.05, df), df, lamda)
    
}
my_palette <- c( "#E41A1C", "#332288", "#E69F00", "#117A65", "#DDCC77", "#377EB8",  "#4DAF4A",  "#56B4E9", "#A6CE39", "#A9A9A9","#882255","#88CCEE", "#Cc1677",  "#AA4499",   "#999933",  "#984EA3")

N_indi <- 7500
### create a new enviroment for one condition
env_p1c11 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p1/p1c11/modelSmr.Rdata", envir = env_p1c11)
#load("~/BalancedPed/Simulations/p1/p1c11/modelSmr.Rdata", envir = env_p1c11)

meanDiffLL_mtam_p1c11 <- env_p1c11$smr2$Minus2LogLikelihood - env_p1c11$smr1$Minus2LogLikelihood
PN <- ncol(env_p1c11[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p1c11[["smr1"]][["dataSummary"]])
lamdaUnit_p1c11 <- meanDiffLL_mtam_p1c11/PN
SSize_p1c11 <- 1: N_indi
LamdaVec_p1c11 <- lamdaUnit_p1c11*SSize_p1c11
powVec_p1c11 <- as.numeric(lapply(LamdaVec_p1c11,powerCal, df = 2))

df_p1c11 <- data.frame(Nped = SSize_p1c11, 
                      power = powVec_p1c11, 
                      Combination = rep("power1", N_indi))


### create a new enviroment for one condition
env_p2c11 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p2/p2c11/modelSmr.Rdata", envir = env_p2c11)
#load("~/BalancedPed/Simulations/p2/p2c11/modelSmr.Rdata", envir = env_p2c11)

meanDiffLL_mtam_p2c11 <- env_p2c11$smr2$Minus2LogLikelihood - env_p2c11$smr1$Minus2LogLikelihood
PN <- ncol(env_p2c11[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p2c11[["smr1"]][["dataSummary"]])
lamdaUnit_p2c11 <- meanDiffLL_mtam_p2c11/PN
SSize_p2c11 <- 1: N_indi
LamdaVec_p2c11 <- lamdaUnit_p2c11*SSize_p2c11
powVec_p2c11 <- as.numeric(lapply(LamdaVec_p2c11,powerCal, df = 2))

df_p2c11 <- data.frame(Nped = SSize_p2c11, 
                      power = powVec_p2c11, 
                      Combination = rep("power2", N_indi))

### create a new enviroment for one condition
env_p3c11 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p3/p3c11/modelSmr.Rdata", envir = env_p3c11)
#load("~/BalancedPed/Simulations/p3/p3c11/modelSmr.Rdata", envir = env_p3c11)

meanDiffLL_mtam_p3c11 <- env_p3c11$smr2$Minus2LogLikelihood - env_p3c11$smr1$Minus2LogLikelihood
PN <- ncol(env_p3c11[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p3c11[["smr1"]][["dataSummary"]])
lamdaUnit_p3c11 <- meanDiffLL_mtam_p3c11/PN
SSize_p3c11 <- 1: N_indi
LamdaVec_p3c11 <- lamdaUnit_p3c11*SSize_p3c11
powVec_p3c11 <- as.numeric(lapply(LamdaVec_p3c11,powerCal, df = 2))

df_p3c11 <- data.frame(Nped = SSize_p3c11, 
                      power = powVec_p3c11, 
                      Combination = rep("power3", N_indi))

### create a new enviroment for one condition
env_p4c11 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p4/p4c11/modelSmr.Rdata", envir = env_p4c11)
#load("~/BalancedPed/Simulations/p4/p4c11/modelSmr.Rdata", envir = env_p4c11)

meanDiffLL_mtam_p4c11 <- env_p4c11$smr2$Minus2LogLikelihood - env_p4c11$smr1$Minus2LogLikelihood
PN <- ncol(env_p4c11[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p4c11[["smr1"]][["dataSummary"]])
lamdaUnit_p4c11 <- meanDiffLL_mtam_p4c11/PN
SSize_p4c11 <- 1: N_indi
LamdaVec_p4c11 <- lamdaUnit_p4c11*SSize_p4c11
powVec_p4c11 <- as.numeric(lapply(LamdaVec_p4c11,powerCal, df = 2))

df_p4c11 <- data.frame(Nped = SSize_p4c11, 
                      power = powVec_p4c11, 
                      Combination = rep("power4", N_indi))


### create a new enviroment for one condition
env_p5c11 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p5/p5c11/modelSmr.Rdata", envir = env_p5c11)
#load("~/BalancedPed/Simulations/p5/p5c11/modelSmr.Rdata", envir = env_p5c11)

meanDiffLL_mtam_p5c11 <- env_p5c11$smr2$Minus2LogLikelihood - env_p5c11$smr1$Minus2LogLikelihood
PN <- ncol(env_p5c11[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p5c11[["smr1"]][["dataSummary"]])
lamdaUnit_p5c11 <- meanDiffLL_mtam_p5c11/PN
SSize_p5c11 <- 1: N_indi
LamdaVec_p5c11 <- lamdaUnit_p5c11*SSize_p5c11
powVec_p5c11 <- as.numeric(lapply(LamdaVec_p5c11,powerCal, df = 2))

df_p5c11 <- data.frame(Nped = SSize_p5c11, 
                      power = powVec_p5c11, 
                      Combination = rep("power5", N_indi))

### create a new enviroment for one condition
env_p6c11 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p6/p6c11/modelSmr.Rdata", envir = env_p6c11)
#load("~/BalancedPed/Simulations/p6/p6c11/modelSmr.Rdata", envir = env_p6c11)

meanDiffLL_mtam_p6c11 <- env_p6c11$smr2$Minus2LogLikelihood - env_p6c11$smr1$Minus2LogLikelihood
PN <- ncol(env_p6c11[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p6c11[["smr1"]][["dataSummary"]])
lamdaUnit_p6c11 <- meanDiffLL_mtam_p6c11/PN
SSize_p6c11 <- 1: N_indi
LamdaVec_p6c11 <- lamdaUnit_p6c11*SSize_p6c11
powVec_p6c11 <- as.numeric(lapply(LamdaVec_p6c11,powerCal, df = 2))

df_p6c11 <- data.frame(Nped = SSize_p6c11, 
                      power = powVec_p6c11, 
                      Combination = rep("power6", N_indi))

### create a new enviroment for one condition
env_p7c11 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p7/p7c11/modelSmr.Rdata", envir = env_p7c11)
#load("~/BalancedPed/Simulations/p7/p7c11/modelSmr.Rdata", envir = env_p7c11)

meanDiffLL_mtam_p7c11 <- env_p7c11$smr2$Minus2LogLikelihood - env_p7c11$smr1$Minus2LogLikelihood
PN <- ncol(env_p7c11[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p7c11[["smr1"]][["dataSummary"]])
lamdaUnit_p7c11 <- meanDiffLL_mtam_p7c11/PN
SSize_p7c11 <- 1: N_indi
LamdaVec_p7c11 <- lamdaUnit_p7c11*SSize_p7c11
powVec_p7c11 <- as.numeric(lapply(LamdaVec_p7c11,powerCal, df = 2))

df_p7c11 <- data.frame(Nped = SSize_p7c11, 
                      power = powVec_p7c11, 
                      Combination = rep("power7", N_indi))

### create a new enviroment for one condition
env_p8c11 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c11/modelSmr.Rdata", envir = env_p8c11)
#load("~/BalancedPed/Simulations/p8/p8c11/modelSmr.Rdata", envir = env_p8c11)

meanDiffLL_mtam_p8c11 <- env_p8c11$smr2$Minus2LogLikelihood - env_p8c11$smr1$Minus2LogLikelihood
PN <- ncol(env_p8c11[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p8c11[["smr1"]][["dataSummary"]])
lamdaUnit_p8c11 <- meanDiffLL_mtam_p8c11/PN
SSize_p8c11 <- 1: N_indi
LamdaVec_p8c11 <- lamdaUnit_p8c11*SSize_p8c11
powVec_p8c11 <- as.numeric(lapply(LamdaVec_p8c11,powerCal, df = 2))

df_p8c11 <- data.frame(Nped = SSize_p8c11, 
                      power = powVec_p8c11, 
                      Combination = rep("power8", N_indi))

### create a new enviroment for one condition
env_p9c11 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p9/p9c11/modelSmr.Rdata", envir = env_p9c11)
#load("~/BalancedPed/Simulations/p9/p9c11/modelSmr.Rdata", envir = env_p9c11)

meanDiffLL_mtam_p9c11 <- env_p9c11$smr2$Minus2LogLikelihood - env_p9c11$smr1$Minus2LogLikelihood
PN <- ncol(env_p9c11[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p9c11[["smr1"]][["dataSummary"]])
lamdaUnit_p9c11 <- meanDiffLL_mtam_p9c11/PN
SSize_p9c11 <- 1: N_indi
LamdaVec_p9c11 <- lamdaUnit_p9c11*SSize_p9c11
powVec_p9c11 <- as.numeric(lapply(LamdaVec_p9c11,powerCal, df = 2))

df_p9c11 <- data.frame(Nped = SSize_p9c11, 
                      power = powVec_p9c11, 
                      Combination = rep("power9", N_indi))

### create a new enviroment for one condition
env_p10c11 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p10/p10c11/modelSmr.Rdata", envir = env_p10c11)
#load("~/BalancedPed/Simulations/p10/p10c11/modelSmr.Rdata", envir = env_p10c11)

meanDiffLL_mtam_p10c11 <- env_p10c11$smr2$Minus2LogLikelihood - env_p10c11$smr1$Minus2LogLikelihood
PN <- ncol(env_p10c11[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p10c11[["smr1"]][["dataSummary"]])
lamdaUnit_p10c11 <- meanDiffLL_mtam_p10c11/PN
SSize_p10c11 <- 1: N_indi
LamdaVec_p10c11 <- lamdaUnit_p10c11*SSize_p10c11
powVec_p10c11 <- as.numeric(lapply(LamdaVec_p10c11,powerCal, df = 2))

df_p10c11 <- data.frame(Nped = SSize_p10c11, 
                       power = powVec_p10c11, 
                       Combination = rep("power10", N_indi))

### create a new enviroment for one condition
env_p11c11 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p11/p11c11/modelSmr.Rdata", envir = env_p11c11)
#load("~/BalancedPed/Simulations/p11/p11c11/modelSmr.Rdata", envir = env_p11c11)

meanDiffLL_mtam_p11c11 <- env_p11c11$smr2$Minus2LogLikelihood - env_p11c11$smr1$Minus2LogLikelihood
PN <- ncol(env_p11c11[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p11c11[["smr1"]][["dataSummary"]])
lamdaUnit_p11c11 <- meanDiffLL_mtam_p11c11/PN
SSize_p11c11 <- 1: N_indi
LamdaVec_p11c11 <- lamdaUnit_p11c11*SSize_p11c11
powVec_p11c11 <- as.numeric(lapply(LamdaVec_p11c11,powerCal, df = 2))

df_p11c11 <- data.frame(Nped = SSize_p11c11, 
                       power = powVec_p11c11, 
                       Combination = rep("power11", N_indi))
# ### create a new enviroment for one condition
# env_p10c11 <- new.env()
# load("~/R-Project/BalancedPed/Simulations/p10/p10c11/modelSmr.Rdata", envir = env_p10c11)
# #load("~/BalancedPed/Simulations/p10/p10c11/modelSmr.Rdata", envir = env_p10c11)
# 
# meanDiffLL_mtam_p10c11 <- env_p10c11$smr2$Minus2LogLikelihood - env_p10c11$smr1$Minus2LogLikelihood
# PN <- ncol(env_p10c11[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p10c11[["smr1"]][["dataSummary"]])
# lamdaUnit_p10c11 <- meanDiffLL_mtam_p10c11/PN
# SSize_p10c11 <- 1: N_indi
# LamdaVec_p10c11 <- lamdaUnit_p10c11*SSize_p10c11
# powVec_p10c11 <- as.numeric(lapply(LamdaVec_p10c11,powerCal, df = 2))
# 
# df_p10c11 <- data.frame(Nped = SSize_p10c11, 
#                        power = powVec_p10c11, 
#                        Combination = rep("power4", N_indi))

### create a data frame for graphs
df_c11 <- rbind(df_p1c11, df_p2c11, df_p3c11, df_p4c11, df_p5c11, df_p6c11, df_p7c11, df_p8c11, df_p9c11, df_p10c11, df_p11c11)
df_c11$Combination <- as.factor(df_c11$Combination)


g1 <-ggplot(data = df_c11)+ geom_line(mapping = aes(x = Nped, y = power, color= Combination), size = 1.5) +
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

ggsave( "~/R-Project/BalancedPed/Graphs/Appendix/c11all.png",g1, width = 6, height = 4.5,  type = "cairo-png", dpi = 900)

rm(list = ls())
