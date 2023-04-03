library(ggplot2)

powerCal <- function(lamda,df){
    1- pchisq(qchisq(1-.05, df), df, lamda)
    
}
my_palette <- c( "#E41A1C", "#332288", "#E69F00", "#117A65", "#DDCC77", "#377EB8",  "#4DAF4A",  "#56B4E9", "#A6CE39", "#A9A9A9","#88CCEE", "#Cc2677",  "#AA4499",   "#999933", "#882255", "#984EA3")


### create a new enviroment for one condition
env_p1c2 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p1/p1c2/modelSmr.Rdata", envir = env_p1c2)
#load("~/BalancedPed/Simulations/p1/p1c2/modelSmr.Rdata", envir = env_p1c2)

meanDiffLL_mtam_p1c2 <- env_p1c2$smr2$Minus2LogLikelihood - env_p1c2$smr1$Minus2LogLikelihood
PN <- ncol(env_p1c2[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p1c2[["smr1"]][["dataSummary"]])
lamdaUnit_p1c2 <- meanDiffLL_mtam_p1c2/PN
SSize_p1c2 <- 1: 10000
LamdaVec_p1c2 <- lamdaUnit_p1c2*SSize_p1c2
powVec_p1c2 <- as.numeric(lapply(LamdaVec_p1c2,powerCal, df = 2))

df_p1c2 <- data.frame(Nped = SSize_p1c2, 
                      power = powVec_p1c2, 
                      Combination = rep("power1", 10000))


### create a new enviroment for one condition
env_p2c2 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p2/p2c2/modelSmr.Rdata", envir = env_p2c2)
#load("~/BalancedPed/Simulations/p2/p2c2/modelSmr.Rdata", envir = env_p2c2)

meanDiffLL_mtam_p2c2 <- env_p2c2$smr2$Minus2LogLikelihood - env_p2c2$smr1$Minus2LogLikelihood
PN <- ncol(env_p2c2[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p2c2[["smr1"]][["dataSummary"]])
lamdaUnit_p2c2 <- meanDiffLL_mtam_p2c2/PN
SSize_p2c2 <- 1: 10000
LamdaVec_p2c2 <- lamdaUnit_p2c2*SSize_p2c2
powVec_p2c2 <- as.numeric(lapply(LamdaVec_p2c2,powerCal, df = 2))

df_p2c2 <- data.frame(Nped = SSize_p2c2, 
                      power = powVec_p2c2, 
                      Combination = rep("power2", 10000))

### create a new enviroment for one condition
env_p3c2 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p3/p3c2/modelSmr.Rdata", envir = env_p3c2)
#load("~/BalancedPed/Simulations/p3/p3c2/modelSmr.Rdata", envir = env_p3c2)

meanDiffLL_mtam_p3c2 <- env_p3c2$smr2$Minus2LogLikelihood - env_p3c2$smr1$Minus2LogLikelihood
PN <- ncol(env_p3c2[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p3c2[["smr1"]][["dataSummary"]])
lamdaUnit_p3c2 <- meanDiffLL_mtam_p3c2/PN
SSize_p3c2 <- 1: 10000
LamdaVec_p3c2 <- lamdaUnit_p3c2*SSize_p3c2
powVec_p3c2 <- as.numeric(lapply(LamdaVec_p3c2,powerCal, df = 2))

df_p3c2 <- data.frame(Nped = SSize_p3c2, 
                      power = powVec_p3c2, 
                      Combination = rep("power3", 10000))

# ### create a new enviroment for one condition
# env_p10c2 <- new.env()
# load("~/R-Project/BalancedPed/Simulations/p10/p10c2/modelSmr.Rdata", envir = env_p10c2)
# #load("~/BalancedPed/Simulations/p10/p10c2/modelSmr.Rdata", envir = env_p10c2)
# 
# meanDiffLL_mtam_p10c2 <- env_p10c2$smr2$Minus2LogLikelihood - env_p10c2$smr1$Minus2LogLikelihood
# PN <- ncol(env_p10c2[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p10c2[["smr1"]][["dataSummary"]])
# lamdaUnit_p10c2 <- meanDiffLL_mtam_p10c2/PN
# SSize_p10c2 <- 1: 10000
# LamdaVec_p10c2 <- lamdaUnit_p10c2*SSize_p10c2
# powVec_p10c2 <- as.numeric(lapply(LamdaVec_p10c2,powerCal, df = 2))
# 
# df_p10c2 <- data.frame(Nped = SSize_p10c2, 
#                        power = powVec_p10c2, 
#                        Combination = rep("power4", 10000))

### create a data frame for graphs
df_c2 <- rbind(df_p1c2, df_p2c2, df_p3c2)
df_c2$Combination <- as.factor(df_c2$Combination)


g1 <-ggplot(data = df_c2)+ geom_line(mapping = aes(x = Nped, y = power, color= Combination), size = 1.5) +
    scale_color_manual(values=my_palette[1:3],
                       name="Pedigree Structures",
                       breaks=c("power1", "power2", "power3"),
                       labels=c("k = 2, s = 4, m = 15",
                                "k = 3, s = 4, m = 29", 
                                "k = 4, s = 4, m = 56")
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

ggsave( "~/R-Project/BalancedPed/Graphs/graph_c2p123.png",g1, width = 6, height = 4.5,  type = "cairo-png", dpi = 900)

rm(list = ls())
