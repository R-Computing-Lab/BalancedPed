library(ggplot2)

powerCal <- function(lamda,df){
    1- pchisq(qchisq(1-.05, df), df, lamda)
    
}
my_palette <- c( "#E41A1C", "#332288", "#E69F00", "#A6CE39", "#DDCC77", "#377EB8",  "#4DAF4A", "#117A65", "#56B4E9", "#A6CE39", "#A9A9A9","#88CCEE", "#CC6677",  "#AA4499",   "#999933", "#882255", "#984EA3")

N_indi <- 2500

### create a new enviroment for one condition
env_p2c7 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p2/p2c7/modelSmr.Rdata", envir = env_p2c7)
#load("~/BalancedPed/Simulations/p2/p2c7/modelSmr.Rdata", envir = env_p2c7)

meanDiffLL_mtam_p2c7 <- env_p2c7$smr2$Minus2LogLikelihood - env_p2c7$smr1$Minus2LogLikelihood
PN <- ncol(env_p2c7[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p2c7[["smr1"]][["dataSummary"]])
lamdaUnit_p2c7 <- meanDiffLL_mtam_p2c7/PN
SSize_p2c7 <- 1: N_indi
LamdaVec_p2c7 <- lamdaUnit_p2c7*SSize_p2c7
powVec_p2c7 <- as.numeric(lapply(LamdaVec_p2c7,powerCal, df = 2))

df_p2c7 <- data.frame(Nped = SSize_p2c7, 
                      power = powVec_p2c7, 
                      Combination = rep("power1", N_indi))


### create a new enviroment for one condition
env_p5c7 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p5/p5c7/modelSmr.Rdata", envir = env_p5c7)
#load("~/BalancedPed/Simulations/p5/p5c7/modelSmr.Rdata", envir = env_p5c7)

meanDiffLL_mtam_p5c7 <- env_p5c7$smr2$Minus2LogLikelihood - env_p5c7$smr1$Minus2LogLikelihood
PN <- ncol(env_p5c7[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p5c7[["smr1"]][["dataSummary"]])
lamdaUnit_p5c7 <- meanDiffLL_mtam_p5c7/PN
SSize_p5c7 <- 1: N_indi
LamdaVec_p5c7 <- lamdaUnit_p5c7*SSize_p5c7
powVec_p5c7 <- as.numeric(lapply(LamdaVec_p5c7,powerCal, df = 2))

df_p5c7 <- data.frame(Nped = SSize_p5c7, 
                      power = powVec_p5c7, 
                      Combination = rep("power2", N_indi))

### create a new enviroment for one condition
env_p8c7 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c7/modelSmr.Rdata", envir = env_p8c7)
#load("~/BalancedPed/Simulations/p8/p8c7/modelSmr.Rdata", envir = env_p8c7)

meanDiffLL_mtam_p8c7 <- env_p8c7$smr2$Minus2LogLikelihood - env_p8c7$smr1$Minus2LogLikelihood
PN <- ncol(env_p8c7[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p8c7[["smr1"]][["dataSummary"]])
lamdaUnit_p8c7 <- meanDiffLL_mtam_p8c7/PN
SSize_p8c7 <- 1: N_indi
LamdaVec_p8c7 <- lamdaUnit_p8c7*SSize_p8c7
powVec_p8c7 <- as.numeric(lapply(LamdaVec_p8c7,powerCal, df = 2))

df_p8c7 <- data.frame(Nped = SSize_p8c7, 
                      power = powVec_p8c7, 
                      Combination = rep("power3", N_indi))
### create a new enviroment for one condition
env_p10c7 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p10/p10c7/modelSmr.Rdata", envir = env_p10c7)
#load("~/BalancedPed/Simulations/p10/p10c7/modelSmr.Rdata", envir = env_p10c7)


meanDiffLL_mtam_p10c7 <- env_p10c7$smr2$Minus2LogLikelihood - env_p10c7$smr1$Minus2LogLikelihood
PN <- ncol(env_p10c7[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p10c7[["smr1"]][["dataSummary"]])
lamdaUnit_p10c7 <- meanDiffLL_mtam_p10c7/PN
SSize_p10c7 <- 1: N_indi
LamdaVec_p10c7 <- lamdaUnit_p10c7*SSize_p10c7
powVec_p10c7 <- as.numeric(lapply(LamdaVec_p10c7,powerCal, df = 2))

df_p10c7 <- data.frame(Nped = SSize_p10c7, 
                       power = powVec_p10c7, 
                       Combination = rep("power4", N_indi))

### create a data frame for graphs
df_c7 <- rbind(df_p2c7, df_p5c7, df_p8c7, df_p10c7)
df_c7$Combination <- as.factor(df_c7$Combination)


g1 <-ggplot(data = df_c7)+ geom_line(mapping = aes(x = Nped, y = power, color= Combination), size = 1.5) +
    scale_color_manual(values=my_palette[1:4],
                       name="Pedigree Structures",
                       breaks=c("power1", "power2", "power3", "power4"),
                       labels=c("k = 3, G = 4, m = 29",
                                "k = 3, G = 5, m = 61", 
                                "k = 3, G = 8, m = 509",
                                "k = 8, G = 4, m = 316")
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

ggsave( "~/R-Project/BalancedPed/Graphs/Appendix/c7all.png",g1, width = 6, height = 4.5,  type = "cairo-png", dpi = 900)

rm(list = ls())
