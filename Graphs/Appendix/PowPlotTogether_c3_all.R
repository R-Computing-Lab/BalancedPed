library(ggplot2)

powerCal <- function(lamda,df){
    1- pchisq(qchisq(1-.05, df), df, lamda)
    
}
my_palette <- c( "#E41A1C", "#332288", "#E69F00", "#A6CE39", "#DDCC77", "#377EB8",  "#4DAF4A", "#117A65", "#56B4E9", "#A6CE39", "#A9A9A9","#88CCEE", "#CC6677",  "#AA4499",   "#999933", "#882255", "#984EA3")

N_indi <- 10000

### create a new enviroment for one condition
env_p2c3 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p2/p2c3/modelSmr.Rdata", envir = env_p2c3)
#load("~/BalancedPed/Simulations/p2/p2c3/modelSmr.Rdata", envir = env_p2c3)

meanDiffLL_mtam_p2c3 <- env_p2c3$smr2$Minus2LogLikelihood - env_p2c3$smr1$Minus2LogLikelihood
PN <- ncol(env_p2c3[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p2c3[["smr1"]][["dataSummary"]])
lamdaUnit_p2c3 <- meanDiffLL_mtam_p2c3/PN
SSize_p2c3 <- 1: N_indi
LamdaVec_p2c3 <- lamdaUnit_p2c3*SSize_p2c3
powVec_p2c3 <- as.numeric(lapply(LamdaVec_p2c3,powerCal, df = 2))

df_p2c3 <- data.frame(Nped = SSize_p2c3, 
                      power = powVec_p2c3, 
                      Combination = rep("power1", N_indi))


### create a new enviroment for one condition
env_p5c3 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p5/p5c3/modelSmr.Rdata", envir = env_p5c3)
#load("~/BalancedPed/Simulations/p5/p5c3/modelSmr.Rdata", envir = env_p5c3)

meanDiffLL_mtam_p5c3 <- env_p5c3$smr2$Minus2LogLikelihood - env_p5c3$smr1$Minus2LogLikelihood
PN <- ncol(env_p5c3[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p5c3[["smr1"]][["dataSummary"]])
lamdaUnit_p5c3 <- meanDiffLL_mtam_p5c3/PN
SSize_p5c3 <- 1: N_indi
LamdaVec_p5c3 <- lamdaUnit_p5c3*SSize_p5c3
powVec_p5c3 <- as.numeric(lapply(LamdaVec_p5c3,powerCal, df = 2))

df_p5c3 <- data.frame(Nped = SSize_p5c3, 
                      power = powVec_p5c3, 
                      Combination = rep("power2", N_indi))

### create a new enviroment for one condition
env_p8c3 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p8/p8c3/modelSmr.Rdata", envir = env_p8c3)
#load("~/BalancedPed/Simulations/p8/p8c3/modelSmr.Rdata", envir = env_p8c3)

meanDiffLL_mtam_p8c3 <- env_p8c3$smr2$Minus2LogLikelihood - env_p8c3$smr1$Minus2LogLikelihood
PN <- ncol(env_p8c3[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p8c3[["smr1"]][["dataSummary"]])
lamdaUnit_p8c3 <- meanDiffLL_mtam_p8c3/PN
SSize_p8c3 <- 1: N_indi
LamdaVec_p8c3 <- lamdaUnit_p8c3*SSize_p8c3
powVec_p8c3 <- as.numeric(lapply(LamdaVec_p8c3,powerCal, df = 2))

df_p8c3 <- data.frame(Nped = SSize_p8c3, 
                      power = powVec_p8c3, 
                      Combination = rep("power3", N_indi))
### create a new enviroment for one condition
env_p10c3 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p10/p10c3/modelSmr.Rdata", envir = env_p10c3)
#load("~/BalancedPed/Simulations/p10/p10c3/modelSmr.Rdata", envir = env_p10c3)


meanDiffLL_mtam_p10c3 <- env_p10c3$smr2$Minus2LogLikelihood - env_p10c3$smr1$Minus2LogLikelihood
PN <- ncol(env_p10c3[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p10c3[["smr1"]][["dataSummary"]])
lamdaUnit_p10c3 <- meanDiffLL_mtam_p10c3/PN
SSize_p10c3 <- 1: N_indi
LamdaVec_p10c3 <- lamdaUnit_p10c3*SSize_p10c3
powVec_p10c3 <- as.numeric(lapply(LamdaVec_p10c3,powerCal, df = 2))

df_p10c3 <- data.frame(Nped = SSize_p10c3, 
                       power = powVec_p10c3, 
                       Combination = rep("power4", N_indi))

### create a data frame for graphs
df_c3 <- rbind(df_p2c3, df_p5c3, df_p8c3, df_p10c3)
df_c3$Combination <- as.factor(df_c3$Combination)


g1 <-ggplot(data = df_c3)+ geom_line(mapping = aes(x = Nped, y = power, color= Combination), size = 1.5) +
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

ggsave( "~/R-Project/BalancedPed/Graphs/Appendix/c3all.png",g1, width = 6, height = 4.5,  type = "cairo-png", dpi = 900)

rm(list = ls())
