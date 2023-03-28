library(ggplot2)

powerCal <- function(lamda,df){
      1- pchisq(qchisq(1-.05, df), df, lamda)
      
}
my_palette <- c(  "#332288","#377EB8", "#E69F00","#999933",  "#117A65", "#DDCC77",   "#4DAF4A",  "#56B4E9", "#A6CE39", "#A9A9A9","#88CCEE", "#Cc5677",  "#AA4499", "#E41A1C"  , "#882255", "#984EA3")


### create a new enviroment for one condition
env_p3c5 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p3/p3c5/modelSmr.Rdata", envir = env_p3c5)
#load("~/BalancedPed/Simulations/p3/p3c5/modelSmr.Rdata", envir = env_p3c5)

meanDiffLL_mtam_p3c5 <- env_p3c5$smr2$Minus2LogLikelihood - env_p3c5$smr1$Minus2LogLikelihood
PN <- ncol(env_p3c5[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p3c5[["smr1"]][["dataSummary"]])
lamdaUnit_p3c5 <- meanDiffLL_mtam_p3c5/PN
SSize_p3c5 <- 1: 10000
LamdaVec_p3c5 <- lamdaUnit_p3c5*SSize_p3c5
powVec_p3c5 <- as.numeric(lapply(LamdaVec_p3c5,powerCal, df = 2))

df_p3c5 <- data.frame(Nped = SSize_p3c5, 
                      power = powVec_p3c5, 
                      Combination = rep("power1", 10000))


### create a new enviroment for one condition
env_p5c5 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p5/p5c5/modelSmr.Rdata", envir = env_p5c5)
#load("~/BalancedPed/Simulations/p5/p5c5/modelSmr.Rdata", envir = env_p5c5)

meanDiffLL_mtam_p5c5 <- env_p5c5$smr2$Minus2LogLikelihood - env_p5c5$smr1$Minus2LogLikelihood
PN <- ncol(env_p5c5[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p5c5[["smr1"]][["dataSummary"]])
lamdaUnit_p5c5 <- meanDiffLL_mtam_p5c5/PN
SSize_p5c5 <- 1: 10000
LamdaVec_p5c5 <- lamdaUnit_p5c5*SSize_p5c5
powVec_p5c5 <- as.numeric(lapply(LamdaVec_p5c5,powerCal, df = 2))

df_p5c5 <- data.frame(Nped = SSize_p5c5, 
                      power = powVec_p5c5, 
                      Combination = rep("power2", 10000))

### create a new enviroment for one condition
env_p6c5 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p6/p6c5/modelSmr.Rdata", envir = env_p6c5)
#load("~/BalancedPed/Simulations/p6/p6c5/modelSmr.Rdata", envir = env_p6c5)

meanDiffLL_mtam_p6c5 <- env_p6c5$smr2$Minus2LogLikelihood - env_p6c5$smr1$Minus2LogLikelihood
PN <- ncol(env_p6c5[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p6c5[["smr1"]][["dataSummary"]])
lamdaUnit_p6c5 <- meanDiffLL_mtam_p6c5/PN
SSize_p6c5 <- 1: 10000
LamdaVec_p6c5 <- lamdaUnit_p6c5*SSize_p6c5
powVec_p6c5 <- as.numeric(lapply(LamdaVec_p6c5,powerCal, df = 2))

df_p6c5 <- data.frame(Nped = SSize_p6c5, 
                      power = powVec_p6c5, 
                      Combination = rep("power3", 10000))

### create a new enviroment for one condition
env_p9c5 <- new.env()
load("~/R-Project/BalancedPed/Simulations/p9/p9c5/modelSmr.Rdata", envir = env_p9c5)
#load("~/BalancedPed/Simulations/p9/p9c5/modelSmr.Rdata", envir = env_p9c5)

meanDiffLL_mtam_p9c5 <- env_p9c5$smr2$Minus2LogLikelihood - env_p9c5$smr1$Minus2LogLikelihood
PN <- ncol(env_p9c5[["smr1"]][["dataSummary"]][["fam1"]])*length(env_p9c5[["smr1"]][["dataSummary"]])
lamdaUnit_p9c5 <- meanDiffLL_mtam_p9c5/PN
SSize_p9c5 <- 1: 10000
LamdaVec_p9c5 <- lamdaUnit_p9c5*SSize_p9c5
powVec_p9c5 <- as.numeric(lapply(LamdaVec_p9c5,powerCal, df = 2))

df_p9c5 <- data.frame(Nped = SSize_p9c5, 
                       power = powVec_p9c5, 
                       Combination = rep("power4", 10000))

### create a data frame for graphs
df_c5 <- rbind(df_p3c5, df_p5c5, df_p6c5, df_p9c5)
df_c5$Combination <- as.factor(df_c5$Combination)


g1 <-ggplot(data = df_c5)+ geom_line(mapping = aes(x = Nped, y = power, color= Combination), size = 1.5) +
      scale_color_manual(values=my_palette[1:4],
                         name="Pedigree Structures",
                         breaks=c( "power2", "power3", "power1", "power4" ),
                         labels=c("k = 3, s = 5, m = 61, p =.5, r = .67",
                                  "k = 3, s = 5, m = 61, p =.6, r = .67", 
                                  "k = 4, s = 4, m = 56, p =.5, r = .67",
                                  "k = 4, s = 4, m = 75, p =.5, r = .8")
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

ggsave( "~/R-Project/BalancedPed/Graphs/graph_c5p3569.png",g1, width = 6, height = 4.5,  type = "cairo-png", dpi = 900)

rm(list = ls())
