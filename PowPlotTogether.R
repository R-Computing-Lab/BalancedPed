powerCal <- function(lamda,df){
      1- pchisq(qchisq(1-.05, df), df, lamda)
      
}

### create a new enviroment for one condition
env_p1c2 <- new.env()
load("~/BalancedPed/Simulations/p1/p1c2/modelSmr.Rdata", envir = env_p1c2)

meanDiffLL_mtam_p1c2 <- env_p1c2$smr2$Minus2LogLikelihood - env_p1c2$smr1$Minus2LogLikelihood
lamdaUnit_p1c2 <- meanDiffLL_mtam_p1c2/env_p1c2$smr1$numObs
SSize_p1c2 <- 1: 1000
LamdaVec_p1c2 <- lamdaUnit_p1c2*SSize_p1c2
powVec_p1c2 <- as.numeric(lapply(LamdaVec_p1c2,powerCal, df = 2))

### create a new enviroment for one condition
env_p1c5 <- new.env()
load("~/BalancedPed/Simulations/p1/p1c5/modelSmr.Rdata", envir = env_p1c5)

meanDiffLL_mtam_p1c5 <- env_p1c5$smr2$Minus2LogLikelihood - env_p1c5$smr1$Minus2LogLikelihood
lamdaUnit_p1c5 <- meanDiffLL_mtam_p1c5/env_p1c5$smr1$numObs
SSize_p1c5 <- 1: 1000
LamdaVec_p1c5 <- lamdaUnit_p1c5*SSize_p1c5
powVec_p1c5 <- as.numeric(lapply(LamdaVec_p1c5,powerCal, df = 2))

### create a new enviroment for one condition
env_p1c8 <- new.env()
load("~/BalancedPed/Simulations/p1/p1c8/modelSmr.Rdata", envir = env_p1c8)

meanDiffLL_mtam_p1c8 <- env_p1c8$smr2$Minus2LogLikelihood - env_p1c8$smr1$Minus2LogLikelihood
lamdaUnit_p1c8 <- meanDiffLL_mtam_p1c8/env_p1c8$smr1$numObs
SSize_p1c8 <- 1: 1000
LamdaVec_p1c8 <- lamdaUnit_p1c8*SSize_p1c8
powVec_p1c8 <- as.numeric(lapply(LamdaVec_p1c8,powerCal, df = 2))

### create a new enviroment for one condition
env_p1c11 <- new.env()
load("~/BalancedPed/Simulations/p1/p1c11/modelSmr.Rdata", envir = env_p1c11)

meanDiffLL_mtam_p1c11 <- env_p1c11$smr2$Minus2LogLikelihood - env_p1c11$smr1$Minus2LogLikelihood
lamdaUnit_p1c11 <- meanDiffLL_mtam_p1c11/env_p1c11$smr1$numObs
SSize_p1c11 <- 1: 1000
LamdaVec_p1c11 <- lamdaUnit_p1c11*SSize_p1c11
powVec_p1c11 <- as.numeric(lapply(LamdaVec_p1c11,powerCal, df = 2))


ggplot()+ geom_line(mapping = aes(x = SSize_p1c2, y = powVec_p1c2), size = 1.5, color = "#fed976") +
       geom_line(mapping = aes(x = SSize_p1c5, y = powVec_p1c5), size = 1.5, color = "#fd8d3c") +
       geom_line(mapping = aes(x = R, y = power3), size = 1.5, color = "#bd0026") +
      geom_line(mapping = aes(x = R, y = power3), size = 1.5, color = "#ac6036") +
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
      xlab("N of Pedigrees")+
      scale_y_continuous(n.breaks = 6)+
      ylab("Power:Mt+Am")+
      geom_hline(yintercept = .8, linetype = 5, size = .8, color = "grey")
#annotate(geom = "text",x = 0.62, y =.92, label = "A = .7, C = .2", family="Calibri", color = "gray40",size = 3)+
#annotate(geom = "text",x = 0.73, y =.6, label = "A = .5, C = .2", family="Calibri", color = "gray40",size = 3)+
#annotate(geom = "text",x = 0.9, y =.6, label = "A = .3, C = .2", family="Calibri", color = "gray40",size = 3)
ggsave( "p1.png",p1,width = 6, height = 4.5,  type = "cairo-png", dpi = 900)