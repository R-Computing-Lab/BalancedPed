# Some decrepit codes

# seperate old graphs
g1 <- ggplot()+ geom_line(mapping = aes(x = SSize_p1c2, y = powVec_p1c2), size = 1.5, color = "red") +
      geom_line(mapping = aes(x = SSize_p1c5, y = powVec_p1c5), size = 1.5, color = "blue") +
      geom_line(mapping = aes(x = SSize_p1c8, y = powVec_p1c8), size = 1.5, color = "green") +
      #geom_line(mapping = aes(x = SSize_p1c11, y = powVec_p1c11), size = 1.5, color = "purple") +
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
      geom_hline(yintercept = .8, linetype = 5, size = .8, color = "grey") +
      annotate(geom = "text",x = 0.62, y =.92, label = "a\u00B2 = .6, mt\u00B2 = .05", family="Calibri", color = "gray40",size = 3)+
      annotate(geom = "text",x = 0.73, y =.6, label = "a\u00B2 = .4, mt\u00B2 = .05", family="Calibri", color = "gray40",size = 3)+
      annotate(geom = "text",x = 0.9, y =.6, label = "a\u00B2 = .4, mt\u00B2 = .05", family="Calibri", color = "gray40",size = 3)
ggsave( "~/R-Project/BalancedPed/Simulations/p1/p1c2/graph1.png",g1,width = 6, height = 4.5,  type = "cairo-png", dpi = 900)


###
set.seed(100)
x1 <- SimPed()
x2 <- SimPed()
