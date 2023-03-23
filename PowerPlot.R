### this is a script to generate power plots for the simulation results


## Run all the scripts
for (p in 1: 11){
      for (c in 1: 11){
            dir <- paste0("~/R-Project/BalancedPed/Simulations/p",p,"/p",p, "c",c)
            if (dir.exists(dir)){
                  starttime <- Sys.time()
                  cat("running", dir)
                  setwd(dir)
                  source(paste0("p",p,"c",c,".R"))
                  cat(dir, "finished. Time used:",starttime, "~", Sys.time())
            }else{
                  next
            }
      }
      
}

## Generate power plots for all the conditions

meanDiffLL <- smr3$Minus2LogLikelihood - smr1$Minus2LogLikelihood

meanDiffLL2 <- meanDiffLL/667*400

1- pchisq(qchisq(1-.05, 1), 1, meanDiffLL2)
