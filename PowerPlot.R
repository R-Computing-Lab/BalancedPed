### this is a script to generate power plots for the simulation results

run_protected_loop <- function(){
      for (p in 1: 11){
            for (c in 1: 11){
                  dir <- paste0("~/R-Project/BalancedPed/Simulations/p",p,"/p",p, "c",c)
                  if (dir.exists(dir)){
                        cat("starttime:", as.character(Sys.time())) 
                        cat("running", dir)
                        setwd(dir)
                        source(paste0("p",p,"c",c,".R"))
                        cat("finished at" , as.character(Sys.time()))
                  }else{
                        next
                  }
            }
            
      }
}
## Run all the scripts
run_protected_loop()


## Generate power plots for all the conditions

meanDiffLL <- smr3$Minus2LogLikelihood - smr1$Minus2LogLikelihood

meanDiffLL2 <- meanDiffLL/667*400

1- pchisq(qchisq(1-.05, 1), 1, meanDiffLL2)
