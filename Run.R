### this is a script to generate power plots for the simulation results

run_protected_loop <- function(){
<<<<<<< HEAD
      for (p in 8: 11){
=======
      for (p in 3: 7){
>>>>>>> 76e5be90ae8dd27f6344cc20fa40ea903bfb4620
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

