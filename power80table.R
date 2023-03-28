### a script to calculate the number of individuals for a power of .8 in all conditions.
vec_p <- paste("p", 1:11, sep = "")
vec_c <- paste("c", 1:11, sep = "")
df_results <- matrix(nrow = 11, ncol = 11)      
colnames(df_results) <- vec_p
rownames(df_results) <- vec_c

lunit <- .00001
ssize <- 1:1000000
l_vec <- lunit*ssize
powerCal <- function(lamda,df){
      1- pchisq(qchisq(1-.05, df), df, lamda)
      
}
powVec <- as.numeric(lapply(l_vec,powerCal, df = 2))
tgt_lunit <- which.min(abs(powVec-.80))*lunit

for (p in 1: 11){
            for (c in 1: 11){
                  dir <- paste0("~/R-Project/BalancedPed/Simulations/p",p,"/p",p, "c",c)
                  if (dir.exists(dir)){
                        #cat("starttime:", as.character(Sys.time())) 
                        cat("\n","running", dir)
                        setwd(dir)
                        load("modelSmr.Rdata")
                        meanDiffLL_mtam <- smr2$Minus2LogLikelihood - smr1$Minus2LogLikelihood
                        PN <- ncol(smr1[["dataSummary"]][["fam1"]])*length(smr1[["dataSummary"]])
                        lamdaUnit <- meanDiffLL_mtam/PN
                        df_results[c,p] <- round(tgt_lunit/lamdaUnit) 
                        #cat("finished at" , as.character(Sys.time()))
                  }else{
                        next
                  }
            }
            
      }

write.table(df_results, "~/R-Project/BalancedPed/Graphs/PowerTable.csv", quote = FALSE, sep = ",")
