# A function to calculate the expected family size given 
# 1) Kids per couple
# 2) Number of generations
# 3) Marriage rate

# Parameters:
# kpc: kids per couple 
# Ngen: number of generations
# marR: marriage rate
famSizeCal <- function(kpc, Ngen, marR){
      if (Ngen < 1){
            stop("The number of generations should be integers greater or equal than 1")
      }
      if (Ngen == 1){
            size = 2
      }
      if (Ngen >= 2){
            sizeMid = sizeMidCal(kpc=kpc, Ngen = Ngen, marR = marR)
            size = 2 + sizeMid + kpc^(Ngen-1) * marR^(Ngen-2)
      } else{
            stop()
      }
      return(size)
}

# A function to compute the number of family members between Gen-1 and Gen-g using an exponential function of kpc, Ngen and marR.
# Ideally, all there parameters should be inherited from the parent function famSizeCal
sizeMidCal <- function(kpc, Ngen, marR){
      Nmid <- Ngen -2
      midGens <- numeric(length = Nmid)
      for(i in 2 : (Ngen - 1)){
            midGens[i-1] <- kpc^(i-1) * marR^(i-2) * (1+marR)
      }
      return(sum(midGens))
}

# test the function: sizeMidCal green
#results <- sizeMidCal(3,4,2/3)

# test the function famSizeCal: green
# results <- famSizeCal(3,4,2/3)
