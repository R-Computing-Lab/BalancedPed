# The goal of this R file is to design a set of functions to simulate "balanced" pedigrees when setting a group of parameters:
# 1) Kids per couple
# 2) Number of generations
# 3) Sex ratio for offspring
# 4) Marriage rate
# 5) Fixed local pedigree pattern: for example, force the 2nd generation to be a pair of twins


## The final function:
# kpc: kids per couple 
# Ngen: number of generations
# sexR: sex ratio for offspring
# marR: marriage rate
# balancedSex: if the sex of kids is balanced within a specific generation
# balancedMar: if the married couples are balanced within a specific generation
SimPed <- function(kpc = 2,
                   Ngen = 3,
                   sexR = .5,
                   marR = 2/3,
                   balancedSex = FALSE,
                   balancedmar = FALSE
                   ){
      # Calculate the expected family size
      
      
      # Adapted from simfam package by Alejandro Ochoa
      fam <- data.frame(
            fam = 'fam1', # place in desired order, but there aren't families really
            # names of individuals in first generation (founders)
            id = paste0( '1-', 1 : n[1] ),
            pat = NA,
            mat = NA,
            sex = sex
      #      ,pheno = 0
      )
      
}