###code to run first trial of simulation


source("evenInsert.R")
source("famSizeCal.R")
source("SimPed.R")
source("HelperFunctions.R")

library(OpenMx)

sampleFam <- SimPed(kpc = 2, Ngen = 5, sexR = .5, marR = 2/3)
Addmat <- as.matrix(ped2add(sampleFam, verbose = TRUE))
Nucmat <- ped2cn(sampleFam)
Extmat <- ped2ce(sampleFam)
Mtdmat <- ped2mt_v3(sampleFam)
Envmat <- diag(1,nrow = nrow(Addmat))
dimnames(Envmat) <- dimnames(Addmat)
Amimat <- Addmat*Mtdmat
Dmgmat <- Addmat*Addmat

# Trial-Combination 11
ad2 <- c( .6, .6, .6, .4, .4, .4, .2, .2, .2, .4, .4, .4)
dd2 <- c( .0, .0, .0, .0, .0, .0, .0, .0, .0, .0, .1, .0)
cn2 <- c( .1, .1, .1, .1, .1, .1, .1, .1, .1, .2, .1, .1)
ce2 <- c( .1, .1, .1, .1, .1, .1, .1, .1, .1, .1, .1, .1)
mt2 <- c( .1,.05,.01, .1,.05,.01, .1,.05,.01,.05,.05, .1)
am2 <- c(.05,.05,.05,.05,.05,.05,.05,.05,.05,.05,.05, .1)
ee2 <- c(.05, .1,.19,.25,.30,.39,.45,.50,.59, .2, .2, .2)


## generate data
library(mvtnorm)

comb <- 5

sumCov <- ad2[comb]*Addmat + dd2[comb]*Addmat*Addmat + cn2[comb]*Nucmat + ce2[comb]*Extmat + mt2[comb]*Mtdmat + am2[comb]*Addmat*Mtdmat + ee2[comb]*Envmat
set.seed(13271)
numfam <- 10
dat <- rmvnorm(numfam, sigma = sumCov)


# fit ML model

totalVar <- 1
totalMea <- 0

Model1 <- mxModel(
      "ModelOne",
      mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE, values = ad2[comb]*totalVar, labels = "vad", name = "Vad", lbound = 1e-10),
      mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE, values = dd2[comb]*totalVar, labels = "vdd", name = "Vdd", lbound = 1e-10),
      mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE, values = cn2[comb]*totalVar, labels = "vcn", name = "Vcn", lbound = 1e-10),
      mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE, values = ce2[comb]*totalVar, labels = "vce", name = "Vce", lbound = 1e-10),
      mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE, values = mt2[comb]*totalVar, labels = "vmt", name = "Vmt", lbound = 1e-10),
      mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE, values = am2[comb]*totalVar, labels = "vam", name = "Vam", lbound = 1e-10),
      mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE, values = ee2[comb]*totalVar, labels = "ver", name = "Ver", lbound = 1e-10)
)

ll <- list()
for (i in 1:numfam){
      ll[[i]] <- dat[i,]
}

modList <- list()
modNames <- paste0("fam", 1:numfam)

for(afam in 1:numfam){
      ytemp <- paste('S', rownames (Addmat))
      fsize <- nrow(Addmat)
      modList[[afam]] <- mxModel(name=modNames[afam],
                                 mxMatrix("Iden", nrow=fsize, ncol=fsize, name="I"), 
                                 mxMatrix("Unit", nrow=fsize, ncol=fsize, name='U'),
                                 mxMatrix("Symm", nrow=fsize, ncol=fsize, values=Addmat, name="A"), 
                                 mxMatrix("Symm", nrow=fsize, ncol=fsize, values=Dmgmat, name="D"), 
                                 mxMatrix("Symm", nrow=fsize, ncol=fsize, values=Nucmat, name="Cn"), 
                                 mxMatrix("Symm", nrow=fsize, ncol=fsize, values=Extmat, name="Ce"), 
                                 mxMatrix("Symm", nrow=fsize, ncol=fsize, values=Amimat, name="Am"), 
                                 mxMatrix("Symm", nrow=fsize, ncol=fsize, values=Mtdmat, name="Mt"),
                                 mxData(observed = matrix(ll[[afam]], nrow=1, dimnames=list(NULL, ytemp)), type="raw", sort=FALSE),
                                 mxMatrix('Full', nrow=1, ncol=fsize, name='M', free=TRUE, labels='meanLI',
                                          dimnames=list(NULL, ytemp)),
                                 mxAlgebra ((A %x% ModelOne.Vad) 
                                            + (D %x% ModelOne.Vdd) 
                                            + (Cn %x% ModelOne.Vcn) 
                                            + (U %x% ModelOne.Vce) 
                                            + (Mt %x% ModelOne.Vmt) 
                                            + (Am %x% ModelOne.Vam) 
                                            + (I %x% ModelOne.Ver), 
                                            name="V", dimnames=list(ytemp, ytemp)),
                                 mxExpectationNormal(covariance='V', means='M'), 
                                 mxFitFunctionML()
      )
}
container <- mxModel('Model2', Model1, modList, mxFitFunctionMultigroup(modNames))
container <- mxOption(container, 'Checkpoint Units', 'minutes')
container <- mxOption(container, 'Checkpoint Count', 1)
containerRun <- mxRun(container, intervals=FALSE, checkpoint=TRUE) 
summary(containerRun)
#summary(containerRun, verbose=TRUE)
#mxEval(cbind(vad, vcn, ver)/(vad+vcn+ver), containerRun)


