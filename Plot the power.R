meanDiffLL <- smr3$Minus2LogLikelihood - smr1$Minus2LogLikelihood

meanDiffLL2 <- meanDiffLL/667*400

1- pchisq(qchisq(1-.05, 1), 1, meanDiffLL2)
