### R code from vignette source 'ESTIMATE_Vignette.Rnw'

###################################################
### code chunk number 1: load
###################################################
library(estimate)
OvarianCancerExpr <- system.file("extdata", "sample_input.txt", package="estimate")
filterCommonGenes(input.f=OvarianCancerExpr, output.f="OV_10412genes.gct", id="GeneSymbol")


###################################################
### code chunk number 2: estimate
###################################################
estimateScore("OV_10412genes.gct", "OV_estimate_score.gct", platform="affymetrix")


###################################################
### code chunk number 3: fig1plot
###################################################
plotPurity(scores="OV_estimate_score.gct", samples="s516", platform="affymetrix")


###################################################
### code chunk number 4: runningtime1
###################################################
ptm <- proc.time()
filterCommonGenes(input.f=OvarianCancerExpr, output.f="OV_10412genes.gct", id="GeneSymbol")
proc.time()-ptm


###################################################
### code chunk number 5: runningtime2
###################################################
ptm <- proc.time()
estimateScore("OV_10412genes.gct", "OV_estimate_score.gct", platform="affymetrix")
proc.time()-ptm


