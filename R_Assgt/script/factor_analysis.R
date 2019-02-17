############################################################################
#
#             Factor Analysis - This script includes various Factor analysis
#
#############################################################################


personality_data <- read.csv("PersonalityData.csv")
personality_data
eigenval=eigen(cor(personality_data[,2:13]))
eigenval$values
library(psych)
fit <- principal(personality_data[, 2:13],nfactors=5, rotate="variance")
fit$loadings

fit <- principal(personality_data[, 2:13],nfactors=3, rotate="variance")
fit$loadings
fit$weights
  colnames(fit$weights) = c("Extrovert", "Dependable", "Anxious")
  fit$weights

fit <- principal(personality_data[, 2:13],nfactors=3, rotate="variance")
colnames(fit$scores) = c("Extrovert", "Dependable", "Anxious")
fit$scores

colnames(fit$scores) = c("Extrovert", "Dependable", "Anxious") # renaming RC1 as "Verbal" factor and RC2 as "Math" factor
biplot(fit$scores, fit$loadings, xlabs=personality_data[,1], main = "Perceptual Map\n\n") # Perceptual map
abline(h=0)

abline(v=0) #add a vertical line in the graph
