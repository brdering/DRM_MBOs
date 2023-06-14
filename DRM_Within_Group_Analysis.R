# Data analysis code for Jackson & Dering: 
# 'Investigating recognition and false memory after alcohol-induced blackouts in sober young adults' 
# Created 20/5/23
# Authors: B. Dering & J. Jackson

library(WRS)
library(philentropy)
library(RColorBrewer)
library(scales)
library(BayesFactor)

###### MBO Group Analysis: D prime ######
# Load data from .csv file
DRM_MBO_kdes <- read.csv(file = 'DRM_DPrime_Encoding_Within.csv', header = TRUE, stringsAsFactors = T)
DRM_MBO_tests <- read.csv(file = 'DRM_MBO_group.csv', header = TRUE, stringsAsFactors = T)
View(DRM_MBO_kdes)
View(DRM_MBO_tests)

###### Descriptives ######
# Run for trimmed means then use FUN = trimse for standard error
aggregate(x = dFree ~ Alcohol, data = DRM_MBO_kdes, FUN = trimse)
aggregate(x = dSerial ~ Alcohol, data = DRM_MBO_kdes, FUN = trimse)
aggregate(x = cFree ~ Alcohol, data = DRM_MBO_kdes, FUN = trimse)
aggregate(x = cSerial ~ Alcohol, data = DRM_MBO_kdes, FUN = trimse)
aggregate(x = PrFree ~ Alcohol, data = DRM_MBO_kdes, FUN = trimse)
aggregate(x = PrSerial ~ Alcohol, data = DRM_MBO_kdes, FUN = trimse)


###### d Prime analysis: Robust within by within ANOVA, 20% trimmed means ######

# Split dataframe for analysis
dprime <- DRM_MBO_tests[,c("dFree_Before","dFree_After","dFree_AfterMBO","dSerial_Before","dSerial_After","dSerial_AfterMBO")]
View(dprime)

# x <- as.matrix(dprime) # Only necessary if data needs to be in a matrix
# typeof(x) # Check data type
wwtrim(2,3,dprime, tr = 0.2)
wwmcppb(2,3,dprime, tr = 0.2) # Multiple comparisons


###### C analysis: Robust within by within ANOVA, 20% trimmed means ######

# Split dataframe for analysis
Cdecision <- DRM_MBO_tests[,c("CFree_Before","CFree_After","CFree_AfterMBO","CSerial_Before","CSerial_After","CSerial_AfterMBO")]
View(Cdecision)

# x <- as.matrix(dprime) # Only necessary if data needs to be in a matrix
# typeof(x) # Check data type
wwtrim(2,3,Cdecision, tr = 0.2)
wwmcppb(2,3,Cdecision, tr = 0.2) # Multiple comparisons, if needed (factor has more than two levels)


###### Shift Function Analyses ######
# Test the deciles between each condition (dprime only)
# Bootstraps the shifts between deciles of a distribution (runs on the data, not the KDE)
# Serial encoding
Dqcomhd(DRM_MBO_kdes$PrSerial[DRM_MBO_kdes$Alcohol=="Before"],
        DRM_MBO_kdes$PrSerial[DRM_MBO_kdes$Alcohol=="After"],
        q = c(1:9)/10, nboot = 2000, plotit = TRUE, SEED = TRUE, alpha = 0.05)
Dqcomhd(DRM_MBO_kdes$PrSerial[DRM_MBO_kdes$Alcohol=="Before"],
        DRM_MBO_kdes$PrSerial[DRM_MBO_kdes$Alcohol=="AfterMBO"],
        q = c(1:9)/10, nboot = 2000, plotit = TRUE, SEED = TRUE, alpha = 0.05)
Dqcomhd(DRM_MBO_kdes$PrSerial[DRM_MBO_kdes$Alcohol=="After"],
        DRM_MBO_kdes$PrSerial[DRM_MBO_kdes$Alcohol=="AfterMBO"],
        q = c(1:9)/10, nboot = 2000, plotit = TRUE, SEED = TRUE, alpha = 0.05)
# Free encoding
Dqcomhd(DRM_MBO_kdes$PrFree[DRM_MBO_kdes$Alcohol=="Before"],
        DRM_MBO_kdes$PrFree[DRM_MBO_kdes$Alcohol=="After"],
        q = c(1:9)/10, nboot = 2000, plotit = TRUE, SEED = TRUE, alpha = 0.05)
Dqcomhd(DRM_MBO_kdes$PrFree[DRM_MBO_kdes$Alcohol=="Before"],
        DRM_MBO_kdes$PrFree[DRM_MBO_kdes$Alcohol=="AfterMBO"],
        q = c(1:9)/10, nboot = 2000, plotit = TRUE, SEED = TRUE, alpha = 0.05)
Dqcomhd(DRM_MBO_kdes$PrFree[DRM_MBO_kdes$Alcohol=="After"],
        DRM_MBO_kdes$PrFree[DRM_MBO_kdes$Alcohol=="AfterMBO"],
        q = c(1:9)/10, nboot = 2000, plotit = TRUE, SEED = TRUE, alpha = 0.05)

# Comparing across encoding types
Dqcomhd(DRM_MBO_kdes$PrSerial[DRM_MBO_kdes$Alcohol=="Before"],
        DRM_MBO_kdes$PrFree[DRM_MBO_kdes$Alcohol=="Before"],
        q = c(1:9)/10, nboot = 2000, plotit = TRUE, SEED = TRUE, alpha = 0.05)
Dqcomhd(DRM_MBO_kdes$PrSerial[DRM_MBO_kdes$Alcohol=="After"],
        DRM_MBO_kdes$PrFree[DRM_MBO_kdes$Alcohol=="After"],
        q = c(1:9)/10, nboot = 2000, plotit = TRUE, SEED = TRUE, alpha = 0.05)
Dqcomhd(DRM_MBO_kdes$PrSerial[DRM_MBO_kdes$Alcohol=="AfterMBO"],
        DRM_MBO_kdes$PrFree[DRM_MBO_kdes$Alcohol=="AfterMBO"],
        q = c(1:9)/10, nboot = 2000, plotit = TRUE, SEED = TRUE, alpha = 0.05)

###### Within Group Analysis of Recollection/IRK ######

# Load MBO group RKG data (with Encoding removed)
DRM_MBOTotalHits_kdes <- read.csv("DRM_Within_TotalHits_kdes.csv", header = TRUE, stringsAsFactors = T)
View(DRM_MBOTotalHits_kdes)
str(DRM_MBOTotalHits_kdes)

DRM_MBOTotalHits_tests <- read.csv("DRM_MBOTotalHits_tests.csv", header = TRUE, stringsAsFactors = T)
View(DRM_MBOTotalHits_tests)
str(DRM_MBOTotalHits_tests)

###### Descriptives - Without Encoding Factor ######
# Run for trimmed means then use FUN = trimse for standard error
aggregate(Remember ~ Alcohol, data = DRM_MBOTotalHits_kdes, FUN = tmean)  #changed from original code by removing x= before the dFree within brackets.Aggr
aggregate(IRK ~ Alcohol, data = DRM_MBOTotalHits_kdes, FUN = tmean)
aggregate(Guess ~ Alcohol, data = DRM_MBOTotalHits_kdes, FUN = tmean)
aggregate(Remember ~ Alcohol, data = DRM_MBOTotalHits_kdes, FUN = trimse)
aggregate(IRK ~ Alcohol, data = DRM_MBOTotalHits_kdes, FUN = trimse)
aggregate(Guess ~ Alcohol, data = DRM_MBOTotalHits_kdes, FUN = trimse)


# Within Hits: Alcohol x Remember Only
# Robust ANOVA with trimmed means (20%) using WRS package and rmanova function.
Remember <- Hits_RKG[,c(1,4,7)] #Retains columns 1,4 & 7 from Hits_RKG dataframe
View(Remember) # Check this worked
rmanova(Remember, tr= .2)
rmmcp(Remember)

# Within Hits: Alcohol x IRK only
IRK <- Hits_RKG[,c(2,5,8)] #Retains columns 2,5 & 8 from Hits_RKG dataframe
View(IRK) # Check this worked
rmanova(IRK, tr= .2)
rmmcp(IRK)

###### Within Group Analysis: False Alarms  ######
# Load data from .csv file

DRM_FA_Within <- read.csv("DRM_FA_Within.csv", header = TRUE, stringsAsFactors = T)
View(DRM_FA_Within)

# Descriptives for trimmed means and standard error
aggregate(UnrelFATotal ~ Alcohol, data = DRM_FA_Within, FUN = tmean)
aggregate(UnrelFATotal ~ Alcohol, data = DRM_FA_Within, FUN = trimse)
aggregate(FACritLure ~ Alcohol, data = DRM_FA_Within, FUN = tmean)
aggregate(FAWeakLure ~ Alcohol, data = DRM_FA_Within, FUN = tmean)
aggregate(FACritLure~ Alcohol, data = DRM_FA_Within, FUN = trimse)
aggregate(FAWeakLure ~ Alcohol, data = DRM_FA_Within, FUN = trimse)

#### Run ANOVA's
FAWithCrit = bw2list(DRM_FA_Within, 1, c(3,4,5)) # Prep file for ANOVA
wwtrim(3,3,FAWithCrit, tr = 0.2)   # Factors go into this model as Alcohol x Rel/Unrel
wwmcppb(3,3, FAWithCrit, tr =0.2) # multiple comparisons


###### DRM - Sleep by DPrime ######

DRMCorr_Data1 <- read.csv("DRM_Sleep_Cor.csv")
DRMCorr_Data <- as.data.frame(DRMCorr_Data1)
str(DRMCorr_Data)
DRM_CorrDP <- DRMCorr_Data[1:23, c("Minutes", "dPrime")]
str(DRM_CorrDP)

modeldrmdp <- lm(formula = dPrime ~ Minutes, 
                 data = DRM_CorrDP)
summary(modeldrmdp)

baysianDRM = regressionBF(dPrime~.,data=DRM_CorrDP) 
baysianDRM
# [1] Minutes : 0.3962129 ±0%


###### DRM - Sleep by False Alarms ######

DRM_CorrFA <- DRMCorr_Data[1:23, c("Minutes", "TFA_T1T3")]
str(DRM_CorrFA)

modeldrmfa <- lm(formula = TFA_T1T3 ~ Minutes, 
                 data = DRM_CorrFA)
summary(modeldrmfa)

baysianDRMFA = regressionBF(TFA_T1T3 ~.,data=DRM_CorrFA) 
baysianDRMFA

###### Kullback Leibler Divergence (relative entropy) ######
# This analysis compares two probability distributions, returning their divergence in Shannon bits.
# Analysis compares the KDE's from each condition

# Form a list for DRM data - Change between dFree and dSerial as required.
# Need to compare across Free and Serial encoding to show magnitude of effects.
x1 <- DRM_MBO_tests$dFree_Before
x2 <- DRM_MBO_tests$dFree_After
x3 <- DRM_MBO_tests$dFree_AfterMBO
x1_out <- x1[!x1 %in% boxplot.stats(x1)$out]
x2_out <- x2[!x2 %in% boxplot.stats(x2)$out]
x3_out <- x3[!x3 %in% boxplot.stats(x3)$out] # Remove massive outlier in dFree AfterMBO condition
boxplot(x1, x2, x3)
xs <- list(x1_out, x2_out, x3_out)

# Testing variances and non-parametric differences
# bprm(xs) # p.434 Wilcox. Brunner's (2002) method for improving on Freidman's test
# rmrvar(xs, est = sd) # Comparing variances using percentile bootstrapping

# Decided to use optimal bandwidths selected from R's base density plot (can change this, but diffs are minimal)
dx1 <- density(x1_out)#, bw = 0.4)
dx2 <- density(x2_out)#, bw = 0.4)
dx3 <- density(x3_out)#, bw = 0.4)

plot(dx1, lwd = 2, xlim = c(min(dx1$x, dx2$x, dx3$x), c(max(dx1$x, dx2$x, dx3$x))),  # Min and Max X-axis limits
     ylim = c(min(dx1$y, dx2$y, dx3$y), c(max(dx1$y, dx2$y, dx3$y))))
lines(dx2, col = 2, lwd = 2)
lines(dx3, col = 3, lwd = 2)

# Determine the overall range of x values
xmin <- c(min(dx1$x, dx2$x, dx3$x))
xmax <- c(max(dx1$x, dx2$x, dx3$x))

# Create a new x-axis vector that spans the overall range with desired number of points
n_points <- 1000
new_x <- seq(from = xmin, to = xmax, length.out = n_points)

# Interpolate each KDE estimate onto the new x-axis
new_ys <- list()
for (x in xs) {
  kde <- density(x)#, bw = 0.4)
  new_ys[[length(new_ys) + 1]] <- approx(x = kde$x, y = kde$y, xout = new_x, method = "linear", rule = 2)$y
}

# Replace any negative values with zeros
new_ys <- lapply(new_ys, function(y) replace(y, y < 0, 0))

plot(new_ys[[1]], type = "l")
lines(new_ys[[2]], col = 2)
lines(new_ys[[3]], col = 3)

# Split KDE's into equal bins (50) and integrate over each bin, to form probabilities
low <- c(1L,20L,40L,60L,80L,100L,120L,140L,160L,180L,200L,220L,240L,260L,280L,300L,320L,340L,360L,380L,400L,420L,440L,460L,480L,500L,520L,540L,560L,580L,600L,620L,640L,660L,680L,700L,720L,740L,760L,780L,800L,820L,840L,860L,880L,900L,920L,940L,960L,980L)
upp <- c(20L,40L,60L,80L,100L,120L,140L,160L,180L,200L,220L,240L,260L,280L,300L,320L,340L,360L,380L,400L,420L,440L,460L,480L,500L,520L,540L,560L,580L,600L,620L,640L,660L,680L,700L,720L,740L,760L,780L,800L,820L,840L,860L,880L,900L,920L,940L,960L,980L,1000L)

p_probs <- c() # Test distribution
q_probs <- c() # Approximating distribution

# Ignore warnings, loop fills p_probs & q_probs with the probability estimates
for(i in 1:50){
  p_area <- integrate(approxfun(new_x,new_ys[[1]]), new_x[low[i]], new_x[upp[i]])
  q_area <- integrate(approxfun(new_x,new_ys[[2]]), new_x[low[i]], new_x[upp[i]])
  p_probs[i] <- p_area
  q_probs[i] <- q_area
}
p_probs
q_probs
# Change list to vectors
p <- unlist(p_probs,use.names = FALSE)
q <- unlist(q_probs,use.names = FALSE)
sum(p) # Test that the vectors sum to 1, then correct estimates (each one equally) to make precisely 1
sum(q)

# Change estimates to sum precisely to 1
# Starting from 50 (number of bins), adjust the constant to converge on number 
# by checking the count of values greater than the constant (sum(p > constant_p).
constant_p <- (sum(p)-1)/50 
constant_q <- (sum(q)-1)/50
sum(p > constant_p)
sum(q > constant_q)

p[p > constant_p] <- p[p > constant_p] - constant_p
q[q > constant_q] <- q[q > constant_q] - constant_q
sum(p)
sum(q)

# KL test
xtest <- rbind(p,q)
Rel_entropy <- KL(xtest, unit = "log2")
Rel_entropy

# Shannon Entropy
Entropy <- H(p, unit = "log2")
Rel_entropy/Entropy

# K(P||Q) is the extra encoding cost above the minimum encoding cost
# that would have been obtained by using P instead of Q. In the KL test, Q is approximating P,
# therefore, this is a measure of divergence between Q approximating P. If H(p) is calculated
# can do K(P||Q)/H(p) to get percentage extra bits needed to describe the situation with Q
# rather than P.