# Data analysis code for Jackson & Dering: 
# 'Investigating recognition and false memory after alcohol-induced blackouts in sober young adults' 
# Created 20/5/23
# Authors: B. Dering & J. Jackson

library(WRS)
library(philentropy)
library(RColorBrewer)
library(scales)

###### Between Groups Analysis: D prime ######
# Load data from .csv file
DRM_descriptives <- read.csv(file = 'DRM_DPrime_Encoding_Between.csv', header = TRUE, stringsAsFactors = T)
DRM_MBO_tests <- read.csv(file = 'DRM_DPrime_Between_tests.csv', header = TRUE, stringsAsFactors = T)
dprimeC <- read.csv(file = 'DRM_Between_DPrimeC.csv', header = TRUE, stringsAsFactors = T)
View(DRM_descriptives)
View(DRM_MBO_tests)
View(dprimeC)

###### Descriptives ######
# Run for trimmed means then use FUN = trimse for standard error
aggregate(x = dFree ~ Alcohol + Group, data = DRM_descriptives, FUN = tmean)
aggregate(x = dSerial ~ Alcohol + Group, data = DRM_descriptives, FUN = tmean)
aggregate(x = cFree ~ Alcohol + Group, data = DRM_descriptives, FUN = tmean)
aggregate(x = cSerial ~ Alcohol + Group, data = DRM_descriptives, FUN = tmean)


###### d Prime analysis: Robust within by within ANOVA, 20% trimmed means ######

# Split dataframe for analysis
dprime <- DRM_MBO_tests[,c("Group","Before_dFree","After_dFree","Before_dSerial","After_dSerial")]
View(dprime)

# x <- as.matrix(dprime) # Only necessary if data needs to be in a matrix
# typeof(x) # Check data type
d = bw2list(dprime, 1, c(2,3,4,5))
bwwtrim(2,2,2, d, tr = 0.2)
bwwmcppb(2,2,2, d, tr = 0.2) # Multiple bootstrap comparisons


###### C analysis: Robust within by within ANOVA, 20% trimmed means ######

# Split dataframe for analysis
Cdecision <- dprimeC[,c("Group","Before_cFree","After_cFree","Before_cSerial","After_cSerial")]
View(Cdecision)

# x <- as.matrix(dprime) # Only necessary if data needs to be in a matrix
# typeof(x) # Check data type
C = bw2list(Cdecision, 1, c(2,3,4,5))
bwwtrim(2,2,2,C, tr = 0.2)
bwwmcppb(2,2,2,C, tr = 0.2) # Multiple comparisons, if needed (factor has more than two levels)


###### Shift Function Analyses ######
# Test the deciles between each condition (dprime only)
# Bootstraps the shifts between deciles of a distribution (runs on the data, not the KDE)

# Free encoding, Controls and MBO Participants
Dqcomhd(DRM_MBO_tests$Control_Before_dFree,
        DRM_MBO_tests$Control_After_dFree,
        q = c(1:9)/10, nboot = 2000, plotit = TRUE, SEED = TRUE, alpha = 0.05)
Dqcomhd(DRM_MBO_tests$MBO_Before_dFree,
        DRM_MBO_tests$MBO_After_dFree,
        q = c(1:9)/10, nboot = 2000, plotit = TRUE, SEED = TRUE, alpha = 0.05)
qcomhd(DRM_MBO_tests$Control_Before_dFree,
       DRM_MBO_tests$MBO_Before_dFree,
       q = c(1:9)/10, nboot = 2000, plotit = TRUE, SEED = TRUE, alpha = 0.05)
qcomhd(DRM_MBO_tests$Control_After_dFree,
       DRM_MBO_tests$MBO_After_dFree,
       q = c(1:9)/10, nboot = 2000, plotit = TRUE, SEED = TRUE, alpha = 0.05)
# Serial encoding
Dqcomhd(DRM_MBO_tests$Control_Before_dSerial,
        DRM_MBO_tests$Control_After_dSerial,
        q = c(1:9)/10, nboot = 2000, plotit = TRUE, SEED = TRUE, alpha = 0.05)
Dqcomhd(DRM_MBO_tests$MBO_Before_dSerial,
        DRM_MBO_tests$MBO_After_dSerial,
        q = c(1:9)/10, nboot = 2000, plotit = TRUE, SEED = TRUE, alpha = 0.05)
qcomhd(DRM_MBO_tests$Control_Before_dSerial,
       DRM_MBO_tests$MBO_Before_dSerial,
       q = c(1:9)/10, nboot = 2000, plotit = TRUE, SEED = TRUE, alpha = 0.05)
qcomhd(DRM_MBO_tests$Control_After_dSerial,
       DRM_MBO_tests$MBO_After_dSerial,
       q = c(1:9)/10, nboot = 2000, plotit = TRUE, SEED = TRUE, alpha = 0.05)
# Free vs. Serial
Dqcomhd(DRM_MBO_tests$Control_Before_dFree,
        DRM_MBO_tests$Control_Before_dSerial,
        q = c(1:9)/10, nboot = 2000, plotit = TRUE, SEED = TRUE, alpha = 0.05)
Dqcomhd(DRM_MBO_tests$Control_After_dFree,
        DRM_MBO_tests$Control_After_dSerial,
        q = c(1:9)/10, nboot = 2000, plotit = TRUE, SEED = TRUE, alpha = 0.05)
Dqcomhd(DRM_MBO_tests$MBO_Before_dFree,
        DRM_MBO_tests$MBO_Before_dSerial,
        q = c(1:9)/10, nboot = 2000, plotit = TRUE, SEED = TRUE, alpha = 0.05)
Dqcomhd(DRM_MBO_tests$MBO_After_dFree,
        DRM_MBO_tests$MBO_After_dSerial,
        q = c(1:9)/10, nboot = 2000, plotit = TRUE, SEED = TRUE, alpha = 0.05)

###### Between Groups Analysis: Recollection/Familiarity estimates ######

# Load between group data
DRM_Bet_Desc <- read.csv("DRM_Bet_Hits_RKG_Long.csv", header = TRUE, stringsAsFactors = T)
DRM_BetHits_wide <- read.csv("DRM_Between_Hits_RKG.csv", header = TRUE, stringsAsFactors = T)
View(DRM_BetHits_wide)

###### Descriptives ######
aggregate(Remember ~ Alcohol + Group, data = DRM_Bet_Desc, FUN = tmean)
aggregate(Remember ~ Alcohol + Group, data = DRM_Bet_Desc, FUN = trimse)
aggregate(IRK ~ Alcohol + Group, data = DRM_Bet_Desc, FUN = tmean)
aggregate(IRK ~ Alcohol + Group, data = DRM_Bet_Desc, FUN = trimse)
aggregate(Guess ~ Alcohol + Group, data = DRM_Bet_Desc, FUN = tmean)
aggregate(Guess ~ Alcohol + Group, data = DRM_Bet_Desc, FUN = trimse)

# Group x Remember Responses across alcohol conditions
RemBet = bw2list(DRM_BetHits_wide, 1, c(2,5)) # 1 means the grouping column, the others is the Remember response columns
bwtrim(2,2,RemBet, tr = 0.2)   # Factors go into this model as Group x Alcohol x Encoding x RKG
bwmcppb(2,2, RemBet, tr =0.2) 

# Group x IRK Responses across alcohol conditions
IRKBet = bw2list(DRM_BetHits_wide, 1, c(3,6)) # 1 means the grouping column, the others is the Remember response columns
bwtrim(2,2,IRKBet, tr = 0.2)   # Factors go into this model as Group x Alcohol x Encoding x RKG
bwmcppb(2,2, IRKBet, tr =0.2) 


###### Between Group Analysis: False Alarms ######
# Load data from .csv file
DRM_FA_Bet <- read.csv("DRM_FalseAlarms_Between.csv", header = TRUE, stringsAsFactors = T)
View(DRM_FA_Bet)

# Descriptives for trimmed means and for standard error
aggregate(Before_FAUnrel ~ Group, data = DRM_FA_Bet, FUN = tmean)
aggregate(After_FAUnrel ~ Group, data = DRM_FA_Bet, FUN = tmean)
aggregate(Before_FAUnrel ~ Group, data = DRM_FA_Bet, FUN = trimse)
aggregate(After_FAUnrel ~ Group, data = DRM_FA_Bet, FUN = trimse)
aggregate(Before_FACriticalLure ~ Group, data = DRM_FA_Bet, FUN = tmean)
aggregate(Before_FAWeakLure ~ Group, data = DRM_FA_Bet, FUN = tmean)
aggregate(After_FACriticalLure ~  Group, data = DRM_FA_Bet, FUN = tmean)
aggregate(After_FAWeakLure ~ Group, data = DRM_FA_Bet, FUN = tmean)
aggregate(Before_FACriticalLure ~ Group, data = DRM_FA_Bet, FUN = trimse)
aggregate(Before_FAWeakLure ~ Group, data = DRM_FA_Bet, FUN = trimse)
aggregate(After_FACriticalLure ~  Group, data = DRM_FA_Bet, FUN = trimse)
aggregate(After_FAWeakLure ~ Group, data = DRM_FA_Bet, FUN = trimse)

# Run ANOVA's
FABetCrit = bw2list(DRM_FA_Bet, 1, c(4,5,6,7,8,9))
bwwtrim(2,3,2,FABetCrit, tr = 0.2)
bwwmcppb(2,3,2, FABetCrit, tr =0.2) # multiple comparisons

###### Kullback Leibler Divergence (relative entropy) ######
# This analysis compares two probability distributions, returning their divergence in Shannon bits.

# Form a list for DRM data - Change between dFree and dSerial as required.
x1 <- DRM_MBO_tests$MBO_Before_dFree
x2 <- DRM_MBO_tests$MBO_Before_dSerial
x3 <- DRM_MBO_tests$MBO_After_dFree
x4 <- DRM_MBO_tests$MBO_After_dSerial
# Check for and remove outliers
boxplot(x1, x2, x3, x4)
x1_out <- x1[!x1 %in% boxplot.stats(x1)$out]
x2_out <- x2[!x2 %in% boxplot.stats(x2)$out]
x3_out <- x3[!x3 %in% boxplot.stats(x3)$out]
x4_out <- x4[!x4 %in% boxplot.stats(x4)$out] # Remove massive outlier in dFree AfterMBO condition

x1 <- na.omit(x1_out) # If comparing Controls to MBOs, with unequal sample sizes, use na.omit
x2 <- na.omit(x2_out)
x3 <- na.omit(x3_out)
x4 <- na.omit(x4_out)
xs <- list(x1, x2, x3, x4)

# Decided to use optimal bandwidths selected from R's base density plot (can change this, but diffs are minimal)
dx1 <- density(x1)#, bw = 0.4)
dx2 <- density(x2)#, bw = 0.4)
dx3 <- density(x3)#, bw = 0.4)
dx4 <- density(x4)

plot(dx1, lwd = 2, xlim = c(min(dx1$x, dx2$x, dx3$x, dx4$x), c(max(dx1$x, dx2$x, dx3$x, dx4$x))),  # Min and Max X-axis limits
     ylim = c(min(dx1$y, dx2$y, dx3$y, dx4$y), c(max(dx1$y, dx2$y, dx3$y, dx4$y))))
lines(dx2, col = 2, lwd = 2)
lines(dx3, col = 3, lwd = 2)
lines(dx4, col = 4, lwd = 2)

# Determine the overall range of x values
xmin <- c(min(dx1$x, dx2$x, dx3$x, dx4$x))
xmax <- c(max(dx1$x, dx2$x, dx3$x, dx4$x))

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
lines(new_ys[[4]], col = 4)

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

# Remove constant value from bins
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