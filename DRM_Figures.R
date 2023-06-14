# Code to produce figures for Jackson & Dering: 
# 'Investigating recognition and false memory after alcohol-induced blackouts in sober young adults' 
# Created 20/5/23
# Authors: B. Dering & J. Jackson

# Packages needed
library(WRS)
library(RColorBrewer)
library(scales)
library(ggplot2)  
library(ggsci)  

###### Figure 1: dPrime and C, between groups and for MBO group only ######

# 1) Load DRM Dprime and C Between groups data
DRM_dPrimeTrim <- read.csv("DRM_Bet_DPrimeTrim.csv", header = TRUE, stringsAsFactors = T)
DRM_DPrimeT <- as.data.frame(DRM_dPrimeTrim)
View(DRM_DPrimeT)

# Create two datasets by splitting DRM_DPrimeT into dPrime and C
DRM_dpT <- DRM_DPrimeT[1:8, c("Group", "Alcohol","Condition", "Mean", "StdEr")]
DRM_cT <-  DRM_DPrimeT[9:16, c("Group", "Alcohol", "Condition", "Mean", "StdEr")]

# 2) Load DPrime and C for MBO within group data
DRM_dPrimeTrimMBO <- read.csv("DRM_MBO_DPrimeTrim.csv", header = TRUE, stringsAsFactors = T)
DRM_DPrimeTMBO <- as.data.frame(DRM_dPrimeTrimMBO)
View(DRM_DPrimeTMBO)

# Create two separate data sets for dPrime and C within MBO group.
DRM_dpTMBO <- DRM_DPrimeTMBO[1:6, c("Alcohol","Condition", "Mean", "StdEr")]
DRM_cTMBO <-  DRM_DPrimeTMBO[7:12, c("Alcohol", "Condition", "Mean", "StdEr")]
View(DRM_dpTMBO)
View(DRM_cTMBO)

###### Graph 1. dPrime Between Groups - Using trimmed means ######

pd<- position_dodge(.3) # dodge position for distancing geoms

dprimeALLT<-ggplot(DRM_dpT, aes(Alcohol, Mean, colour=Group)) 

dprimeT <- dprimeALLT + scale_x_discrete(labels = c("Before" = "Before\nAlcohol","After" = "After\nAlcohol"), 
                                         limits = c("Before", "After")) +
  facet_grid(.~Condition) +
  geom_line(aes(Alcohol, Mean, group = Group), position = pd) +
  geom_point(aes(group = Group), position = pd) +
  geom_errorbar (aes(ymin = Mean - StdEr,
                     ymax = Mean + StdEr),
                 width = .2, size = 0.5, position = pd) +
  scale_fill_npg() +
  labs(title = "a.", x = "", y = "d'",colour = "" ) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 18, angle = 90, hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.ticks.length = unit(.5, "cm"))+
  theme(text = element_text(size = 20)) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = c(.12,.18)) + guides(fill= guide_legend(title = NULL)) + 
  theme(legend.background = element_rect(fill = "white", colour = "black", size = .5)) +
  theme(legend.key.size = unit(0.05,"cm")) + theme(legend.spacing.y = unit(0.05, "cm"))
dprimeT

ggsave("dprimeT.svg", width = 6, height = 4, units = "in")
plot(dprimeT)
dev.off()

###### Graph 2. DRM c Between Groups Using Trimmed Means  ######

DRM_cT <-  DRM_DPrimeT[9:16, c("Group", "Alcohol", "Condition", "Mean", "StdEr")]
View(DRM_cT)

# Graph the data as a faceted line graph
cALLT<-ggplot(DRM_cT, aes(Alcohol, Mean, colour=Group)) 

c_betweenT <- cALLT + scale_x_discrete(labels = c("Before" = "Before\nAlcohol","After" = "After\nAlcohol"), 
                                       limits = c("Before", "After")) +
  facet_grid(.~Condition) +
  geom_line(aes(Alcohol, Mean, group = Group), position = pd) +
  geom_point(aes(group = Group), position = pd) +
  geom_errorbar (aes(ymin = Mean - StdEr,
                     ymax = Mean + StdEr),
                 width = .2, size = 0.5, position = pd) +
  scale_fill_npg() +
  labs(title = "b.", x = "", y = "C", colour = "Group") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 18, angle = 90, hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.ticks.length = unit(.5, "cm"))+
  theme(text = element_text(size = 20)) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none")

c_betweenT

ggsave("c_betweenT.svg", width = 6, height = 4, units = "in", dpi = 300)
plot(c_betweenT)
dev.off()

######  Graph 3. DRM False Alarms Between Groups Using Trimmed Means ######

# As a faceted line graph

FATrim<-ggplot(DRM_FATrim, aes(Alcohol, Mean, colour=FAType)) 

FA_BetweenT <- FATrim+ scale_x_discrete(labels = c("Before" = "Before\nAlcohol","After" = "After\nAlcohol"), 
                                        limits = c("Before", "After")) +
  facet_grid(.~ Group) +
  geom_line(aes(Alcohol, Mean, group = FAType), position = pd) +
  geom_point(aes(group = FAType), position = pd) +
  geom_errorbar (aes(ymin = Mean - StdEr,
                     ymax = Mean + StdEr),
                 width = .2, size = 0.5, position = pd) +
  scale_colour_npg() +
  labs(title = "c.", x = "", y = "False Alarm %",colour = "" ) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 18, angle = 90, hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.ticks.length = unit(.5, "cm"))+
  theme(text = element_text(size = 20)) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = c(.12,.48)) + guides(fill= guide_legend(title = NULL)) + 
  theme(legend.background = element_rect(fill = "white", colour = "black", size = .6)) +
  theme(legend.key.size = unit(0.05,"cm")) + theme(legend.spacing.y = unit(0.05, "cm"))
FA_BetweenT

ggsave("FA_BetweenT.svg", width = 6, height = 4, units = "in")
plot(FA_BetweenT)
dev.off()


###### Graph 4. DRM, dPrime, Within MBO Group Trimmed Means   ######
line_dprimeTM<-ggplot(DRM_dpTMBO, aes(Alcohol, Mean, colour = Condition))+ 
  scale_x_discrete(labels = c("Before" = "Before\nAlcohol", 
                              "After" = "After\nAlcohol", 
                              "AfterMBO" = "After\nMBO"),
                   limits = c("Before", "After", "AfterMBO"))+ 
  geom_line(aes(Alcohol, Mean, group = Condition), position = pd) +
  geom_point(aes(group = Condition), position = pd) +
  geom_errorbar (aes(ymin = Mean - StdEr,
                     ymax = Mean + StdEr),
                 width = .2, size = 0.5, position = pd) +
  scale_fill_npg() +
  labs(title = "c.", x = "", y = "d'", colour = "") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 18, angle = 90, hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.ticks.length = unit(.5, "cm"))+
  theme(text = element_text(size = 20)) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = c(.12,.2)) + guides(fill= guide_legend(title = NULL)) + 
  theme(legend.background = element_rect(fill = "white", colour = "black", size = .5)) +
  theme(legend.key.size = unit(0.05,"cm")) + theme(legend.spacing.y = unit(0.05, "cm"))

line_dprimeTM

ggsave("line_dprimeTM.svg", width = 6, height = 4, units = "in", dpi = 300)
plot(line_dprimeTM)
dev.off()

###### Graph 5. DRM, C, Within MBO Group using Trimmed Means  ######
line_DRM_CMT<-ggplot(DRM_cTMBO, aes(Alcohol, Mean, colour = Condition))+ 
  scale_x_discrete(labels = c("Before" = "Before\nAlcohol", 
                              "After" = "After\nAlcohol", 
                              "AfterMBO" = "After\nMBO"),
                   limits = c("Before", "After", "AfterMBO"))+ 
  geom_line(aes(Alcohol, Mean, group = Condition), position = pd) +
  geom_point(aes(group = Condition), position = pd) +
  geom_errorbar (aes(ymin = Mean - StdEr,
                     ymax = Mean + StdEr),
                 width = .2, size = 0.5, position = pd) +
  scale_fill_npg() +
  labs(title = "d.", x = "", y = "c", colour = "") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 18, angle = 90, hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.ticks.length = unit(.5, "cm"))+
  theme(text = element_text(size = 20)) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none")

line_DRM_CMT

ggsave("line_DRM_CMT.svg", width = 6, height = 4, units = "in", dpi = 300)
plot(line_DRM_CMT)
dev.off()


######    Graph 6. FA Within MBO Group using Trimmed Means ######

FA_WithinT <- ggplot(DRM_FATrimM, aes(Alcohol, Mean, colour = FAType))+ 
  scale_x_discrete(labels = c("Before" = "Before\nAlcohol", 
                              "After" = "After\nAlcohol", 
                              "AfterMBO" = "After\nMBO"),
                   limits = c("Before", "After", "AfterMBO"))+ 
  geom_line(aes(Alcohol, Mean, group = FAType), position = pd) +
  geom_point(aes(group = FAType), position = pd) +
  geom_errorbar (aes(ymin = Mean - StdEr,
                     ymax = Mean + StdEr),
                 width = .2, size = 0.5, position = pd) +
  scale_colour_npg() +
  labs(title = "f.", x = "", y = "False Alarm %", colour = "") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 18, angle = 90, hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 18))+
  theme(axis.ticks.length = unit(.5, "cm"))+
  theme(text = element_text(size = 20)) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = c(.12,.48)) + guides(fill= guide_legend(title = NULL)) + 
  theme(legend.background = element_rect(fill = "white", colour = "black", size = .5)) +
  theme(legend.key.size = unit(0.05,"cm")) + theme(legend.spacing.y = unit(0.05, "cm"))

FA_WithinT

ggsave("FA_WithinT.svg", width = 6, height = 4, units = "in", dpi = 300)
plot(FA_WithinT)
dev.off()


###### Figure 2: KDE plots for between groups ######

setwd("C:/Users/Benjamin Dering/Documents/R/Jackson_DRMpaper")

DRM_MBO_tests <- read.csv(file = 'DRM_DPrime_Between_tests.csv', header = TRUE, stringsAsFactors = T)
KDEs <- DRM_MBO_tests[,c("Control_Before_dFree","Control_After_dFree","Control_Before_dSerial","Control_After_dSerial")]
KDEs <- na.omit(KDEs)
View(KDEs)

x1 <- KDEs$Control_Before_dFree
x2 <- KDEs$Control_Before_dSerial
x3 <- KDEs$Control_After_dFree
x4 <- KDEs$Control_After_dSerial
x5 <- DRM_MBO_tests$MBO_Before_dFree
x6 <- DRM_MBO_tests$MBO_Before_dSerial
x7 <- DRM_MBO_tests$MBO_After_dFree
x8 <- DRM_MBO_tests$MBO_After_dSerial

x1_out <- x1[!x1 %in% boxplot.stats(x1)$out] # Find outliers according to the boxplot rule
x2_out <- x2[!x2 %in% boxplot.stats(x2)$out]
x3_out <- x3[!x3 %in% boxplot.stats(x3)$out]
x4_out <- x4[!x4 %in% boxplot.stats(x4)$out]
x5_out <- x5[!x5 %in% boxplot.stats(x5)$out]
x6_out <- x6[!x6 %in% boxplot.stats(x6)$out]
x7_out <- x7[!x7 %in% boxplot.stats(x7)$out]
x8_out <- x8[!x8 %in% boxplot.stats(x8)$out]

x1 <- na.omit(x1_out) # Omit outliers
x2 <- na.omit(x2_out)
x3 <- na.omit(x3_out)
x4 <- na.omit(x4_out)
x5 <- na.omit(x5_out)
x6 <- na.omit(x6_out)
x7 <- na.omit(x7_out)
x8 <- na.omit(x8_out)

# Calculate deciles using WRS package
deciles_d1 <- deciles(x1)
deciles_d2 <- deciles(x2)
deciles_d3 <- deciles(x3)
deciles_d4 <- deciles(x4)
deciles_d5 <- deciles(x5)
deciles_d6 <- deciles(x6)
deciles_d7 <- deciles(x7)
deciles_d8 <- deciles(x8)

# Densities
dx1 <- density(x1)
dx2 <- density(x2)
dx3 <- density(x3)
dx4 <- density(x4)
dx5 <- density(x5)
dx6 <- density(x6)
dx7 <- density(x7)
dx8 <- density(x8)

# Find the closest point to each decile on the density estimate
# Should have looped this...
d1_Dec_10 <- which.min(abs(dx1$x - deciles_d1[1])) 
d1_Dec_20 <- which.min(abs(dx1$x - deciles_d1[2]))
d1_Dec_30 <- which.min(abs(dx1$x - deciles_d1[3]))
d1_Dec_40 <- which.min(abs(dx1$x - deciles_d1[4]))
d1_Dec_50 <- which.min(abs(dx1$x - deciles_d1[5]))
d1_Dec_60 <- which.min(abs(dx1$x - deciles_d1[6]))
d1_Dec_70 <- which.min(abs(dx1$x - deciles_d1[7]))
d1_Dec_80 <- which.min(abs(dx1$x - deciles_d1[8]))
d1_Dec_90 <- which.min(abs(dx1$x - deciles_d1[9])) 

d2_Dec_10 <- which.min(abs(dx2$x - deciles_d2[1])) 
d2_Dec_20 <- which.min(abs(dx2$x - deciles_d2[2]))
d2_Dec_30 <- which.min(abs(dx2$x - deciles_d2[3]))
d2_Dec_40 <- which.min(abs(dx2$x - deciles_d2[4]))
d2_Dec_50 <- which.min(abs(dx2$x - deciles_d2[5]))
d2_Dec_60 <- which.min(abs(dx2$x - deciles_d2[6]))
d2_Dec_70 <- which.min(abs(dx2$x - deciles_d2[7]))
d2_Dec_80 <- which.min(abs(dx2$x - deciles_d2[8]))
d2_Dec_90 <- which.min(abs(dx2$x - deciles_d2[9]))                       

d3_Dec_10 <- which.min(abs(dx3$x - deciles_d3[1])) 
d3_Dec_20 <- which.min(abs(dx3$x - deciles_d3[2]))
d3_Dec_30 <- which.min(abs(dx3$x - deciles_d3[3]))
d3_Dec_40 <- which.min(abs(dx3$x - deciles_d3[4]))
d3_Dec_50 <- which.min(abs(dx3$x - deciles_d3[5]))
d3_Dec_60 <- which.min(abs(dx3$x - deciles_d3[6]))
d3_Dec_70 <- which.min(abs(dx3$x - deciles_d3[7]))
d3_Dec_80 <- which.min(abs(dx3$x - deciles_d3[8]))
d3_Dec_90 <- which.min(abs(dx3$x - deciles_d3[9]))                       

d4_Dec_10 <- which.min(abs(dx4$x - deciles_d4[1])) 
d4_Dec_20 <- which.min(abs(dx4$x - deciles_d4[2]))
d4_Dec_30 <- which.min(abs(dx4$x - deciles_d4[3]))
d4_Dec_40 <- which.min(abs(dx4$x - deciles_d4[4]))
d4_Dec_50 <- which.min(abs(dx4$x - deciles_d4[5]))
d4_Dec_60 <- which.min(abs(dx4$x - deciles_d4[6]))
d4_Dec_70 <- which.min(abs(dx4$x - deciles_d4[7]))
d4_Dec_80 <- which.min(abs(dx4$x - deciles_d4[8]))
d4_Dec_90 <- which.min(abs(dx4$x - deciles_d4[9]))

d5_Dec_10 <- which.min(abs(dx5$x - deciles_d5[1])) 
d5_Dec_20 <- which.min(abs(dx5$x - deciles_d5[2]))
d5_Dec_30 <- which.min(abs(dx5$x - deciles_d5[3]))
d5_Dec_40 <- which.min(abs(dx5$x - deciles_d5[4]))
d5_Dec_50 <- which.min(abs(dx5$x - deciles_d5[5]))
d5_Dec_60 <- which.min(abs(dx5$x - deciles_d5[6]))
d5_Dec_70 <- which.min(abs(dx5$x - deciles_d5[7]))
d5_Dec_80 <- which.min(abs(dx5$x - deciles_d5[8]))
d5_Dec_90 <- which.min(abs(dx5$x - deciles_d5[9]))

d6_Dec_10 <- which.min(abs(dx6$x - deciles_d6[1])) 
d6_Dec_20 <- which.min(abs(dx6$x - deciles_d6[2]))
d6_Dec_30 <- which.min(abs(dx6$x - deciles_d6[3]))
d6_Dec_40 <- which.min(abs(dx6$x - deciles_d6[4]))
d6_Dec_50 <- which.min(abs(dx6$x - deciles_d6[5]))
d6_Dec_60 <- which.min(abs(dx6$x - deciles_d6[6]))
d6_Dec_70 <- which.min(abs(dx6$x - deciles_d6[7]))
d6_Dec_80 <- which.min(abs(dx6$x - deciles_d6[8]))
d6_Dec_90 <- which.min(abs(dx6$x - deciles_d6[9]))

d7_Dec_10 <- which.min(abs(dx7$x - deciles_d7[1])) 
d7_Dec_20 <- which.min(abs(dx7$x - deciles_d7[2]))
d7_Dec_30 <- which.min(abs(dx7$x - deciles_d7[3]))
d7_Dec_40 <- which.min(abs(dx7$x - deciles_d7[4]))
d7_Dec_50 <- which.min(abs(dx7$x - deciles_d7[5]))
d7_Dec_60 <- which.min(abs(dx7$x - deciles_d7[6]))
d7_Dec_70 <- which.min(abs(dx7$x - deciles_d7[7]))
d7_Dec_80 <- which.min(abs(dx7$x - deciles_d7[8]))
d7_Dec_90 <- which.min(abs(dx7$x - deciles_d7[9]))

d8_Dec_10 <- which.min(abs(dx8$x - deciles_d8[1])) 
d8_Dec_20 <- which.min(abs(dx8$x - deciles_d8[2]))
d8_Dec_30 <- which.min(abs(dx8$x - deciles_d8[3]))
d8_Dec_40 <- which.min(abs(dx8$x - deciles_d8[4]))
d8_Dec_50 <- which.min(abs(dx8$x - deciles_d8[5]))
d8_Dec_60 <- which.min(abs(dx8$x - deciles_d8[6]))
d8_Dec_70 <- which.min(abs(dx8$x - deciles_d8[7]))
d8_Dec_80 <- which.min(abs(dx8$x - deciles_d8[8]))
d8_Dec_90 <- which.min(abs(dx8$x - deciles_d8[9]))

# Set some figure attributes
# windowsFonts(A = windowsFont("Times New Roman"))
labels <- c("1","2","3","4","5","6","7","8","9") # Numbers above points
colour_scale <- brewer.pal(10, "RdYlGn") # Colours for points

# layout.matrix
par(mar = c(4, 4, 0.2, 0.2)) # Change margins for session
layout.matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
layout(mat = layout.matrix,
       heights = c(2,2,2,2), # Heights of the rows
       widths = c(3,3,3,3)) # Widths of the columns

# Plot Control data, Free encoding, Before (dx1) After (dx3)
plot(dx1, lwd = 2, family = "A", bty = "n", main = NA, frame = FALSE, 
     xlim = c(min(dx1$x, dx3$x), c(max(dx1$x, dx2$x))),  # Min and Max X-axis limits
     ylim = c(0,1.4), xlab = NA)
mtext(expression(paste(bold("a."))), side = 3, line = 1, # Add letters for subplots in top left corner
      at = par("usr")[1]+0.05*diff(par("usr")[1:2]))
lines(deciles_d1[1], dx1$y[d1_Dec_10], type = "b", pch = 21, cex = 1.5, bg = colour_scale[1], col = "black", lwd = 2)
lines(deciles_d1[2], dx1$y[d1_Dec_20], type = "b", pch = 21, cex = 1.5, bg = colour_scale[2], col = "black", lwd = 2)
lines(deciles_d1[3], dx1$y[d1_Dec_30], type = "b", pch = 21, cex = 1.5, bg = colour_scale[3], col = "black", lwd = 2)
lines(deciles_d1[4], dx1$y[d1_Dec_40], type = "b", pch = 21, cex = 1.5, bg = colour_scale[4], col = "black", lwd = 2)
lines(deciles_d1[5], dx1$y[d1_Dec_50], type = "b", pch = 21, cex = 1.5, bg = colour_scale[5], col = "black", lwd = 2)
lines(deciles_d1[6], dx1$y[d1_Dec_60], type = "b", pch = 21, cex = 1.5, bg = colour_scale[6], col = "black", lwd = 2)
lines(deciles_d1[7], dx1$y[d1_Dec_70], type = "b", pch = 21, cex = 1.5, bg = colour_scale[7], col = "black", lwd = 2)
lines(deciles_d1[8], dx1$y[d1_Dec_80], type = "b", pch = 21, cex = 1.5, bg = colour_scale[8], col = "black", lwd = 2)
lines(deciles_d1[9], dx1$y[d1_Dec_90], type = "b", pch = 21, cex = 1.5, bg = colour_scale[9], col = "black", lwd = 2)
# We didn't loop this code to be able to manually adjust elements, if warranted
text(x = deciles_d1[1], y = dx1$y[d1_Dec_10] + 0.13, labels[1], col = "black", family = "A")
#text(x = deciles_d1[2], y = dx1$y[d1_Dec_20] + 0.13, labels[2], col = "black", family = "A")
text(x = deciles_d1[3], y = dx1$y[d1_Dec_30] + 0.13, labels[3], col = "black", family = "A")
#text(x = deciles_d1[4], y = dx1$y[d1_Dec_40] + 0.13, labels[4], col = "black", family = "A")
text(x = deciles_d1[5], y = dx1$y[d1_Dec_50] + 0.13, labels[5], col = "black", family = "A")
#text(x = deciles_d1[6], y = dx1$y[d1_Dec_60] + 0.13, labels[6], col = "black", family = "A")
text(x = deciles_d1[7], y = dx1$y[d1_Dec_70] + 0.13, labels[7], col = "black", family = "A")
#text(x = deciles_d1[8], y = dx1$y[d1_Dec_80] + 0.13, labels[8], col = "black", family = "A")
text(x = deciles_d1[9], y = dx1$y[d1_Dec_90] + 0.13, labels[9], col = "black", family = "A")

lines(dx3, lwd = 2, family = "A", xlab=NULL, ylab=NULL, main=NULL, frame = FALSE, col = colour_scale[2])
lines(deciles_d3[1], dx3$y[d3_Dec_10], type = "b", pch = 21, cex = 1.5, bg = colour_scale[1], col = colour_scale[2], lwd = 2)
lines(deciles_d3[2], dx3$y[d3_Dec_20], type = "b", pch = 21, cex = 1.5, bg = colour_scale[2], col = colour_scale[2], lwd = 2)
lines(deciles_d3[3], dx3$y[d3_Dec_30], type = "b", pch = 21, cex = 1.5, bg = colour_scale[3], col = colour_scale[2], lwd = 2)
lines(deciles_d3[4], dx3$y[d3_Dec_40], type = "b", pch = 21, cex = 1.5, bg = colour_scale[4], col = colour_scale[2], lwd = 2)
lines(deciles_d3[5], dx3$y[d3_Dec_50], type = "b", pch = 21, cex = 1.5, bg = colour_scale[5], col = colour_scale[2], lwd = 2)
lines(deciles_d3[6], dx3$y[d3_Dec_60], type = "b", pch = 21, cex = 1.5, bg = colour_scale[6], col = colour_scale[2], lwd = 2)
lines(deciles_d3[7], dx3$y[d3_Dec_70], type = "b", pch = 21, cex = 1.5, bg = colour_scale[7], col = colour_scale[2], lwd = 2)
lines(deciles_d3[8], dx3$y[d3_Dec_80], type = "b", pch = 21, cex = 1.5, bg = colour_scale[8], col = colour_scale[2], lwd = 2)
lines(deciles_d3[9], dx3$y[d3_Dec_90], type = "b", pch = 21, cex = 1.5, bg = colour_scale[9], col = colour_scale[2], lwd = 2)
text(x = deciles_d3[1], y = dx3$y[d3_Dec_10] + 0.13, labels[1], col = "red", family = "A")
#text(x = deciles_d3[2], y = dx3$y[d3_Dec_20] + 0.13, labels[2], col = "red", family = "A")
text(x = deciles_d3[3], y = dx3$y[d3_Dec_30] + 0.13, labels[3], col = "red", family = "A")
#text(x = deciles_d3[4], y = dx3$y[d3_Dec_40] + 0.13, labels[4], col = "red", family = "A")
text(x = deciles_d3[5], y = dx3$y[d3_Dec_50] + 0.13, labels[5], col = "red", family = "A")
#text(x = deciles_d3[6], y = dx3$y[d3_Dec_60] + 0.13, labels[6], col = "red", family = "A")
text(x = deciles_d3[7], y = dx3$y[d3_Dec_70] + 0.13, labels[7], col = "red", family = "A")
#text(x = deciles_d3[8], y = dx3$y[d3_Dec_80] - 0.13, labels[8], col = "red", family = "A")
text(x = deciles_d3[9], y = dx3$y[d3_Dec_90] + 0.13, labels[9], col = "red", family = "A")

# Controls, Serial encoding, Before (dx2) After (dx4)
plot(dx2, lwd = 2, family = "A", bty = "n", main = NA, frame = FALSE, 
     xlim = c(min(dx2$x, dx4$x), c(max(dx2$x, dx4$x))), ylim = c(0,1.4), ylab = NA, xlab = substitute(paste(italic("d'"))))
mtext(expression(paste(bold("c."))), side = 3, line = 1, 
      at = par("usr")[1]+0.05*diff(par("usr")[1:2]))
lines(deciles_d2[1], dx2$y[d2_Dec_10], type = "b", pch = 21, cex = 1.5, bg = colour_scale[1], col = "black", lwd = 2)
lines(deciles_d2[2], dx2$y[d2_Dec_20], type = "b", pch = 21, cex = 1.5, bg = colour_scale[2], col = "black", lwd = 2)
lines(deciles_d2[3], dx2$y[d2_Dec_30], type = "b", pch = 21, cex = 1.5, bg = colour_scale[3], col = "black", lwd = 2)
lines(deciles_d2[4], dx2$y[d2_Dec_40], type = "b", pch = 21, cex = 1.5, bg = colour_scale[4], col = "black", lwd = 2)
lines(deciles_d2[5], dx2$y[d2_Dec_50], type = "b", pch = 21, cex = 1.5, bg = colour_scale[5], col = "black", lwd = 2)
lines(deciles_d2[6], dx2$y[d2_Dec_60], type = "b", pch = 21, cex = 1.5, bg = colour_scale[6], col = "black", lwd = 2)
lines(deciles_d2[7], dx2$y[d2_Dec_70], type = "b", pch = 21, cex = 1.5, bg = colour_scale[7], col = "black", lwd = 2)
lines(deciles_d2[8], dx2$y[d2_Dec_80], type = "b", pch = 21, cex = 1.5, bg = colour_scale[8], col = "black", lwd = 2)
lines(deciles_d2[9], dx2$y[d2_Dec_90], type = "b", pch = 21, cex = 1.5, bg = colour_scale[9], col = "black", lwd = 2)
text(x = deciles_d2[1], y = dx2$y[d2_Dec_10] + 0.13, labels[1], col = "black", family = "A")
#text(x = deciles_d2[2], y = dx2$y[d2_Dec_20] + 0.13, labels[2], col = "black", family = "A")
text(x = deciles_d2[3], y = dx2$y[d2_Dec_30] + 0.13, labels[3], col = "black", family = "A")
#text(x = deciles_d2[4], y = dx2$y[d2_Dec_40] + 0.13, labels[4], col = "black", family = "A")
text(x = deciles_d2[5], y = dx2$y[d2_Dec_50] + 0.13, labels[5], col = "black", family = "A")
#text(x = deciles_d2[6], y = dx2$y[d2_Dec_60] + 0.13, labels[6], col = "black", family = "A")
text(x = deciles_d2[7], y = dx2$y[d2_Dec_70] + 0.13, labels[7], col = "black", family = "A")
#text(x = deciles_d2[8], y = dx2$y[d2_Dec_80] + 0.13, labels[8], col = "black", family = "A")
text(x = deciles_d2[9], y = dx2$y[d2_Dec_90] + 0.13, labels[9], col = "black", family = "A")

lines(dx4, lwd = 2, family = "A", xlab=NULL, ylab=NULL, main=NULL, frame = FALSE, col = colour_scale[2])
lines(deciles_d4[1], dx4$y[d4_Dec_10], type = "b", pch = 21, cex = 1.5, bg = colour_scale[1], col = colour_scale[2], lwd = 2)
lines(deciles_d4[2], dx4$y[d4_Dec_20], type = "b", pch = 21, cex = 1.5, bg = colour_scale[2], col = colour_scale[2], lwd = 2)
lines(deciles_d4[3], dx4$y[d4_Dec_30], type = "b", pch = 21, cex = 1.5, bg = colour_scale[3], col = colour_scale[2], lwd = 2)
lines(deciles_d4[4], dx4$y[d4_Dec_40], type = "b", pch = 21, cex = 1.5, bg = colour_scale[4], col = colour_scale[2], lwd = 2)
lines(deciles_d4[5], dx4$y[d4_Dec_50], type = "b", pch = 21, cex = 1.5, bg = colour_scale[5], col = colour_scale[2], lwd = 2)
lines(deciles_d4[6], dx4$y[d4_Dec_60], type = "b", pch = 21, cex = 1.5, bg = colour_scale[6], col = colour_scale[2], lwd = 2)
lines(deciles_d4[7], dx4$y[d4_Dec_70], type = "b", pch = 21, cex = 1.5, bg = colour_scale[7], col = colour_scale[2], lwd = 2)
lines(deciles_d4[8], dx4$y[d4_Dec_80], type = "b", pch = 21, cex = 1.5, bg = colour_scale[8], col = colour_scale[2], lwd = 2)
lines(deciles_d4[9], dx4$y[d4_Dec_90], type = "b", pch = 21, cex = 1.5, bg = colour_scale[9], col = colour_scale[2], lwd = 2)
text(x = deciles_d4[1], y = dx4$y[d4_Dec_10] + 0.13, labels[1], col = "red", family = "A")
#text(x = deciles_d4[2], y = dx4$y[d4_Dec_20] + 0.13, labels[2], col = "red", family = "A")
text(x = deciles_d4[3], y = dx4$y[d4_Dec_30] + 0.13, labels[3], col = "red", family = "A")
#text(x = deciles_d4[4], y = dx4$y[d4_Dec_40] + 0.13, labels[4], col = "red", family = "A")
text(x = deciles_d4[5], y = dx4$y[d4_Dec_50] + 0.13, labels[5], col = "red", family = "A")
#text(x = deciles_d4[6], y = dx4$y[d4_Dec_60] + 0.13, labels[6], col = "red", family = "A")
text(x = deciles_d4[7], y = dx4$y[d4_Dec_70] - 0.13, labels[7], col = "red", family = "A")
#text(x = deciles_d4[8], y = dx4$y[d4_Dec_80] - 0.13, labels[8], col = "red", family = "A")
text(x = deciles_d4[9], y = dx4$y[d4_Dec_90] - 0.13, labels[9], col = "red", family = "A")

# MBOs, Free encoding, Before (dx5) After (dx7)
plot(dx5, lwd = 2, family = "A", bty = "n", main = NA, frame = FALSE, 
     xlim = c(min(dx5$x, dx7$x), c(max(dx5$x, dx7$x))), ylim = c(0,1), ylab = NA, xlab = NA)
mtext(expression(paste(bold("b."))), side = 3, line = 1, 
      at = par("usr")[1]+0.05*diff(par("usr")[1:2]))
lines(deciles_d5[1], dx5$y[d5_Dec_10], type = "b", pch = 21, cex = 1.5, bg = colour_scale[1], col = "black", lwd = 2)
lines(deciles_d5[2], dx5$y[d5_Dec_20], type = "b", pch = 21, cex = 1.5, bg = colour_scale[2], col = "black", lwd = 2)
lines(deciles_d5[3], dx5$y[d5_Dec_30], type = "b", pch = 21, cex = 1.5, bg = colour_scale[3], col = "black", lwd = 2)
lines(deciles_d5[4], dx5$y[d5_Dec_40], type = "b", pch = 21, cex = 1.5, bg = colour_scale[4], col = "black", lwd = 2)
lines(deciles_d5[5], dx5$y[d5_Dec_50], type = "b", pch = 21, cex = 1.5, bg = colour_scale[5], col = "black", lwd = 2)
lines(deciles_d5[6], dx5$y[d5_Dec_60], type = "b", pch = 21, cex = 1.5, bg = colour_scale[6], col = "black", lwd = 2)
lines(deciles_d5[7], dx5$y[d5_Dec_70], type = "b", pch = 21, cex = 1.5, bg = colour_scale[7], col = "black", lwd = 2)
lines(deciles_d5[8], dx5$y[d5_Dec_80], type = "b", pch = 21, cex = 1.5, bg = colour_scale[8], col = "black", lwd = 2)
lines(deciles_d5[9], dx5$y[d5_Dec_90], type = "b", pch = 21, cex = 1.5, bg = colour_scale[9], col = "black", lwd = 2)
text(x = deciles_d5[1], y = dx5$y[d5_Dec_10] + 0.13, labels[1], col = "black", family = "A")
#text(x = deciles_d5[2], y = dx5$y[d5_Dec_20] - 0.13, labels[2], col = "black", family = "A")
text(x = deciles_d5[3], y = dx5$y[d5_Dec_30] + 0.13, labels[3], col = "black", family = "A")
#text(x = deciles_d5[4], y = dx5$y[d5_Dec_40] + 0.13, labels[4], col = "black", family = "A")
text(x = deciles_d5[5], y = dx5$y[d5_Dec_50] + 0.13, labels[5], col = "black", family = "A")
#text(x = deciles_d5[6], y = dx5$y[d5_Dec_60] + 0.13, labels[6], col = "black", family = "A")
text(x = deciles_d5[7], y = dx5$y[d5_Dec_70] + 0.13, labels[7], col = "black", family = "A")
#text(x = deciles_d5[8], y = dx5$y[d5_Dec_80] + 0.13, labels[8], col = "black", family = "A")
text(x = deciles_d5[9], y = dx5$y[d5_Dec_90] + 0.13, labels[9], col = "black", family = "A")

lines(dx7, lwd = 2, family = "A", xlab=NULL, ylab=NULL, main=NULL, frame = FALSE, col = colour_scale[2])
lines(deciles_d7[1], dx7$y[d7_Dec_10], type = "b", pch = 21, cex = 1.5, bg = colour_scale[1], col = colour_scale[2], lwd = 2)
lines(deciles_d7[2], dx7$y[d7_Dec_20], type = "b", pch = 21, cex = 1.5, bg = colour_scale[2], col = colour_scale[2], lwd = 2)
lines(deciles_d7[3], dx7$y[d7_Dec_30], type = "b", pch = 21, cex = 1.5, bg = colour_scale[3], col = colour_scale[2], lwd = 2)
lines(deciles_d7[4], dx7$y[d7_Dec_40], type = "b", pch = 21, cex = 1.5, bg = colour_scale[4], col = colour_scale[2], lwd = 2)
lines(deciles_d7[5], dx7$y[d7_Dec_50], type = "b", pch = 21, cex = 1.5, bg = colour_scale[5], col = colour_scale[2], lwd = 2)
lines(deciles_d7[6], dx7$y[d7_Dec_60], type = "b", pch = 21, cex = 1.5, bg = colour_scale[6], col = colour_scale[2], lwd = 2)
lines(deciles_d7[7], dx7$y[d7_Dec_70], type = "b", pch = 21, cex = 1.5, bg = colour_scale[7], col = colour_scale[2], lwd = 2)
lines(deciles_d7[8], dx7$y[d7_Dec_80], type = "b", pch = 21, cex = 1.5, bg = colour_scale[8], col = colour_scale[2], lwd = 2)
lines(deciles_d7[9], dx7$y[d7_Dec_90], type = "b", pch = 21, cex = 1.5, bg = colour_scale[9], col = colour_scale[2], lwd = 2)
text(x = deciles_d7[1], y = dx7$y[d7_Dec_10] + 0.13, labels[1], col = "red", family = "A")
#text(x = deciles_d7[2], y = dx7$y[d7_Dec_20] + 0.13, labels[2], col = "red", family = "A")
text(x = deciles_d7[3], y = dx7$y[d7_Dec_30] + 0.13, labels[3], col = "red", family = "A")
#text(x = deciles_d7[4], y = dx7$y[d7_Dec_40] + 0.13, labels[4], col = "red", family = "A")
text(x = deciles_d7[5], y = dx7$y[d7_Dec_50] + 0.13, labels[5], col = "red", family = "A")
#text(x = deciles_d7[6], y = dx7$y[d7_Dec_60] + 0.13, labels[6], col = "red", family = "A")
text(x = deciles_d7[7], y = dx7$y[d7_Dec_70] - 0.13, labels[7], col = "red", family = "A")
#text(x = deciles_d7[8], y = dx7$y[d7_Dec_80] - 0.13, labels[8], col = "red", family = "A")
text(x = deciles_d7[9], y = dx7$y[d7_Dec_90] - 0.13, labels[9], col = "red", family = "A")

# MBOs, Serial encoding, Before (dx6) After (dx8)
plot(dx6, lwd = 2, family = "A", bty = "n", main = NA, frame = FALSE, 
     xlim = c(min(dx6$x, dx8$x), c(max(dx6$x, dx8$x))), ylim = c(0,1), ylab = NA, xlab = NA)
mtext(expression(paste(bold("d."))), side = 3, line = 1, 
      at = par("usr")[1]+0.05*diff(par("usr")[1:2]))
lines(deciles_d6[1], dx6$y[d6_Dec_10], type = "b", pch = 21, cex = 1.5, bg = colour_scale[1], col = "black", lwd = 2)
lines(deciles_d6[2], dx6$y[d6_Dec_20], type = "b", pch = 21, cex = 1.5, bg = colour_scale[2], col = "black", lwd = 2)
lines(deciles_d6[3], dx6$y[d6_Dec_30], type = "b", pch = 21, cex = 1.5, bg = colour_scale[3], col = "black", lwd = 2)
lines(deciles_d6[4], dx6$y[d6_Dec_40], type = "b", pch = 21, cex = 1.5, bg = colour_scale[4], col = "black", lwd = 2)
lines(deciles_d6[5], dx6$y[d6_Dec_50], type = "b", pch = 21, cex = 1.5, bg = colour_scale[5], col = "black", lwd = 2)
lines(deciles_d6[6], dx6$y[d6_Dec_60], type = "b", pch = 21, cex = 1.5, bg = colour_scale[6], col = "black", lwd = 2)
lines(deciles_d6[7], dx6$y[d6_Dec_70], type = "b", pch = 21, cex = 1.5, bg = colour_scale[7], col = "black", lwd = 2)
lines(deciles_d6[8], dx6$y[d6_Dec_80], type = "b", pch = 21, cex = 1.5, bg = colour_scale[8], col = "black", lwd = 2)
lines(deciles_d6[9], dx6$y[d6_Dec_90], type = "b", pch = 21, cex = 1.5, bg = colour_scale[9], col = "black", lwd = 2)
text(x = deciles_d6[1], y = dx6$y[d6_Dec_10] - 0.13, labels[1], col = "black", family = "A")
#text(x = deciles_d6[2], y = dx6$y[d6_Dec_20] - 0.13, labels[2], col = "black", family = "A")
text(x = deciles_d6[3], y = dx6$y[d6_Dec_30] + 0.13, labels[3], col = "black", family = "A")
#text(x = deciles_d6[4], y = dx6$y[d6_Dec_40] + 0.13, labels[4], col = "black", family = "A")
text(x = deciles_d6[5], y = dx6$y[d6_Dec_50] + 0.13, labels[5], col = "black", family = "A")
#text(x = deciles_d6[6], y = dx6$y[d6_Dec_60] + 0.13, labels[6], col = "black", family = "A")
text(x = deciles_d6[7], y = dx6$y[d6_Dec_70] + 0.13, labels[7], col = "black", family = "A")
#text(x = deciles_d6[8], y = dx6$y[d6_Dec_80] + 0.13, labels[8], col = "black", family = "A")
text(x = deciles_d6[9], y = dx6$y[d6_Dec_90] + 0.13, labels[9], col = "black", family = "A")

lines(dx8, lwd = 2, family = "A", xlab=NULL, ylab=NULL, main=NULL, frame = FALSE, col = colour_scale[2])
lines(deciles_d8[1], dx8$y[d8_Dec_10], type = "b", pch = 21, cex = 1.5, bg = colour_scale[1], col = colour_scale[2], lwd = 2)
lines(deciles_d8[2], dx8$y[d8_Dec_20], type = "b", pch = 21, cex = 1.5, bg = colour_scale[2], col = colour_scale[2], lwd = 2)
lines(deciles_d8[3], dx8$y[d8_Dec_30], type = "b", pch = 21, cex = 1.5, bg = colour_scale[3], col = colour_scale[2], lwd = 2)
lines(deciles_d8[4], dx8$y[d8_Dec_40], type = "b", pch = 21, cex = 1.5, bg = colour_scale[4], col = colour_scale[2], lwd = 2)
lines(deciles_d8[5], dx8$y[d8_Dec_50], type = "b", pch = 21, cex = 1.5, bg = colour_scale[5], col = colour_scale[2], lwd = 2)
lines(deciles_d8[6], dx8$y[d8_Dec_60], type = "b", pch = 21, cex = 1.5, bg = colour_scale[6], col = colour_scale[2], lwd = 2)
lines(deciles_d8[7], dx8$y[d8_Dec_70], type = "b", pch = 21, cex = 1.5, bg = colour_scale[7], col = colour_scale[2], lwd = 2)
lines(deciles_d8[8], dx8$y[d8_Dec_80], type = "b", pch = 21, cex = 1.5, bg = colour_scale[8], col = colour_scale[2], lwd = 2)
lines(deciles_d8[9], dx8$y[d8_Dec_90], type = "b", pch = 21, cex = 1.5, bg = colour_scale[9], col = colour_scale[2], lwd = 2)
text(x = deciles_d8[1], y = dx8$y[d8_Dec_10] + 0.13, labels[1], col = "red", family = "A")
#text(x = deciles_d8[2], y = dx8$y[d8_Dec_20] + 0.13, labels[2], col = "red", family = "A")
text(x = deciles_d8[3], y = dx8$y[d8_Dec_30] + 0.13, labels[3], col = "red", family = "A")
#text(x = deciles_d8[4], y = dx8$y[d8_Dec_40] + 0.13, labels[4], col = "red", family = "A")
text(x = deciles_d8[5], y = dx8$y[d8_Dec_50] + 0.13, labels[5], col = "red", family = "A")
#text(x = deciles_d8[6], y = dx8$y[d8_Dec_60] - 0.13, labels[6], col = "red", family = "A")
text(x = deciles_d8[7], y = dx8$y[d8_Dec_70] - 0.13, labels[7], col = "red", family = "A")
#text(x = deciles_d8[8], y = dx8$y[d8_Dec_80] - 0.13, labels[8], col = "red", family = "A")
text(x = deciles_d8[9], y = dx8$y[d8_Dec_90] - 0.13, labels[9], col = "red", family = "A")


###### Figure 3: MBO group shift functions ######

# Read in data
DRM_MBO_tests <- read.csv(file = 'DRM_MBO_group.csv', header = TRUE, stringsAsFactors = T)

# Note, to make figure split by encoding, change between dFree and dSerial as required.
x1 <- DRM_MBO_tests$dFree_Before
x2 <- DRM_MBO_tests$dFree_After
x3 <- DRM_MBO_tests$dFree_AfterMBO
x1_out <- x1[!x1 %in% boxplot.stats(x1)$out]
x2_out <- x2[!x2 %in% boxplot.stats(x2)$out]
x3_out <- x3[!x3 %in% boxplot.stats(x3)$out]

# Get density estimates
dx1 <- density(x1_out)
dx2 <- density(x2_out)
dx3 <- density(x3_out)

# Get Deciles
deciles_d1 <- deciles(x1_out)
deciles_d2 <- deciles(x2_out)
deciles_d3 <- deciles(x3_out)

# Define values for polygons
d1_Dec_00 <- 1L
d1_Dec_10 <- min(which(dx1$x >= deciles_d1[1]))
d1_Dec_20 <- min(which(dx1$x >= deciles_d1[2]))
d1_Dec_30 <- min(which(dx1$x >= deciles_d1[3]))
d1_Dec_40 <- min(which(dx1$x >= deciles_d1[4]))
d1_Dec_50 <- min(which(dx1$x >= deciles_d1[5]))
d1_Dec_60 <- min(which(dx1$x >= deciles_d1[6]))
d1_Dec_70 <- min(which(dx1$x >= deciles_d1[7]))
d1_Dec_80 <- min(which(dx1$x >= deciles_d1[8]))
d1_Dec_90 <- min(which(dx1$x >= deciles_d1[9]))
d1_Dec_100 <- 512L

d2_Dec_00 <- 1L
d2_Dec_10 <- min(which(dx2$x >= deciles_d2[1]))
d2_Dec_20 <- min(which(dx2$x >= deciles_d2[2]))
d2_Dec_30 <- min(which(dx2$x >= deciles_d2[3]))
d2_Dec_40 <- min(which(dx2$x >= deciles_d2[4]))
d2_Dec_50 <- min(which(dx2$x >= deciles_d2[5]))
d2_Dec_60 <- min(which(dx2$x >= deciles_d2[6]))
d2_Dec_70 <- min(which(dx2$x >= deciles_d2[7]))
d2_Dec_80 <- min(which(dx2$x >= deciles_d2[8]))
d2_Dec_90 <- min(which(dx2$x >= deciles_d2[9]))
d2_Dec_100 <- 512L

d3_Dec_00 <- 1L
d3_Dec_10 <- min(which(dx3$x >= deciles_d3[1]))
d3_Dec_20 <- min(which(dx3$x >= deciles_d3[2]))
d3_Dec_30 <- min(which(dx3$x >= deciles_d3[3]))
d3_Dec_40 <- min(which(dx3$x >= deciles_d3[4]))
d3_Dec_50 <- min(which(dx3$x >= deciles_d3[5]))
d3_Dec_60 <- min(which(dx3$x >= deciles_d3[6]))
d3_Dec_70 <- min(which(dx3$x >= deciles_d3[7]))
d3_Dec_80 <- min(which(dx3$x >= deciles_d3[8]))
d3_Dec_90 <- min(which(dx3$x >= deciles_d3[9]))
d3_Dec_100 <- 512L

# layout.matrix
# svg("Within_Free_dPrime_4.svg", width = 5, height = 1.5)
layout.matrix <- matrix(c(1, 2, 3, 4), nrow = 4, ncol = 1)
layout(mat = layout.matrix,
       heights = c(2,2,2,2), # Heights of the rows
       widths = c(3,3,3,3)) # Widths of the columns

coul <- brewer.pal(10, "RdYlGn")

# plots
# First is simply the overlapping KDEs
par(mar = c(2,2,2,2))
plot(dx1, xlim = c(min(dx1$x, dx2$x, dx3$x), c(max(dx1$x, dx2$x, dx3$x))),  # Min and Max X-axis limits
     ylim = c(min(dx1$y, dx2$y, dx3$y), c(max(dx1$y, dx2$y, dx3$y))),
     bty = "n", lwd = 2, main = NA)
lines(dx2, col = 2, lwd = 2)
lines(dx3, col = 3, lwd = 2)
mtext(expression(paste(bold("a."))), side = 3, line = 1, 
      at = par("usr")[1]+0.05*diff(par("usr")[1:2]))

# Shift function for After Alcohol condition
par(mar = c(2,2,2,2))
plot(dx2, xlim = c(min(dx1$x, dx2$x, dx3$x), c(max(dx1$x, dx2$x, dx3$x))),  # Min and Max X-axis limits
     ylim = c(min(dx1$y, dx2$y, dx3$y), c(max(dx1$y, dx2$y, dx3$y))),
     bty = "n", lwd = 2, main = NA)
legend("topleft", legend = c("After"), bty = "n")
# polygon fills the area under the curve, here from decile to decile
polygon(c(dx2$x[c(d2_Dec_00, d2_Dec_00:d2_Dec_10, d2_Dec_10)]), c(0, dx2$y[d2_Dec_00:d2_Dec_10], 0),
        col = alpha(coul[1], 0.5))
polygon(c(dx2$x[c(d2_Dec_10, d2_Dec_10:d2_Dec_20, d2_Dec_20)]), c(0, dx2$y[d2_Dec_10:d2_Dec_20], 0),
        col = alpha(coul[2], 0.5))
polygon(c(dx2$x[c(d2_Dec_20, d2_Dec_20:d2_Dec_30, d2_Dec_30)]), c(0, dx2$y[d2_Dec_20:d2_Dec_30], 0),
        col = alpha(coul[3], 0.5))
polygon(c(dx2$x[c(d2_Dec_30, d2_Dec_30:d2_Dec_40, d2_Dec_40)]), c(0, dx2$y[d2_Dec_30:d2_Dec_40], 0),
        col = alpha(coul[4], 0.5))
polygon(c(dx2$x[c(d2_Dec_40, d2_Dec_40:d2_Dec_50, d2_Dec_50)]), c(0, dx2$y[d2_Dec_40:d2_Dec_50], 0),
        col = alpha(coul[5], 0.5))
polygon(c(dx2$x[c(d2_Dec_50, d2_Dec_50:d2_Dec_60, d2_Dec_60)]), c(0, dx2$y[d2_Dec_50:d2_Dec_60], 0),
        col = alpha(coul[6], 0.5))
polygon(c(dx2$x[c(d2_Dec_60, d2_Dec_60:d2_Dec_70, d2_Dec_70)]), c(0, dx2$y[d2_Dec_60:d2_Dec_70], 0),
        col = alpha(coul[7], 0.5))
polygon(c(dx2$x[c(d2_Dec_70, d2_Dec_70:d2_Dec_80, d2_Dec_80)]), c(0, dx2$y[d2_Dec_70:d2_Dec_80], 0),
        col = alpha(coul[8], 0.5))
polygon(c(dx2$x[c(d2_Dec_80, d2_Dec_80:d2_Dec_90, d2_Dec_90)]), c(0, dx2$y[d2_Dec_80:d2_Dec_90], 0),
        col = alpha(coul[9], 0.5))
polygon(c(dx2$x[c(d2_Dec_90, d2_Dec_90:d2_Dec_100, d2_Dec_100)]), c(0, dx2$y[d2_Dec_90:d2_Dec_100], 0),
        col = alpha(coul[10], 0.5))


par(mar = c(2,2,2,2))
plot(dx1, xlim = c(min(dx1$x, dx2$x, dx3$x), c(max(dx1$x, dx2$x, dx3$x))),  # Min and Max X-axis limits
     ylim = c(min(dx1$y, dx2$y, dx3$y), c(max(dx1$y, dx2$y, dx3$y))),
     bty = "n", lwd = 2, main = NA)
legend("topleft", legend = c("Before"), bty = "n")
polygon(c(dx1$x[c(d1_Dec_00, d1_Dec_00:d1_Dec_10, d1_Dec_10)]), c(0, dx1$y[d1_Dec_00:d1_Dec_10], 0),
        col = alpha(coul[1], 0.5))
polygon(c(dx1$x[c(d1_Dec_10, d1_Dec_10:d1_Dec_20, d1_Dec_20)]), c(0, dx1$y[d1_Dec_10:d1_Dec_20], 0),
        col = alpha(coul[2], 0.5))
polygon(c(dx1$x[c(d1_Dec_20, d1_Dec_20:d1_Dec_30, d1_Dec_30)]), c(0, dx1$y[d1_Dec_20:d1_Dec_30], 0),
        col = alpha(coul[3], 0.5))
polygon(c(dx1$x[c(d1_Dec_30, d1_Dec_30:d1_Dec_40, d1_Dec_40)]), c(0, dx1$y[d1_Dec_30:d1_Dec_40], 0),
        col = alpha(coul[4], 0.5))
polygon(c(dx1$x[c(d1_Dec_40, d1_Dec_40:d1_Dec_50, d1_Dec_50)]), c(0, dx1$y[d1_Dec_40:d1_Dec_50], 0),
        col = alpha(coul[5], 0.5))
polygon(c(dx1$x[c(d1_Dec_50, d1_Dec_50:d1_Dec_60, d1_Dec_60)]), c(0, dx1$y[d1_Dec_50:d1_Dec_60], 0),
        col = alpha(coul[6], 0.5))
polygon(c(dx1$x[c(d1_Dec_60, d1_Dec_60:d1_Dec_70, d1_Dec_70)]), c(0, dx1$y[d1_Dec_60:d1_Dec_70], 0),
        col = alpha(coul[7], 0.5))
polygon(c(dx1$x[c(d1_Dec_70, d1_Dec_70:d1_Dec_80, d1_Dec_80)]), c(0, dx1$y[d1_Dec_70:d1_Dec_80], 0),
        col = alpha(coul[8], 0.5))
polygon(c(dx1$x[c(d1_Dec_80, d1_Dec_80:d1_Dec_90, d1_Dec_90)]), c(0, dx1$y[d1_Dec_80:d1_Dec_90], 0),
        col = alpha(coul[9], 0.5))
polygon(c(dx1$x[c(d1_Dec_90, d1_Dec_90:d1_Dec_100, d1_Dec_100)]), c(0, dx1$y[d1_Dec_90:d1_Dec_100], 0),
        col = alpha(coul[10], 0.5))


par(mar = c(2,2,2,2))
plot(dx3, xlim = c(min(dx1$x, dx2$x, dx3$x), c(max(dx1$x, dx2$x, dx3$x))),  # Min and Max X-axis limits
     ylim = c(min(dx1$y, dx2$y, dx3$y), c(max(dx1$y, dx2$y, dx3$y))), 
     bty = "n", lwd = 2, main = NA)
legend("topleft", legend = c("AfterMBO"), bty = "n")
# rug.jitter()
polygon(c(dx3$x[c(d3_Dec_00, d3_Dec_00:d3_Dec_10, d3_Dec_10)]), c(0, dx3$y[d3_Dec_00:d3_Dec_10], 0),
        col = alpha(coul[1], 0.5))
polygon(c(dx3$x[c(d3_Dec_10, d3_Dec_10:d3_Dec_20, d3_Dec_20)]), c(0, dx3$y[d3_Dec_10:d3_Dec_20], 0),
        col = alpha(coul[2], 0.5))
polygon(c(dx3$x[c(d3_Dec_20, d3_Dec_20:d3_Dec_30, d3_Dec_30)]), c(0, dx3$y[d3_Dec_20:d3_Dec_30], 0),
        col = alpha(coul[3], 0.5))
polygon(c(dx3$x[c(d3_Dec_30, d3_Dec_30:d3_Dec_40, d3_Dec_40)]), c(0, dx3$y[d3_Dec_30:d3_Dec_40], 0),
        col = alpha(coul[4], 0.5))
polygon(c(dx3$x[c(d3_Dec_40, d3_Dec_40:d3_Dec_50, d3_Dec_50)]), c(0, dx3$y[d3_Dec_40:d3_Dec_50], 0),
        col = alpha(coul[5], 0.5))
polygon(c(dx3$x[c(d3_Dec_50, d3_Dec_50:d3_Dec_60, d3_Dec_60)]), c(0, dx3$y[d3_Dec_50:d3_Dec_60], 0),
        col = alpha(coul[6], 0.5))
polygon(c(dx3$x[c(d3_Dec_60, d3_Dec_60:d3_Dec_70, d3_Dec_70)]), c(0, dx3$y[d3_Dec_60:d3_Dec_70], 0),
        col = alpha(coul[7], 0.5))
polygon(c(dx3$x[c(d3_Dec_70, d3_Dec_70:d3_Dec_80, d3_Dec_80)]), c(0, dx3$y[d3_Dec_70:d3_Dec_80], 0),
        col = alpha(coul[8], 0.5))
polygon(c(dx3$x[c(d3_Dec_80, d3_Dec_80:d3_Dec_90, d3_Dec_90)]), c(0, dx3$y[d3_Dec_80:d3_Dec_90], 0),
        col = alpha(coul[9], 0.5))
polygon(c(dx3$x[c(d3_Dec_90, d3_Dec_90:d3_Dec_100, d3_Dec_100)]), c(0, dx3$y[d3_Dec_90:d3_Dec_100], 0),
        col = alpha(coul[10], 0.5))


###### Figure 4: Sleep Correlations ######
# Load Sleep Data
DRMCorr_Data1 <- read.csv("DRM_Sleep_Cor.csv", header = TRUE, stringsAsFactors = T)
DRMCorr_Data <- as.data.frame(DRMCorr_Data1)
str(DRMCorr_Data)

# Create two datasets split by dprime and total false alarms
DRM_CorrDP <- DRMCorr_Data[1:26, c("Minutes", "dPrime")]
DRM_CorrFA <- DRMCorr_Data[1:26, c("Minutes", "TFA_T1T3")]
str(DRM_CorrFA)

# Graph 1. DRM, dPrime by Sleep, MBO Group
str(DRM_CorrDP)

drmcorrdp<-ggplot(DRM_CorrDP, aes(Minutes, dPrime))

sleepdp <- drmcorrdp + geom_point(color = "#3C5488", alpha = 1) +
  geom_smooth(method = "lm", se = FALSE,color = "#00A087", alpha = 1)+ 
  geom_hline(yintercept=0,linetype = "dashed", color = "#00A087", alpha = 1) +
  labs(title = "", 
       x="Sleep in Minutes", 
       y= "Mean difference (d')") +
  theme_classic()+
  theme(text = element_text(size = 18)) +
  theme(axis.text.y = element_text(size = 16, angle = 90, hjust = 0.5, colour = "black")) +
  theme(axis.text.x = element_text(size = 16, colour = "black"))+
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.ticks.length = unit(.5, "cm"), axis.ticks = element_line(colour = "black"))
sleepdp

ggsave("sleepdp.svg", width = 6, height = 4, units = "in")
plot(sleepdp)
dev.off()

#  Graph 2. DRM - False Alarms by Sleep
drmcorrfa<-ggplot(DRM_CorrFA, aes(Minutes, TFA_T1T3))

sleepfa <- drmcorrfa + geom_point(color = "#3C5488", alpha = 1) +
  geom_smooth(method = "lm", se = FALSE,color = "#00A087", alpha = 1)+ 
  geom_hline(yintercept=0,linetype = "dashed", color = "#00A087", alpha = 1) +
  labs(title = "", 
       x="Sleep in Minutes", 
       y= "Mean differences (FA%)") +
  theme_classic()+
  theme(text = element_text(size = 18)) +
  theme(axis.text.y = element_text(size = 16, angle = 90, hjust = 0.5, colour = "black")) +
  theme(axis.text.x = element_text(size = 16, colour = "black"))+
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.ticks.length = unit(.5, "cm"), axis.ticks = element_line(colour = "black"))
sleepfa

ggsave("sleepfa.svg", width = 6, height = 4, units = "in")
plot(sleepfa)
dev.off()