#Practical 9

states <-as.data.frame(state.x77)
str(states)


#Question 2
colnames(states)[colnames(states) == "Life Exp"] <- "Life_Exp"
colnames(states)[colnames(states) == "HS Grad"] <- "HS_Grad"
View(states)



#Question 3, Pairs to test for linearity
pairs.panels(states,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

#Scatter plots to test for linearity
windows(20,12)
par(mfrow= c(4,2))

scatter.smooth(x = states$Population,
               y = states$Murder,
               main = "Correlation of Murder & Population",
               xlab = "Population",
               ylab = "Murder %")

scatter.smooth(x = states$Income,
               y = states$Murder,
               main = "Correlation of Murder & Income",
               xlab = "Income",
               ylab = "Murder %")

scatter.smooth(x = states$Illiteracy,
               y = states$Murder,
               main = "Correlation of Murder & Illiteracy",
               xlab = "Illiteracy %",
               ylab = "Murder %")

scatter.smooth(x = states$Life_Exp,
               y = states$Murder,
               main = "Correlation of Murder & Life_Exp",
               xlab = "Life_Exp",
               ylab = "Murder %")

scatter.smooth(x = states$HS_Grad,
               y = states$Murder,
               main = "Correlation of Murder & HS_Grad",
               xlab = "HS_Grad %",
               ylab = "Murder %")

scatter.smooth(x = states$Frost,
               y = states$Murder,
               main = "Correlation of Murder & Frost",
               xlab = "Frost",
               ylab = "Murder %")

scatter.smooth(x = states$Area,
               y = states$Murder,
               main = "Correlation of Murder & Area",
               xlab = "Area",
               ylab = "Murder %")

#Correlation Matrix to test for linearity
correlationmatrix <- cor(states)
round(correlationmatrix, 2)

attach(states)
paste("Correlation for Murder & Area: ",
      round(cor(Murder, Area),2))
paste("Correlation for Murder & Frost: ",
      round(cor(Murder, Frost),2))
paste("Correlation for Murder & HS_Grad: ",
      round(cor(Murder, HS_Grad),2))
paste("Correlation for Murder & Life_Exp: ",
      round(cor(Murder, Life_Exp),2))
paste("Correlation for Murder & Illiteracy: ",
      round(cor(Murder, Illiteracy),2))
paste("Correlation for Murder & Income: ",
      round(cor(Murder, Income),2))
paste("Correlation for Murder & Population: ",
      round(cor(Murder, Population),2))
