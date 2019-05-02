# This script includes STATISTICAL ANALYSES for secondary dispersal.
source("./starters/secondary_dispersal_data_preparer.R")
# Two-Way ANOVA FOR SECONDARY DISPERSAL ####
# Models:
# full model:
options(na.action = na.omit)
fit.sec <- lm(log(1 / Uref) ~ type * surface, data = data.sec)
summary(fit.sec)  # R2 = 0.9002
# Call:
#   lm(formula = log(1/Uref) ~ type * surface, data = data.sec)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.46342 -0.05755  0.01643  0.07182  0.22479 
# 
# Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                              -0.72971    0.02492 -29.278  < 2e-16 ***
# typeCalligonum arborescens               -0.04128    0.03525  -1.171 0.242187    
# typeCalligonum klementzii                -0.11162    0.03525  -3.167 0.001648 ** 
# typeCalligonum leucocladum               -0.35145    0.03525  -9.971  < 2e-16 ***
# typeCalligonum rubicundum                -0.61048    0.03525 -17.320  < 2e-16 ***
# typedisc                                 -0.43839    0.03525 -12.438  < 2e-16 ***
# typefour-winged                          -0.35254    0.03396 -10.380  < 2e-16 ***
# typeT-type                               -0.31982    0.03525  -9.074  < 2e-16 ***
# typey-type                               -0.35012    0.03525  -9.933  < 2e-16 ***
# typeY-type                               -0.42624    0.03525 -12.093  < 2e-16 ***
# surfacegravel                            -0.81838    0.04170 -19.623  < 2e-16 ***
# surfaceloam                              -0.75440    0.04170 -18.089  < 2e-16 ***
# surfacesand                              -0.21916    0.04170  -5.255 2.30e-07 ***
# typeCalligonum arborescens:surfacegravel -0.02947    0.05898  -0.500 0.617529    
# typeCalligonum klementzii:surfacegravel   0.10537    0.05898   1.787 0.074684 .  
# typeCalligonum leucocladum:surfacegravel  0.14684    0.05898   2.490 0.013151 *  
# typeCalligonum rubicundum:surfacegravel   0.14869    0.05898   2.521 0.012051 *  
# typedisc:surfacegravel                    0.27450    0.05898   4.654 4.30e-06 ***
# typefour-winged:surfacegravel             0.44143    0.05822   7.582 2.02e-13 ***
# typeT-type:surfacegravel                  0.30580    0.05898   5.185 3.29e-07 ***
# typey-type:surfacegravel                  0.18955    0.05898   3.214 0.001405 ** 
# typeY-type:surfacegravel                  0.44548    0.05898   7.553 2.45e-13 ***
# typeCalligonum arborescens:surfaceloam    0.15881    0.05898   2.693 0.007357 ** 
# typeCalligonum klementzii:surfaceloam     0.06412    0.05898   1.087 0.277553    
# typeCalligonum leucocladum:surfaceloam    0.22501    0.05898   3.815 0.000156 ***
# typeCalligonum rubicundum:surfaceloam     0.27501    0.05898   4.663 4.13e-06 ***
# typedisc:surfaceloam                      0.18633    0.05898   3.159 0.001690 ** 
# typefour-winged:surfaceloam               0.41763    0.05822   7.173 3.10e-12 ***
# typeT-type:surfaceloam                    0.30728    0.05898   5.210 2.90e-07 ***
# typey-type:surfaceloam                    0.22603    0.05898   3.832 0.000145 ***
# typeY-type:surfaceloam                    0.46137    0.05898   7.823 3.82e-14 ***
# typeCalligonum arborescens:surfacesand   -0.08210    0.05898  -1.392 0.164591    
# typeCalligonum klementzii:surfacesand    -0.07253    0.05898  -1.230 0.219416    
# typeCalligonum leucocladum:surfacesand    0.05590    0.05898   0.948 0.343771    
# typeCalligonum rubicundum:surfacesand     0.07133    0.05898   1.209 0.227125    
# typedisc:surfacesand                     -0.06639    0.05898  -1.126 0.260946    
# typefour-winged:surfacesand               0.15195    0.05822   2.610 0.009366 ** 
# typeT-type:surfacesand                    0.04998    0.05898   0.847 0.397253    
# typey-type:surfacesand                   -0.11100    0.05898  -1.882 0.060495 .  
# typeY-type:surfacesand                    0.11855    0.05898   2.010 0.045025 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1057 on 443 degrees of freedom
# Multiple R-squared:  0.9002,	Adjusted R-squared:  0.8914 
# F-statistic: 102.5 on 39 and 443 DF,  p-value: < 2.2e-16

# test for significance of interaction term:
drop1(fit.sec, test = "F")
# Single term deletions
# 
# Model:
#   log(1/Uref) ~ type * surface
#              Df Sum of Sq    RSS     AIC F value    Pr(>F)    
# <none>                    4.9532 -2132.1                      
# type:surface 27    2.5412 7.4944 -1986.1  8.4179 < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# interaction is significant (F27, 443 = 8.42, P < 2.2e-16)
# the full model is also the minimal adequate model.

# model checking plots
source("./starters/plot_starter.R")
par(mfrow = c(2, 2))
plot(fit.sec)  # the model checking plots look good
par(opar)

# Further simplification, excluding interaction:
fit.sec1 <- lm(log(1 / Uref) ~ type + surface, data = data.sec)
summary(fit.sec1)
# Call:
#   lm(formula = log(1/Uref) ~ type + surface, data = data.sec)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.51097 -0.07161  0.00895  0.09478  0.25444 
# 
# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                -0.82388    0.01967 -41.879  < 2e-16 ***
# typeCalligonum arborescens -0.03144    0.02578  -1.220 0.223214    
# typeCalligonum klementzii  -0.09142    0.02578  -3.547 0.000429 ***
# typeCalligonum leucocladum -0.26234    0.02578 -10.178  < 2e-16 ***
# typeCalligonum rubicundum  -0.50735    0.02578 -19.683  < 2e-16 ***
# typedisc                   -0.35622    0.02578 -13.820  < 2e-16 ***
# typefour-winged            -0.14877    0.02540  -5.858 8.84e-09 ***
# typeT-type                 -0.18168    0.02578  -7.049 6.48e-12 ***
# typey-type                 -0.28667    0.02578 -11.122  < 2e-16 ***
# typeY-type                 -0.21261    0.02578  -8.249 1.62e-15 ***
# surfacegravel              -0.61374    0.01571 -39.078  < 2e-16 ***
# surfaceloam                -0.52042    0.01571 -33.136  < 2e-16 ***
# surfacesand                -0.20577    0.01571 -13.102  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1263 on 470 degrees of freedom
# Multiple R-squared:  0.849,	Adjusted R-squared:  0.8452 
# F-statistic: 220.3 on 12 and 470 DF,  p-value: < 2.2e-16

# the model without interaction has very similar R2 ( = 0.849)
# (partial R^2 = 0.0512)
anova(fit.sec1, fit.sec)  # significantly different
# Analysis of Variance Table
# 
# Model 1: log(1/Uref) ~ type + surface
# Model 2: log(1/Uref) ~ type * surface
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1    470 7.4944                                  
# 2    443 4.9532 27    2.5412 8.4179 < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# use dredge function
options(na.action = na.fail)
fit.sec.dredge <- dredge(fit.sec)
fit.sec.best <- get.models(fit.sec.dredge, 1)[[1]]
summary(fit.sec.best)
# Call:
#   lm(formula = log(1/Uref) ~ surface + type + surface:type + 1, 
#      data = data.sec)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.46342 -0.05755  0.01643  0.07182  0.22479 
# 
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                              -0.72971    0.02492 -29.278  < 2e-16 ***
# surfacegravel                            -0.81838    0.04170 -19.623  < 2e-16 ***
# surfaceloam                              -0.75440    0.04170 -18.089  < 2e-16 ***
# surfacesand                              -0.21916    0.04170  -5.255 2.30e-07 ***
# typeCalligonum arborescens               -0.04128    0.03525  -1.171 0.242187    
# typeCalligonum klementzii                -0.11162    0.03525  -3.167 0.001648 ** 
# typeCalligonum leucocladum               -0.35145    0.03525  -9.971  < 2e-16 ***
# typeCalligonum rubicundum                -0.61048    0.03525 -17.320  < 2e-16 ***
# typedisc                                 -0.43839    0.03525 -12.438  < 2e-16 ***
# typefour-winged                          -0.35254    0.03396 -10.380  < 2e-16 ***
# typeT-type                               -0.31982    0.03525  -9.074  < 2e-16 ***
# typey-type                               -0.35012    0.03525  -9.933  < 2e-16 ***
# typeY-type                               -0.42624    0.03525 -12.093  < 2e-16 ***
# surfacegravel:typeCalligonum arborescens -0.02947    0.05898  -0.500 0.617529    
# surfaceloam:typeCalligonum arborescens    0.15881    0.05898   2.693 0.007357 ** 
# surfacesand:typeCalligonum arborescens   -0.08210    0.05898  -1.392 0.164591    
# surfacegravel:typeCalligonum klementzii   0.10537    0.05898   1.787 0.074684 .  
# surfaceloam:typeCalligonum klementzii     0.06412    0.05898   1.087 0.277553    
# surfacesand:typeCalligonum klementzii    -0.07253    0.05898  -1.230 0.219416    
# surfacegravel:typeCalligonum leucocladum  0.14684    0.05898   2.490 0.013151 *  
# surfaceloam:typeCalligonum leucocladum    0.22501    0.05898   3.815 0.000156 ***
# surfacesand:typeCalligonum leucocladum    0.05590    0.05898   0.948 0.343771    
# surfacegravel:typeCalligonum rubicundum   0.14869    0.05898   2.521 0.012051 *  
# surfaceloam:typeCalligonum rubicundum     0.27501    0.05898   4.663 4.13e-06 ***
# surfacesand:typeCalligonum rubicundum     0.07133    0.05898   1.209 0.227125    
# surfacegravel:typedisc                    0.27450    0.05898   4.654 4.30e-06 ***
# surfaceloam:typedisc                      0.18633    0.05898   3.159 0.001690 ** 
# surfacesand:typedisc                     -0.06639    0.05898  -1.126 0.260946    
# surfacegravel:typefour-winged             0.44143    0.05822   7.582 2.02e-13 ***
# surfaceloam:typefour-winged               0.41763    0.05822   7.173 3.10e-12 ***
# surfacesand:typefour-winged               0.15195    0.05822   2.610 0.009366 ** 
# surfacegravel:typeT-type                  0.30580    0.05898   5.185 3.29e-07 ***
# surfaceloam:typeT-type                    0.30728    0.05898   5.210 2.90e-07 ***
# surfacesand:typeT-type                    0.04998    0.05898   0.847 0.397253    
# surfacegravel:typey-type                  0.18955    0.05898   3.214 0.001405 ** 
# surfaceloam:typey-type                    0.22603    0.05898   3.832 0.000145 ***
# surfacesand:typey-type                   -0.11100    0.05898  -1.882 0.060495 .  
# surfacegravel:typeY-type                  0.44548    0.05898   7.553 2.45e-13 ***
# surfaceloam:typeY-type                    0.46137    0.05898   7.823 3.82e-14 ***
# surfacesand:typeY-type                    0.11855    0.05898   2.010 0.045025 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1057 on 443 degrees of freedom
# Multiple R-squared:  0.9002,	Adjusted R-squared:  0.8914 
# F-statistic: 102.5 on 39 and 443 DF,  p-value: < 2.2e-16

drop1(fit.sec.best, test = "F")
# Single term deletions
# 
# Model:
#   log(1/Uref) ~ surface + type + surface:type + 1
#              Df Sum of Sq    RSS     AIC F value    Pr(>F)    
# <none>                    4.9532 -2132.1                      
# surface:type 27    2.5412 7.4944 -1986.1  8.4179 < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1