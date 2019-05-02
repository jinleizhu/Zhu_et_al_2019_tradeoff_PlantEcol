# This script includes STATISTICAL ANALYSES for primary dispersal.
# ANCOVA FOR PRIMARY DISPERSAL DISTANCE ####
source("./starters/primary_dispersal_data_preparer.R")
# Models:
# full model:
options(na.action = na.omit)
fit.prim <-
  lm(log.dist ~ type * (log.ws + log.rh), data = data.prim)
summary(fit.prim)  # R2 = 0.8096
# Call:
#   lm(formula = log.dist ~ type * (log.ws + log.rh), data = data.prim)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.55969 -0.16471 -0.01479  0.15901  1.18695 
# 
# Coefficients:
#                                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       -4.804103   0.137177 -35.021  < 2e-16 ***
# typeCalligonum arborescens         0.251790   0.193998   1.298 0.194370    
# typeCalligonum klementzii          0.887252   0.194702   4.557 5.29e-06 ***
# typeCalligonum leucocladum         0.169066   0.193998   0.871 0.383524    
# typeCalligonum rubicundum          0.344030   0.193998   1.773 0.076218 .  
# typedisc                           0.885318   0.239422   3.698 0.000219 ***
# typefour-winged                   -0.717331   0.193998  -3.698 0.000220 ***
# typeT-type                         0.304487   0.193998   1.570 0.116575    
# typey-type                         0.530440   0.194552   2.726 0.006420 ** 
# typeY-type                        -0.289556   0.193998  -1.493 0.135600    
# log.ws                             1.136175   0.029746  38.196  < 2e-16 ***
# log.rh                             0.781205   0.029746  26.263  < 2e-16 ***
# typeCalligonum arborescens:log.ws -0.090549   0.042067  -2.153 0.031396 *  
# typeCalligonum klementzii:log.ws  -0.058478   0.042185  -1.386 0.165725    
# typeCalligonum leucocladum:log.ws -0.018509   0.042067  -0.440 0.659955    
# typeCalligonum rubicundum:log.ws  -0.470401   0.042067 -11.182  < 2e-16 ***
# typedisc:log.ws                   -0.085396   0.051800  -1.649 0.099287 .  
# typefour-winged:log.ws            -0.021276   0.042067  -0.506 0.613042    
# typeT-type:log.ws                 -0.031622   0.042067  -0.752 0.452252    
# typey-type:log.ws                 -0.146498   0.042175  -3.474 0.000517 ***
# typeY-type:log.ws                  0.008644   0.042067   0.205 0.837207    
# typeCalligonum arborescens:log.rh -0.049767   0.042067  -1.183 0.236838    
# typeCalligonum klementzii:log.rh  -0.196546   0.042211  -4.656 3.29e-06 ***
# typeCalligonum leucocladum:log.rh  0.001475   0.042067   0.035 0.972036    
# typeCalligonum rubicundum:log.rh   0.041969   0.042067   0.998 0.318473    
# typedisc:log.rh                   -0.056077   0.051737  -1.084 0.278457    
# typefour-winged:log.rh             0.243448   0.042067   5.787 7.52e-09 ***
# typeT-type:log.rh                  0.046258   0.042067   1.100 0.271539    
# typey-type:log.rh                  0.042628   0.042133   1.012 0.311702    
# typeY-type:log.rh                  0.155645   0.042067   3.700 0.000218 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2578 on 6035 degrees of freedom
# (15 observations deleted due to missingness)
# Multiple R-squared:  0.8096,	Adjusted R-squared:  0.8087 
# F-statistic:   885 on 29 and 6035 DF,  p-value: < 2.2e-16

# test for significance of interaction terms:
# drop1(fit.prim, test = "F")
# Single term deletions
# 
# Model:
#   log.dist ~ type * (log.ws + log.rh)
#             Df Sum of Sq    RSS    AIC F value    Pr(>F)    
# <none>                   401.11 -16413                      
# type:log.ws  9   13.5138 414.63 -16230  22.592 < 2.2e-16 ***
# type:log.rh  9    9.3304 410.44 -16291  15.598 < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# interaction between seed morphology and wind speed (F9, 6035 = 22.59, P < 2.2e-16)
# interaction between seed morphology and release height (F9, 6035 = 15.60, P < 2.2e-16)
# the full model is also the minimal adequate model.

# model checking plots
source("./starters/plot_starter.R")
par(mfrow = c(2, 2))
plot(fit.prim)  # the model checking plots look good
par(opar)

# further simplifications of the model fit.prim, excluding interactions:
fit.prim1 <- lm(log.dist ~ type + log.ws + log.rh, data = data.prim)
summary(fit.prim1)
# Call:
#   lm(formula = log.dist ~ type + log.ws + log.rh, data = data.prim)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.59623 -0.16451 -0.01225  0.16372  1.10787 
# 
# Coefficients:
#                             Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                -4.746080   0.046839 -101.328  < 2e-16 ***
# typeCalligonum arborescens -0.128002   0.014795   -8.652  < 2e-16 ***
# typeCalligonum klementzii  -0.046932   0.014841   -3.162  0.00157 ** 
# typeCalligonum leucocladum  0.140266   0.014795    9.481  < 2e-16 ***
# typeCalligonum rubicundum  -0.369077   0.014795  -24.947  < 2e-16 ***
# typedisc                    0.488830   0.018177   26.893  < 2e-16 ***
# typefour-winged             0.263137   0.014795   17.786  < 2e-16 ***
# typeT-type                  0.438660   0.014795   29.650  < 2e-16 ***
# typey-type                  0.432341   0.014818   29.177  < 2e-16 ***
# typeY-type                  0.379337   0.014795   25.640  < 2e-16 ***
# log.ws                      1.044601   0.009922  105.278  < 2e-16 ***
# log.rh                      0.808647   0.009921   81.512  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2647 on 6053 degrees of freedom
# (15 observations deleted due to missingness)
# Multiple R-squared:  0.7988,	Adjusted R-squared:  0.7984 
# F-statistic:  2184 on 11 and 6053 DF,  p-value: < 2.2e-16

# The model without interactions between type and release height or wind speed
# has the similar R2 (= 0.7988) (partial R^2 = 0.0108)
anova(fit.prim1, fit.prim)  # significantly different
# Analysis of Variance Table
# 
# Model 1: log.dist ~ type + log.ws + log.rh
# Model 2: log.dist ~ type * (log.ws + log.rh)
#   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1   6053 423.96                                  
# 2   6035 401.11 18     22.85 19.099 < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# dredge function:
options(na.action = na.fail)
dat.tem <- na.omit(data.prim)
fit.prim <- lm(log.dist ~ type * (log.ws + log.rh), data = dat.tem)
fit.prim.dredge <- dredge(fit.prim)
fit.prim.best <- get.models(fit.prim.dredge, 1)[[1]]
summary(fit.prim.best)
# Call:
#   lm(formula = log.dist ~ log.rh + log.ws + type + log.rh:type + 
#        log.ws:type + 1, data = dat.tem)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.55969 -0.16471 -0.01479  0.15901  1.18695 
# 
# Coefficients:
#                                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       -4.804103   0.137177 -35.021  < 2e-16 ***
# log.rh                             0.781205   0.029746  26.263  < 2e-16 ***
# log.ws                             1.136175   0.029746  38.196  < 2e-16 ***
# typeCalligonum arborescens         0.251790   0.193998   1.298 0.194370    
# typeCalligonum klementzii          0.887252   0.194702   4.557 5.29e-06 ***
# typeCalligonum leucocladum         0.169066   0.193998   0.871 0.383524    
# typeCalligonum rubicundum          0.344030   0.193998   1.773 0.076218 .  
# typedisc                           0.885318   0.239422   3.698 0.000219 ***
# typefour-winged                   -0.717331   0.193998  -3.698 0.000220 ***
# typeT-type                         0.304487   0.193998   1.570 0.116575    
# typey-type                         0.530440   0.194552   2.726 0.006420 ** 
# typeY-type                        -0.289556   0.193998  -1.493 0.135600    
# log.rh:typeCalligonum arborescens -0.049767   0.042067  -1.183 0.236838    
# log.rh:typeCalligonum klementzii  -0.196546   0.042211  -4.656 3.29e-06 ***
# log.rh:typeCalligonum leucocladum  0.001475   0.042067   0.035 0.972036    
# log.rh:typeCalligonum rubicundum   0.041969   0.042067   0.998 0.318473    
# log.rh:typedisc                   -0.056077   0.051737  -1.084 0.278457    
# log.rh:typefour-winged             0.243448   0.042067   5.787 7.52e-09 ***
# log.rh:typeT-type                  0.046258   0.042067   1.100 0.271539    
# log.rh:typey-type                  0.042628   0.042133   1.012 0.311702    
# log.rh:typeY-type                  0.155645   0.042067   3.700 0.000218 ***
# log.ws:typeCalligonum arborescens -0.090549   0.042067  -2.153 0.031396 *  
# log.ws:typeCalligonum klementzii  -0.058478   0.042185  -1.386 0.165725    
# log.ws:typeCalligonum leucocladum -0.018509   0.042067  -0.440 0.659955    
# log.ws:typeCalligonum rubicundum  -0.470401   0.042067 -11.182  < 2e-16 ***
# log.ws:typedisc                   -0.085396   0.051800  -1.649 0.099287 .  
# log.ws:typefour-winged            -0.021276   0.042067  -0.506 0.613042    
# log.ws:typeT-type                 -0.031622   0.042067  -0.752 0.452252    
# log.ws:typey-type                 -0.146498   0.042175  -3.474 0.000517 ***
# log.ws:typeY-type                  0.008644   0.042067   0.205 0.837207    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2578 on 6035 degrees of freedom
# Multiple R-squared:  0.8096,	Adjusted R-squared:  0.8087 
# F-statistic:   885 on 29 and 6035 DF,  p-value: < 2.2e-16

drop1(fit.prim.best, test = "F")
# Single term deletions
# 
# Model:
#   log.dist ~ log.rh + log.ws + type + log.rh:type + log.ws:type + 
#   1
#             Df Sum of Sq    RSS    AIC F value    Pr(>F)    
# <none>                   401.11 -16413                      
# log.rh:type  9    9.3304 410.44 -16291  15.598 < 2.2e-16 ***
# log.ws:type  9   13.5138 414.63 -16230  22.592 < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1