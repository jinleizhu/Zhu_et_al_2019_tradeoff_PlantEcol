# This script includes STATISTICAL ANALYSES for figure 3 ####
# to plot figure 3, this script needs to be run first.
source("./starters/analysis_starter.R")
source("./starters/figure3_data_preparer.R")
for (surface in levels(data.relatp$surface))
{
  #analysis for all species
  data.surface10 <- data.relatp[data.relatp$surface == surface, ]
  unimodal.max10[[surface]] <-
    lmer(
      log(distance) ~ (log(WS) + log(RH)) * (log.Isdp + log.Isdp:log.Isdp2) + (1 |
                                                                                 primblock) + (1 | type),
      data = data.surface10,
      REML = F
    )
  unimodal.dredge10[[surface]] <- dredge(unimodal.max10[[surface]])
  unimodal.best10[[surface]] <-
    get.models(unimodal.dredge10[[surface]], 1)[[1]]
  
  #analysis without C. rubicundum
  data.surface9 <-
    data.relatp[data.relatp$surface == surface &
                  data.relatp$type != "Calligonum rubicundum", ]
  unimodal.max9[[surface]] <-
    lmer(
      log(distance) ~ (log(WS) + log(RH)) * (log.Isdp + log.Isdp:log.Isdp2) + (1 |
                                                                                 primblock) + (1 | type),
      data = data.surface9,
      REML = F
    )
  unimodal.dredge9[[surface]] <- dredge(unimodal.max9[[surface]])
  unimodal.best9[[surface]] <-
    get.models(unimodal.dredge9[[surface]], 1)[[1]]
  slope.avgmodel9 <- c(
    slope.avgmodel9,
    predict.model.avg(
      dd = unimodal.dredge9[[surface]],
      model.max = unimodal.max9[[surface]],
      newdata = preddat.Isdp.max
    )
    - predict.model.avg(
      dd = unimodal.dredge9[[surface]],
      model.max = unimodal.max9[[surface]],
      newdata = preddat.Isdp.min
    )
  )
}

slopedat$slope.avgmodel9 <- slope.avgmodel9

boxplot(slope.avgmodel9 ~ surface, data = slopedat)
abline(h = 0)
# the above boxplot shows that the trade-off became weaker as surface roughness
# increased: it was strongest on brick, but absent on gravel.

boxplot(slope.avgmodel9 ~ surface, data = slopedat, plot = F)
# $stats
# [,1]      [,2]      [,3]      [,4]
# [1,] 0.9118303 -1.299876 -1.531972 -2.183681
# [2,] 1.8276033 -1.236418 -1.513621 -2.139048
# [3,] 3.2428059 -1.152905 -1.495106 -2.067272
# [4,] 4.2857615 -1.091359 -1.481461 -1.969878
# [5,] 4.8292875 -1.049867 -1.467980 -1.899627
# 
# $n
# [1] 16 16 16 16
# 
# $conf
# [,1]      [,2]      [,3]      [,4]
# [1,] 2.271833 -1.210203 -1.507809 -2.134094
# [2,] 4.213778 -1.095606 -1.482403 -2.000450
# 
# $out
# numeric(0)
# 
# $group
# numeric(0)
# 
# $names
# [1] "gravel" "loam"   "sand"   "brick"

for (surface in levels(data.relatp$surface))
{
  print(surface)
  print(drop1(unimodal.best10[[surface]], test = "Chisq"))
}
# this shows the likelihood-ratio test for interactions in lmer models for 10 species.

# [1] "gravel"
# Single term deletions
# 
# Model:
#   log(distance) ~ log(RH) + log(WS) + log.Isdp + (1 | primblock) + 
#   (1 | type) + log(RH):log.Isdp + log(WS):log.Isdp + log.Isdp:log.Isdp2 + 
#   log(RH):log.Isdp:log.Isdp2 + log(WS):log.Isdp:log.Isdp2
#                            Df     AIC     LRT  Pr(Chi)   
# <none>                        -146.30                    
# log(RH):log.Isdp:log.Isdp2  1 -149.11 -0.8090 1.000000   
# log(WS):log.Isdp:log.Isdp2  1 -140.64  7.6589 0.005649 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# [1] "loam"
# Single term deletions
# 
# Model:
#   log(distance) ~ log(RH) + log(WS) + log.Isdp + (1 | primblock) + 
#   (1 | type) + log(WS):log.Isdp + log.Isdp:log.Isdp2 + log(WS):log.Isdp:log.Isdp2
#                            Df     AIC    LRT   Pr(Chi)    
# <none>                        -141.57                     
# log(RH)                     1 -111.90 31.672 1.825e-08 ***
# log(WS):log.Isdp:log.Isdp2  1 -141.97  1.602    0.2056    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# [1] "sand"
# Single term deletions
# 
# Model:
#   log(distance) ~ log(RH) + log(WS) + log.Isdp + (1 | primblock) + 
#   (1 | type) + log(WS):log.Isdp
#                  Df      AIC    LRT   Pr(Chi)    
# <none>              -135.182                     
# log(RH)           1  -96.618 40.564 1.903e-10 ***
# log(WS):log.Isdp  1 -130.638  6.544   0.01052 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# [1] "brick"
# Single term deletions
# 
# Model:
#   log(distance) ~ log(RH) + log(WS) + log.Isdp + (1 | primblock) + 
#   (1 | type) + log(WS):log.Isdp + log.Isdp:log.Isdp2 + log(WS):log.Isdp:log.Isdp2
#                            Df     AIC    LRT  Pr(Chi)    
# <none>                        -148.96                    
# log(RH)                     1 -111.90 39.060 4.11e-10 ***
# log(WS):log.Isdp:log.Isdp2  1 -141.97  8.989 0.002715 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

for (surface in levels(data.relatp$surface))
{
  print(surface)
  print(drop1(unimodal.best9[[surface]], test = "Chisq"))
}
# this shows the likelihood-ratio test for interactions in lmer models for nine species, without c. rubicundum.

# [1] "gravel"
# Single term deletions
# 
# Model:
#   log(distance) ~ log(RH) + log(WS) + log.Isdp + (1 | primblock) + 
#   (1 | type) + log(RH):log.Isdp + log.Isdp:log.Isdp2 + log(RH):log.Isdp:log.Isdp2
#                            Df     AIC    LRT   Pr(Chi)    
# <none>                        -149.27                     
# log(WS)                     1 -106.44 44.828 2.151e-11 ***
# log(RH):log.Isdp:log.Isdp2  1 -157.07 -5.806         1    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# [1] "loam"
# Single term deletions
# 
# Model:
#   log(distance) ~ log(RH) + log(WS) + log.Isdp + (1 | primblock) + 
#   (1 | type) + log(RH):log.Isdp
#                  Df     AIC    LRT   Pr(Chi)    
# <none>              -147.30                     
# log(WS)           1 -110.17 39.134 3.956e-10 ***
# log(RH):log.Isdp  1 -157.35 -8.053         1    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# [1] "sand"
# Single term deletions
# 
# Model:
#   log(distance) ~ log(RH) + log(WS) + log.Isdp + (1 | primblock) + 
#   (1 | type)
#          Df     AIC    LRT   Pr(Chi)    
# <none>      -153.43                     
# log(RH)   1 -119.12 36.311 1.682e-09 ***
# log(WS)   1 -108.73 46.696 8.290e-12 ***
# log.Isdp  1 -145.99  9.436  0.002128 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# [1] "brick"
# Single term deletions
# 
# Model:
#   log(distance) ~ log(RH) + log(WS) + log.Isdp + (1 | primblock) + 
#   (1 | type) + log(RH):log.Isdp
#                  Df     AIC    LRT  Pr(Chi)    
# <none>              -158.79                    
# log(WS)           1 -110.17 50.622 1.12e-12 ***
# log(RH):log.Isdp  1 -157.35  3.435  0.06383 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1