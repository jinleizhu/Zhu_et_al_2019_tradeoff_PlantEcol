# load data of seed morphology:
source("./starters/analysis_starter.R")
source("./functions/velocity.fac.log.R")

data.morp <-
  read.csv(file = "./data_in/data_seed_morphology.csv", header = T) # seed morphology data of 10 seed morphologies
data.morp$vh.ratio <-
  data.morp$height / ((data.morp$length + data.morp$width) / 2)
# Calculate the interception parameter p and add them to data.morp:
data.morp$p.b <-
  velocity.fac.log(data.morp$height, 0.01749171, 0, 1000) # on brick
data.morp$p.s <-
  velocity.fac.log(data.morp$height, 0.03806755, 0, 1000) # on sand
data.morp$p.l <-
  velocity.fac.log(data.morp$height, 0.9000706, 0, 1000) # on loam
data.morp$p.g <-
  velocity.fac.log(data.morp$height, 0.885093, 0, 1000) # on gravel
data.morp$type <-
  factor(data.morp$type,
         levels = morph.levels)
