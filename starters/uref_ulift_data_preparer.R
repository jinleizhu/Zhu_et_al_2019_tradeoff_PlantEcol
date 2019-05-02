source("./starters/seed_morp_data_preparer.R")
mean.vh.ratio <-
  with(data.morp, tapply(vh.ratio, list(type), mean)) # calculate mean of v/h ratio
mean.vt <-
  with(data.morp, tapply(terminal.velocity, list(type), mean)) # calculate mean of terminal velocity
# load and prepare data of mean Uref and Ulift
data.uref.ulift <- read.csv("./data_in/data_uref_and_ulift.csv")
# add the column of mean terminal velocity to data.uref.ulift:
data.uref.ulift$terminal.velocity <-
  mean.vt[as.character(data.uref.ulift$type)]
data.uref.ulift$vh.ratio <-
  mean.vh.ratio[as.character(data.uref.ulift$type)]
data.uref.ulift$surface <-
  factor(data.uref.ulift$surface,
         levels = c("brick", "sand", "loam", "gravel"))
data.uref.ulift$type <-
  factor(data.uref.ulift$type,
         levels = morph.levels)
dat.ab <- subset(data.uref.ulift, surface == "brick")