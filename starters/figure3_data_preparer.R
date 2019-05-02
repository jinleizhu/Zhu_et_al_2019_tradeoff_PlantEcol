# load and prepare data for ploting Figure 3 and relevant data analyses:####
source("./starters/analysis_starter.R")
data.relatp <-
  read.csv(file = "./data_in/data_relationship_bt_prim_and_sec_dispersal.csv", header = T)
data.relatp$surface <-
  factor(data.relatp$surface,
         levels = sur.levels)
data.relatp$type <-
  factor(data.relatp$type,
         levels = morph.levels)

data.relatp$primblock <- interaction(data.relatp$RH, data.relatp$WS)

unimodal.max10 <- vector("list", 4)
names(unimodal.max10) <- levels(data.relatp$surface)

unimodal.best9 <-
  unimodal.dredge9 <-
  unimodal.max9 <-
  unimodal.best10 <- unimodal.dredge10 <- unimodal.max10

options(na.action = na.fail)

RHWSdat <-
  expand.grid(RH = unique(data.relatp$RH),
              WS = unique(data.relatp$WS))
slopedat <-
  expand.grid(
    RH = unique(data.relatp$RH),
    WS = unique(data.relatp$WS),
    surface = levels(data.relatp$surface)
  )

slope.avgmodel9 <- numeric(0)
data.relatp$log.Isdp <- scale(log(data.relatp$Isdp))
data.relatp$log.Isdp2 <- data.relatp$log.Isdp

preddat.Isdp.min <- RHWSdat
preddat.Isdp.min$log.Isdp = min(data.relatp$log.Isdp)
preddat.Isdp.min$log.Isdp2 <- preddat.Isdp.min$log.Isdp

preddat.Isdp.max <- RHWSdat
preddat.Isdp.max$log.Isdp = max(data.relatp$log.Isdp)
preddat.Isdp.max$log.Isdp2 <- preddat.Isdp.max$log.Isdp