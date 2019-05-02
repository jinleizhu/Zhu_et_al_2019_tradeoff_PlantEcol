# load and prepare data for Fig. 2 ####
source("./starters/analysis_starter.R")
source("./starters/primary_dispersal_data_preparer.R")
source("./starters/secondary_dispersal_data_preparer.R")

# for panels a and b####
# calculating mean dispersal distance under different wind speeds:
data.prim.mean.ws <-
  aggregate(distance ~ type + wind.speed, data = data.prim, FUN = "mean")
names(data.prim.mean.ws)[3] <- "distance.ws"
# calculating SE of dispersal distance under different wind speeds:
data.prim.se.ws <-
  aggregate(distance ~ type + wind.speed, data = data.prim, FUN = "se")
names(data.prim.se.ws)[3] <- "se.ws"
# merge the mean and SE:
data.prim.mean.se.ws <-
  merge(data.prim.mean.ws, data.prim.se.ws, by = c("type", "wind.speed"))
# calculating mean dispersal distance under different release heights:
data.prim.mean.rh <-
  aggregate(distance ~ type + release.height,
            data = data.prim,
            FUN = "mean")
names(data.prim.mean.rh)[3] <- "distance.rh"
# calculating SE of dispersal distance under different release heights:
data.prim.se.rh <-
  aggregate(distance ~ type + release.height,
            data = data.prim,
            FUN = "se")
names(data.prim.se.rh)[3] <- "se.rh"
# merge the mean and SE:
data.prim.mean.se.rh <-
  merge(data.prim.mean.rh,
        data.prim.se.rh,
        by = c("type", "release.height"))
# merge the data:
data.prim.mean.se <-
  merge(data.prim.mean.se.ws, data.prim.mean.se.rh, by = "type")
data.prim.mean.se$type <- factor(data.prim.mean.se$type,
                                 levels = morph.levels)

# for panel c####
# calculate the mean Isdp on different surfaces:
data.sec.mean.Isdp <-
  aggregate(I(1 / Uref) ~ type + surface, data = data.sec, FUN = "mean")
names(data.sec.mean.Isdp)[3] <- "mean.Isdp"

# calculate the SE of Isdp on different surfaces:
data.sec.se.Isdp <-
  aggregate(I(1 / Uref) ~ type + surface, data = data.sec, FUN = "se")
names(data.sec.se.Isdp)[3] <- "se.Isdp"

# merge the data:
data.sec.mean.se.Isdp <-
  merge(data.sec.mean.Isdp, data.sec.se.Isdp, by = c("type", "surface"))

data.sec.mean.se.Isdp$surface <-
  factor(data.sec.mean.se.Isdp$surface,
         levels = sur.levels)

data.sec.mean.se.Isdp$type <- factor(data.sec.mean.se.Isdp$type,
                                     levels = morph.levels)