# analysis starter:####
# load required packages:
source("./starters/pkg_loader.R")

# load required functions:
source("./functions/predict.model.avg.R")
source("./functions/se.R")

morph.levels <- c(
  "disc",
  "y-type",
  "T-type",
  "Y-type",
  "four-winged",
  "Calligonum leucocladum",
  "Calligonum alaschanicum",
  "Calligonum arborescens",
  "Calligonum klementzii",
  "Calligonum rubicundum"
)

morph.levels.new <-
  c(
    "disc",
    "y-type",
    "T-type",
    "Y-type",
    "four-winged",
    "Calligonum arborescens",
    "Calligonum alaschanicum",
    "Calligonum klementzii",
    "Calligonum rubicundum",
    "Calligonum leucocladum"
  )
# new levels!

sur.levels <- c("gravel", "loam", "sand", "brick")