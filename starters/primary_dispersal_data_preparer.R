# read and prepare data for primary dispersal:####
# read data:
data.prim <- read.csv("./data_in/data_primary_dispersal.csv")

# data preparation:
data.prim$log.ws <- log(data.prim$wind.speed)
data.prim$log.rh <- log(data.prim$release.height)
data.prim$log.dist <- log(data.prim$distance)