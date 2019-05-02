#plot Fig. 3####
source("./starters/plot_starter.R")
source("./analysis_figure3.R")
png(
  filename = "./plots/Figure3.png",
  width = 9.6,
  height = 8,
  units = "in",
  res = 300
)
cols <- brewer.pal(n = 4, name = "Set1")
par(
  mfrow = c(2, 2),
  bty = "l",
  las = 1,
  tck = 0.03,
  mar = c(4, 5, 3, 1) + 0.1,
  mgp = c(3, 1, 0)
)
name <- c("(d) gravel", "(c) loam", "(b) sand", "(a) brick")

surfval <- levels(data.relatp$surface)
WSval <- c(4, 6, 8, 10)
for (j in 4:1) {
  plotdat10 <-
    data.relatp[data.relatp$RH == 80 &
                  data.relatp$surface == surfval[j],]
  plotdat9 <- plotdat10[plotdat10$type != "Calligonum rubicundum", ]
  
  plot(
    distance ~ I(Isdp),
    data = plotdat10,
    log = "y",
    xlim = c(floor(100 * min(plotdat10$Isdp)), ceiling(100 * max(plotdat10$Isdp))) /
      100,
    ylim = c(floor(10 * min(
      plotdat10$distance
    )), ceiling(10 * max(
      plotdat10$distance
    ))) / 10,
    pch = c(19, 17)[(plotdat10$type == "Calligonum rubicundum") + 1],
    col = cols[factor(WS)],
    xlab = expression("Secondary dispersal potential " * italic("I"["sdp"]) * "(s " * m ^
                        -1 * ")"),
    ylab = "Primary dispersal distance (m)",
    cex = 1.8,
    cex.axis = 1.6,
    cex.lab = 1.6
  )
  
  title(main = name[j],
        cex.main = 1.8,
        adj = 0)
  if (j == 3)
    legend(
      x = "topright",
      paste0(levels(factor(data.relatp$WS)), rep(" m/s", 4)),
      horiz = F,
      fill = cols,
      bty = "n",
      cex = 1.2,
      y.intersp = 0.8
    )
  for (i in 1:4) {
    newdata <-
      data.frame(
        Isdp = seq(min(plotdat10$Isdp), max(plotdat10$Isdp), length = 200),
        WS = WSval[i],
        RH = 80,
        surface = surfval[j]
      )
    newdata$log.Isdp <-
      drop(scale(
        log(newdata$Isdp),
        center = attr(data.relatp$log.Isdp, "scaled:center"),
        scale = attr(data.relatp$log.Isdp, "scaled:scale")
      ))
    newdata$log.Isdp2 <- newdata$log.Isdp
    newdata$predict.unimodal.ten <-
      exp(
        predict.model.avg(
          dd = unimodal.dredge10[[surfval[j]]],
          model.max = unimodal.max10[[surfval[j]]],
          newdata = newdata
        )
      )
    lines(
      predict.unimodal.ten ~ Isdp,
      data = newdata,
      lwd = 3,
      col = cols[i]
    )
    
    newdata <-
      data.frame(
        Isdp = seq(min(plotdat9$Isdp), max(plotdat9$Isdp), length = 200),
        WS = WSval[i],
        RH = 80,
        surface = surfval[j]
      )
    newdata$log.Isdp <-
      drop(scale(
        log(newdata$Isdp),
        center = attr(data.relatp$log.Isdp, "scaled:center"),
        scale = attr(data.relatp$log.Isdp, "scaled:scale")
      ))
    newdata$log.Isdp2 <- newdata$log.Isdp
    newdata$predict.unimodal.nine <-
      exp(
        predict.model.avg(
          dd = unimodal.dredge9[[surfval[j]]],
          model.max = unimodal.max9[[surfval[j]]],
          newdata = newdata
        )
      )
    lines(
      predict.unimodal.nine ~ Isdp,
      data = newdata,
      lwd = 3,
      lty = 2,
      col = cols[i]
    )
  }
}
par(opar)
dev.off()
source("./starters/work_env_cleaner.R")