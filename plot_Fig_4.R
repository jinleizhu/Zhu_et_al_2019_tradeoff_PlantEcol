# This script plots the correlations between various factors only on brick surface ####
source("./starters/plot_starter.R")
source("./starters/figure4_data_preparer.R")
png(
  filename = "./plots/Figure4.png",
  width = 8,
  height = 12,
  units = "in",
  res = 300
)
pchvals <- 1:10
par(
  bty = "l",
  mfrow = c(3, 2),
  mar = c(5, 7, 4, 2) + 0.1,
  mgp = c(4, 1, 0),
  las = 1,
  tck = 0.03
)
# panel a:
plot(
  Uref ~ terminal.velocity,
  data = dat.ab,
  xlim = c(floor(10 * min(
    dat.ab$terminal.velocity
  )), ceiling(10 * max(
    dat.ab$terminal.velocity
  ))) / 10,
  ylim = c(floor(10 * min(dat.ab$Uref)), ceiling(10 * max(dat.ab$Uref))) /
    10,
  log = "xy",
  pch = pchvals[dat.ab$type],
  xlab = expression("Terminal velocity " * italic(" V"["t"]) * " (m " * s ^
                      -1 * ")"),
  ylab = expression("Reference velocity " * italic(" U"["ref"]) * " (m " * s ^
                      -1 * ")"),
  cex = 2.1,
  cex.axis = 2.1,
  cex.lab = 2.1
)
title(main = "(a)",
      cex.main = 2.7,
      adj = 0)

# panel b:
plot(
  lift.off.velocity ~ vh.ratio,
  data = dat.ab,
  xlim = c(floor(10 * min(dat.ab$vh.ratio)), ceiling(10 * max(dat.ab$vh.ratio))) /
    10,
  ylim = c(floor(10 * min(
    dat.ab$lift.off.velocity
  )), ceiling(10 * max(
    dat.ab$lift.off.velocity
  ))) / 10,
  pch = pchvals[dat.ab$type],
  # col = cols[surface],
  xlab = expression(italic("v/h") * "-ratio"),
  ylab = expression("Lift-off velocity " * italic(" U"["lift"]) * " (m " * s ^
                      -1 * ")"),
  cex = 2.1,
  cex.axis = 2.1,
  cex.lab = 2.1
)
title(main = "(b)",
      cex.main = 2.7,
      adj = 0)

# panel c:
plot(
  p.b ~ vh.ratio,
  data = data.morp,
  xlim = c(floor(10 * min(data.morp$vh.ratio)), ceiling(10 * max(data.morp$vh.ratio))) /
    10,
  ylim = c(floor(100 * min(data.morp$p.b)), ceiling(100 * max(data.morp$p.b))) /
    100,
  log = "xy",
  yaxt = "n",
  pch = pchvals[data.morp$type],
  xlab = expression(italic("v/h") * "-ratio"),
  ylab = expression("Wind interception parameter " * italic(" p")),
  cex = 2.1,
  cex.axis = 2.1,
  cex.lab = 2.1
)
axis(
  2,
  at = c(0.3, 0.4, 0.5, 0.6),
  labels = as.character(c(0.3, 0.4, 0.5, 0.6)),
  cex.axis = 2.1,
  cex.lab = 2.1
)
title(main = "(c)",
      cex.main = 2.7,
      adj = 0)

# panel d:
plot(
  terminal.velocity ~ vh.ratio,
  data = data.morp,
  xlim = c(floor(10 * min(data.morp$vh.ratio)), ceiling(10 * max(data.morp$vh.ratio))) /
    10,
  ylim = c(floor(100 * min(
    data.morp$terminal.velocity
  )), ceiling(100 * max(
    data.morp$terminal.velocity
  ))) / 100,
  log = "xy",
  pch = pchvals[data.morp$type],
  xlab = expression(italic("v/h") * "-ratio"),
  ylab = expression("Terminal velocity " * italic(" V"["t"]) * " (m " * s ^
                      -1 * ")"),
  cex = 2.1,
  cex.axis = 2.1,
  cex.lab = 2.1
)
title(main = "(d)",
      cex.main = 2.7,
      adj = 0)

# panel e:
plot(
  wing.loading ~ vh.ratio,
  data = data.morp,
  xlim = c(floor(10 * min(data.morp$vh.ratio)), ceiling(10 * max(data.morp$vh.ratio))) /
    10,
  ylim = c(floor(100 * min(
    data.morp$wing.loading
  )), ceiling(100 * max(
    data.morp$wing.loading
  ))) / 100,
  log = "xy",
  pch = pchvals[data.morp$type],
  xlab = expression(italic("v/h") * "-ratio"),
  ylab = expression("Wing loading (mg " * mm ^ -2 * ")"),
  cex = 2.1,
  cex.axis = 2.1,
  cex.lab = 2.1
)
title(main = "(e)",
      cex.main = 2.7,
      adj = 0)

# legend:
plot.new()
legend(
  "topleft",
  legend = expression(
    italic("Zygophyllum xanthoxylon") * " (disc)",
    italic("Z. xanthoxylon") * " (" * lambda * "-type)",
    italic("Z. xanthoxylon") * " (T-type)",
    italic("Z. xanthoxylon") * " (Y-type)",
    italic("Z. xanthoxylon") * " (four-winged)",
    italic("Calligonum leucocladum") * " (wing)",
    italic("C. alaschanicum") * " (bristle)",
    italic("C. arborescens") * " (bristle)",
    italic("C. klementzii") * " (bristle + wing)",
    italic("C. rubicundum") * " (bristle + wing)"
  ),
  bty = "n",
  pch = pchvals,
  cex = 1.6,
  xjust = 0,
  y.intersp = 1.1
)
par(opar)
dev.off()
source("./starters/work_env_cleaner.R")