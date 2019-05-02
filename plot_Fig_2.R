# plot Figure 2:####
source("./starters/plot_starter.R")
source("./starters/figure2_data_preparer.R")
png(
  filename = "./plots/Figure2.png",
  width = 24,
  height = 18,
  units = "in",
  res = 300
)
par(
  mfrow = c(2, 2),
  bty = "l",
  las = 1,
  tck = 0.02,
  mar = c(8, 10, 8, 2) + 0.1,
  mgp = c(5.1, 0, 0)
)
colfunc <- colorRampPalette(c("red", "blue"))
cols10 <- colfunc(10)
const <- rep(1, 10)
pchvals <- 1:10
ltyvals <- c(1:5, 1:5)

# panel a:####
with(
  data.prim.mean.se,
  plot(
    x = wind.speed * const[type],
    y = distance.ws,
    log = "xy",
    pch = pchvals[type],
    col = cols10[type],
    xaxt = "n",
    yaxt = "n",
    xlab = expression("Wind speed (m " * s ^ -1 * ")"),
    ylab = "Primary dispersal distance (m)",
    ylim = c(0.92, 6),
    cex = 6,
    cex.axis = 4,
    cex.lab = 3.5
  )
)
axis(
  1,
  at = c(4, 6, 8, 10),
  labels = c("4", "6", "8", "10"),
  cex.axis = 3.5,
  cex.lab = 3.5,
  padj = 1
)
axis(
  2,
  at = c(1, 2, 3, 4, 5, 6),
  labels = c("1", "2", "3", "4", "5", "6"),
  cex.axis = 3.5,
  cex.lab = 3.5,
  hadj = 2
)
with(
  data.prim.mean.se,
  arrows(
    wind.speed * const[type],
    distance.ws + se.ws,
    wind.speed * const[type],
    distance.ws - se.ws,
    col = cols10[type],
    code = 3,
    angle = 90,
    length = 0.05,
    lwd = 4
  )
)
for (i in 1:10) {
  with(
    subset(data.prim.mean.se, type == levels(data.prim.mean.se$type)[i]),
    lines(
      x = wind.speed[order(wind.speed, decreasing = T)],
      y = distance.ws[order(wind.speed, decreasing = T)],
      lwd = 4,
      lty = ltyvals[type],
      col = cols10[i],
      type = "o"
    )
  )
}
title("(a)", cex.main = 5, adj = 0)

# panel b:####
with(
  data.prim.mean.se,
  plot(
    x = release.height * const[type],
    y = distance.rh,
    log = "xy",
    pch = pchvals[type],
    col = cols10[type],
    xaxt = "n",
    yaxt = "n",
    xlab = "Release height (cm)",
    ylab = "Primary dispersal distance (m)",
    ylim = c(0.75, 5),
    cex = 6,
    cex.axis = 4,
    cex.lab = 3.5
  )
)
axis(
  1,
  at = c(40, 60, 80, 100),
  labels = c("40", "60", "80", "100"),
  cex.axis = 3.5,
  cex.lab = 3.5,
  padj = 1
)
axis(
  2,
  at = c(1, 2, 3, 4, 5),
  labels = c("1", "2", "3", "4", "5"),
  cex.axis = 3.5,
  cex.lab = 3.5,
  hadj = 2
)
with(
  data.prim.mean.se,
  arrows(
    release.height * const[type],
    distance.rh + se.rh,
    release.height * const[type],
    distance.rh - se.rh,
    col = cols10[type],
    code = 3,
    angle = 90,
    length = 0.05,
    lwd = 4
  )
)
for (i in 1:10) {
  with(
    subset(data.prim.mean.se, type == levels(data.prim.mean.se$type)[i]),
    lines(
      x = release.height[order(release.height, decreasing = T)],
      y = distance.rh[order(release.height, decreasing = T)],
      lwd = 4,
      lty = ltyvals[type],
      col = cols10[i],
      type = "o"
    )
  )
}
title("(b)", cex.main = 5, adj = 0)

# panel c:####
const1 <- c(1:4)
with(
  data.sec.mean.se.Isdp,
  plot(
    x = const1[surface] * const[type],
    y = mean.Isdp,
    log = "y",
    lty = ltyvals[type],
    pch = pchvals[type],
    col = cols10[type],
    xaxt = "n",
    yaxt = "n",
    xlab = "",
    ylab = expression("Secondary dispersal potential (s " * m ^ -1 * ")"),
    cex = 6,
    cex.axis = 4,
    cex.lab = 3.5
  )
)
axis(
  1,
  at = c(1:4),
  labels = levels(data.sec.mean.se.Isdp$surface),
  cex.axis = 3.5,
  cex.lab = 3.5,
  padj = 1
)
axis(
  2,
  at = c(0.15, 0.25, 0.35, 0.45),
  labels = c("0.15", "0.25", "0.35", "0.45"),
  cex.axis = 3.5,
  cex.lab = 3.5,
  hadj = 1
)
with(
  data.sec.mean.se.Isdp,
  arrows(
    const1[surface] * const[type],
    mean.Isdp + se.Isdp,
    const1[surface] * const[type],
    mean.Isdp - se.Isdp,
    col = cols10[type],
    code = 3,
    angle = 90,
    length = 0.05,
    lwd = 4
  )
)
for (i in 1:10) {
  with(
    subset(
      data.sec.mean.se.Isdp,
      type == levels(data.sec.mean.se.Isdp$type)[i]
    ),
    lines(
      x = surface[order(surface, decreasing = T)],
      y = mean.Isdp[order(surface, decreasing = T)],
      lwd = 4,
      lty = ltyvals[type],
      col = cols10[i],
      type = "o"
    )
  )
}
title("(c)", cex.main = 5, adj = 0)

# legend:
plot.new()
legend(
  "topleft",
  expression(
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
  fill = cols10,
  bty = "n",
  pch = pchvals,
  lty = ltyvals,
  lwd = 3,
  cex = 3.5
)
par(opar)
dev.off()
source("./starters/work_env_cleaner.R")