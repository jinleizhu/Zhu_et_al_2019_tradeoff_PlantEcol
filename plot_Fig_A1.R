# APPENDIX ####
source("./starters/plot_starter.R")
## Fig. A1:
png(filename = "./plots/Fig.A1.png", width = 14, height = 7, units = "in", res = 300, bg = "white")
par(mar = c(0, 0, 0, 0) + 0.1)
layout(matrix(1:2, 1, 2, byrow = T))
# panel d of Fig. A1:
plot(1:20, type = "n", xlim = c(0, 20), ylim = c(3, 19), xlab = "", ylab = "", axes = FALSE)
segments(rep(0, 2), c(5, 15), rep(20, 2), c(5, 15), col = "black", lwd = 2)
arrows(0, 7, x1 = 2, angle = 30, lwd = 2)
arrows(0, 13, x1 = 2, angle = 30, lwd = 2, col = "black")
text(0, 10, "Horizontal wind", adj = 0, cex = 2, col = "black")
segments(9, 18, y1 = 10, col = "black", lwd = 2)
segments(9, 10, x1 = 8.8, col = "black")
text(9, 18, "Pitot tube", pos = 2, cex = 2, col = "black")
segments(11, 17, y1 = 9, col = "black", lwd = 2)
segments(11.1, 17, y1 = 9, col = "black", lwd = 2)
text(11.1, 17, "Release tube", pos = 4, cex = 2)
arrows(11.5, 5, y1 = 9, length = 0.1, angle = 90, code = 3, col = "black", lwd = 2)
arrows(11.5, 5, y1 = 9, length = 0.1, angle = 30, code = 3, col = "black", lwd = 2)
text(11.5, 7, "Release height", pos = 2, cex = 2)
myfunc <- function(x) {
  y <- - 4 * x^2 / 49 + 88 * x / 49 - 43 / 49
  return(y)
}
curve(myfunc, 11, 18, n = 200, add = T, col = "black")
arrows(11, 4.5, x1 = 18, length = 0.1, angle = 90, code = 3, col = "black", lwd = 2)
arrows(11, 4.5, x1 = 18, length = 0.1, angle = 30, code = 3, col = "black", lwd = 2)
text(13, 4.5, "Primary dispersal distance", pos = 1, cex = 2, col = "black")
text(0, 18, "(D)", pos = 4, cex = 2)

# panel e of Fig. A1:
plot(1:20, type = "n", xlim = c(0, 20), ylim = c(3, 19), xlab = "", ylab = "", axes = FALSE)
segments(rep(0, 2), c(5, 15), rep(20, 2), c(5, 15), col = "black", lwd = 2)
arrows(0, 7, x1 = 2, angle = 30, col = "black", lwd = 2)
arrows(0, 13, x1 = 2, angle = 30, col = "black", lwd = 2)
text(0, 10, "Horizontal wind", adj = 0, col = "black", cex = 2)
segments(9, 18, y1 = 10, col = "black", lwd = 2)
segments(9, 10, x1 = 8.8, col = "black", lwd = 2)
text(9, 18, "Pitot tube", pos = 2, col = "black", cex = 2)
points(10, 5.2, pty = 16, col = "black", cex = 2)
circle <- function(x) {
  y <- 5.2 + sqrt(0.09 - (x - 10)^2)
  return(y)
}
curve(circle, 9.71, 10, n = 200, add = TRUE, col = "black")
arrows(10, 5.5, x1 = 10.1, angle = 30, length = 0.1, col = "black", lwd = 2)
text(10, 5.6, "Seed", pos = 3, col = "black", cex = 2)
text(0, 18, "(E)", pos = 4, cex = 2)
par(opar)
dev.off()