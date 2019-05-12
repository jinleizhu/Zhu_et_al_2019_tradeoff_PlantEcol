# correlation tests between variables in Fig.4####
source("./starters/figure4_data_preparer.R")
names(data.uref.ulift)

# panel a:####
# between reference velocity and terminal velocity:
y <- subset(data.uref.ulift, surface == "brick")$Uref
x <- subset(data.uref.ulift, surface == "brick")$terminal.velocity
cor.test(y, x)
# Pearson's product-moment correlation
# 
# data:  y and x
# t = -1.2822, df = 8, p-value = 0.2357
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.8274163  0.2928725
# sample estimates:
#       cor 
# -0.412891

y <- subset(data.uref.ulift, surface == "sand")$Uref
x <- subset(data.uref.ulift, surface == "sand")$terminal.velocity
cor.test(y, x)
# Pearson's product-moment correlation
# 
# data:  y and x
# t = -1.5468, df = 8, p-value = 0.1605
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.8520392  0.2146548
# sample estimates:
# cor 
# -0.4798204

y <- subset(data.uref.ulift, surface == "loam")$Uref
x <- subset(data.uref.ulift, surface == "loam")$terminal.velocity
cor.test(y, x)
# Pearson's product-moment correlation
# 
# data:  y and x
# t = -0.3148, df = 8, p-value = 0.761
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.6920443  0.5578629
# sample estimates:
#        cor 
# -0.1106171

y <- subset(data.uref.ulift, surface == "gravel")$Uref
x <- subset(data.uref.ulift, surface == "gravel")$terminal.velocity
cor.test(y, x)
# Pearson's product-moment correlation
# 
# data:  y and x
# t = 0.59903, df = 8, p-value = 0.5657
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.4858102  0.7402499
# sample estimates:
#       cor 
# 0.2071917

# panel b:####
# between lift-off velocity and v-h ratio:
y <- subset(data.uref.ulift, surface == "brick")$lift.off.velocity
x <- subset(data.uref.ulift, surface == "brick")$vh.ratio
cor.test(y, x)
# Pearson's product-moment correlation
# 
# data:  y and x
# t = 0.070574, df = 8, p-value = 0.9455
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.6143307  0.6444489
# sample estimates:
#        cor 
# 0.02494382

y <- subset(data.uref.ulift, surface == "sand")$lift.off.velocity
x <- subset(data.uref.ulift, surface == "sand")$vh.ratio
cor.test(y, x)
# Pearson's product-moment correlation
# 
# data:  y and x
# t = -0.10575, df = 8, p-value = 0.9184
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.6516592  0.6065318
# sample estimates:
#   cor 
# -0.03736304

y <- subset(data.uref.ulift, surface == "loam")$lift.off.velocity
x <- subset(data.uref.ulift, surface == "loam")$vh.ratio
cor.test(y, x)
# Pearson's product-moment correlation
# 
# data:  y and x
# t = 3.3945, df = 8, p-value = 0.009436
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.2685231 0.9421518
# sample estimates:
#       cor 
# 0.7682603

y <- subset(data.uref.ulift, surface == "gravel")$lift.off.velocity
x <- subset(data.uref.ulift, surface == "gravel")$vh.ratio
cor.test(y, x)
# Pearson's product-moment correlation
# 
# data:  y and x
# t = 3.175, df = 8, p-value = 0.0131
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.2209219 0.9361820
# sample estimates:
#       cor 
# 0.7466856

# panel c:####
# between wind interception parameter and v-h ratio:
names(data.morp)
x <- data.morp$vh.ratio
# on brick:
y <- data.morp$p.b
cor.test(y, x)
# Pearson's product-moment correlation
# 
# data:  y and x
# t = 23.247, df = 378, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.7222140 0.8055224
# sample estimates:
# cor 
# 0.7670826

# on sand:
y <- data.morp$p.s
cor.test(y, x)
# Pearson's product-moment correlation
# 
# data:  y and x
# t = 23.245, df = 378, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.7221937 0.8055075
# sample estimates:
#       cor 
# 0.7670652

# on loam:
y <- data.morp$p.l
cor.test(y, x)
# Pearson's product-moment correlation
# 
# data:  y and x
# t = 23.131, df = 378, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.7203776 0.8041740
# sample estimates:
#       cor 
# 0.7655024

# on gravel:
y <- data.morp$p.g
cor.test(y, x)
# Pearson's product-moment correlation
# 
# data:  y and x
# t = 23.134, df = 378, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.7204289 0.8042117
# sample estimates:
#       cor 
# 0.7655466

# panel d:####
# between terminal velocity and v-h ratio:
y <- data.morp$terminal.velocity
x <- data.morp$vh.ratio
cor.test(y, x)
# Pearson's product-moment correlation
# 
# data:  y and x
# t = 22.58, df = 378, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.7114362 0.7975970
# sample estimates:
# cor 
# 0.7578009

# panel e:####
# between wing loading and v-h ratio:
y <- data.morp$wing.loading
x <- data.morp$vh.ratio
cor.test(y, x)
# Pearson's product-moment correlation
# 
# data:  y and x
# t = 21.88, df = 378, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.6995263 0.7888044
# sample estimates:
#       cor 
# 0.7475223