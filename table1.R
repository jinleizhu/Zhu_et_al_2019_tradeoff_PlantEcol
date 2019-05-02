# TABLE 1:####
# calculate the mean and SE of aerodynamic characteristics of 10 seed morphologies
# load the function to calculate SE:
source("./starters/analysis_starter.R")
source("./starters/seed_morp_data_preparer.R")
data.morp$type <- factor(data.morp$type,
                         levels = morph.levels.new)
disptraits.mean <-
  aggregate(data.morp[, c(2:9)], by = list(type = data.morp$type), mean)
disptraits.se <-
  aggregate(data.morp[, c(2:9)], by = list(type = data.morp$type), se)

disptraits.mean
#                       type   length    width   height      mass      area wing.loading terminal.velocity  vh.ratio
# 1                     disc 24.23950 20.51300  3.40050 0.0578500 3.7708650    0.1553196          1.357476 0.1523274
# 2                   y-type 25.62250 21.47100  9.08500 0.0707500 4.3276400    0.1634725          1.319015 0.3862666
# 3                   T-type 28.66250 23.19625 14.95425 0.1031250 5.4051400    0.1911503          1.386166 0.5777891
# 4                   Y-type 29.47700 23.13675 18.98050 0.1343750 5.5408525    0.2448354          1.481893 0.7277679
# 5              four-winged 25.16275 18.80950 14.87175 0.1067750 3.7427700    0.2863131          1.567095 0.6776750
# 6   Calligonum arborescens 23.08675 19.12650 18.94625 0.1267250 2.2051475    0.5731619          2.091820 0.8983753
# 7  Calligonum alaschanicum 14.19850 13.37375 13.04600 0.0418100 0.9001225    0.4668870          2.054648 0.9475447
# 8    Calligonum klementzii 22.35825 21.36425 21.18950 0.0884750 1.3309700    0.6749695          1.855976 0.9719399
# 9    Calligonum rubicundum 19.80500 17.51050 15.98025 0.1281500 2.2068650    0.5942120          1.990332 0.8570174
# 10  Calligonum leucocladum 15.12575 12.21125 10.88325 0.0530775 1.4723525    0.3634035          1.734574 0.7977044
disptraits.se
#                       type    length     width    height        mass       area wing.loading terminal.velocity    vh.ratio
# 1                     disc 0.5126584 0.5971018 0.1848932 0.002742718 0.16013771  0.006614167        0.02681423 0.008135934
# 2                   y-type 0.3918531 0.3158915 0.3515730 0.003441033 0.09753433  0.006083745        0.01954677 0.014311626
# 3                   T-type 0.5195834 0.3914067 0.3088793 0.004761651 0.18332563  0.006238196        0.01942863 0.009301318
# 4                   Y-type 0.6506208 0.5110854 0.3714856 0.005238502 0.20949050  0.005613068        0.01598759 0.014354136
# 5              four-winged 0.4711164 0.4323595 0.4093957 0.005107371 0.14821498  0.009466651        0.02018348 0.014553148
# 6   Calligonum arborescens 0.2954960 0.2192924 0.2569996 0.004117310 0.05987877  0.007790848        0.01071985 0.008765474
# 7  Calligonum alaschanicum 0.2621686 0.2445563 0.2388492 0.001510696 0.02968585  0.009646231        0.01636393 0.009646802
# 8    Calligonum klementzii 0.5208195 0.4530925 0.4326761 0.002711856 0.04613392  0.016259658        0.01362632 0.010725065
# 9    Calligonum rubicundum 0.2241520 0.3318165 0.3411764 0.003319629 0.05560740  0.020597119        0.01231687 0.015090069
# 10  Calligonum leucocladum 0.1498524 0.1866806 0.1882396 0.001584033 0.03449311  0.009860733        0.01388925 0.013468019