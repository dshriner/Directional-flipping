#Daniel Shriner
#February 27, 2024
#failure to replicate as an outcome of heterogeneity of phenotypic variance

library(pwr)
var.CFS <- 101.657
var.MESA <- 34.66308
pwr.r.test(1000,sqrt(0.005),0.05)$power
pwr.r.test(1000,sqrt(0.005*var.CFS/var.MESA),0.05)$power
pwr.r.test(1000,sqrt(0.005*var.MESA/var.CFS),0.05)$power
