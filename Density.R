# Generate random data from a uniform distribution
my_data=runif(100,-100,100)

library("ggpubr")
ggdensity(my_data, 
  main = "Density plot of random data",
  xlab = "Metric")
ggqqplot(my_data)

# Shapiro-Wilk test of normality
# Statistical significance indicates difference from normality
shapiro.test(my_data)

# Kolmogorov-Smirnov Test
# H0: samples are the same distribution
ks.test(my_data,"pnorm")

# Baumgartner-Weiss-Schindler Test
# 2 sample, symmetric
install.packages("BWStest")
require(BWStest)
bws_stat()
