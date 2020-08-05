# One-Sample Z test
# One sample Z test is used to compare the population mean to a sample.
# Population standard deviation must be known to perform the Z test and data should be normally distributed.

import numpy as np
from random import randint
import statistics as stats
from scipy.stats import norm

# Genrating a random population
population  = []
for i in range(0,200):
  n = randint(1,1000)
  population.append(n)

# Generating a random sample
sample = population[0:30]

# Sample and Population Mean
x = stats.mean(sample)
mu = stats.mean(population)

# Standard Deviation of Population  and Sample Size
s = stats.stdev(population)
n = len(sample)

# Calculation Z-statistics
z =  (x - mu)/(s / np.sqrt(n))

# Calculating Prob
p = 1 - norm.cdf(z)

# Define alpha
alpha = 0.05

# Result
if p < alpha:
   print("Reject Null Hypothesis")
else:
   print("Failed to Reject Null Hypothesis")
