# One-Sample T-test
# One sample T-test is used to compare the population mean to a sample.
# This test is similar to the Z test as it checks the same thing.
# The difference between these two tests is that the Z test needs population standard deviation
# but the T-test needs sample standard deviation. If the sample size is greater than 30/40 .
# One sample T-test is always used regardless of the known population standard deviation.

# Load the packages
import numpy as np
from random import randint
import statistics as stats
import scipy.stats as sstats
# from scipy import stats

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
s = stats.stdev(sample)
n = len(sample)

# Calculation t-statistics
tstats  =  (x - mu)/(s / np.sqrt(n))

# Degrees of Freedom
df = len(sample) - 1

# Calculating Prob
p = 1 - sstats.t.cdf(tstats, df)

# Define alpha
alpha = 0.05

# Result
if p < alpha:
   print("Reject Null Hypothesis")
else:
   print("Failed to Reject Null Hypothesis")
